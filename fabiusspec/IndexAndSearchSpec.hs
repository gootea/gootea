{-# LANGUAGE OverloadedStrings #-}

module IndexAndSearchSpec where

import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Fabius.Indexer as I
import Common.Models.InfoHash
import Fabius.Key
import Fabius.Searcher
import Fabius.Term
import Fabius.Torrent

spec :: Spec
spec = do
  describe "Index a set of torrents and search them" $ do
    it "test1" search1
    it "test2" search2

search1 :: Assertion
search1 =
  let store =
        newStore
          [newKey $ newTerm "t6"]
          [torrent1, torrent2, torrent3, torrent4, torrent5, torrent6]
      test query torrents =
        assertEqual (show query) (S.fromList torrents) (search query store)
   in do test "t1" [torrent1]
         test "t6" [] -- Because "t6" is non discriminative
         test "t1 t6" [torrent1]
         test "t4 t6" [torrent1, torrent2, torrent3, torrent4]
         test "t2 t3" [torrent1, torrent2]
         test "toto" []

search2 :: Assertion
search2 =
  let store =
        newStore
          (newKey <$> newTerm <$> ["t1", "t2", "t3", "t4", "t5", "t6"])
          [torrent1, torrent2, torrent3, torrent4, torrent5, torrent6]
      test query torrents =
        assertEqual (show query) (S.fromList torrents) (search query store)
   in do test "t1 t6" [torrent1]
         test "t2 t3" [torrent1, torrent2]
         test "t6 t5 t4 t3" [torrent1, torrent2, torrent3]
         test "toto" []

-- Helpers
data Store =
  Store
    { ihByKeys :: M.Map Key (S.Set InfoHash)
    , torrentByIH :: M.Map InfoHash Torrent
    , nonDiscriminativeKeys :: S.Set Key
    }
  deriving (Show)

newStore :: [Key] -> [Torrent] -> Store
newStore ndks torrents =
  foldr addTorrent (Store M.empty M.empty (S.fromList ndks)) torrents

addTorrent :: Torrent -> Store -> Store
addTorrent torrent store =
  store
    { ihByKeys =
        foldr insertIntoStore (ihByKeys store) $
        listKeysToIndex (nonDiscriminativeKeys store) torrent
    , torrentByIH = M.insert ih torrent $ torrentByIH store
    }
  where
    ih = torrentInfoHash torrent
    insertIntoStore k m = M.alter alterFn k m
    alterFn (Just s) = Just $ S.insert ih s
    alterFn Nothing = Just $ S.singleton ih

listKeysToIndex :: S.Set Key -> Torrent -> [Key]
listKeysToIndex nonDiscriminativeKeys torrent =
  S.toList $
  I.keys $ updateIndexer initialIndexer (S.toList $ I.keys initialIndexer)
  where
    initialIndexer = I.fromTorrent torrent
    updateIndexer i [] = i
    updateIndexer i (firstKey:rest) =
      if S.member firstKey nonDiscriminativeKeys
        then let (newI, newKeys) = I.reportNonDiscriminativeKey i firstKey
              in updateIndexer newI (rest ++ newKeys)
        else updateIndexer i rest
      where


search :: T.Text -> Store -> S.Set Torrent
search query store =
  S.fromList $
  fromMaybe [] $ getResults $ uncurry processSearch $ newSearcher query
  where
    ndks = nonDiscriminativeKeys store
    processSearch :: Searcher -> [Output] -> Searcher
    processSearch s [] = s
    processSearch s ((KeyRequest key):rest) =
      if S.member key ndks
        then let (newS, newKeys) = reportNonDiscriminativeKey s key
              in processSearch newS (rest ++ newKeys)
        else case M.lookup key $ ihByKeys store of
               Just infoHashes -> processSearch newSearch (rest ++ outputs)
                 where (newSearch, outputs) = handleKeyResults s key infoHashes
               Nothing -> processSearch s rest
    processSearch s ((InfoHashRequest ih):rest) =
      case M.lookup ih $ torrentByIH store of
        Just torrent -> processSearch (handleInfoHashResult s ih torrent) rest
        Nothing -> processSearch s rest

-- Fixtures
torrent1 = newTorrent (newInfoHash "torrent1") "t1 t2 t3 t4 t5 t6"

torrent2 = newTorrent (newInfoHash "torrent2") "   t2 t3 t4 t5 t6"

torrent3 = newTorrent (newInfoHash "torrent3") "      t3 t4 t5 t6"

torrent4 = newTorrent (newInfoHash "torrent4") "         t4 t5 t6"

torrent5 = newTorrent (newInfoHash "torrent5") "            t5 t6"

torrent6 = newTorrent (newInfoHash "torrent6") "               t6"
