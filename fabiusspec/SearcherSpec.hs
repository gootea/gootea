{-# LANGUAGE OverloadedStrings #-}

module SearcherSpec where

import Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Common.Models.InfoHash
import Fabius.Key
import Fabius.Searcher
import Fabius.Term
import Fabius.Torrent

spec :: Spec
spec = do
  describe "Search when every term is a highly discriminative key" $ do
    it "test1" search1
    it "test2" search2
    it "test3" search3
    it "test4" search4
  describe "Search with non discriminative keys" $ do
    it "NDK test1" searchWithNDK1
    it "NDK test2" searchWithNDK2

search1 :: Assertion
search1 = doSearchTest terms S.empty "term1 term2 term3" [torrent1, torrent2]
  where
    terms =
      M.fromList
        [ (key1, [ih1, ih2, ih3, ih4, ih5, ih6, ih7, ih8])
        , (key2, [ih1, ih2, ih3, ih4, ih9, ih10, ih11, ih12])
        , (key3, [ih1, ih2])
        ]

search2 :: Assertion
search2 =
  doSearchTest
    terms
    S.empty
    "term1 term2"
    [torrent1, torrent2, torrent3, torrent4]
  where
    terms =
      M.fromList
        [ (key1, [ih1, ih2, ih3, ih4, ih5, ih6, ih7, ih8])
        , (key2, [ih1, ih2, ih3, ih4, ih9, ih10, ih11, ih12])
        ]

search3 :: Assertion
search3 = doSearchTest terms S.empty "term1 term2" []
  where
    terms =
      M.fromList
        [(key1, [ih1, ih2, ih3, ih4, ih5, ih6, ih7, ih8]), (key2, [ih11, ih12])]

search4 :: Assertion
search4 = doSearchTest terms S.empty "term1 term2" []
  where
    terms = M.fromList [(key1, []), (key2, [ih11, ih12])]

searchWithNDK1 :: Assertion
searchWithNDK1 = doSearchTest results ndkTerms "term1 term2 term3" [torrent1]
  where
    ndkTerms = S.fromList [key1, key2]
    results =
      M.fromList
        [ (key1 <> key2, [ih1, ih3])
        , (key1 <> key3, [ih1, ih2, ih3])
        , (key2 <> key3, [ih1, ih4])
        , (key3, [ih1, ih10])
        ]

searchWithNDK2 :: Assertion
searchWithNDK2 = doSearchTest results ndks "term1 term2 term3 term4" [torrent1]
  where
    ndks =
      S.fromList
        [ key1
        , key2
        , key3
        , key4
        , key1 <> key2
        , key1 <> key3
        , key1 <> key4
        , key2 <> key3
        , key2 <> key4
        , key3 <> key4
        , key1 <> key2 <> key3
        , key1 <> key2 <> key4
        , key1 <> key3 <> key4
        , key2 <> key3 <> key4
        ]
    results = M.fromList [(key1 <> key2 <> key3 <> key4, [ih1])]

-- Helpers
doSearchTest ::
     M.Map Key [InfoHash] -> S.Set Key -> T.Text -> [Torrent] -> Assertion
doSearchTest index ndks query results =
  case doSearch index ndks query of
    Just result -> S.fromList result @?= S.fromList results
    Nothing -> assertFailure "a result should be found"

doSearch :: M.Map Key [InfoHash] -> S.Set Key -> T.Text -> Maybe [Torrent]
doSearch index ndks query = processQueue queue search
  where
    (search, queue) = newSearcher query
    processQueue [] search = getResults search
    processQueue (head:tail) search =
      case head of
        KeyRequest key ->
          if S.member key ndks
            then let (newSearcher, newQueue) =
                       reportNonDiscriminativeKey search key
                  in processQueue (tail ++ newQueue) newSearcher
            else case M.lookup key index of
                   Just ihs ->
                     let (newSearcher, newQueue) =
                           handleKeyResults search key (S.fromList ihs)
                      in processQueue (tail ++ newQueue) newSearcher
                   Nothing -> processQueue tail search
        InfoHashRequest ih ->
          case M.lookup ih torrents of
            Just torrent ->
              let newSearcher = handleInfoHashResult search ih torrent
               in processQueue tail newSearcher
            Nothing -> processQueue tail search

-- Objects
ih1 = newInfoHash "ih1"

ih2 = newInfoHash "ih2"

ih3 = newInfoHash "ih3"

ih4 = newInfoHash "ih4"

ih5 = newInfoHash "ih5"

ih6 = newInfoHash "ih6"

ih7 = newInfoHash "ih7"

ih8 = newInfoHash "ih8"

ih9 = newInfoHash "ih9"

ih10 = newInfoHash "ih10"

ih11 = newInfoHash "ih11"

ih12 = newInfoHash "ih12"

ih13 = newInfoHash "ih13"

ih14 = newInfoHash "ih14"

ih15 = newInfoHash "ih15"

ih16 = newInfoHash "ih16"

ih17 = newInfoHash "ih17"

ih18 = newInfoHash "ih18"

ih19 = newInfoHash "ih19"

ih20 = newInfoHash "ih20"

torrent1 = newTorrent ih1 "torrent1"

torrent2 = newTorrent ih2 "torrent2"

torrent3 = newTorrent ih3 "torrent3"

torrent4 = newTorrent ih4 "torrent4"

torrent5 = newTorrent ih5 "torrent5"

torrent6 = newTorrent ih6 "torrent6"

torrent7 = newTorrent ih7 "torrent7"

torrent8 = newTorrent ih8 "torrent8"

torrent9 = newTorrent ih9 "torrent9"

torrent10 = newTorrent ih10 "torrent10"

torrent11 = newTorrent ih11 "torrent11"

torrent12 = newTorrent ih12 "torrent12"

torrent13 = newTorrent ih13 "torrent13"

torrent14 = newTorrent ih14 "torrent14"

torrent15 = newTorrent ih15 "torrent15"

torrent16 = newTorrent ih16 "torrent16"

torrent17 = newTorrent ih17 "torrent17"

torrent18 = newTorrent ih18 "torrent18"

torrent19 = newTorrent ih19 "torrent19"

torrent20 = newTorrent ih20 "torrent20"

term1 = newTerm "term1"

term2 = newTerm "term2"

term3 = newTerm "term3"

term4 = newTerm "term4"

key1 = newKey term1

key2 = newKey term2

key3 = newKey term3

key4 = newKey term4

torrents =
  M.fromList
    [ (ih1, torrent1)
    , (ih2, torrent2)
    , (ih3, torrent3)
    , (ih4, torrent4)
    , (ih5, torrent5)
    , (ih6, torrent6)
    , (ih7, torrent7)
    , (ih8, torrent8)
    , (ih9, torrent9)
    , (ih10, torrent10)
    , (ih11, torrent11)
    , (ih12, torrent12)
    , (ih13, torrent13)
    , (ih14, torrent14)
    , (ih15, torrent15)
    , (ih16, torrent16)
    , (ih17, torrent17)
    , (ih18, torrent18)
    , (ih19, torrent19)
    , (ih20, torrent20)
    ]
