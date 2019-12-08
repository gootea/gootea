{-# LANGUAGE OverloadedStrings #-}

module Fabius.Indexer
  ( Indexer
  , fromQuery
  , keys
  , reportNonDiscriminativeKey
  , reportKeyUpdated
  , isDone
  , fromTorrent
  ) where

import qualified Data.Set as S
import qualified Fabius.HDKMap as H
import Fabius.Key
import Fabius.Query
import Fabius.Torrent

data Indexer =
  Indexer (H.HDKMap KeyIndexState)

data KeyIndexState
  = Pending
  | Done
  deriving (Eq)

-- | Create a new Indexer from a Query
fromQuery :: Query -> Indexer
fromQuery = Indexer . H.fromTerms Pending . termsFromQuery

-- | Create a new Indexer from a Torrent
fromTorrent :: Torrent -> Indexer
fromTorrent =
  Indexer . H.fromTerms Pending . termsFromQuery . queryFromText . torrentName

-- | Return all keys (considered as highly discriminative so far)
keys :: Indexer -> S.Set Key
keys (Indexer hdkmap) = H.keys hdkmap

-- | Report a key as non discriminative
reportNonDiscriminativeKey :: Indexer -> Key -> (Indexer, [Key])
reportNonDiscriminativeKey (Indexer hdkmap) key = (Indexer newMap, newKeys)
  where
    (newMap, newKeys) = H.reportNonDiscriminativeKey hdkmap Pending key

-- | Report Key as correctly updated
reportKeyUpdated :: Indexer -> Key -> Indexer
reportKeyUpdated (Indexer hdkmap) key =
  Indexer $ H.handleKeyValues hdkmap key Done

-- | Returns True if the indexing is done for all Keys
isDone :: Indexer -> Bool
isDone (Indexer hdkmap) = all ((==) Done) $ H.elems hdkmap
