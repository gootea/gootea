{-# LANGUAGE OverloadedStrings #-}

module Fabius.Searcher
  ( Searcher
  , Output(..)
  , newSearcher
  , handleKeyResults
  , reportNonDiscriminativeKey
  , handleInfoHashResult
  , getResults
  ) where

import qualified Data.Set as S
import qualified Data.Text as T
import Fabius.InfoHash
import Fabius.Key
import Fabius.Query
import qualified Fabius.SearchResults as SR
import Fabius.Torrent

data Searcher
  = SearchResults SR.SearchResults
  | LookupTorrents
      (S.Set InfoHash) -- InfoHash for which torrents are missing
      [Torrent] -- Torrents that have been fetched

data Output
  = KeyRequest Key
  | InfoHashRequest InfoHash
  deriving (Show)

-- | Create a new Searcher
newSearcher :: T.Text -> (Searcher, [Output])
newSearcher query = (SearchResults sr, fmap KeyRequest terms)
  where
    (sr, terms) = SR.newSearchResults $ queryFromText query

-- | Handle the response to a KeyRequest
handleKeyResults :: Searcher -> Key -> S.Set InfoHash -> (Searcher, [Output])
handleKeyResults search@(LookupTorrents _ _) _ _ = (search, [])
handleKeyResults (SearchResults sr) key hashes =
  let updatedSr = SR.handleKeyResults sr key hashes
   in case SR.getResults updatedSr of
        Nothing -> (SearchResults updatedSr, [])
        Just results -> (LookupTorrents results [], output)
          where output = fmap InfoHashRequest $ S.toList results

-- | Report that a Key is non discriminative
reportNonDiscriminativeKey :: Searcher -> Key -> (Searcher, [Output])
reportNonDiscriminativeKey search@(LookupTorrents _ _) _ = (search, [])
reportNonDiscriminativeKey (SearchResults sr) key =
  (SearchResults updatedSr, output)
  where
    (updatedSr, terms) = SR.reportNonDiscriminativeKey sr key
    output = fmap KeyRequest terms

-- | Handle the Torrent reply for a given InfoHash
handleInfoHashResult :: Searcher -> InfoHash -> Torrent -> Searcher
handleInfoHashResult search@(SearchResults _) _ _ = search
handleInfoHashResult search@(LookupTorrents missing collected) ih torrent =
  if (S.member ih missing)
    then LookupTorrents (S.delete ih missing) (torrent : collected)
    else search

getResults :: Searcher -> Maybe [Torrent]
getResults (SearchResults _) = Nothing
getResults (LookupTorrents missing results) =
  if (S.null missing)
    then Just results
    else Nothing
