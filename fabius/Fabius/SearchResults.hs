{-# LANGUAGE OverloadedStrings #-}

module Fabius.SearchResults
  ( SearchResults
  , newSearchResults
  , handleKeyResults
  , reportNonDiscriminativeKey
  , getResults
  ) where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Fabius.HDKMap as HM
import Fabius.InfoHash
import Fabius.Key
import Fabius.Query

data SearchResults =
  SearchResults (HM.HDKMap (Maybe (S.Set InfoHash)))

-- | Create a new Search
newSearchResults :: Query -> (SearchResults, [Key])
newSearchResults query = (SearchResults hdkmap, S.toList $ HM.keys hdkmap)
  where
    hdkmap = HM.fromTerms Nothing $ termsFromQuery query

-- | Handle the response to a Key
handleKeyResults :: SearchResults -> Key -> S.Set InfoHash -> SearchResults
handleKeyResults (SearchResults hdkmap) key values =
  SearchResults $ HM.handleKeyValues hdkmap key (Just values)

-- | Report that a key is non discriminative
reportNonDiscriminativeKey :: SearchResults -> Key -> (SearchResults, [Key])
reportNonDiscriminativeKey (SearchResults hdkmap) key =
  (SearchResults newMap, keys)
  where
    (newMap, keys) = HM.reportNonDiscriminativeKey hdkmap Nothing key

-- Build the list of infohash results if all terms have been found
data Fold
  = Init
  | Incomplete
  | Result (S.Set InfoHash)

getResults :: SearchResults -> Maybe (S.Set InfoHash)
getResults (SearchResults hdkmap) =
  case HM.foldr foldfn Init hdkmap of
    Init -> Nothing
    Incomplete -> Nothing
    Result results -> Just results
  where
    foldfn Nothing _ = Incomplete
    foldfn (Just s) Init = Result s
    foldfn (Just s1) (Result s2) = Result $ S.intersection s1 s2
    foldfn _ Incomplete = Incomplete
