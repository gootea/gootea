{-# LANGUAGE OverloadedStrings #-}

module Fabius.HDKMap
  ( HDKMap
  , fromTerms
  , keys
  , handleKeyValues
  , reportNonDiscriminativeKey
  , foldr
  , elems
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Fabius.Key
import Fabius.Term
import Prelude hiding (foldr, map)
import qualified Prelude as P

-- | HDKMap is a discriminative key index. Given a set of keys it will keep
-- information for highly discriminative keys only.
data HDKMap v =
  HDKMap
    { initialTerms :: S.Set Term
    , nonHDK :: S.Set Key -- Non Highly Discriminative Keys
    , map :: M.Map Key v
    }

-- | Create a new Search
fromTerms :: v -> S.Set Term -> HDKMap v
fromTerms defaultValue terms =
  HDKMap
    { initialTerms = terms
    , nonHDK = S.empty
    , map = M.fromSet (const defaultValue) $ S.map newKey terms
    }

-- | Return all keys (considered as highly discriminative so far)
keys :: HDKMap v -> S.Set Key
keys = M.keysSet . map

-- | Update a value at a specific key
handleKeyValues :: HDKMap v -> Key -> v -> HDKMap v
handleKeyValues dki key value = dki {map = M.adjust (const value) key $ map dki}

-- | Report a key as non discriminative
reportNonDiscriminativeKey :: HDKMap v -> v -> Key -> (HDKMap v, [Key])
reportNonDiscriminativeKey hdkmap defaultValue key =
  if M.member key $ map hdkmap
    then ( hdkmap {map = newValues, nonHDK = S.insert key $ nonHDK hdkmap}
         , newKeys)
    else (hdkmap, [])
  where
    withoutKey = M.delete key $ map hdkmap
    newValues = P.foldr (\k m -> M.insert k defaultValue m) withoutKey newKeys
    newKeys =
      S.toList $
      removeKnownNonHDK $
      removeExistingKeys $
      S.map (mappend key) $
      S.map newKey $ S.difference (initialTerms hdkmap) (keyTerms key)
    removeExistingKeys set = S.difference set (M.keysSet $ map hdkmap)
    removeKnownNonHDK set = S.difference set (nonHDK hdkmap)

-- | Fold the values in the map using the given right-associative binary
-- operator
foldr :: (v -> b -> b) -> b -> HDKMap v -> b
foldr f i = M.foldr f i . map

-- | Return all elements of the map
elems :: HDKMap v -> [v]
elems = M.elems . map
