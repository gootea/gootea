{-# LANGUAGE OverloadedStrings #-}

module Fabius.Key
  ( Key
  , newKey
  , keyTerms
  ) where

import qualified Data.Set as S
import Fabius.Term

newtype Key =
  Key (S.Set Term)
  deriving (Show, Eq, Ord)

instance Semigroup Key where
  (Key s1) <> (Key s2) = Key (S.union s1 s2)

instance Monoid Key where
  mempty = Key S.empty

newKey :: Term -> Key
newKey = Key . S.singleton

keyTerms :: Key -> S.Set Term
keyTerms (Key terms) = terms
