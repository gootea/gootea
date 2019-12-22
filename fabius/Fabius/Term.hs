{-# LANGUAGE OverloadedStrings #-}

module Fabius.Term
  ( Term
  , newTerm
  ) where

import qualified Data.Text as T

newtype Term =
  Term T.Text
  deriving (Show, Eq, Ord)

newTerm :: T.Text -> Term
newTerm = Term
