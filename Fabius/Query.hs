{-# LANGUAGE OverloadedStrings #-}

module Fabius.Query
  ( Query
  , queryFromText
  , termsFromQuery
  ) where

import qualified Data.Set as S
import qualified Data.Text as T
import Fabius.Term

newtype Query =
  Query T.Text

-- | Create a new Query from a Text
queryFromText :: T.Text -> Query
queryFromText = Query

-- | Get the Terms from a query
termsFromQuery :: Query -> S.Set Term
termsFromQuery (Query query) = S.fromList . fmap newTerm . T.splitOn " " $ query
