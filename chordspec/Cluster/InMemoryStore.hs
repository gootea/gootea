module Cluster.InMemoryStore
  ( InMemoryStore
  , newStore
  ) where

import Chord.ChordMessage
import Chord.ID
import Chord.Store

import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe

-- InMemoryStore
data InMemoryStore v =
  InMemoryStore (M.Map ID [v])

instance Store InMemoryStore where
  addToStore target value (InMemoryStore map) =
    InMemoryStore $ M.alter addOrAppend target map
    where
      addOrAppend Nothing = Just [value]
      addOrAppend (Just existing) = Just (value : existing)
  getFromStore target (InMemoryStore map) = fromMaybe [] $ M.lookup target map

newStore :: InMemoryStore v
newStore = InMemoryStore M.empty
