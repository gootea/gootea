module Chord.Store
  ( Store(..)
  ) where

import Chord.ID

class Store s where
  addToStore :: ID -> v -> s v -> s v
  getFromStore :: ID -> s v -> [v]
