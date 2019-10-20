module Chord.ChordMessage
  ( ChordMessage(..)
  ) where

import Chord.ID
import Chord.Node

-- | Messages used to communicate between nodes
data ChordMessage v
  = FingerTableQuery
  | FingerTableResponse [Node]
  | GetValuesQuery ID
  | GetValuesResponse ID [v]
  | AddValueQuery ID v
