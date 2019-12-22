module Chord.ChordMessage
  ( ChordMessage(..)
  ) where

import Chord.ID
import Chord.Node
import Common.Models.InfoHash

-- | Messages used to communicate between nodes
data ChordMessage
  = FingerTableQuery
  | FingerTableResponse [Node]
  | GetValuesQuery ID
  | GetValuesResponse ID [InfoHash]
  | AddValueQuery ID InfoHash
