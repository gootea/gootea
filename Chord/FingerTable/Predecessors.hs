module Chord.FingerTable.Predecessors
  ( Predecessors
  , newPredecessors
  , addPredecessor
  , fingerTableIsResponsibleOfID
  ) where

import Chord.ID

-- | Data holding between O and 3 predecessors
-- The ID is the reference of the FingerTable
data Predecessors i
  = NoPredecessor ID
  | OnePredecessor ID i
  | TwoPredecessors ID i i
  | ThreePredecessors ID i i i

-- | Create a new empty Predecessors object
newPredecessors :: ID -> Predecessors n
newPredecessors = NoPredecessor

-- | Insert a Node in a Predecessors
addPredecessor :: ChordID n => Predecessors n -> n -> Predecessors n
addPredecessor (NoPredecessor self) node = OnePredecessor self node
addPredecessor (OnePredecessor self one) node =
  if isBetweenEI (chordID one) self (chordID node)
    then TwoPredecessors self one node
    else TwoPredecessors self node one
addPredecessor (TwoPredecessors self one two) node =
  if isBetweenEI (chordID one) self (chordID node)
    then if isBetweenEI (chordID two) self (chordID node)
           then ThreePredecessors self one two node
           else ThreePredecessors self one node two
    else ThreePredecessors self node one two
addPredecessor (ThreePredecessors self one two three) node =
  let n = chordID node
      o = chordID one
      t = chordID two
      r = chordID three
   in if isBetweenEI o self n
        then if isBetweenEI t self n
               then if isBetweenEI r self n
                      then ThreePredecessors self two three node
                      else ThreePredecessors self two node three
               else ThreePredecessors self node two three
        else ThreePredecessors self one two three

-- | Return True if the associated FingerTable is responsible of the given ID
fingerTableIsResponsibleOfID :: ChordID n => Predecessors n -> ID -> Bool
fingerTableIsResponsibleOfID (NoPredecessor _) _ = True
fingerTableIsResponsibleOfID (OnePredecessor self one) i =
  isBetweenEI (chordID one) self i
fingerTableIsResponsibleOfID (TwoPredecessors self one _) i =
  isBetweenEI (chordID one) self i
fingerTableIsResponsibleOfID (ThreePredecessors self one _ _) i =
  isBetweenEI (chordID one) self i
