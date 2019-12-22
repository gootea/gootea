module Chord.FingerTable
  ( FingerTable
  , newFingerTable
  , listNodes
  , addNode
  , isInterestingNode
  , isResponsibleOfID
  ) where

import qualified Chord.Finger as F
import qualified Chord.FingerTable.Predecessors as P
import Chord.ID
import Chord.Node
import Data.List (find)
import Data.Maybe (fromMaybe)

-- | 
data FingerTable =
  FingerTable
    { fingers :: [F.Finger] -- Fingers
    , predecessors :: P.Predecessors ID
    }

-- | New empty FingerTable
newFingerTable :: ID -> FingerTable
newFingerTable self =
  let allRanges = fmap (fingerRange self) [0 .. 159]
      -- As we have 3 nodes per Finger, we can merge the 2 first Finger that
      -- hold 1 and 2 nodes each.
      ranges =
        case allRanges of
          (f, _):(_, s):rest -> (f, s) : rest
          impossible -> impossible
      fingers = fmap (uncurry F.newFinger) ranges
   in FingerTable {fingers = fingers, predecessors = P.newPredecessors self}

-- | List all nodes of the FingerTable
listNodes :: FingerTable -> [Node]
listNodes = (>>= F.nodes) . fingers

-- | Consider adding the given node to our FingerTable
addNode :: FingerTable -> Node -> FingerTable
addNode ft node = ft {fingers = newFingers, predecessors = newPredecessors}
  where
    newFingers = fmap insert . fingers $ ft
    insert finger =
      if F.isInFingerRange finger node
        then F.addNode finger node
        else finger
    newPredecessors = P.addPredecessor (predecessors ft) (chordID node)

-- | Return true if the given Node is a candidate that would be interesting to
-- get into our FingerTable
isInterestingNode :: FingerTable -> Node -> Bool
isInterestingNode ft node =
  fromMaybe False . -- None should not happen (Fingers cover the whole circle)
  fmap (flip F.isInterestingNode node) .
  find (flip F.isInFingerRange node) . fingers $
  ft

-- | Return true if the given FingerTable is responsible of the given ID
isResponsibleOfID :: FingerTable -> ID -> Bool
isResponsibleOfID ft idToTest =
  P.fingerTableIsResponsibleOfID (predecessors ft) idToTest
