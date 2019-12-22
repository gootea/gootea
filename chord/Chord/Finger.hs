module Chord.Finger
  ( Finger(nodes)
  , newFinger
  , isInterestingNode
  , isInFingerRange
  , addNode
  ) where

import Chord.ID
import Chord.Node

-- | A Finger holds the first 3 nodes that immediately follow the `lower` ID.
-- The nodes in this Finger cannot be greater than the `upper` ID.
data Finger =
  Finger
    { lowerID :: ID -- lower limit for the IDs holded in this Finger
    , upperID :: ID -- upper limit for the IDs holded in this Finger
    , nodes :: [Node]
    }

-- | Create a new Finger
newFinger :: ID -> ID -> Finger
newFinger lower upper = Finger {lowerID = lower, upperID = upper, nodes = []}

-- | Return true if the given Node is a candidate that would be interesting to
-- get into this Finger
isInterestingNode :: Finger -> Node -> Bool
isInterestingNode finger node = isGoodRange && isUnknown
  where
    isGoodRange =
      case lastFingerNode finger of
        Nothing -> True -- Return True for an empty Finger
        Just lastFinger ->
          isBetweenIE (lowerID finger) (chordID lastFinger) (chordID node)
    isUnknown = not $ elem node (nodes finger)

-- | Check wether the given Node falls into the responsability of this Finger
isInFingerRange :: Finger -> Node -> Bool
isInFingerRange finger node =
  isBetweenIE (lowerID finger) (upperID finger) (chordID node)

-- | Add a new Node in this Finger
addNode :: Finger -> Node -> Finger
addNode finger node = finger {nodes = newNodes}
  where
    newNodes = take 3 . insertNode . nodes $ finger
    insertNode (first:rest) =
      if isBetweenIE (lowerID finger) (chordID first) nid
        then node : first : rest
        else first : (insertNode rest)
    insertNode [] = [node]
    nid = chordID node

-- Return the last Node of this Finger
lastFingerNode :: Finger -> Maybe Node
lastFingerNode = foldl (\_ e -> Just e) Nothing . nodes
