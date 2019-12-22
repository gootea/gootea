module Chord.Operation
  ( Operation(..)
  , Kind(..)
  , newGet
  , newAdd
  , target
  , pickNodes
  ) where

import Chord.Finger
import Chord.ID
import Chord.Node

-- | An Operation is an ongoing process that may need to exchange messages with
-- other Nodes in order to return a result
-- class Operation o where
data Operation v =
  Operation
    { target :: ID
    , kind :: Kind v
    , bestID :: ID -- Best ID so far
    }

data Kind v
  = Add v
  | Get

-- | Create a new Get Operation
newGet :: ID -> ID -> Operation v
newGet selfID target = Operation target Get selfID

-- | Create a new Add Operation
newAdd :: ID -> ID -> v -> Operation v
newAdd selfID target value = Operation target (Add value) selfID

-- | If the given list of Nodes contains some nodes that are closer to the
-- target, keep them as best Node so far and return those that are responsible
-- for the target and those that can help continue looking for closest nodes
--
-- Result is (new operation, nodes responsible for target, nodes to query for
-- fingers)
pickNodes :: Operation v -> [Node] -> (Operation v, [Node], [Node])
pickNodes operation nodes = foldl fun (operation, [], []) nodes
  where
    key = target operation
    fun (op, targets, fingerNodes) node =
      if nodeIsResponsibleOfID key node
        then (op, node : targets, fingerNodes)
        else if isBetweenEI (bestID op) key (chordID node)
               then (op {bestID = chordID node}, targets, node : fingerNodes)
               else (op, targets, fingerNodes)
