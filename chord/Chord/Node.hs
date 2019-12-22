module Chord.Node
  ( Node
  , newNode
  , nodeIsResponsibleOfID
  ) where

import qualified Chord.FingerTable.Predecessors as P
import Chord.ID

import Network.Socket

data Node =
  Node
    { nodeID :: ID
    , netAdd :: SockAddr
    , predecessors :: P.Predecessors ID
    }

instance Eq Node where
  a == b = (nodeID a) == (nodeID b) && (netAdd a) == (netAdd b)

instance Show Node where
  show = show . nodeID

instance ChordID Node where
  chordID = nodeID

-- | Create a new Node
newNode :: ID -> Node
newNode nid = Node nid (SockAddrUnix "toto") (P.newPredecessors nid)

-- | Returns True if Node is responsible for the given ID
nodeIsResponsibleOfID :: ID -> Node -> Bool
nodeIsResponsibleOfID target node =
  P.fingerTableIsResponsibleOfID (predecessors node) target
