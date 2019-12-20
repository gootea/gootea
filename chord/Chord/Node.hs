module Chord.Node
  ( Node(nodeId, nodeAddr)
  , newNode
  , nodeIsResponsibleOfID
  ) where

import qualified Chord.FingerTable.Predecessors as P
import Chord.ID

import Network.Socket
import Test.QuickCheck

data Node =
  Node
    { nodeId :: ID
    , nodeAddr :: SockAddr
    , predecessors :: P.Predecessors ID
    }

instance Eq Node where
  a == b = (nodeId a) == (nodeId b) && (nodeAddr a) == (nodeAddr b)

instance Show Node where
  show n = "Node " ++ (show $ nodeId n) ++ " " ++ (show $ nodeAddr n)

instance ChordID Node where
  chordID = nodeId

instance Arbitrary Node where
  arbitrary = Node <$> arbitrary <*> addr <*> arbitrary
    where
      addr =
        oneof
          [ SockAddrInet <$> portNumber <*> hostAddr
          , SockAddrInet6 <$> portNumber <*> (pure 0) <*> hostAddr6 <*> (pure 0)
          ]
      portNumber = fromInteger <$> choose (0, 65535)
      hostAddr = tupleToHostAddress <$> arbitrary
      hostAddr6 = tupleToHostAddress6 <$> arbitrary

-- | Create a new Node
newNode :: ID -> SockAddr -> Node
newNode nid addr = Node nid addr (P.newPredecessors nid)

-- | Returns True if Node is responsible for the given ID
nodeIsResponsibleOfID :: ID -> Node -> Bool
nodeIsResponsibleOfID target node =
  P.fingerTableIsResponsibleOfID (predecessors node) target
