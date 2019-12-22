module Chord.Service
  ( Service
  , Output(..)
  , ChordMessage(..)
  , Error(..)
  , empty
  , addNode
  , get
  , add
  , handleChordMessage
  ) where

import Chord.ChordMessage
import qualified Chord.FingerTable as FT
import Chord.ID
import qualified Chord.Node as Node
import qualified Chord.Operation as Op
import Chord.Store
import Common.Models.InfoHash
import qualified Control.Monad.RWS.Lazy as M

data Service store =
  Service
    { serviceID :: ID
    , fingers :: FT.FingerTable
    , operations :: [Op.Operation InfoHash]
    , store :: store InfoHash
    }

instance ChordID (Service s) where
  chordID = serviceID

data Output
  = SendMessage Node.Node ChordMessage
  | GetReply ID [InfoHash]

data Error =
  NoFingerFound
  deriving (Eq, Show)

type ServiceMonad s = M.RWS () [Output] (Service s)

-- | Create a new empty Service
empty :: Store s => ID -> s InfoHash -> Service s
empty selfID store =
  Service
    { serviceID = selfID
    , fingers = FT.newFingerTable selfID
    , operations = []
    , store = store
    }

-- | Add a new node to the Chord topology
addNode :: Service s -> Node.Node -> (Service s, [Output])
addNode service node = (service, [SendMessage node FingerTableQuery])

-- | Ask the Chord service to get a list of values for the given ID
get :: Store s => Service s -> ID -> (Service s, [Output])
get svc target =
  if FT.isResponsibleOfID (fingers svc) target
    then (svc, [GetReply target (getFromStore target (store svc))])
    else M.execRWS (addGetOperation target) () svc

-- | Asks the Chord service to add a new value for the given ID
add :: Store s => Service s -> ID -> InfoHash -> (Service s, [Output])
add svc key value = M.execRWS transformation () svc
  where
    transformation =
      if FT.isResponsibleOfID (fingers svc) key
        then addValueToStore key value
        else addAddOperation key value

-- | Handle ChordMessages received from other nodes
handleChordMessage ::
     Store s => Service s -> Node.Node -> ChordMessage -> (Service s, [Output])
handleChordMessage service sender message = M.execRWS (monad message) () service
  where
    monad FingerTableQuery = replyToQueryFingerTable sender
    monad (FingerTableResponse ftNodes) =
      handleFingerTableResponse sender ftNodes
    monad (GetValuesQuery key) = replyWithValues sender key
    monad (GetValuesResponse key values) = tellReply key values
    monad (AddValueQuery key value) = addValueToStore key value

-- Create the reply to a QueryFinger query
replyToQueryFingerTable :: Node.Node -> ServiceMonad s ()
replyToQueryFingerTable querier = do
  service <- M.get
  let nodes = FT.listNodes . fingers $ service
  M.tell $ [SendMessage querier $ FingerTableResponse nodes]

-- Handle a response to a FingerTable query
handleFingerTableResponse :: Node.Node -> [Node.Node] -> ServiceMonad s ()
handleFingerTableResponse node ftnodes = do
  letOperationsPickNodes ftnodes
  updateFingerTableWithNewNodes node ftnodes

-- When receiving new nodes from the FingerTable of some remote node, go
-- through all pending operations and let them progress according to the new
-- information they can get from those new nodes
letOperationsPickNodes :: [Node.Node] -> ServiceMonad s ()
letOperationsPickNodes nodes = do
  service <- M.get
  newOps <- sequence . fmap (opPickNodesAndTell nodes) . operations $ service
  M.put $ service {operations = newOps}

-- For a list of Nodes and a given operation, get the nodes that we need to
-- query for much closer nodes and the nodes that are responsible of the
-- Operation's target and send appropriate messages to them
opPickNodesAndTell ::
     [Node.Node]
  -> Op.Operation InfoHash
  -> ServiceMonad s (Op.Operation InfoHash)
opPickNodesAndTell nodes op =
  let (newOp, dstNodes, nextNodes) = Op.pickNodes op nodes
      opTarget = Op.target op
      dstMsg =
        case Op.kind op of
          Op.Get -> GetValuesQuery opTarget
          Op.Add value -> AddValueQuery opTarget value
       -- Query nodes to reach closer nodes
   in do M.tell $ fmap (\node -> SendMessage node FingerTableQuery) nextNodes
       -- Query nodes to get/add values
         M.tell $ fmap (\node -> SendMessage node dstMsg) dstNodes
         return newOp

-- Save in our own FingerTable usefull information that we received from the
-- FingerTable of a remote Node
updateFingerTableWithNewNodes :: Node.Node -> [Node.Node] -> ServiceMonad s ()
updateFingerTableWithNewNodes responder newNodes = do
  service <- M.get
  let newFT = FT.addNode (fingers service) responder
  M.put $ service {fingers = newFT}
  let interestingNodes = filter (FT.isInterestingNode newFT) newNodes
  M.tell . fmap (\node -> SendMessage node FingerTableQuery) $ interestingNodes

-- Add a new Get Operation in the operations list
addGetOperation :: ID -> ServiceMonad s ()
addGetOperation target = do
  service <- M.get
  operation <-
    opPickNodesAndTell
      (FT.listNodes . fingers $ service)
      (Op.newGet (serviceID service) target)
  M.put $ service {operations = operation : (operations service)}

-- | Add a new Add Operation in the operations list
addAddOperation :: ID -> InfoHash -> ServiceMonad s ()
addAddOperation target value = do
  service <- M.get
  operation <-
    opPickNodesAndTell
      (FT.listNodes . fingers $ service)
      (Op.newAdd (serviceID service) target value)
  M.put $ service {operations = operation : (operations service)}

-- Reply to node with values
replyWithValues :: Store s => Node.Node -> ID -> ServiceMonad s ()
replyWithValues querier key = do
  store <- fmap store $ M.get
  let values = getFromStore key store
  M.tell $ [SendMessage querier $ GetValuesResponse key values]

-- Return the reply of a Get operation
tellReply :: ID -> [InfoHash] -> ServiceMonad s ()
tellReply key values = M.tell $ [GetReply key values]

-- Add a value to the store
addValueToStore :: Store s => ID -> InfoHash -> ServiceMonad s ()
addValueToStore key value =
  M.state $ \svc -> ((), svc {store = addToStore key value (store svc)})
