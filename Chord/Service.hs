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
import qualified Control.Monad.RWS.Lazy as M

data Service store value =
  Service
    { serviceID :: ID
    , fingers :: FT.FingerTable
    , operations :: [Op.Operation value]
    , store :: store value
    }

instance ChordID (Service s v) where
  chordID = serviceID

data Output v
  = SendMessage Node.Node (ChordMessage v)
  | GetReply ID [v]

data Error =
  NoFingerFound
  deriving (Eq, Show)

type ServiceMonad s v = M.RWS () [Output v] (Service s v)

-- | Create a new empty Service
empty :: Store s => ID -> s v -> Service s v
empty selfID store =
  Service
    { serviceID = selfID
    , fingers = FT.newFingerTable selfID
    , operations = []
    , store = store
    }

-- | Add a new node to the Chord topology
addNode :: Service s v -> Node.Node -> (Service s v, [Output v])
addNode service node = (service, [SendMessage node FingerTableQuery])

-- | Ask the Chord service to get a list of values for the given ID
get :: Store s => Service s v -> ID -> (Service s v, [Output v])
get svc target =
  if FT.isResponsibleOfID (fingers svc) target
    then (svc, [GetReply target (getFromStore target (store svc))])
    else M.execRWS (addGetOperation target) () svc

-- | Asks the Chord service to add a new value for the given ID
add :: Store s => Service s v -> ID -> v -> (Service s v, [Output v])
add svc key value = M.execRWS transformation () svc
  where
    transformation =
      if FT.isResponsibleOfID (fingers svc) key
        then addValueToStore key value
        else addAddOperation key value

-- | Handle ChordMessages received from other nodes
handleChordMessage ::
     Store s
  => Service s v
  -> Node.Node
  -> ChordMessage v
  -> (Service s v, [Output v])
handleChordMessage service sender message = M.execRWS (monad message) () service
  where
    monad FingerTableQuery = replyToQueryFingerTable sender
    monad (FingerTableResponse ftNodes) =
      handleFingerTableResponse sender ftNodes
    monad (GetValuesQuery key) = replyWithValues sender key
    monad (GetValuesResponse key values) = tellReply key values
    monad (AddValueQuery key value) = addValueToStore key value

-- Create the reply to a QueryFinger query
replyToQueryFingerTable :: Node.Node -> ServiceMonad s v ()
replyToQueryFingerTable querier = do
  service <- M.get
  let nodes = FT.listNodes . fingers $ service
  M.tell $ [SendMessage querier $ FingerTableResponse nodes]

-- Handle a response to a FingerTable query
handleFingerTableResponse :: Node.Node -> [Node.Node] -> ServiceMonad s v ()
handleFingerTableResponse node ftnodes = do
  letOperationsPickNodes ftnodes
  updateFingerTableWithNewNodes node ftnodes

-- When receiving new nodes from the FingerTable of some remote node, go
-- through all pending operations and let them progress according to the new
-- information they can get from those new nodes
letOperationsPickNodes :: [Node.Node] -> ServiceMonad s v ()
letOperationsPickNodes nodes = do
  service <- M.get
  newOps <- sequence . fmap (opPickNodesAndTell nodes) . operations $ service
  M.put $ service {operations = newOps}

-- For a list of Nodes and a given operation, get the nodes that we need to
-- query for much closer nodes and the nodes that are responsible of the
-- Operation's target and send appropriate messages to them
opPickNodesAndTell ::
     [Node.Node] -> Op.Operation v -> ServiceMonad s v (Op.Operation v)
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
updateFingerTableWithNewNodes :: Node.Node -> [Node.Node] -> ServiceMonad s v ()
updateFingerTableWithNewNodes responder newNodes = do
  service <- M.get
  let newFT = FT.addNode (fingers service) responder
  M.put $ service {fingers = newFT}
  let interestingNodes = filter (FT.isInterestingNode newFT) newNodes
  M.tell . fmap (\node -> SendMessage node FingerTableQuery) $ interestingNodes

-- Add a new Get Operation in the operations list
addGetOperation :: ID -> ServiceMonad s v ()
addGetOperation target = do
  service <- M.get
  operation <-
    opPickNodesAndTell
      (FT.listNodes . fingers $ service)
      (Op.newGet (serviceID service) target)
  M.put $ service {operations = operation : (operations service)}

-- | Add a new Add Operation in the operations list
addAddOperation :: ID -> v -> ServiceMonad s v ()
addAddOperation target value = do
  service <- M.get
  operation <-
    opPickNodesAndTell
      (FT.listNodes . fingers $ service)
      (Op.newAdd (serviceID service) target value)
  M.put $ service {operations = operation : (operations service)}

-- Reply to node with values
replyWithValues :: Store s => Node.Node -> ID -> ServiceMonad s v ()
replyWithValues querier key = do
  store <- fmap store $ M.get
  let values = getFromStore key store
  M.tell $ [SendMessage querier $ GetValuesResponse key values]

-- Return the reply of a Get operation
tellReply :: ID -> [v] -> ServiceMonad s v ()
tellReply key values = M.tell $ [GetReply key values]

-- Add a value to the store
addValueToStore :: Store s => ID -> v -> ServiceMonad s v ()
addValueToStore key value =
  M.state $ \svc -> ((), svc {store = addToStore key value (store svc)})
