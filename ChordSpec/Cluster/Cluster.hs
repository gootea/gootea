module Cluster.Cluster
  ( Cluster(..)
  , newCluster
  , fullyConnectAllPeers
  , actOnPeer
  , processOutputQueue
  ) where

import Chord.ChordMessage
import Chord.ID
import Chord.Node
import qualified Chord.Service as S
import Chord.Store
import Cluster.InMemoryStore

import Control.Monad.State.Strict as State
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe

-- Tests with multiple Peers
data Cluster s v =
  Cluster
    { peers :: M.Map ID (S.Service s v)
    , outputQueue :: [(Node, S.Output v)]
    , replyQueue :: [(ID, [v])]
    }

type CMonad v a = State.State (Cluster InMemoryStore v) a

-- | Create a new Cluster
newCluster :: Store s => [(ID, s v)] -> Cluster s v
newCluster ids =
  Cluster {peers = M.fromList peers, outputQueue = [], replyQueue = []}
  where
    peers = fmap (\(i, s) -> (i, S.empty i s)) ids

serviceToNode :: S.Service s v -> Node
serviceToNode = newNode . chordID

fullyConnectAllPeers :: CMonad v ()
fullyConnectAllPeers =
  let addNode (svc, node) = actOnPeer (chordID svc) (\s -> S.addNode s node)
   in do allPeers <- fmap M.elems $ fmap peers $ State.get
         let nodesToAdd = do
               a <- allPeers
               b <- allPeers
               return (a, serviceToNode b)
         mapM_ addNode nodesToAdd
         processOutputQueue

-- | Execute a function on the peer matching the given ID
actOnPeer ::
     ID
  -> (S.Service InMemoryStore v -> (S.Service InMemoryStore v, [S.Output v]))
  -> CMonad v ()
actOnPeer target fun = do
  cluster <- State.get
  let peersMap = peers cluster
  case M.lookup target peersMap of
    Nothing -> return ()
    Just svc -> do
      let (newSvc, outputs) = fun svc
      let newPeers = M.insert target newSvc peersMap
      let newOutputs = fmap (\msg -> (serviceToNode newSvc, msg)) outputs
      let newOutputQueue = (outputQueue cluster) <> newOutputs
      State.put $ cluster {peers = newPeers, outputQueue = newOutputQueue}
  processOutputQueue

processChordMessage :: Node -> Node -> ChordMessage v -> CMonad v ()
processChordMessage recipient emitter msg = do
  let nid = chordID recipient
  actOnPeer nid (\svc -> S.handleChordMessage svc emitter msg)

dequeueFirstOutput :: CMonad v (Maybe (Node, S.Output v))
dequeueFirstOutput =
  State.state $ \cluster ->
    case uncons $ outputQueue cluster of
      Just (msg, rest) -> (Just msg, cluster {outputQueue = rest})
      Nothing -> (Nothing, cluster)

-- | Process all messages that are in `outputQueue`
processOutputQueue :: CMonad v ()
processOutputQueue = do
  maybeMsg <- dequeueFirstOutput
  case maybeMsg of
    Just (emitter, S.SendMessage recipient chordMessage) -> do
      processChordMessage recipient emitter chordMessage
      processOutputQueue
    Just (_, S.GetReply k v) -> queueReply k v
    Nothing -> return ()

queueReply :: ID -> [v] -> CMonad v ()
queueReply k v =
  State.state $ \s -> ((), s {replyQueue = (k, v) : (replyQueue s)})
