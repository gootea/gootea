module DHT.DHT
  ( DHT(dhtID)
  , DHTCmd(..)
  , DHTOutput(..)
  , emptyDHT
  , handlePacket
  , handleCommand
  , fnGetTransactionType
  , listDHTNodes
  , addDHTNode
  , addDHTPeer
  , listDHTPeerAssocs
  , createTokenForSockAddr
  ) where

import Control.Concurrent.Chan
import Control.Monad.RWS.Lazy
import DHT.Node
import DHT.NodeID
import DHT.Peer
import DHT.PeerStore
import DHT.Routing.Table
import DHT.TokenManager
import DHT.Transactions
import DHT.Types
import Data.Maybe
import Data.Time.Clock
import Network.Socket
import System.Random
import qualified DHT.Distance as D

data DHT = DHT
  { dhtID :: NodeID
  , table :: Table
  , transactions :: Transactions
  , peerStore :: PeerStore
  , tokenManager :: TokenManager
  , randomGen :: StdGen
  }

instance Show DHT where
  show _ = "DHT"

instance ToNodeID DHT where
  toNodeID dht = dhtID dht

data DHTCmd
  = DHTCmdAddHosts [SockAddr]
  | DHTCmdInit
  | DHTCmdGetPeers InfoHash
                   (Chan [SockAddr])
  | DHTTransactionsCheck -- Check if transactions have timed-out or need retransmission

data DHTOutput
  = DHTOutPacket SockAddr
                 Packet
  | DHTOutEvent DHTEvent
  | DHTOutNoNodesToInit
  | DHTGetPeersResponse (Chan [SockAddr])
                        [SockAddr]
  deriving (Eq)

emptyDHT :: NodeID -> StdGen -> DHT
emptyDHT nodeID stdGen =
  DHT
    nodeID
    (emptyTable nodeID)
    emptyTransactions
    emptyPeerStore
    newTokenManager
    stdGen

--
-- Packet handling --
--
handlePacket :: DHT -> SockAddr -> Packet -> (DHT, [DHTOutput])
handlePacket dht srcAddr (Packet transactionID message) =
  execRWS (m message) (srcAddr, transactionID) dht
  where
    m (PingQuery _) = handlePingQuery
    m (FindNodeQuery _ targetID) = handleFindNodeQuery targetID
    m (GetPeersQuery _ ih) = handleGetPeersQuery ih
    m (AnnouncePeerQuery _ ih port token) =
      handleAnnouncePeerQuery ih (fromIntegral port) token
    m (PingResponse peerID) = handlePingResponse peerID
    m (FindNodeResponse _ nodes) = handleFindNodeResponse nodes
    m (GetPeersWithPeersResponse _ _ p) = handleGetPeersWithPeersResponse p
    m (GetPeersWithNodesResponse _ _ n) = handleGetPeersWithNodesResponse n
    m _ = return ()

type DHTRWS = RWS (SockAddr, TransactionID) [DHTOutput] DHT

handlePingQuery :: DHTRWS ()
handlePingQuery = do
  dht <- get
  dhtMessageToOutput $ PingResponse (dhtID dht)

handleFindNodeQuery :: NodeID -> DHTRWS ()
handleFindNodeQuery targetID = do
  dht <- get
  let nodes = findClosests (table dht) targetID
  let responseMessage = FindNodeResponse (dhtID dht) nodes
  dhtMessageToOutput responseMessage

handleGetPeersQuery :: InfoHash -> DHTRWS ()
handleGetPeersQuery ih = do
  token <- genNewToken
  dht <- get
  dhtMessageToOutput (createPeersQueryResponse dht ih token)
  tell [DHTOutEvent $ DHTInfoHashDiscovered ih]

handleAnnouncePeerQuery :: InfoHash -> PortNumber -> Token -> DHTRWS ()
handleAnnouncePeerQuery ih port token = do
  (srcAddr, _) <- ask
  dht <- get
  let maybePeer =
        (\(Peer _ a) -> Peer (fromIntegral port) a) <$> peerFromSockAddr srcAddr
  let tokenOK = checkToken (tokenManager dht) token srcAddr
  case (maybePeer, tokenOK) of
    (Just peer, True) -> do
      updatePeerStore ih peer
      dhtMessageToOutput $ AnnouncePeerResponse (dhtID dht)
      tell [DHTOutEvent $ DHTPeerAdded ih peer]
    _ -> return ()

handlePingResponse :: NodeID -> DHTRWS ()
handlePingResponse peerID = do
  removeCurrentTransaction
  saveCurrentNode peerID

handleFindNodeResponse :: [Node] -> DHTRWS ()
handleFindNodeResponse nodes = do
  replyToCloserNodes nodes
  saveNewNodes nodes

handleGetPeersWithPeersResponse :: [Peer] -> DHTRWS ()
handleGetPeersWithPeersResponse peers = do
  dht <- get
  (_, transactionID) <- ask
  let newTransactions =
        addPeersToTransactionGetPeers (transactions dht) transactionID peers
  put $ dht {transactions = newTransactions}

handleGetPeersWithNodesResponse :: [Node] -> DHTRWS ()
handleGetPeersWithNodesResponse nodes = do
  dht <- get
  (_, transactionID) <- ask
  let (newTransactions, nodesToContact) =
        updateTransactionGetPeersWithNodes
          (transactions dht)
          transactionID
          nodes
  put $ dht {transactions = newTransactions}
  tell $
    fmap
      (\(n, ih) -> buildOutPacketForGetPeersQuery transactionID (dhtID dht) ih n)
      nodesToContact

dhtMessageToOutput :: DHTMessage -> DHTRWS ()
dhtMessageToOutput message = do
  (srcAddr, tid) <- ask
  tell [DHTOutPacket srcAddr $ Packet tid message]

genNewToken :: DHTRWS Token
genNewToken = do
  (srcAddr, _) <- ask
  dht <- get
  let (token, newTM, newGen) =
        newToken (tokenManager dht) (randomGen dht) srcAddr
  put $ dht {tokenManager = newTM, randomGen = newGen}
  return token

updatePeerStore :: InfoHash -> Peer -> DHTRWS ()
updatePeerStore ih peer = do
  dht <- get
  let pStore = addPeer (peerStore dht) ih peer
  put $ dht {peerStore = pStore}

-- Remove the transaction associated to the packet currently being handled
removeCurrentTransaction :: DHTRWS ()
removeCurrentTransaction = do
  (_, transactionID) <- ask
  dht <- get
  put $ dht {transactions = removeTransaction (transactions dht) transactionID}

-- Add the node send the packet currently being handled to the DHT
saveCurrentNode :: NodeID -> DHTRWS ()
saveCurrentNode peerID = do
  (srcAddr, _) <- ask
  case nodeFromSockAddr peerID srcAddr of
    Just node -> do
      dht <- get
      put $ saveNode dht node
      tell [DHTOutEvent $ DHTNodeAdded node]
    Nothing -> return ()

saveNewNodes :: [Node] -> DHTRWS ()
saveNewNodes nodes = do
  dht <- get
  let isNewNode (Node nodeID _ _) = isNothing $ findNode (table dht) nodeID
  let newNodes = filter isNewNode nodes
  put $ foldl saveNode dht newNodes
  tell $ (DHTOutEvent . DHTNodeAdded) <$> newNodes

replyToCloserNodes :: [Node] -> DHTRWS ()
replyToCloserNodes nodes = do
  dht <- get
  let closests = findClosests (table dht) (dhtID dht)
  let closerNodes =
        case D.closest dht closests of
          Nothing -> nodes
          Just closest -> filter (D.isCloser dht closest) nodes
  let myID = dhtID dht
  replyToNodes (FindNodeQuery myID myID) closerNodes

replyToNodes :: DHTMessage -> [Node] -> DHTRWS ()
replyToNodes message nodes = do
  (_, transactionID) <- ask
  let packet = Packet transactionID message
  tell $ (flip DHTOutPacket packet . nodeToSockAddr) <$> nodes

createPeersQueryResponse :: DHT -> InfoHash -> Token -> DHTMessage
createPeersQueryResponse dht ih token =
  case getPeers (peerStore dht) ih of
    Just peers -> GetPeersWithPeersResponse (dhtID dht) token peers
    Nothing -> GetPeersWithNodesResponse (dhtID dht) token nodes
      where nodes = findClosests (table dht) (ihToNodeID ih)

--
-- Command handling --
--
handleCommand :: DHT -> UTCTime -> DHTCmd -> (DHT, [DHTOutput])
handleCommand dht _ (DHTCmdAddHosts hosts) = doPingHost dht hosts
handleCommand dht _ DHTCmdInit =
  if null (listNodes $ table dht)
    then (dht, [DHTOutNoNodesToInit])
    else doFindNode dht (dhtID dht)
handleCommand dht time (DHTCmdGetPeers ih chan) = (newDHT, output)
  where
    (newDHT, tid) =
      createTransaction
        dht
        (newGetPeersTransaction ih closestNodeID expire chan)
    expire = addUTCTime (fromInteger 10) time
    closestNodeID = fromMaybe (dhtID dht) (toNodeID <$> D.closest ih nodes)
    nodes = findClosests (table dht) (ihToNodeID ih)
    output = fmap (buildOutPacketForGetPeersQuery tid (dhtID dht) ih) nodes
handleCommand dht time DHTTransactionsCheck = (newDHT, output)
  where
    (newTransactions, expiredTransactions) =
      getExpiredTransaction time (transactions dht)
    output = expiredTransactions >>= toOutput
    toOutput (TransactionGetPeers peers _ _ _ chan) =
      [DHTGetPeersResponse chan (fmap peerToSockAddr peers)]
    toOutput _ = []
    newDHT = dht {transactions = newTransactions}

buildOutPacketForGetPeersQuery ::
     TransactionID -> NodeID -> InfoHash -> Node -> DHTOutput
buildOutPacketForGetPeersQuery tid selfID ih node =
  DHTOutPacket (nodeToSockAddr node) (Packet tid $ GetPeersQuery selfID ih)

doPingHost :: DHT -> [SockAddr] -> (DHT, [DHTOutput])
doPingHost dht hosts = (newDHT, fmap createOutput hosts)
  where
    createOutput sockAddr = DHTOutPacket sockAddr message
    message = Packet tid $ PingQuery (dhtID dht)
    (newDHT, tid) = createTransaction dht TransactionPing

doFindNode :: DHT -> NodeID -> (DHT, [DHTOutput])
doFindNode dht nodeID = (newDHT, outputs)
  where
    outputs = fmap buildOutput nodes
    nodes = listNodes (table dht)
    buildOutput node =
      DHTOutPacket (nodeToSockAddr node) (Packet transactionID message)
      where
        message = FindNodeQuery (dhtID dht) nodeID
    (newDHT, transactionID) = createTransaction dht TransactionFindNode

-- Save the node on the table
saveNode :: DHT -> Node -> DHT
saveNode dht node = dht {table = insertOrUpdate (table dht) node id nodeID}
  where
    Node nodeID _ _ = node

createTransaction :: DHT -> Transaction -> (DHT, TransactionID)
createTransaction dht transaction =
  (dht {transactions = ts, randomGen = newGen}, tid)
  where
    (tid, newGen) = random (randomGen dht)
    ts = addTransaction (transactions dht) tid transaction

fnGetTransactionType :: DHT -> TransactionID -> Maybe TransactionType
fnGetTransactionType dht =
  fmap transactionType . getTransaction (transactions dht)

--
-- Expose some attributes of the DHT --
--
listDHTNodes :: DHT -> [Node]
listDHTNodes dht = listNodes (table dht)

addDHTNode :: DHT -> Node -> DHT
addDHTNode = saveNode

addDHTPeer :: DHT -> InfoHash -> Peer -> DHT
addDHTPeer dht ih p = dht {peerStore = newPeerStore}
  where
    newPeerStore = addPeer (peerStore dht) ih p

listDHTPeerAssocs :: DHT -> [(InfoHash, [Peer])]
listDHTPeerAssocs dht = listAssocs (peerStore dht)

-- Create a new token for the given node
createTokenForSockAddr :: DHT -> SockAddr -> (DHT, Token)
createTokenForSockAddr dht sockAddr = (newDHT, token)
  where
    (token, tm, _) = newToken (tokenManager dht) (randomGen dht) sockAddr
    newDHT = dht {tokenManager = tm}
