module DHT.DHT
  ( DHT(dhtID)
  , DHTCmd(..)
  , DHTOutput(..)
  , emptyDHT
  , handlePacket
  , handleCommand
  , getGetTransactionType
  , listDHTNodes
  , addDHTNode
  , addDHTPeer
  , listDHTPeerAssocs
  , createTokenForSockAddr
  ) where

import DHT.Node
import DHT.Peer
import DHT.PeerStore
import DHT.Routing.Table
import DHT.TokenManager
import DHT.Transactions
import DHT.Types
import Data.Maybe
import Network.Socket
import System.Random

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

data DHTCmd
  = DHTCmdAddHosts [SockAddr]
  | DHTCmdInit

data DHTOutput
  = DHTOutPacket SockAddr
                 Packet
  | DHTOutEvent DHTEvent
  | DHTOutNoNodesToInit
  deriving (Eq, Show)

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
  case message of
    PingQuery _ ->
      ( dht
      , [DHTOutPacket srcAddr $ Packet transactionID $ PingResponse (dhtID dht)])
    FindNodeQuery _ targetID ->
      (dht, [DHTOutPacket srcAddr $ Packet transactionID responseMessage])
      where responseMessage = FindNodeResponse (dhtID dht) nodes
            nodes = findClosests (table dht) targetID
    GetPeersQuery _ ih -> (updatedDht, [outputPacket, outputEvent])
      where updatedDht = dht {tokenManager = newTM, randomGen = newGen}
            (token, newTM, newGen) =
              newToken (tokenManager dht) (randomGen dht) srcAddr
            packet =
              Packet transactionID $ createPeersQueryResponse dht ih token
            outputPacket = DHTOutPacket srcAddr packet
            outputEvent = DHTOutEvent $ DHTInfoHashDiscovered ih
    AnnouncePeerQuery _ ih port token ->
      fromMaybe (dht, []) $ do
        Peer _ peerAddr <- peerFromSockAddr srcAddr
        let peer = Peer (fromIntegral port) peerAddr
        updatedPeerStore <-
          if checkToken (tokenManager dht) token srcAddr
            then Just $ addPeer (peerStore dht) ih peer
            else Nothing
        let updatedDHT = dht {peerStore = updatedPeerStore}
        let outputPacket =
              DHTOutPacket srcAddr $
              Packet transactionID $ AnnouncePeerResponse (dhtID dht)
        let outputEvent = DHTOutEvent $ DHTPeerAdded ih peer
        return (updatedDHT, [outputPacket, outputEvent])
    PingResponse peerID ->
      case nodeFromSockAddr peerID srcAddr of
        Just node -> (dhtWithNode {transactions = newTransactions}, [event])
          where dhtWithNode = saveNode dht node
                newTransactions =
                  removeTransaction (transactions dht) transactionID
                event = DHTOutEvent $ DHTNodeAdded node
        Nothing -> (dht, [])
    FindNodeResponse _ nodes -> (dhtWithNewNodes, output ++ events)
      where newNodes = filter isNewNode nodes
            isNewNode (Node nodeID _ _) =
              isNothing $ findNode (table dht) nodeID
            closerNodes = filter isCloser nodes
            isCloser node =
              all
                (\n -> (distanceTo (dhtID dht) node) < distanceTo (dhtID dht) n)
                (findClosests (table dht) (dhtID dht))
            dhtWithNewNodes = foldl saveNode dht newNodes
            events = fmap (DHTOutEvent . DHTNodeAdded) newNodes
            output =
              fmap
                (buildOutPacketForFindNodeQuery transactionID (dhtID dht))
                closerNodes
    _ -> (dht, [])

createPeersQueryResponse :: DHT -> InfoHash -> Token -> DHTMessage
createPeersQueryResponse dht ih token =
  case getPeers (peerStore dht) ih of
    Just peers -> GetPeersWithPeersResponse (dhtID dht) token peers
    Nothing -> GetPeersWithNodesResponse (dhtID dht) token nodes
      where nodes = findClosests (table dht) (ihToNodeID ih)

--
-- Command handling --
--
handleCommand :: DHT -> DHTCmd -> (DHT, [DHTOutput])
handleCommand dht (DHTCmdAddHosts hosts) = doPingHost dht hosts
handleCommand dht DHTCmdInit =
  if null (listNodes $ table dht)
    then (dht, [DHTOutNoNodesToInit])
    else doFindNode dht (dhtID dht)

doPingHost :: DHT -> [SockAddr] -> (DHT, [DHTOutput])
doPingHost dht hosts = (newDHT, fmap createOutput hosts)
  where
    createOutput sockAddr = DHTOutPacket sockAddr message
    message = Packet tid $ PingQuery (dhtID dht)
    (newDHT, Transaction tid _) = createTransaction dht Ping

doFindNode :: DHT -> NodeID -> (DHT, [DHTOutput])
doFindNode dht nodeID = (newDHT, outputs)
  where
    outputs = fmap buildOutput nodes
    nodes = listNodes (table dht)
    buildOutput node =
      DHTOutPacket (nodeToSockAddr node) (Packet transactionID message)
      where
        message = FindNodeQuery (dhtID dht) nodeID
    (newDHT, Transaction transactionID _) = createTransaction dht FindNode

buildOutPacketForFindNodeQuery :: TransactionID -> NodeID -> Node -> DHTOutput
buildOutPacketForFindNodeQuery tid selfID node =
  DHTOutPacket (nodeToSockAddr node) (Packet tid message)
  where
    message = FindNodeQuery selfID selfID

-- Save the node on the table
saveNode :: DHT -> Node -> DHT
saveNode dht node = dht {table = insertOrUpdate (table dht) node id nodeID}
  where
    Node nodeID _ _ = node

createTransaction :: DHT -> TransactionType -> (DHT, Transaction)
createTransaction dht ttype =
  (dht {transactions = ts, randomGen = newGen}, transaction)
  where
    transaction = Transaction tid ttype
    (tid, newGen) = random (randomGen dht)
    ts = addTransaction (transactions dht) transaction

getGetTransactionType :: DHT -> TransactionID -> Maybe TransactionType
getGetTransactionType dht =
  (fmap extractType) . getTransaction (transactions dht)
  where
    extractType (Transaction _ ttype) = ttype

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
