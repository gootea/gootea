module DHTSpec where

import Control.Applicative (liftA2)

import Common.Models.InfoHash
import Control.Monad.State.Lazy
import DHT.DHT
import qualified DHT.Distance as D
import DHT.Node
import DHT.NodeID
import DHT.Peer
import DHT.Transactions (TransactionID(..))
import DHT.Types
import qualified Data.ByteString as B
import Data.Maybe
import qualified Data.Set as S
import Data.Time.Calendar
import Data.Time.Clock
import Network.Socket
import System.Random
import Test.Hspec
import Test.Hspec.Expectations
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "DHT" $ do
    prop "handle DHTCmdAddHosts" propDHTCmdAddHosts
    prop "handle DHTCmdInit" $ propDHTCmdInit
    prop "handle DHTCmdGetPeers" $ propDHTCmdGetPeers
    prop "reply to PingQuery" $ propPingQuery
    prop "reply to FindNodeQuery" $ propFindNodeQuery
    prop "reply to GetPeersQuery" $ propGetPeersQuery
    prop "reply to AnnouncePeerQuery and save the peer" $ propAnnouncePeerQuery
    prop "get peers when asked for" $ propDHTGetPeersScenario
    prop "correctly handle FindNodeResponse" $ propFindNodeResponse

-------------------------
-- Arbitrary Instances --
-------------------------
instance Arbitrary NodeID where
  arbitrary = NodeID . B.pack <$> vectorOf 20 arbitrary

instance Arbitrary Node where
  arbitrary = liftA2 Node arbitrary pn <*> arbitrary
    where
      pn = fmap fromInteger arbitrary

instance Arbitrary Peer where
  arbitrary = liftA2 Peer arbitrary arbitrary

instance Arbitrary DHT where
  arbitrary = emptyDHT <$> arbitrary <*> (mkStdGen <$> arbitrary)

instance Arbitrary TransactionID where
  arbitrary = TransactionID . B.pack <$> vectorOf 8 arbitrary

instance Arbitrary PortNumber where
  arbitrary = fromInteger <$> arbitrary

instance Arbitrary SockAddr where
  arbitrary = liftA2 SockAddrInet arbitrary arbitrary

-- DHT prepopulated with nodes
newtype DHTWithNodes =
  DHTWN DHT

instance Show DHTWithNodes where
  show (DHTWN dht) = "DHTWithNodes: " ++ (show $ listDHTNodes dht)

instance Arbitrary DHTWithNodes where
  arbitrary = DHTWN <$> liftA2 addNodesToDHT arbitrary arbitrary
    where
      addNodesToDHT :: DHT -> [Node] -> DHT
      addNodesToDHT = foldl addDHTNode

-- DHT prepopulated with nodes
data DHTWithPeers =
  DHTWP DHT InfoHash [Peer]

instance Show DHTWithPeers where
  show (DHTWP dht _ _) = "DHTWithPeers: " ++ (show $ listDHTPeerAssocs dht)

instance Arbitrary DHTWithPeers where
  arbitrary = do
    dht <- arbitrary
    ih <- arbitrary
    peers <- arbitrary
    let dhtwp = addPeersToDHT ih dht peers
    return $ DHTWP dhtwp ih peers
    where
      addPeersToDHT :: InfoHash -> DHT -> [Peer] -> DHT
      addPeersToDHT ih = foldl $ flip addDHTPeer ih

-- DHT prepopulated with a valid Token for a given Node
data DHTWithToken =
  DHTWT DHT Node Token

instance Show DHTWithToken where
  show (DHTWT _ n t) =
    "DHTWithToken: token " ++ (show t) ++ " , node: " ++ (show n)

instance Arbitrary DHTWithToken where
  arbitrary = do
    dht <- arbitrary
    node <- arbitrary
    let sockAddr = nodeToSockAddr node
    let token = createTokenForSockAddr dht sockAddr
    return $ DHTWT dht node token

-------------------
-- Test Commands --
-------------------
propDHTCmdAddHosts :: DHT -> [SockAddr] -> Bool
propDHTCmdAddHosts dht sockAddrs =
  all correctOutput outputs &&
  S.fromList outputSockAddrs == S.fromList sockAddrs
  where
    (_, outputs) = handleCommand dht undefined $ DHTCmdAddHosts sockAddrs
    correctOutput (DHTOutPacket _ (Packet _ (PingQuery nodeID))) =
      nodeID == dhtID dht
    correctOutput _ = False
    outputSockAddrs = fmap (\(DHTOutPacket s _) -> s) outputs

-- Test the init process which consist of sending FindNode queries
propDHTCmdInit :: DHTWithNodes -> Bool
propDHTCmdInit (DHTWN dht) =
  let nodes = listDHTNodes dht
      (_, outputs) = handleCommand dht undefined DHTCmdInit
   in if null nodes
        then outputs == [DHTOutNoNodesToInit]
        else all correctOutput outputs &&
             S.fromList (fmap outputToHost outputs) ==
             S.fromList (nodeToHost <$> nodes)
  where
    correctOutput (DHTOutPacket _ (Packet _ (FindNodeQuery id1 id2))) =
      id1 == myID && id2 == myID
    correctOutput _ = False
    myID = dhtID dht
    outputToHost (DHTOutPacket (SockAddrInet pn ha) _) = (pn, ha)
    outputToHost _ = undefined
    nodeToHost (Node _ pn ha) = (pn, ha)

propDHTCmdGetPeers :: DHTWithNodes -> InfoHash -> Bool
propDHTCmdGetPeers (DHTWN dht) ih = all isCorrectOutput outputs
  where
    (_, outputs) = handleCommand dht undefined (DHTCmdGetPeers ih undefined)
    isCorrectOutput (DHTOutPacket _ (Packet _ (GetPeersQuery myID queriedIH))) =
      myID == (dhtID dht) && queriedIH == ih
    isCorrectOutput _ = False

-----------------------------
-- Full Get Peers scenario --
-----------------------------
data GetPeersScenario =
  GetPeersScenario
    { _gpsDht :: DHT
    , _gpsTime :: UTCTime
    , _gpsIH :: InfoHash -- Queried InfoHash
    , _gpsTransaction :: Maybe TransactionID
    , _gpsPeers :: [SockAddr]
    }
  deriving (Show)

instance Arbitrary GetPeersScenario where
  arbitrary = do
    DHTWN dht <- arbitrary :: Gen DHTWithNodes
    time <-
      liftA2
        UTCTime
        (ModifiedJulianDay <$> arbitrary)
        (fromInteger <$> arbitrary)
    ih <- arbitrary
    peers <- arbitrary
    return $ GetPeersScenario dht time ih Nothing peers

propDHTGetPeersScenario :: GetPeersScenario -> Expectation
propDHTGetPeersScenario scenario =
  if length (listDHTNodes (_gpsDht scenario)) == 0
    then return ()
    else (evalState state scenario) `shouldMatchList` (_gpsPeers scenario)
  where
    state = do
      sendDHTCmdGetPeers
      sendGetPeersReply
      sendTransactionCheck
    sendDHTCmdGetPeers = do
      state <- get
      let dht = _gpsDht state
      let time = _gpsTime state
      let command = DHTCmdGetPeers (_gpsIH state) undefined
      let (newDht, outputs) = handleCommand dht time command
      let (DHTOutPacket _ (Packet tid _)) = head outputs
      put $ state {_gpsDht = newDht, _gpsTransaction = Just tid}
    sendGetPeersReply = do
      state <- get
      let tid = fromJust $ _gpsTransaction state
      let peers = catMaybes (peerFromSockAddr <$> _gpsPeers state)
      let packet =
            Packet tid (GetPeersWithPeersResponse undefined undefined peers)
      let (newDht, outputs) = handlePacket (_gpsDht state) undefined packet
      put $ state {_gpsDht = newDht}
    sendTransactionCheck = do
      state <- get
      let newTime = addUTCTime (fromInteger 60) (_gpsTime state)
      let (newDht, outputs) =
            handleCommand (_gpsDht state) newTime DHTTransactionsCheck
      put $ state {_gpsDht = newDht}
      let (DHTGetPeersResponse _ peers) = head outputs
      return peers

--------------------
-- Test Packets   --
--------------------
--
-- Test GetPeersQuery
--
propPingQuery :: DHT -> TransactionID -> Node -> Bool
propPingQuery dht transactionID node = outputs == [DHTOutPacket addr outPacket]
  where
    outPacket = Packet transactionID (PingResponse $ dhtID dht)
    inputPacket = Packet transactionID (PingQuery $ nodeToNodeID node)
    (_, outputs) = handlePacket dht addr inputPacket
    addr = nodeToSockAddr node

--
-- Test FindNodeQuery handling
--
propFindNodeQuery :: DHTWithNodes -> Node -> TransactionID -> Bool
propFindNodeQuery (DHTWN dht) node transactionID =
  areReturnedNodesTheClosests && areReturnedNodesKnownNodes
  where
    (_, outputs) =
      handlePacket dht (nodeToSockAddr node) $
      Packet transactionID $ FindNodeQuery targetID targetID
    targetID = nodeToNodeID node
    returnedNodes = S.fromList $ outputs >>= outputToNodes
    outputToNodes (DHTOutPacket (SockAddrInet _ _) (Packet _ (FindNodeResponse _ nodes))) =
      nodes
    outputToNodes _ = []
    initialNodes = S.fromList $ listDHTNodes dht
    areReturnedNodesKnownNodes = returnedNodes `S.isSubsetOf` initialNodes
    nonReturnedNodes = initialNodes S.\\ returnedNodes
    areReturnedNodesTheClosests =
      areNodesCloserTo
        targetID
        (S.toList returnedNodes)
        (S.toList nonReturnedNodes)

--
-- Test GetPeersQuery
--
propGetPeersQuery :: DHTWithPeers -> Node -> TransactionID -> Bool
propGetPeersQuery (DHTWP dht ih peers) node transactionID =
  if null peers
    then isOutputResponseWithNodes
    else isOutputResponseWithPeers && areReturnedPeersKnown
  where
    (_, outputs) =
      handlePacket dht (nodeToSockAddr node) $
      Packet transactionID $ GetPeersQuery (nodeToNodeID node) ih
    returnedPeers = outputs >>= outputToPeers
    outputToPeers (DHTOutPacket _ (Packet _ (GetPeersWithPeersResponse _ _ p))) =
      p
    outputToPeers _ = []
    isOutputResponseWithPeers = not $ null returnedPeers
    isOutputResponseWithNodes = null returnedPeers
    areReturnedPeersKnown =
      S.fromList returnedPeers `S.isSubsetOf` S.fromList peers

--
-- Test AnnouncePeerQuery
--
propAnnouncePeerQuery ::
     DHTWithToken -> InfoHash -> TransactionID -> PortNumber -> Bool
propAnnouncePeerQuery (DHTWT dht node token) infoHash transactionID port =
  outputIsCorrect && storeContainsPeer
  where
    outputIsCorrect = all isExpectedOutput outputs
    isExpectedOutput (DHTOutPacket _ (Packet _ (AnnouncePeerResponse nID))) =
      nID == (dhtID dht)
    isExpectedOutput (DHTOutEvent (DHTPeerAdded outputIH outputPeer)) =
      outputIH == infoHash && outputPeer == peer
    isExpectedOutput _ = False
    storedPeers = listDHTPeerAssocs newDHT
    expectedPeers = [(infoHash, [peer])]
    storeContainsPeer = storedPeers == expectedPeers
    (newDHT, outputs) =
      handlePacket dht (nodeToSockAddr node) $
      Packet transactionID $
      AnnouncePeerQuery (nodeToNodeID node) infoHash portAsInt token
    portAsInt = (fromIntegral . toInteger) port
    Node _ _ nodeAddr = node
    peer = Peer port nodeAddr

-- Test FindNodeResponse by checking that the function handling
-- FindNodeResponse sends a FindNodeQuery only if the node is closer to the
-- dhtID than any other already known nodes.
propFindNodeResponse ::
     DHTWithNodes -> Node -> TransactionID -> [Node] -> Expectation
propFindNodeResponse (DHTWN dht) node transactionID foundNodes =
  contactedHosts `shouldMatchList` (nodeToHost <$> closerNodes)
  where
    initialNodes = listDHTNodes dht
    (_, outputs) =
      handlePacket dht (nodeToSockAddr node) $
      Packet transactionID $ FindNodeResponse (nodeToNodeID node) foundNodes
    myID = dhtID dht
    closerNodes = filter isNodeCloser foundNodes
    isNodeCloser node = all (\n -> D.isCloser myID n node) initialNodes
    nodeToHost (Node _ pn ha) = (pn, ha)
    contactedHosts = outputs >>= maybeToList . outputToHost
    outputToHost (DHTOutPacket (SockAddrInet pn ha) _) = Just (pn, ha)
    outputToHost _ = Nothing

--
-- Helpers
--
-- Return true if all node from the first list are closer to the target that
-- all nodes of the second list
areNodesCloserTo :: NodeID -> [Node] -> [Node] -> Bool
areNodesCloserTo _ [] [] = True
areNodesCloserTo _ [] _ = False
areNodesCloserTo _ _ [] = True
areNodesCloserTo target first second = maxFirstDistances <= minSecondDistances
  where
    maxFirstDistances = maximum . (fmap $ D.distanceTo target) $ first
    minSecondDistances = minimum . (fmap $ D.distanceTo target) $ second
