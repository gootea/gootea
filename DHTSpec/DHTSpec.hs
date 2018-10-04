module DHTSpec where

import Control.Applicative (liftA2)

import DHT.DHT
import DHT.Node
import DHT.NodeID
import DHT.Peer
import DHT.Transactions (TransactionID(..))
import DHT.Types
import qualified Data.ByteString as B

-- import Data.List ((\\), intersect)
import Data.Maybe
import qualified Data.Set as S
import Network.Socket

-- import Prelude hiding (insert)
-- import System.Exit
import System.Random

-- import Test.HUnit hiding (Node)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.Expectations
import Test.QuickCheck

import Debug.Trace

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
    prop "correctly handle FindNodeResponse" $ propFindNodeResponse

-- Unit tests
-- nodeID = NodeID (B.pack $ replicate 20 97)
-- transactionID = TransactionID $ B.pack [1, 2, 3, 4]
-------------------------
-- Arbitrary Instances --
-------------------------
instance Arbitrary InfoHash where
  arbitrary = InfoHash . B.pack <$> vectorOf 20 arbitrary

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
  DHTWP DHT
        InfoHash
        [Peer]

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
  DHTWT DHT
        Node
        Token

instance Show DHTWithToken where
  show (DHTWT _ n t) =
    "DHTWithToken: token " ++ (show t) ++ " , node: " ++ (show n)

instance Arbitrary DHTWithToken where
  arbitrary = do
    dht <- arbitrary
    node <- arbitrary
    let sockAddr = nodeToSockAddr node
    let (dhtwt, token) = createTokenForSockAddr dht sockAddr
    return $ DHTWT dhtwt node token

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
propFindNodeResponse :: DHTWithNodes -> Node -> TransactionID -> [Node] -> Expectation
propFindNodeResponse (DHTWN dht) node transactionID foundNodes =
  contactedHosts `shouldMatchList` (nodeToHost <$> closerNodes)
  where
    initialNodes = listDHTNodes dht
    (_, outputs) =
      handlePacket dht (nodeToSockAddr node) $
      Packet transactionID $ FindNodeResponse (nodeToNodeID node) foundNodes
    myID = dhtID dht
    closerNodes = filter isNodeCloser foundNodes
    isNodeCloser node =
      all (\n -> (distanceTo myID node) < (distanceTo myID n)) initialNodes
    nodeToHost (Node _ pn ha) = (pn, ha)
    contactedHosts = outputs >>= maybeToList . outputToHost
    outputToHost (DHTOutPacket (SockAddrInet pn ha) _) = Just (pn, ha)
    outputToHost _ = Nothing

--
-- Helpers
--
-- Return true is all node from the first list are closer to the target that
-- all nodes of the second list
areNodesCloserTo :: NodeID -> [Node] -> [Node] -> Bool
areNodesCloserTo _ [] [] = True
areNodesCloserTo _ [] _ = False
areNodesCloserTo _ _ [] = True
areNodesCloserTo target first second = maxFirstDistances <= minSecondDistances
  where
    maxFirstDistances = maximum . (fmap $ distanceTo target) $ first
    minSecondDistances = minimum . (fmap $ distanceTo target) $ second
