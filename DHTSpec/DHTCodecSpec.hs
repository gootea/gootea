module DHTCodecSpec where

import BEncode (BType(..))
import Control.Applicative
import DHT.Codec
import DHT.Node
import DHT.NodeID
import DHT.Types
import DHT.Transactions (TransactionID(..), TransactionType(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M
import Network.Socket
import System.Exit
import Test.HUnit hiding (Node)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "BEncode" $ do
    it "should encode and decode a ping query" $ do testPingQuery
    it "should encode and decode a find node query" $ do testFindNodeQuery
    it "should encode and decode a get peers query" $ do testGetPeersQuery
    it "should encode and decode an announce peer query" $ do
      testAnnouncePeerQuery
    it "should encode and decode a ping response" $ do testPingResponse
    it "should encode and decode a find node response" $ do testFindNodeResponse
    it "should encode and decode a get peers with peers response" $ do
      testGetPeersWithPeersResponse
    it "should encode and decode a get peers with nodes response" $ do
      testGetPeersWithNodesResponse
    it "should encode and decode an announce peer response" $ do
      testAnnouncePeerResponse
    prop "(decode . encode) = id" $ propEncodeDecode

-- Unit tests
nodeIDbytes = "aaaaaaaaaaaaaaaaaaaa"

nodeID = NodeID $ C.pack nodeIDbytes

nodeID2 = NodeID $ C.pack "nnnnnnnnnnnnnnnnnnnn"

hostPort = 42 :: PortNumber

hostAddr = tupleToHostAddress (127, 0, 0, 1)

testPingQuery = do
  assertEqual "a ping query is correctly encoded" encoded (encode packet)
  assertEqual
    "a ping query is correctly decoded"
    (Just packet)
    (decode fnType encoded)
  where
    fnType _ = Just Ping
    packet = Packet (TransactionID $ C.pack "transactionID") (PingQuery nodeID)
    encoded =
      BDict $
      M.fromList
        [ (C.pack "t", BString $ C.pack "transactionID")
        , (C.pack "y", BString $ C.pack "q")
        , (C.pack "q", BString $ C.pack "ping")
        , ( C.pack "a"
          , BDict $ M.fromList [(C.pack "id", BString $ C.pack nodeIDbytes)])
        ]

testFindNodeQuery = do
  assertEqual "a find node query is correctly encoded" encoded (encode packet)
  assertEqual
    "a find node query is correctly decoded"
    (Just packet)
    (decode fnType encoded)
  where
    fnType _ = Just FindNode
    packet =
      Packet
        (TransactionID $ C.pack "transactionID")
        (FindNodeQuery nodeID nodeID)
    encoded =
      BDict $
      M.fromList
        [ (C.pack "t", BString $ C.pack "transactionID")
        , (C.pack "y", BString $ C.pack "q")
        , (C.pack "q", BString $ C.pack "find_node")
        , ( C.pack "a"
          , BDict $
            M.fromList
              [ (C.pack "id", BString $ C.pack nodeIDbytes)
              , (C.pack "target", BString $ C.pack nodeIDbytes)
              ])
        ]

testGetPeersQuery = do
  assertEqual "a get peers query is correctly encoded" encoded (encode packet)
  assertEqual
    "a get peers query is correctly decoded"
    (Just packet)
    (decode fnType encoded)
  where
    fnType _ = Just GetPeers
    packet =
      Packet
        (TransactionID $ C.pack "transactionID")
        (GetPeersQuery nodeID (InfoHash $ C.pack "infohash"))
    encoded =
      BDict $
      M.fromList
        [ (C.pack "t", BString $ C.pack "transactionID")
        , (C.pack "y", BString $ C.pack "q")
        , (C.pack "q", BString $ C.pack "get_peers")
        , ( C.pack "a"
          , BDict $
            M.fromList
              [ (C.pack "id", BString $ C.pack nodeIDbytes)
              , (C.pack "info_hash", BString $ C.pack "infohash")
              ])
        ]

testAnnouncePeerQuery = do
  assertEqual
    "an announce peer query is correctly encoded"
    encoded
    (encode packet)
  assertEqual
    "an announce peer query is correctly decoded"
    (Just packet)
    (decode fnType encoded)
  where
    fnType _ = Just AnnouncePeer
    packet =
      Packet
        (TransactionID $ C.pack "transactionID")
        (AnnouncePeerQuery
           nodeID
           (InfoHash $ C.pack "infohash")
           6881
           (Token $ C.pack "token"))
    encoded =
      BDict $
      M.fromList
        [ (C.pack "t", BString $ C.pack "transactionID")
        , (C.pack "y", BString $ C.pack "q")
        , (C.pack "q", BString $ C.pack "announce_peer")
        , ( C.pack "a"
          , BDict $
            M.fromList
              [ (C.pack "id", BString $ C.pack nodeIDbytes)
              , (C.pack "info_hash", BString $ C.pack "infohash")
              , (C.pack "port", BInt 6881)
              , (C.pack "token", BString $ C.pack "token")
              ])
        ]

testPingResponse = do
  assertEqual "a ping response is correctly encoded" encoded (encode packet)
  assertEqual
    "a ping response is correctly decoded"
    (Just packet)
    (decode fnType encoded)
  where
    fnType _ = Just Ping
    packet =
      Packet (TransactionID $ C.pack "transactionID") (PingResponse nodeID)
    encoded =
      BDict $
      M.fromList
        [ (C.pack "t", BString $ C.pack "transactionID")
        , (C.pack "y", BString $ C.pack "r")
        , ( C.pack "r"
          , BDict $ M.fromList [(C.pack "id", BString $ C.pack nodeIDbytes)])
        ]

testFindNodeResponse = do
  assertEqual
    "a find node response is correctly encoded"
    encoded
    (encode packet)
  assertEqual
    "a find node response is correctly decoded"
    (Just packet)
    (decode fnType encoded)
  where
    fnType _ = Just FindNode
    packet =
      Packet
        (TransactionID $ C.pack "transactionID")
        (FindNodeResponse nodeID nodes)
    nodes = [Node nodeID hostPort hostAddr, Node nodeID2 hostPort hostAddr]
    encoded =
      BDict $
      M.fromList
        [ (C.pack "t", BString $ C.pack "transactionID")
        , (C.pack "y", BString $ C.pack "r")
        , ( C.pack "r"
          , BDict $
            M.fromList
              [ (C.pack "id", BString $ C.pack nodeIDbytes)
              , (C.pack "nodes", BString nodesBytes)
              ])
        ]
    nodesBytes =
      B.pack
        [ 97
        , 97
        , 97
        , 97
        , 97
        , 97
        , 97
        , 97
        , 97
        , 97
        , 97
        , 97
        , 97
        , 97
        , 97
        , 97
        , 97
        , 97
        , 97
        , 97
        , 127
        , 0
        , 0
        , 1
        , 0
        , 42
        , 110
        , 110
        , 110
        , 110
        , 110
        , 110
        , 110
        , 110
        , 110
        , 110
        , 110
        , 110
        , 110
        , 110
        , 110
        , 110
        , 110
        , 110
        , 110
        , 110
        , 127
        , 0
        , 0
        , 1
        , 0
        , 42
        ]

testGetPeersWithPeersResponse = do
  assertEqual
    "a get peers response with peers is correctly encoded"
    encoded
    (encode packet)
  assertEqual
    "a get peers response with peers is correctly decoded"
    (Just packet)
    (decode fnType encoded)
  where
    fnType _ = Just GetPeers
    packet =
      Packet
        (TransactionID $ C.pack "transactionID")
        (GetPeersWithPeersResponse
           nodeID
           (Token $ C.pack "token")
           [Peer 42 $ tupleToHostAddress (127, 0, 0, 1)])
    encoded =
      BDict $
      M.fromList
        [ (C.pack "t", BString $ C.pack "transactionID")
        , (C.pack "y", BString $ C.pack "r")
        , ( C.pack "r"
          , BDict $
            M.fromList
              [ (C.pack "id", BString $ C.pack nodeIDbytes)
              , (C.pack "token", BString $ C.pack "token")
              , (C.pack "values", BString $ B.pack [127, 0, 0, 1, 0, 42])
              ])
        ]

testGetPeersWithNodesResponse = do
  assertEqual
    "a get peers response with peers is correctly encoded"
    encoded
    (encode packet)
  assertEqual
    "a get peers response with peers is correctly decoded"
    (Just packet)
    (decode fnType encoded)
  where
    fnType _ = Just GetPeers
    packet =
      Packet
        (TransactionID $ C.pack "transactionID")
        (GetPeersWithNodesResponse nodeID (Token $ C.pack "token") [node])
    encoded =
      BDict $
      M.fromList
        [ (C.pack "t", BString $ C.pack "transactionID")
        , (C.pack "y", BString $ C.pack "r")
        , ( C.pack "r"
          , BDict $
            M.fromList
              [ (C.pack "id", BString $ C.pack nodeIDbytes)
              , (C.pack "token", BString $ C.pack "token")
              , (C.pack "nodes", nodesBytes)
              ])
        ]
    node =
      Node
        (NodeID $
         B.pack [8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8])
        42
        (tupleToHostAddress (127, 0, 0, 1))
    nodesBytes =
      BString $
      B.pack
        [ 8
        , 8
        , 8
        , 8
        , 8
        , 8
        , 8
        , 8
        , 8
        , 8
        , 8
        , 8
        , 8
        , 8
        , 8
        , 8
        , 8
        , 8
        , 8
        , 8
        , 127
        , 0
        , 0
        , 1
        , 0
        , 42
        ]

testAnnouncePeerResponse = do
  assertEqual
    "an announce peer response is correctly encoded"
    encoded
    (encode packet)
  assertEqual
    "an announce peer response is correctly decoded"
    (Just packet)
    (decode fnType encoded)
  where
    fnType _ = Just AnnouncePeer
    packet =
      Packet
        (TransactionID $ C.pack "transactionID")
        (AnnouncePeerResponse nodeID)
    encoded =
      BDict $
      M.fromList
        [ (C.pack "t", BString $ C.pack "transactionID")
        , (C.pack "y", BString $ C.pack "r")
        , ( C.pack "r"
          , BDict $ M.fromList [(C.pack "id", BString $ C.pack nodeIDbytes)])
        ]

-- Property tests
instance Arbitrary Packet where
  arbitrary = liftA2 Packet arbitrary genDHTMessage
    where
      genDHTMessage =
        oneof
          [ genPingQuery
          , genFindNodeQuery
          , genGetPeersQuery
          , genAnnouncePeerQuery
          , genPingResponse
          , genFindNodeResponse
          , genGetPeersWithPeersResponse
          , genGetPeersWithNodesResponse
          , genAnnouncePeerResponse
          ]
      genPingQuery = fmap PingQuery arbitrary
      genFindNodeQuery = liftA2 FindNodeQuery arbitrary arbitrary
      genGetPeersQuery =
        liftA2 GetPeersQuery arbitrary (fmap InfoHash genByteString)
      genAnnouncePeerQuery =
        liftA3 AnnouncePeerQuery arbitrary (fmap InfoHash arbitrary) arbitrary <*>
        arbitrary
      genPingResponse = fmap PingResponse arbitrary
      genFindNodeResponse = liftA2 FindNodeResponse arbitrary (listOf arbitrary)
      genGetPeersWithPeersResponse =
        liftA3 GetPeersWithPeersResponse arbitrary arbitrary arbitrary
      genGetPeersWithNodesResponse =
        liftA3 GetPeersWithNodesResponse arbitrary arbitrary arbitrary
      genAnnouncePeerResponse = fmap AnnouncePeerResponse arbitrary
      genByteString = fmap C.pack $ listOf arbitrary

instance Arbitrary TransactionID where
  arbitrary = fmap (TransactionID . C.pack) $ listOf arbitrary

instance Arbitrary Node where
  arbitrary = fmap Node arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Peer where
  arbitrary = liftA2 Peer arbitrary arbitrary

instance Arbitrary NodeID where
  arbitrary = fmap NodeID genNodeID
    where
      genNodeID = fmap B.pack $ vectorOf 20 arbitrary

instance Arbitrary PortNumber where
  arbitrary = fmap fromInteger arbitrary

instance Arbitrary Token where
  arbitrary = fmap Token arbitrary

instance Arbitrary B.ByteString where
  arbitrary = fmap B.pack $ listOf arbitrary

propEncodeDecode :: Packet -> Bool
propEncodeDecode packet = (fnDecode . encode) packet == Just packet
  where
    fnDecode = decode $ \_ -> getTransactionType dhtMessage
    Packet _ dhtMessage = packet
    getTransactionType :: DHTMessage -> Maybe TransactionType
    getTransactionType (PingQuery _) = Just Ping
    getTransactionType (FindNodeQuery _ _) = Just FindNode
    getTransactionType (GetPeersQuery _ _) = Just GetPeers
    getTransactionType (AnnouncePeerQuery _ _ _ _) = Just AnnouncePeer
    getTransactionType (PingResponse _) = Just Ping
    getTransactionType (FindNodeResponse _ _) = Just FindNode
    getTransactionType (GetPeersWithPeersResponse _ _ _) = Just GetPeers
    getTransactionType (GetPeersWithNodesResponse _ _ _) = Just GetPeers
    getTransactionType (AnnouncePeerResponse _) = Just AnnouncePeer
