module RoutingBucketSpec where

import BEncode
import Control.Applicative (liftA2)
import Control.Monad
import DHT.Node (Node(..))
import DHT.NodeID (NodeID(..))
import DHT.Routing.Bucket
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network.Socket
import System.Exit
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "DHT Routing Bucket" $ do
    it "insert and find a node" $ do testInsertFindNode
    it "update a node if it exists" $ do testUpdateNodeIfExists
    it "insert a node if it doesn't exists" $ do testInsertNodeIfNotExists
    it "should limit insertion up to 8 nodes" $ do testLimitInsertion
        -- prop "(decode . encode) = id" $ propEncodeDecode

-- Unit tests
nodeID = NodeID $ C.pack "12345678901234567890"

hostAddr = tupleToHostAddress (127, 0, 0, 1)

hostPort = 1 :: PortNumber

buildNode nodeID = Node nodeID hostPort (tupleToHostAddress (127, 0, 0, 1))

testInsertFindNode = assertEqual "an inserted node should be found" expected res
  where
    expected = Just node
    res = findNode bucketWithNode nodeID
    bucketWithNode = insert emptyBucket node
    node = Node nodeID 1 (tupleToHostAddress (127, 0, 0, 1))

testUpdateNodeIfExists =
  assertEqual "an existing node should be update" expected res
  where
    expected = Just $ Node nodeID 42 hostAddr
    res = findNode updatedBucket nodeID
    updatedBucket = insertOrUpdate bucket node fnUpdate nodeID
    node = buildNode nodeID
    bucket = insert emptyBucket node
    fnUpdate (Node i _ h) = Node i 42 h

testInsertNodeIfNotExists =
  assertEqual "an node should be inserted" expected res
  where
    expected = Just node
    res = findNode updatedBucket nodeID
    updatedBucket = insertOrUpdate emptyBucket node fnUpdate nodeID
    node = buildNode nodeID
    fnUpdate (Node i _ h) = Node i 42 h

testLimitInsertion =
  assertEqual "only 8 nodes are inserted" 8 (length insertedNodes)
  where
    nodes = fmap (buildNode . NodeID . B.pack . (\i -> [i .. i + 19])) [1 .. 20]
    bucket :: Bucket
    bucket = foldl (\b -> \n -> insert b n) emptyBucket nodes
    insertedNodes = listNodes bucket
