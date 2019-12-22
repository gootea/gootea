module RoutingTableSpec where

import BEncode
import Control.Applicative (liftA2)
import Control.Monad
import qualified DHT.Distance as D
import DHT.Node (Node(..))
import DHT.NodeID (NodeID(..))
import DHT.Routing.Table
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List ((\\), sortOn)
import qualified Data.Map.Strict as M
import Network.Socket
import Prelude hiding (insert)
import System.Exit
import Test.HUnit hiding (Node)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "DHT Routing Table" $ do
    it "insert and find a node" $ do testInsertFindNode
    it "insert nodes into right buckets" $ do testInsertNodesIntoBuckets
    it "should find the closest nodes" $ do testFindClosestNodes1
    it "should find the closest nodes" $ do testFindClosestNodes2
    prop "find closests nodes" $ propFindClosestNodes

-- Unit tests
hostAddr = tupleToHostAddress (127, 0, 0, 1)

hostPort = 1 :: PortNumber

buildNode nodeID = Node nodeID 42 (tupleToHostAddress (127, 0, 0, 1))

testInsertFindNode =
  assertEqual "insert and find a node in a routing table" expected res
  where
    expected = Just node
    res = findNode tableWithNode nodeID
    tableWithNode = insert (emptyTable tableID) node
    nodeID = NodeID $ C.pack "12345678901234567890"
    node = Node nodeID hostPort hostAddr
    tableID = NodeID $ C.pack "abcdefghijklmnopqrst"

testInsertNodesIntoBuckets =
  assertEqual "insert nodes into appropriate buckets" expected res
  where
    tableID =
      NodeID $
      B.pack
        [ 13
        , 34
        , 14
        , 3
        , 59
        , 38
        , 23
        , 27
        , 26
        , 20
        , 39
        , 35
        , 54
        , 2
        , 45
        , 63
        , 44
        , 29
        , 43
        , 30
        ]
    targetID =
      NodeID $
      B.pack
        [ 60
        , 59
        , 55
        , 56
        , 35
        , 45
        , 19
        , 60
        , 55
        , 53
        , 46
        , 3
        , 53
        , 38
        , 4
        , 62
        , 0
        , 2
        , 61
        , 62
        ]
    id1 =
      [ 2
      , 49
      , 52
      , 13
      , 35
      , 64
      , 31
      , 38
      , 28
      , 33
      , 43
      , 38
      , 30
      , 10
      , 34
      , 0
      , 27
      , 17
      , 57
      , 18
      ]
    id2 =
      [ 56
      , 17
      , 37
      , 45
      , 2
      , 52
      , 11
      , 52
      , 38
      , 47
      , 49
      , 24
      , 20
      , 40
      , 59
      , 5
      , 8
      , 5
      , 17
      , 40
      ]
    id3 =
      [ 54
      , 5
      , 47
      , 36
      , 49
      , 42
      , 11
      , 29
      , 42
      , 14
      , 7
      , 17
      , 50
      , 1
      , 33
      , 4
      , 41
      , 12
      , 10
      , 63
      ]
    id4 =
      [ 44
      , 22
      , 1
      , 19
      , 61
      , 15
      , 57
      , 20
      , 34
      , 11
      , 11
      , 36
      , 32
      , 27
      , 24
      , 32
      , 59
      , 48
      , 29
      , 2
      ]
    id5 =
      [ 55
      , 8
      , 50
      , 8
      , 37
      , 2
      , 51
      , 46
      , 64
      , 4
      , 2
      , 61
      , 40
      , 26
      , 33
      , 60
      , 26
      , 25
      , 62
      , 49
      ]
    id6 =
      [38, 12, 6, 49, 55, 9, 57, 7, 40, 24, 51, 7, 51, 29, 20, 54, 6, 2, 36, 38]
    id7 =
      [ 35
      , 4
      , 6
      , 11
      , 33
      , 46
      , 34
      , 3
      , 33
      , 47
      , 31
      , 36
      , 53
      , 12
      , 24
      , 20
      , 50
      , 57
      , 24
      , 61
      ]
    id8 =
      [ 40
      , 63
      , 16
      , 12
      , 38
      , 44
      , 18
      , 22
      , 17
      , 22
      , 24
      , 53
      , 20
      , 26
      , 9
      , 30
      , 44
      , 15
      , 8
      , 64
      ]
    id9 =
      [ 0
      , 24
      , 29
      , 9
      , 57
      , 18
      , 64
      , 62
      , 24
      , 53
      , 33
      , 4
      , 4
      , 42
      , 18
      , 61
      , 51
      , 28
      , 31
      , 22
      ] -- Not inserted
    id10 =
      [ 13
      , 8
      , 50
      , 8
      , 37
      , 2
      , 51
      , 46
      , 64
      , 4
      , 2
      , 61
      , 40
      , 26
      , 33
      , 60
      , 26
      , 25
      , 62
      , 49
      ]
    id11 =
      [13, 12, 6, 49, 55, 9, 57, 7, 40, 24, 51, 7, 51, 29, 20, 54, 6, 2, 36, 38]
    id12 =
      [ 13
      , 4
      , 6
      , 11
      , 33
      , 46
      , 34
      , 3
      , 33
      , 47
      , 31
      , 36
      , 53
      , 12
      , 24
      , 20
      , 50
      , 57
      , 24
      , 61
      ]
    id13 =
      [ 13
      , 63
      , 16
      , 12
      , 38
      , 44
      , 18
      , 22
      , 17
      , 22
      , 24
      , 53
      , 20
      , 26
      , 9
      , 30
      , 44
      , 15
      , 8
      , 64
      ]
    nodes =
      intsToNodes
        [id1, id2, id3, id4, id5, id6, id7, id8, id9, id10, id11, id12, id13]
    res = listNodes (tableWithNodes tableID nodes)
    expected =
      intsToNodes
        [id8, id7, id6, id5, id4, id3, id2, id1, id13, id12, id11, id10]

testFindClosestNodes1 = assertEqual "find closest nodes 1" expected res
  where
    tableID =
      NodeID $
      B.pack
        [ 13
        , 34
        , 14
        , 3
        , 59
        , 38
        , 23
        , 27
        , 26
        , 20
        , 39
        , 35
        , 54
        , 2
        , 45
        , 63
        , 44
        , 29
        , 43
        , 30
        ]
    targetID =
      NodeID $
      B.pack
        [ 60
        , 59
        , 55
        , 56
        , 35
        , 45
        , 19
        , 60
        , 55
        , 53
        , 46
        , 3
        , 53
        , 38
        , 4
        , 62
        , 0
        , 2
        , 61
        , 62
        ]
    nodes = intsToNodes [id1, id2, id3, id4, id5, id6, id7, id8, id9]
    id1 =
      [ 2
      , 49
      , 52
      , 13
      , 35
      , 64
      , 31
      , 38
      , 28
      , 33
      , 43
      , 38
      , 30
      , 10
      , 34
      , 0
      , 27
      , 17
      , 57
      , 18
      ]
    id2 =
      [ 56
      , 17
      , 37
      , 45
      , 2
      , 52
      , 11
      , 52
      , 38
      , 47
      , 49
      , 24
      , 20
      , 40
      , 59
      , 5
      , 8
      , 5
      , 17
      , 40
      ]
    id3 =
      [ 54
      , 5
      , 47
      , 36
      , 49
      , 42
      , 11
      , 29
      , 42
      , 14
      , 7
      , 17
      , 50
      , 1
      , 33
      , 4
      , 41
      , 12
      , 10
      , 63
      ]
    id4 =
      [ 44
      , 22
      , 1
      , 19
      , 61
      , 15
      , 57
      , 20
      , 34
      , 11
      , 11
      , 36
      , 32
      , 27
      , 24
      , 32
      , 59
      , 48
      , 29
      , 2
      ]
    id5 =
      [ 55
      , 8
      , 50
      , 8
      , 37
      , 2
      , 51
      , 46
      , 64
      , 4
      , 2
      , 61
      , 40
      , 26
      , 33
      , 60
      , 26
      , 25
      , 62
      , 49
      ]
    id6 =
      [38, 12, 6, 49, 55, 9, 57, 7, 40, 24, 51, 7, 51, 29, 20, 54, 6, 2, 36, 38]
    id7 =
      [ 35
      , 4
      , 6
      , 11
      , 33
      , 46
      , 34
      , 3
      , 33
      , 47
      , 31
      , 36
      , 53
      , 12
      , 24
      , 20
      , 50
      , 57
      , 24
      , 61
      ]
    id8 =
      [ 40
      , 63
      , 16
      , 12
      , 38
      , 44
      , 18
      , 22
      , 17
      , 22
      , 24
      , 53
      , 20
      , 26
      , 9
      , 30
      , 44
      , 15
      , 8
      , 64
      ]
    id9 =
      [ 0
      , 24
      , 29
      , 9
      , 57
      , 18
      , 64
      , 62
      , 24
      , 53
      , 33
      , 4
      , 4
      , 42
      , 18
      , 61
      , 51
      , 28
      , 31
      , 22
      ] -- Not inserted, bucket full
    res = findClosests (tableWithNodes tableID nodes) targetID
    expected = intsToNodes [id2, id3, id5, id4, id8, id6, id7, id1]

testFindClosestNodes2 = assertEqual "find closest nodes 2" expected res
  where
    tableID =
      NodeID $
      B.pack
        [ 7
        , 39
        , 4
        , 17
        , 2
        , 39
        , 58
        , 0
        , 29
        , 44
        , 56
        , 64
        , 32
        , 51
        , 47
        , 22
        , 30
        , 19
        , 27
        , 62
        ]
    targetID =
      NodeID $
      B.pack
        [ 43
        , 32
        , 5
        , 55
        , 37
        , 6
        , 43
        , 25
        , 59
        , 13
        , 47
        , 54
        , 41
        , 39
        , 53
        , 25
        , 55
        , 13
        , 61
        , 3
        ]
    nodes = intsToNodes [id1, id2, id3, id4, id5, id6, id7, id8, id9]
    id1 =
      [ 39
      , 38
      , 29
      , 28
      , 52
      , 58
      , 13
      , 49
      , 34
      , 9
      , 56
      , 9
      , 56
      , 31
      , 23
      , 46
      , 62
      , 39
      , 15
      , 21
      ]
    id2 =
      [ 0
      , 18
      , 42
      , 23
      , 20
      , 10
      , 34
      , 15
      , 11
      , 26
      , 17
      , 28
      , 24
      , 23
      , 62
      , 41
      , 0
      , 6
      , 33
      , 9
      ]
    id3 =
      [ 57
      , 64
      , 9
      , 9
      , 40
      , 32
      , 64
      , 26
      , 5
      , 50
      , 39
      , 13
      , 59
      , 7
      , 50
      , 35
      , 2
      , 62
      , 57
      , 10
      ]
    id4 =
      [ 28
      , 19
      , 16
      , 4
      , 51
      , 56
      , 16
      , 35
      , 47
      , 64
      , 55
      , 60
      , 9
      , 48
      , 56
      , 47
      , 13
      , 63
      , 14
      , 46
      ]
    id5 =
      [ 20
      , 1
      , 9
      , 24
      , 24
      , 57
      , 47
      , 18
      , 5
      , 39
      , 22
      , 3
      , 62
      , 35
      , 4
      , 55
      , 40
      , 10
      , 33
      , 27
      ]
    id6 =
      [ 54
      , 42
      , 57
      , 42
      , 12
      , 33
      , 27
      , 3
      , 59
      , 61
      , 52
      , 62
      , 44
      , 17
      , 63
      , 17
      , 39
      , 56
      , 27
      , 10
      ]
    id7 =
      [ 3
      , 55
      , 5
      , 18
      , 33
      , 52
      , 11
      , 40
      , 6
      , 13
      , 42
      , 27
      , 31
      , 3
      , 30
      , 38
      , 10
      , 5
      , 29
      , 22
      ]
    id8 =
      [ 55
      , 12
      , 39
      , 34
      , 40
      , 35
      , 30
      , 30
      , 59
      , 45
      , 51
      , 15
      , 63
      , 32
      , 55
      , 10
      , 42
      , 0
      , 55
      , 7
      ]
    id9 =
      [ 24
      , 52
      , 52
      , 20
      , 64
      , 41
      , 62
      , 39
      , 41
      , 12
      , 5
      , 63
      , 44
      , 43
      , 49
      , 54
      , 64
      , 57
      , 48
      , 50
      ]
    res = findClosests (tableWithNodes tableID nodes) targetID
    expected = intsToNodes [id1, id3, id8, id6, id7, id2, id4, id5]

-- Helpers
tableWithNodes tableID = foldl insert (emptyTable tableID)

intsToNodes = fmap (buildNode . NodeID . B.pack)

-- Property tests
instance Arbitrary Node where
  arbitrary = fmap buildNode arbitrary

instance Arbitrary NodeID where
  arbitrary = fmap NodeID genNodeID
    where
      genNodeID = fmap B.pack $ vectorOf 20 arbitrary

propFindClosestNodes :: Node -> Node -> [Node] -> Bool
propFindClosestNodes (Node tableID _ _) (Node targetID _ _) nodes =
  all isCloser foundNodes
  where
    tableWithNodes = foldl insert (emptyTable tableID) nodes
    foundNodes = findClosests tableWithNodes targetID
    remainingNodes = (listNodes tableWithNodes) \\ foundNodes
    isCloser node = all (fnIsCloser node) remainingNodes
    fnIsCloser close far =
      (D.distanceTo targetID close) <= (D.distanceTo targetID far)
