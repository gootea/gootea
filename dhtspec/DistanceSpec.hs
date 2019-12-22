module DistanceSpec where

import Control.Applicative (liftA2)
import Control.Monad
import DHT.Distance
import DHT.NodeID
import qualified Data.ByteString as B
import Data.List ((\\))
import Prelude hiding (insert)
import System.Exit
import System.Random
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "NodeID" $ do
    it "correctly compare NodeID - case 1" $ do testCompareXorDistance1
    it "correctly compare NodeID - case 2" $ do testCompareXorDistance2
    it "correctly compare NodeID - case 3" $ do testCompareXorDistance3
    it "correctly compare NodeID - case 4" $ do testCompareXorDistance4
        -- prop "generate random NodeID" $ propGenRandomNodeID
    it "should correctly generate random NodeID" $ do testGenRandomNodeID

-- Unit tests
cmpXorDistance r a b =
  (XorDistance (NodeID r) (NodeID a)) `compare`
  (XorDistance (NodeID r) (NodeID b))

testCompareXorDistance1 = assertEqual "compare distance case 1" expected res
  where
    expected = LT
    res = cmpXorDistance ref idA idB
    ref = B.pack [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 85]
    idA = B.pack [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 42]
    idB = B.pack [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 170]

testCompareXorDistance2 = assertEqual "compare distance case 2" expected res
  where
    expected = GT
    res = cmpXorDistance ref idA idB
    ref = B.pack [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    idA = B.pack [12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 42]
    idB = B.pack [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 170]

testCompareXorDistance3 = assertEqual "compare distance case 3" expected res
  where
    expected = EQ
    res = cmpXorDistance id id id
    id =
      B.pack
        [ 42
        , 42
        , 42
        , 42
        , 42
        , 42
        , 42
        , 42
        , 42
        , 42
        , 42
        , 42
        , 42
        , 42
        , 42
        , 42
        , 42
        , 42
        , 42
        , 42
        ]

testCompareXorDistance4 = assertEqual "compare distance case 4" expected res
  where
    expected = GT
    res = cmpXorDistance ref idA idB
    ref =
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
    idA =
      B.pack
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
    idB =
      B.pack
        [ 38
        , 12
        , 6
        , 49
        , 55
        , 9
        , 57
        , 7
        , 40
        , 24
        , 51
        , 7
        , 51
        , 29
        , 20
        , 54
        , 6
        , 2
        , 36
        , 38
        ]

testGenRandomNodeID = do
  NodeID bytes <- randomIO
  assertEqual "nodeID size" 20 (B.length bytes)
