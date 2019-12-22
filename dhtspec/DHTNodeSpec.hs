module DHTNodeSpec where

import Control.Applicative (liftA2)
import Control.Monad
import DHT.Node
import DHT.NodeID
import qualified Data.ByteString as B
import Data.List ((\\))
import Network.Socket
import Prelude hiding (insert)
import System.Exit
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "NodeID" $ do
    it "correctly creates compact node representation" $ do
      testToCompactNodeInfo
    it "correctly reads compact node representation" $ do
      testFromCompactNodeInfo

-- Unit tests
nodeID =
  NodeID
    (B.pack
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
       ])

node = Node nodeID 65280 (tupleToHostAddress (127, 0, 0, 1))

nodeBytes =
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
    , 255
    , 0
    ]

testToCompactNodeInfo = assertEqual "to compact node info" expected res
  where
    expected = nodeBytes
    res = toCompactNodeInfo node

testFromCompactNodeInfo = assertEqual "to compact node info" expected res
  where
    expected = Just node
    res = fromCompactNodeInfo nodeBytes

-- Property tests
instance Arbitrary NodeID where
  arbitrary = fmap NodeID genNodeID
    where
      genNodeID = fmap B.pack $ vectorOf 20 arbitrary
