module ServiceSpec where

import Test.HUnit
import Test.Hspec

import Chord.ChordMessage
import Chord.ID
import Chord.Node
import qualified Chord.Service as S
import Chord.Store

import Cluster.Cluster
import Cluster.InMemoryStore

import Control.Monad.State.Strict as State
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "get . add (localy)" $ do
    it
      "set and get data from the local store when the FingerTable is empty"
      testAddGetWithEmptyFingerTable
  describe "get . add (with multiple services)" $ do
    it
      "set and get data from wherever it is in the topology"
      testAddGetWithMultiplePeers

-- Tests
testAddGetWithEmptyFingerTable :: Assertion
testAddGetWithEmptyFingerTable = do
  let selfID = newID 42
  let valueID = newID 84
  let value = "valueForTest"
  let emptySvc = S.empty selfID newStore
  let (svcWithValue, addOutput) = S.add emptySvc valueID value
  assertBool
    "Output when adding a value should be empty"
    (length addOutput == 0)
  -- Continue the test by testing that the value can be retrieved
  let (_, getOutput) = S.get svcWithValue valueID
  case getOutput of
    [S.GetReply replyID values] -> do
      replyID @?= valueID
      values @?= [value]
    _ -> assertFailure "We should get only a single GetReply output"

testAddGetWithMultiplePeers :: Assertion
testAddGetWithMultiplePeers =
  let numberOfPeers = 16
      chordCirclePart = maxID `div` numberOfPeers
      ids =
        fmap (\i -> (i, newStore)) . fmap newID . fmap ((*) chordCirclePart) $
        [0 .. numberOfPeers]
      cluster = newCluster ids
      key = newID 730750818665451459101842416358141509827966271488
      value = "value"
      transformation = do
        fullyConnectAllPeers
        actOnPeer (fst $ head ids) (\s -> S.add s key value)
        actOnPeer (fst $ last ids) (\s -> S.get s key)
      finalCluster = State.execState transformation cluster
      resultValues = Set.fromList . (=<<) snd . replyQueue $ finalCluster
   in resultValues @?= Set.singleton value
