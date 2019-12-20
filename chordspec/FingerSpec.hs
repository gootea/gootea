module FingerSpec where

import Test.HUnit hiding (Node)
import Test.Hspec

import Chord.Finger
import Chord.ID
import Chord.Node
import Network.Socket (SockAddr(SockAddrUnix))

spec :: Spec
spec = do
  describe "isInterestingNode" $ do
    it "returns True for an empty Finger and node in Finger range" $
      testIsInterestingNode (60, 100) 80 True
    it "returns False for an empty Finger and node NOT in Finger range" $
      testIsInterestingNode (60, 100) 65535 True
    -- it "returns True for a node whose ID is lower than the nodes of the Finger" $
    --     testIsInterestingNode ()
    it
      "returns False for all nodes if the Finger is full"
      testIsInterestingNodeFullFinger
  describe "isInFingerRange" $ it "gives correct result" testIsInFingerRange
  describe "nodes . addNode" $
    it "add and keeps only interesting nodes" testAddAndKeepNodes

-- Helpers
nodeFromID :: Integer -> Node
nodeFromID n = newNode (newID n) (SockAddrUnix "finger")

-- Tests
testIsInterestingNode :: (Integer, Integer) -> Integer -> Bool -> Assertion
testIsInterestingNode (lower, upper) nodeID result =
  let node = nodeFromID nodeID
      finger = newFinger (newID lower) (newID upper)
   in isInterestingNode finger node @?= result

testIsInterestingNodeFullFinger :: Assertion
testIsInterestingNodeFullFinger =
  let fullFinger = foldl addNode emptyFinger nodes
      emptyFinger = newFinger (newID 42) (newID 74)
      nodes = fmap nodeFromID [42 .. 73]
   in sequence_ $ fmap (\n -> isInterestingNode fullFinger n @?= False) nodes

testIsInFingerRange :: Assertion
testIsInFingerRange =
  let doTest ((lower, upper), node, result) =
        isInFingerRange
          (newFinger (newID lower) (newID upper))
          (nodeFromID node) @?=
        result
      -- ((lowerID, upperID), nodeID, result)
      testCases =
        [ ((10, 20), 15, True)
        , ((10, 20), 10, True)
        , ((10, 20), 20, False)
        , ((65535, 5), 2, True)
        , ((65535, 5), 80000, True)
        , ((10, 20), 25, False)
        , ((10, 20), 0, False)
        , ((65535, 5), 10, False)
        ]
   in sequence_ . fmap doTest $ testCases

testAddAndKeepNodes :: Assertion
testAddAndKeepNodes =
  let doTest ((lower, upper), nodesToInsert, expectedNodes) =
        testResult (fmap nodeFromID expectedNodes) $
        insertNodes (fmap nodeFromID nodesToInsert) $
        newFinger (newID lower) (newID upper)
      testResult expectedNodes finger = nodes finger @?= expectedNodes
      insertNodes nodesToInsert finger = foldl addNode finger nodesToInsert
      -- ((lowerID, upperID), [nodeIDs], [keptNodeIDs])
      testCases :: [((Integer, Integer), [Integer], [Integer])]
      testCases =
        [ ((10, 50), [49, 48, 47, 46, 45, 44, 11], [11, 44, 45])
        , ((65535, 30), [80000, 90000, 1, 2, 3], [80000, 90000, 1])
        , ((11, 14), [11, 12, 13, 14], [11, 12, 13])
        ]
   in sequence_ . fmap doTest $ testCases
