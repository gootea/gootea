module FingerTableSpec where

import Test.HUnit hiding (Node)
import Test.Hspec

import Chord.FingerTable
import Chord.ID
import Chord.Node

spec :: Spec
spec = do
  describe "listNodes . addNode" $
    it "add and keeps only interesting nodes" testAddAndKeepNodes
  describe "isInterestingNode" $
    it "returns False when Fingers are full" testIsInterestingNodeWithFullFinger
  describe "isResponsibleOfID" $
    it "gives a correct result" testIsResponsibleOfID

-- Helpers
nodeFromID :: Integer -> Node
nodeFromID = newNode . newID

-- Tests
testAddAndKeepNodes :: Assertion
testAddAndKeepNodes =
  let doTest (selfID, nodesToInsert, expectedNodes) =
        testResult (fmap nodeFromID expectedNodes) $
        insertNodes (fmap nodeFromID nodesToInsert) $
        newFingerTable (newID selfID)
      testResult expectedNodes finger = listNodes finger @?= expectedNodes
      insertNodes nodesToInsert finger = foldl addNode finger nodesToInsert
      -- (selfID, [nodeIDs], [keptNodeIDs])
      testCases :: [(Integer, [Integer], [Integer])]
      testCases =
        [ ( 2
          , [17, 16, 15, 14, 13, 12, 11, 22, 21, 20, 19, 39, 38, 37, 36, 35]
          , [11, 12, 13, 19, 20, 21, 35, 36, 37])
        , ( 10
          , [11 .. 40]
          , [ 11
            , 12
            , 13 -- Finger 1
            , 14
            , 15
            , 16 -- Finger 2
            , 18
            , 19
            , 20 -- Finger 3
            , 26
            , 27
            , 28
            ])
        ]
   in sequence_ . fmap doTest $ testCases

testIsInterestingNodeWithFullFinger :: Assertion
testIsInterestingNodeWithFullFinger =
  let table = foldl addNode emptyTable nodes
      emptyTable = newFingerTable (newID 10)
      nodes = fmap nodeFromID [11 .. 40]
   in sequence_ $ fmap (\node -> isInterestingNode table node @?= False) nodes

testIsResponsibleOfID :: Assertion
testIsResponsibleOfID =
  let fingerTable = foldl addNode emptyTable nodes
      emptyTable = newFingerTable (newID 42)
      nodes =
        fmap
          nodeFromID
          [ 6 -- Not interesting
          , 8 -- First predecessor that marks the limit of responsability
          , 12 -- Second predecessor
          , 20 -- Third predecessor
          , 44 -- First successor
          , 52
          , 60
          , 80
          ]
      testCases =
        [ (8, False) -- First predecessor
        , (9, True)
        , (12, True)
        , (20, True)
        , (41, True)
        , (42, True) -- me
        , (43, False)
        , (6, False)
        , (7, False)
        , (100, False)
        , (0, False)
        ]
      doTest (idToTest, result) =
        assertEqual
          ("ID: " ++ show idToTest)
          result
          (isResponsibleOfID fingerTable (newID idToTest))
   in sequence_ $ fmap doTest testCases
