module FingerTable.PredecessorsSpec where

import Test.HUnit hiding (Node)
import Test.Hspec

import Chord.FingerTable.Predecessors
import Chord.ID

spec :: Spec
spec = do
  describe "fingerTableIsResponsibleOfID" $
    it "gives a correct result" testFingerTableIsResponsibleOfID

-- Helpers
pWithIDs :: Integer -> [Integer] -> Predecessors ID
pWithIDs self ids =
  let empty = newPredecessors (newID self)
   in foldl addPredecessor empty (fmap newID ids)

-- Tests
testFingerTableIsResponsibleOfID :: Assertion
testFingerTableIsResponsibleOfID =
  let doTest (selfID, ids, testID, result) =
        assertEqual
          ("selfID: " ++ show selfID ++ ", testID" ++ show testID)
          (fingerTableIsResponsibleOfID (pWithIDs selfID ids) (newID testID))
          result
      testCases =
        [ (42, [52, 44, 20, 12, 8], 8, False)
        , (42, [8, 52, 44, 20, 12, 8], 9, True)
        , (42, [8, 20, 52, 44, 12, 8], 20, True)
        , (42, [12, 52, 44, 20, 8], 21, True)
        , (42, [52, 44, 20, 12, 8], 42, True)
        , (42, [52, 44, 20, 12, 8], 43, False)
        , (42, [52, 44, 20, 12, 8], 1, False)
        ]
   in sequence_ $ fmap doTest testCases
