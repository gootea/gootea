module IDSpec where

import Test.HUnit
import Test.Hspec

import Chord.ID

spec :: Spec
spec = do
  describe "isBetweenIE" $ do it "gives correct responses" testIsBetweenIE
  describe "isBetweenEI" $ do it "gives correct responses" testIsBetweenEI
  describe "fingerRange" $ do
    it "gives correct ranges" testFingerRange
    it "gives correct ranges (and loop)" testFingerRangeLoop

testIsBetweenIE :: Assertion
testIsBetweenIE = do
  (isBetweenIE (newID 1) (newID 5) (newID 2)) @?= True
  (isBetweenIE (newID 1) (newID 5) (newID 1)) @?= True
  (isBetweenIE (newID 11) (newID 11) (newID 11)) @?= True
  (isBetweenIE (newID 11) (newID 11) (newID 12)) @?= False
  (isBetweenIE (newID 20) (newID 10) (newID 0)) @?= True
  (isBetweenIE (newID 20) (newID 10) (newID 25)) @?= True
  (isBetweenIE (newID 4000) (newID 0) (newID 5)) @?= False
  (isBetweenIE (newID 2000) (newID 65535) (newID 80000)) @?= False

testIsBetweenEI :: Assertion
testIsBetweenEI =
  let doTest (lower, upper, testID, result) =
        assertEqual
          ("Node with ID: " ++ show testID)
          (isBetweenEI (newID lower) (newID upper) (newID testID))
          result
      testCases =
        [ (1, 5, 1, False)
        , (1, 5, 2, True)
        , (1, 5, 5, True)
        , (11, 11, 11, True)
        , (11, 11, 12, False)
        , (20, 10, 0, True)
        , (20, 10, 10, True)
        , (20, 10, 20, False)
        , (20, 10, 25, True)
        , (4000, 0, 5, False)
        , (2000, 65535, 80000, False)
        ]
   in sequence_ $ fmap doTest testCases

testFingerRange :: Assertion
testFingerRange =
  let base = newID 2
   in do (fingerRange base 0) @?= (newID 3, newID 4)
         (fingerRange base 1) @?= (newID 4, newID 6)
         (fingerRange base 2) @?= (newID 6, newID 10)
         (fingerRange base 3) @?= (newID 10, newID 18)
         (fingerRange base 4) @?= (newID 18, newID 34)

testFingerRangeLoop :: Assertion
testFingerRangeLoop =
  let base = newID 1461501637330902918203684832716283019655932542966 -- 2^160 - 10
   in do (fingerRange base 0) @?=
           ( newID 1461501637330902918203684832716283019655932542967
           , newID 1461501637330902918203684832716283019655932542968)
         (fingerRange base 1) @?=
           ( newID 1461501637330902918203684832716283019655932542968
           , newID 1461501637330902918203684832716283019655932542970)
         (fingerRange base 2) @?=
           ( newID 1461501637330902918203684832716283019655932542970
           , newID 1461501637330902918203684832716283019655932542974)
         (fingerRange base 3) @?=
           (newID 1461501637330902918203684832716283019655932542974, newID 6)
         (fingerRange base 4) @?= (newID 6, newID 22)
         (fingerRange base 5) @?= (newID 22, newID 54)
