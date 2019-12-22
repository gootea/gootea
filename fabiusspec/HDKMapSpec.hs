{-# LANGUAGE OverloadedStrings #-}

module HDKMapSpec where

import qualified Data.Set as S
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Fabius.HDKMap
import Fabius.Key
import Fabius.Term

spec :: Spec
spec = do
  describe "HDKMap" $ do
    it "create a new HDKMap with the right keys" testCreateMapWithRightKeys
    it
      "handle a Non Discriminative Key correctly"
      testReportNonDiscriminativeKey
    it
      "handle two Non Discriminative Key correctly"
      testReport2NonDiscriminativeKey
    it
      "handle three Non Discriminative Key correctly"
      testReport3NonDiscriminativeKey

testCreateMapWithRightKeys :: Assertion
testCreateMapWithRightKeys = mapKeys @?= expectedKeys
  where
    map = fromTerms () (S.fromList [term1, term2, term3, term4])
    mapKeys = keys map
    expectedKeys = S.fromList [key1, key2, key3, key4]

testReportNonDiscriminativeKey :: Assertion
testReportNonDiscriminativeKey = do
  newKeys @?= expectedNewKeys
  (keys newMap) @?= expectedKeys
  where
    map = fromTerms () (S.fromList [term1, term2, term3])
    (newMap, newKeys) = reportNonDiscriminativeKey map () key1
    expectedKeys = S.fromList [key1 <> key2, key1 <> key3, key2, key3]
    expectedNewKeys = [key1 <> key2, key1 <> key3]

testReport2NonDiscriminativeKey :: Assertion
testReport2NonDiscriminativeKey = do
  newKeys @?= expectedNewKeys
  (keys newMap) @?= expectedKeys
  where
    map1 = fromTerms () (S.fromList [term1, term2, term3])
    (map2, _) = reportNonDiscriminativeKey map1 () key1
    (newMap, newKeys) = reportNonDiscriminativeKey map2 () (key1 <> key2)
    expectedKeys = S.fromList [key1 <> key2 <> key3, key1 <> key3, key2, key3]
    expectedNewKeys = [key1 <> key2 <> key3]

testReport3NonDiscriminativeKey :: Assertion
testReport3NonDiscriminativeKey = do
  newKeys @?= expectedNewKeys
  (keys newMap) @?= expectedKeys
  where
    map1 = fromTerms () (S.fromList [term1, term2, term3])
    (map2, _) = reportNonDiscriminativeKey map1 () key1
    (map3, _) = reportNonDiscriminativeKey map2 () (key1 <> key2)
    (newMap, newKeys) = reportNonDiscriminativeKey map3 () key2
    expectedKeys =
      S.fromList [key1 <> key2 <> key3, key1 <> key3, key2 <> key3, key3]
    expectedNewKeys = [key2 <> key3]

-- Fixtures
term1 = newTerm "term1"

term2 = newTerm "term2"

term3 = newTerm "term3"

term4 = newTerm "term4"

key1 = newKey term1

key2 = newKey term2

key3 = newKey term3

key4 = newKey term4
