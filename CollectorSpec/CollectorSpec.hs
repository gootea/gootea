module CollectorSpec where

import Collector.Collector
import Data.Maybe
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Collector" $ do
    prop
      "let object go through only once at the appropriate count"
      propFilterThroughCollection

-------------------------
-- Arbitrary Instances --
-------------------------
newtype Object =
  Object Int
  deriving (Show, Eq)

instance Ord Object where
  compare (Object a) (Object b) = compare a b

instance Arbitrary Object where
  arbitrary = Object <$> arbitrary

newtype MinSeenCount =
  MinSeenCount Int
  deriving (Eq, Show)

instance Arbitrary MinSeenCount where
  arbitrary = (MinSeenCount . (+ 1) . abs) <$> arbitrary

-------------------
-- Test Commands --
-------------------
propFilterThroughCollection :: MinSeenCount -> Object -> Bool
propFilterThroughCollection (MinSeenCount minSeenCount) obj =
  (all id .  fmap snd . repeatFilterThrough . newCollection)
    minSeenCount
  where
    repeatFilterThrough :: Collection Object -> [(Collection Object, Bool)]
    repeatFilterThrough c =
      scanl doFilterThrough (c, True) [0 .. minSeenCount + 10]
    doFilterThrough (c, _) count =
      (isResponseCorrect count) <$> filterThroughCollection c obj
    isResponseCorrect count r =
      (isJust r) == (count == minSeenCount)
