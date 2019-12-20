module ChordMessageSpec where

import Test.HUnit hiding (Node)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Chord.ChordMessage
import Chord.ID
import Chord.Node

spec :: Spec
spec = do
  describe "fromCapnp . toCapnp" $ prop "= id" propConvertFromAndToCapnp

propConvertFromAndToCapnp :: ChordMessage -> Assertion
propConvertFromAndToCapnp message =
  (fromCapnp $ toCapnp message) @?= Just message
