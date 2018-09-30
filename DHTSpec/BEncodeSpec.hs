module BEncodeSpec where

import BEncode
import Control.Applicative (liftA2)
import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as M
import System.Exit
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "BEncode" $ do
    it "should decode a string" $ do testString
    it "should decode an integer" $ do testInt
    it "should decode a list" $ do testList
    it "should decode a dict" $ do testDict
    it "should encode a string" $ do testEncodeString
    it "should encode an int" $ do testEncodeInt
    it "should encode a list" $ do testEncodeList
    it "should encode a dict" $ do testEncodeDict
    prop "(decode . encode) = id" $ propEncodeDecode

-- Unit tests
testString = assertEqual "a string is parsed as a BString" res expected
  where
    res = decode $ C.pack "10:abcdefghij"
    expected = Just $ BString $ C.pack "abcdefghij"

testInt = assertEqual "an int is parsed as a BInt" res expected
  where
    res = decode $ C.pack "i256e"
    expected = Just $ BInt 256

testList = assertEqual "a list is parsed as a BList" res expected
  where
    res = decode $ C.pack "li1ei2ei3ee"
    expected = Just $ BList [BInt 1, BInt 2, BInt 3]

testDict = assertEqual "a dict is parsed as a BDict" res expected
  where
    res = decode $ C.pack "d1:ai1e1:bi2ee"
    expected =
      Just $ BDict $ M.fromList [(C.pack "a", BInt 1), (C.pack "b", BInt 2)]

testEncodeString = assertEqual "a BString is encoded" res expected
  where
    res = BLC.pack "10:abcdefghij"
    expected = encode $ BString $ C.pack "abcdefghij"

testEncodeInt = assertEqual "a BInt is encoded" res expected
  where
    res = BLC.pack "i256e"
    expected = encode $ BInt 256

testEncodeList = assertEqual "a BList is encoded" res expected
  where
    res = BLC.pack "li1ei2ei1024ee"
    expected = encode $ BList [BInt 1, BInt 2, BInt 1024]

testEncodeDict = assertEqual "a BDict is encoded" res expected
  where
    res = BLC.pack "d3:cowi2048e4:spami42ee"
    expected =
      encode $
      BDict (M.fromList [(C.pack "cow", BInt 2048), (C.pack "spam", BInt 42)])

-- Property tests
instance Arbitrary BType where
  arbitrary = oneof [genBInt, genBString, genBList, genBMap]
    where
      genByteString = fmap C.pack $ listOf arbitrary
      genBInt = fmap BInt $ suchThat arbitrary (\i -> i >= 0)
      genBString = fmap BString genByteString
      genBList = fmap BList $ listOf $ oneof [genBInt, genBString]
      genBMap =
        fmap (BDict . M.fromList) $
        listOf $ liftA2 (\a -> \b -> (a, b)) genByteString elements
        where
          elements =
            frequency
              [(33, genBInt), (33, genBString), (33, genBList), (1, genBMap)]

propEncodeDecode :: BType -> Bool
propEncodeDecode obj = (decode . BL.toStrict . encode) obj == Just obj
