module LPMessageSpec where

import qualified Data.ByteString as BS
import Network.Socket
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Network.Bittorrent.LPMessage

spec :: Spec
spec = do
  describe "LPMessage" $ do
    it "should extract an LP message from a ByteString" testExtractLPMessage
    it
      "should not extract an LP message from an incomplete ByteString"
      testExtractLPMessageIncomplete

testExtractLPMessage = res `shouldBe` expected
  where
    initialBytes = BS.pack [0, 0, 1, 10] <> messageBytes <> nextMessageBytes
    messageBytes = BS.pack (replicate 266 42)
    nextMessageBytes = BS.pack [1, 2, 3, 4, 5, 6]
    res = extractLPMessage initialBytes
    expected = Just (messageBytes, nextMessageBytes)

testExtractLPMessageIncomplete = res `shouldBe` expected
  where
    initialBytes = BS.pack [0, 0, 2, 10] <> messageBytes
    messageBytes = BS.pack (replicate 266 42)
    res = extractLPMessage initialBytes
    expected = Nothing
