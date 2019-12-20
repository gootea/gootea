{-# LANGUAGE OverloadedStrings #-}

module Common.Models.InfoHash
  ( InfoHash
  , newInfoHash
  , infoHashToByteString
  , infoHashFromByteString
  ) where

import Data.Bits
import qualified Data.ByteString as B
import Data.Word
import Numeric
import Test.QuickCheck (Arbitrary, arbitrary, vector)

newtype InfoHash =
  InfoHash B.ByteString
  deriving (Eq)

instance Show InfoHash where
  show (InfoHash ih) =
    "(InfoHash " ++ (concat $ convertByte <$> (B.unpack ih)) ++ ")"
    where
      convertByte :: Word8 -> String
      convertByte b = showHex (shiftR b 4) "" ++ (showHex (b .&. 15) "")

instance Ord InfoHash where
  compare (InfoHash a) (InfoHash b) = compare a b

instance Arbitrary InfoHash where
  arbitrary = InfoHash <$> B.pack <$> (vector 20)

-- | Create a new InfoHash from a ByteString
newInfoHash :: B.ByteString -> InfoHash
newInfoHash = InfoHash

-- | Return a ByteString for a given InfoHash
infoHashToByteString :: InfoHash -> B.ByteString
infoHashToByteString (InfoHash bs) = bs

-- | Return a new InfoHash from a ByteString. The difference with `newInfoHash`
-- is that it check the size of the ByteString
infoHashFromByteString :: B.ByteString -> Maybe InfoHash
infoHashFromByteString bs =
  if B.length bs == 20
    then Just $ InfoHash bs
    else Nothing
