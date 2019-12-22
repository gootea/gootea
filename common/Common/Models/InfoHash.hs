{-# LANGUAGE OverloadedStrings #-}

module Common.Models.InfoHash
  ( InfoHash
  , newInfoHash
  , infoHashToByteString
  ) where

import qualified Data.ByteString as B
import Data.Word
import Data.Bits
import Numeric

newtype InfoHash =
  InfoHash B.ByteString
  deriving (Eq)

instance Show InfoHash where
  show (InfoHash ih) = concat $ convertByte <$> (B.unpack ih)
    where
      convertByte :: Word8 -> String
      convertByte b = showHex (shiftR b 4) "" ++ (showHex (b .&. 15) "")

instance Ord InfoHash where
  compare (InfoHash a) (InfoHash b) = compare a b

-- | Create a new InfoHash from a ByteString
newInfoHash :: B.ByteString -> InfoHash
newInfoHash = InfoHash

-- | Return a ByteString for a given InfoHash
infoHashToByteString :: InfoHash -> B.ByteString
infoHashToByteString (InfoHash bs) = bs

