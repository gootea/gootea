module Chord.ID
  ( ID
  , ChordID(..)
  , maxID
  , newID
  , isBetweenIE
  , isBetweenEI
  , fingerRange
  , idToByteString
  , idFromByteString
  ) where

import qualified Data.ByteString as B
import Test.QuickCheck

newtype ID =
  ID Integer
  deriving (Eq, Show, Ord)

-- | The `ChordID` class is used to retreive the chord ID of a value
class ChordID a where
  chordID :: a -> ID

instance ChordID ID where
  chordID = id

instance Arbitrary ID where
  arbitrary = ID <$> choose (0, maxID - 1)

-- | This is the limit over which an ID has to wrap to 0
maxID :: Integer
maxID = 2 ^ 160

-- | Create a new ID
newID :: Integer -> ID
newID = ID

-- | Check wether an ID is between two given IDs
isBetweenIE ::
     ID -- | Lower (included)
  -> ID -- | Upper (excluded)
  -> ID -- | ID to check
  -> Bool
isBetweenIE lower upper i =
  if upper >= lower
    then (lower <= i && i < upper) || lower == i
    else lower <= i || i < upper

-- | Check wether an ID is between two given IDs
isBetweenEI ::
     ID -- | Lower (excluded)
  -> ID -- | Upper (included)
  -> ID -- | ID to check
  -> Bool
isBetweenEI lower upper i =
  if upper >= lower
    then (lower < i && i <= upper) || upper == i
    else lower < i || i <= upper

-- | Give the ID range of finger number `n`
fingerRange ::
     ID -- | Self ID
  -> Int -- | Index of finger
  -> (ID, ID)
fingerRange (ID self) fingerID =
  let lower = (self + 2 ^ fingerID) `mod` maxID
      upper = (self + 2 ^ (fingerID + 1)) `mod` maxID
   in (ID lower, ID upper)

-- | Convert an ID to a ByteString representing the ID value in big endian
-- (network order)
idToByteString :: ID -> B.ByteString
idToByteString (ID value) = B.pack $ fmap trunc [19,18 .. 0]
  where
    trunc n = fromInteger $ quot value (256 ^ n) `mod` 256

idFromByteString :: B.ByteString -> Maybe ID
idFromByteString bs =
  if B.length bs == 20
    then Just $ newID $ B.foldl foldFn 0 bs
    else Nothing
  where
    foldFn total newByte = total * 256 + (toInteger newByte)
