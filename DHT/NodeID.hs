module DHT.NodeID
  ( NodeID(..)
  , fromByteString
  , toByteString
  , XorDistance(..)
  , InfoHash(..)
  , ihToNodeID
  , ToNodeID(toNodeID)
  , isCloser
  , closest
  ) where

import Data.Bits
import qualified Data.ByteString as B
import Data.Word (Word8)
import System.Random

------------
-- NodeID --
------------
newtype NodeID =
  NodeID B.ByteString
  deriving (Eq, Ord)

instance ToNodeID NodeID where
  toNodeID = id

instance Show NodeID where
  show (NodeID bytes) = show $ B.unpack bytes

instance Random NodeID where
  random g = (NodeID $ B.pack bytes, finalGen)
    where
      (bytes, finalGen) = foldl addWord ([], g) [0 .. 19 :: Int]
      addWord (acc, gen) _ = (newElem : acc, newGen)
        where
          (newElem, newGen) = random gen
  randomR (_, _) g = random g

toByteString :: NodeID -> B.ByteString
toByteString (NodeID b) = b

fromByteString :: B.ByteString -> Maybe NodeID
fromByteString b =
  if B.length b == 20
    then Just (NodeID b)
    else Nothing

--------------
-- ToNodeID --
--------------
class ToNodeID a where
  toNodeID :: a -> NodeID

-- True if `elem` is closer to `dst` than `ref`
isCloser :: (ToNodeID a, ToNodeID b, ToNodeID c) => a -> b -> c -> Bool
isCloser dst ref e =
  let dstID = toNodeID dst
   in XorDistance (toNodeID e) dstID < XorDistance (toNodeID ref) dstID

-- Return the closest element to `dst`
closest :: (ToNodeID a, ToNodeID b) => a ->  [b] -> Maybe b
closest _ [] = Nothing
closest dst (e: es) = Just $ foldl keepClosest e es
  where
    keepClosest old new = if (isCloser dst old new) then new else old

--------------
-- InfoHash --
--------------
newtype InfoHash =
  InfoHash B.ByteString
  deriving (Eq, Show)

instance Ord InfoHash where
  compare (InfoHash a) (InfoHash b) = compare a b

instance ToNodeID InfoHash where
  toNodeID (InfoHash ih) = NodeID ih

ihToNodeID :: InfoHash -> NodeID
ihToNodeID (InfoHash ih) = NodeID ih

-----------------
-- XorDistance --
-----------------
data XorDistance =
  XorDistance NodeID
              NodeID

xdUnpack :: XorDistance -> [Word8]
xdUnpack (XorDistance (NodeID a) (NodeID b)) =
  zipWith xor (B.unpack a) (B.unpack b)

instance Eq XorDistance where
  (==) a b = (xdUnpack a) == (xdUnpack b)

instance Ord XorDistance
    -- compare :: XorDistance -> XorDistance -> Ordering
                                                         where
  compare a b = cmp (xdUnpack a) (xdUnpack b)
    where
      cmp (c:rc) (d:rd) =
        case compare c d of
          EQ -> cmp rc rd
          r -> r
      cmp _ _ = EQ
            -- tables should have the same size, so we don't consider all cases
            -- but this should not be a problem
