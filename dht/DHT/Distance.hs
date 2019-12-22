module DHT.Distance
  ( isCloser
  , closest
  , distanceTo
  , XorDistance(..)
  ) where

import DHT.NodeID
import Data.Bits
import qualified Data.ByteString as B
import Data.Word (Word8)

-- True if `elem` is closer to `dst` than `ref`
isCloser :: (ToNodeID a, ToNodeID b, ToNodeID c) => a -> b -> c -> Bool
isCloser dst ref e =
  let dstID = toNodeID dst
   in XorDistance (toNodeID e) dstID < XorDistance (toNodeID ref) dstID

-- Return the closest element to `dst`
closest :: (ToNodeID a, ToNodeID b) => a -> [b] -> Maybe b
closest _ [] = Nothing
closest dst (e:es) = Just $ foldl keepClosest e es
  where
    keepClosest old new =
      if (isCloser dst old new)
        then new
        else old

distanceTo :: (ToNodeID a, ToNodeID b) => a -> b -> XorDistance
distanceTo a b = XorDistance (toNodeID a) (toNodeID b)

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
