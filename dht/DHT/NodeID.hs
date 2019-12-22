module DHT.NodeID
  ( NodeID(..)
  , fromByteString
  , toByteString
  , InfoHash(..)
  , ToNodeID(toNodeID)
  ) where

import Data.Bits
import qualified Data.ByteString as B
import Data.Word
import Numeric
import System.Random
import Common.Models.InfoHash

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

instance ToNodeID InfoHash where
  toNodeID = NodeID . infoHashToByteString

