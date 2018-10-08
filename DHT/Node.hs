module DHT.Node
  ( Node(..)
  , toCompactNodeInfo
  , fromCompactNodeInfo
  , nodeToSockAddr
  , nodeFromSockAddr
  , nodeToNodeID
  ) where

import DHT.NodeID
import qualified Data.ByteString as B
import Network.Socket

data Node =
  Node NodeID
       PortNumber
       HostAddress
  deriving (Show, Ord)

instance Eq Node where
  (==) (Node a _ _) (Node b _ _) = a == b

instance ToNodeID Node where
  toNodeID (Node i _ _) = i


toCompactNodeInfo :: Node -> B.ByteString
toCompactNodeInfo (Node (NodeID nId) port host) = B.pack bytes
  where
    bytes = (B.unpack nId) ++ [ha, hb, hc, hd, fromIntegral pa, fromIntegral pb]
    (ha, hb, hc, hd) = hostAddressToTuple host
    (pa, pb) = quotRem port 256

fromCompactNodeInfo :: B.ByteString -> Maybe Node
fromCompactNodeInfo bytes =
  if B.length bytes == 26
    then Just $ Node nodeID port host
    else Nothing
  where
    nodeID = NodeID (B.take 20 bytes)
    port :: PortNumber
    port =
      foldl (\a r -> a * 256 + r) 0 (fmap fromIntegral $ B.unpack portBytes)
    portBytes = B.drop 4 networkBytes
    networkBytes = B.drop 20 bytes
    hostBytes = B.take 4 networkBytes
    host =
      tupleToHostAddress
        ( B.index hostBytes 0
        , B.index hostBytes 1
        , B.index hostBytes 2
        , B.index hostBytes 3)

nodeToSockAddr :: Node -> SockAddr
nodeToSockAddr (Node _ port addr) = SockAddrInet port addr

nodeFromSockAddr :: NodeID -> SockAddr -> Maybe Node
nodeFromSockAddr nodeID (SockAddrInet port host) = Just $ Node nodeID port host
nodeFromSockAddr _ _ = Nothing

nodeToNodeID :: Node -> NodeID
nodeToNodeID (Node i _ _) = i
