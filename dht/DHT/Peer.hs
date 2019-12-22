module DHT.Peer
  ( Peer(..)
  , toCompactPeerInfo
  , fromCompactPeerInfo
  , peerToSockAddr
  , peerFromSockAddr
  ) where

import qualified Data.ByteString as B
import Network.Socket

data Peer =
  Peer PortNumber
       HostAddress
  deriving (Show, Eq, Ord)

toCompactPeerInfo :: Peer -> B.ByteString
toCompactPeerInfo (Peer port host) = B.pack bytes
  where
    bytes = [ha, hb, hc, hd, fromIntegral pa, fromIntegral pb]
    (ha, hb, hc, hd) = hostAddressToTuple host
    (pa, pb) = quotRem port 256

fromCompactPeerInfo :: B.ByteString -> Maybe Peer
fromCompactPeerInfo bytes =
  if B.length bytes == 6
    then Just $ Peer port host
    else Nothing
  where
    port :: PortNumber
    port =
      foldl (\a r -> a * 256 + r) 0 (fmap fromIntegral $ B.unpack portBytes)
    portBytes = B.drop 4 bytes
    hostBytes = B.take 4 bytes
    host =
      tupleToHostAddress
        ( B.index hostBytes 0
        , B.index hostBytes 1
        , B.index hostBytes 2
        , B.index hostBytes 3)

peerToSockAddr :: Peer -> SockAddr
peerToSockAddr (Peer port addr) = SockAddrInet port addr

peerFromSockAddr :: SockAddr -> Maybe Peer
peerFromSockAddr (SockAddrInet port host) = Just $ Peer port host
peerFromSockAddr _ = Nothing
