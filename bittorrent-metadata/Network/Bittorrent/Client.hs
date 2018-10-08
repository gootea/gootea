module Network.Bittorrent.Client
  ( getMetainfo
  ) where

import Crypto.Hash.SHA1
import qualified Data.BEncode as BE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Torrent
import Data.Word
import Data.Monoid
import Network.Bittorrent.Extension
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString

getMetainfo :: SockAddr -> BS.ByteString -> IO (Either String Metainfo)
getMetainfo sockAddr infohash = do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock sockAddr
  sendHandshake sock infohash nodeID
  recvHandshake sock
  (idx, metadataSize, rest) <- doExtensionHandshake sock
  metadataBytes <- getMetadata sock idx metadataSize rest
  let mHash = hash metadataBytes
  if mHash == infohash
    then return $ BE.decode metadataBytes
    else ioError $
         userError
           "The metadata we got doesn't match with the requested infohash"

nodeID = BS.pack $ replicate 20 42

--------------------------
-- BitTorrent Handshake --
--------------------------
prefixHandshake =
  BS.pack
    [ 19 -- Lenght prefix
    , 66
    , 105
    , 116
    , 84
    , 111
    , 114
    , 114
    , 101
    , 110
    , 116
    , 32
    , 112
    , 114
    , 111
    , 116
    , 111
    , 99
    , 111
    , 108 -- BitTorrent protocol
    ]

sendHandshake :: Socket -> BS.ByteString -> BS.ByteString -> IO ()
sendHandshake sock infohash nodeID = send sock message >> return ()
  where
    message = prefixHandshake <> extensions <> infohash <> nodeID
    extensions = BS.pack [0, 0, 0, 0, 0, 16, 0, 0]

recvHandshake :: Socket -> IO ()
recvHandshake sock = do
  bs <- recv sock 68
  if (BS.length bs == 68)
    then if (BS.isPrefixOf prefixHandshake bs)
           then return ()
           else ioError $ userError "Bad handshake message received"
    else ioError $ userError "Wrong number of bytes received for handshake"
