module Main where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.BEncode as BE
import qualified Data.BEncode.BDict as BD
import Data.BEncode ((.=!), (.:), (.=?), fromDict, (<*>!), (<*>?), (<$>!), (<$>?), req, field)
import System.IO.Error
import Debug.Trace
import Data.Word
import Data.Maybe
import Control.Applicative
import Network.Bittorrent.LPMessage
import Network.Bittorrent.Extension

main :: IO ()
main = do
  sock <- socket AF_INET Stream defaultProtocol
  putStrLn "Socket created"
  connect sock sockAddr
  putStrLn "Socket connected"
  putStrLn ""

  sendHandshake sock infohash nodeID
  putStrLn "Handshake message sent"
  recvHandshake sock
  putStrLn "Received handshake message"
  putStrLn ""

  (idx, rest) <- doExtensionHandshake sock
  putStrLn "Extension handshake is done"
  putStrLn $ show idx

  (pieceNumber, pieceBytes, remaining) <- getMetadataPiece sock idx 0 rest
  putStrLn "Received piece"
  putStrLn $ show pieceNumber
  putStrLn $ show pieceBytes

  -- _ <- recv sock 1024
  -- send sock requestMetadataMessage
  -- putStrLn "Request for first piece sent"
  -- response <- recv sock 1024
  -- putStrLn $ show response
  -- response <- recv sock 1024
  -- putStrLn $ show response

sockAddr = SockAddrInet 6881 (tupleToHostAddress (5,39,91,197))
infohash = BS.pack [254,10,72,13,167,152,42,193,185,141,126,15,9,91,5,223,56,175,46,130] -- InfoHash of Chasing Ice
nodeID = BS.pack [42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42]

-- extensionHandshakeMessage = BS.pack
--   [0,0,0,26 -- Length of the message
--   ,20 -- Type: extension
--   ,0 -- Extension type: handshake
--   ,100,49,58,109,100,49,49,58,117,116,95,109,101,116,97,100,97,116,97,105,49,101,101,101 -- d1:md11:ut_metadatai1eee
--   ]

requestMetadataMessage = BS.pack
  [0,0,0,27 -- Length
  ,20 -- Type: extension
  ,2 -- Extension type: ut_metadata
  ,100,56,58,109,115,103,95,116,121,112,101,105,48,101,53,58,112,105,101,99,101,105,48,101,101 -- d8:msg_typei0e5:piecei0ee
  ]


------------
-- Handshake
------------

prefixHandshake = BS.pack
  [19 -- Lenght prefix
  ,66,105,116,84,111,114,114,101,110,116,32,112,114,111,116,111,99,111,108 -- BitTorrent protocol
  ]

sendHandshake :: Socket -> BS.ByteString -> BS.ByteString -> IO ()
sendHandshake sock infohash nodeID =
  send sock message >> return ()
  where
    message = prefixHandshake <> extensions <> infohash <> nodeID
    extensions = BS.pack [0,0,0,0,0,16,0,0]


recvHandshake :: Socket -> IO ()
recvHandshake sock = do
  bs <- recv sock 68
  if (BS.length bs == 68)
    then if (BS.isPrefixOf prefixHandshake bs)
            then return ()
            else ioError $ userError "Bad handshake message received"
    else ioError $ userError "Wrong number of bytes received for handshake"


