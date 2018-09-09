module Main where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.BEncode as BE
import qualified Data.BEncode.BDict as BD
import Data.BEncode ((.=!), (.:), (.=?))

main :: IO ()
main = do
  sock <- socket AF_INET Stream defaultProtocol
  putStrLn "Socket created"
  connect sock sockAddr
  putStrLn "Socket connected"
  send sock handshakeMessage
  putStrLn "Handshake message sent"
  send sock extensionHandshakeMessage
  putStrLn "Extension handshake sent"
  handShakeResponse <- recv sock 68
  putStrLn $ show handShakeResponse
  _ <- recv sock 1024
  send sock requestMetadataMessage
  putStrLn "Request for first piece sent"
  response <- recv sock 1024
  putStrLn $ show response
  response <- recv sock 1024
  putStrLn $ show response

sockAddr = SockAddrInet 6881 (tupleToHostAddress (5,39,91,197))

handshakeMessage = BS.pack
  [19 -- Lenght prefix
  , 66,105,116,84,111,114,114,101,110,116,32,112,114,111,116,111,99,111,108 -- BitTorrent protocol
  ,0,0,0,0,0,16,0,0 -- Indicate that we support extensions
  , 254,10,72,13,167,152,42,193,185,141,126,15,9,91,5,223,56,175,46,130 -- InfoHash of Chasing Ice
  ,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42 -- NodeID
  ]

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

-- create a length prefixed message
createLPMessage :: BSL.ByteString -> BS.ByteString
createLPMessage bs = BSL.toStrict (BSL.pack [0, 0, 0, length bs] <> bs)
  where length = fromIntegral . toInteger . BSL.length

metadataIdx = 1

extensionHandshakeMessage :: BS.ByteString
extensionHandshakeMessage = createLPMessage $ BSL.pack [20, 0] <> payload
  where
    payload = BE.encode $ EHPayload (Just metadataIdx)

-- Extension Handshake Payload
data EHPayload = EHPayload { ehpMetadataIdx :: Maybe Integer } deriving (Show, Read, Eq)

instance BE.BEncode EHPayload where
  toBEncode (EHPayload idx) = BE.toDict $ (BSC.pack "m") .=! extensionsDict .: BE.endDict
    where
      extensionsDict = BE.toDict $ (BSC.pack "ut_metadata") .=? idx .: BE.endDict
  fromBEncode = undefined


--
recvLPMessage :: Socket -> BS.ByteString -> IO (BS.ByteString, BS.ByteString)
recvLPMessage sock fullbs = case readLPMessage fullbs of
  (Just mbs, restbs) -> return (mbs, restbs)
  (Nothing, restbs) -> do
    recvbs <- recv sock 2048
    recvLPMessage sock (restbs <> recvbs)

readLPMessage :: BS.ByteString -> (Maybe BS.ByteString, BS.ByteString)
readLPMessage fullbs =
  case getMsgLength fullbs of
    (Nothing, _) -> (Nothing, fullbs)
    (Just l, payloadbs) -> splitMessage l payloadbs
  where
    getMsgLength bs = if BS.length lengthPrefix == 4
      then (Just len, payload)
      else (Nothing, bs)
      where
        (lengthPrefix, payload) = BS.splitAt 4 bs
        len = BS.foldr (\w -> \total -> total * 256 + (fromIntegral w)) 0 lengthPrefix
    splitMessage l bs = if (BS.length bs >= l)
      then let (message, rest) = BS.splitAt l bs in (Just message, rest)
      else (Nothing, bs)

