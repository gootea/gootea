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

  send sock extensionHandshakeMessage
  putStrLn "Extension handshake sent"
  (idx, rest) <- recvExtensionHandshakeMessage sock BS.empty
  putStrLn "Received metadata index:"
  putStrLn $ show idx
  putStrLn ""

  askPiece sock (fromJust idx) 0
  putStrLn "Asked for piece 0"

  (piece, size) <- recvPiece sock rest
  putStrLn "Received piece"
  putStrLn $ show piece
  putStrLn $ show size

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


------------------------------------------
-- Generic Bittorrent message manipulation
------------------------------------------

recvLPMessage :: Socket -> BS.ByteString -> IO (BS.ByteString, BS.ByteString)
recvLPMessage sock fullbs = case readLPMessage fullbs of
  (Just mbs, restbs) -> return (mbs, restbs)
  (Nothing, restbs) -> do
    recvbs <- recv sock 2048
    recvLPMessage sock (restbs <> recvbs)

readLPMessage :: BS.ByteString -> (Maybe BS.ByteString, BS.ByteString)
readLPMessage fullbs =
  case (getMsgLength fullbs ) of
    (Nothing, _) -> (Nothing, fullbs)
    (Just l, payloadbs) -> splitMessage (fromInteger l) payloadbs
  where
    getMsgLength bs = if BS.length lengthPrefix == 4
      then (Just len, payload)
      else (Nothing, bs)
      where
        (lengthPrefix, payload) = BS.splitAt 4 bs
        len = BS.foldl (\total -> \w -> total * 256 + (toInteger w)) 0 lengthPrefix
    splitMessage l bs = if (BS.length bs >= l)
      then let (message, rest) = BS.splitAt l bs in (Just message, rest)
      else (Nothing, bs)

-- create a length prefixed message
createLPMessage :: BSL.ByteString -> BS.ByteString
createLPMessage bs = BSL.toStrict (BSL.pack [0, 0, 0, length bs] <> bs)
  where length = fromIntegral . toInteger . BSL.length

sendLPMessage :: Socket -> BSL.ByteString -> IO Int
sendLPMessage sock = send sock . createLPMessage

----------------------------------
-- Extension messages manipulation
----------------------------------

data ExtensionMessage = EMHandshake EHPayload
                      | EMMetadata UTMetadata
                      deriving (Eq, Show)

parseExtensionMessage :: BS.ByteString -> Maybe ExtensionMessage
parseExtensionMessage bs = do
  embs <- case BS.uncons bs of
    Just (20, rest) -> Just rest
    _ -> Nothing
  em <- case BS.uncons embs of
    Just (0, message) -> EMHandshake <$> parseEMHandshake message
    Just (1, message) -> EMMetadata <$> parseEMMetadata message
    _ -> traceShowId Nothing
  return em

parseEMHandshake :: BS.ByteString -> Maybe EHPayload
parseEMHandshake bs = case BE.decode bs of
  Left _ -> Nothing
  Right m -> Just m

parseEMMetadata :: BS.ByteString -> Maybe UTMetadata
parseEMMetadata bs = case BE.decode bs of
  Left _ -> Nothing
  Right m -> Just m

recvExtensionMessage :: Socket -> BS.ByteString -> IO (ExtensionMessage, BS.ByteString)
recvExtensionMessage sock previous = do
  (message, rest) <- recvLPMessage sock previous
  case parseExtensionMessage message of
    Just em -> return (em, rest)
    Nothing -> recvExtensionMessage sock rest

sendExtensionMessage :: Socket -> Word8 -> ExtensionMessage -> IO Int
sendExtensionMessage sock metadataIdx = sendLPMessage sock . encodeEM
  where
    encodeEM (EMHandshake p) = BSL.pack [20, 0] <> BE.encode p
    encodeEM (EMMetadata p) = BSL.pack [20, metadataIdx] <> BE.encode p


------------------------------
-- Extension Handshake message
------------------------------

metadataIdx = 1

extensionHandshakeMessage :: BS.ByteString
extensionHandshakeMessage = createLPMessage $ BSL.pack [20, 0] <> payload
  where
    payload = BE.encode $ EHPayload $ EHMPayload (Just metadataIdx)

-- Extension Handshake Payload
data EHPayload = EHPayload { m :: EHMPayload } deriving (Show, Read, Eq)

instance BE.BEncode EHPayload where
  toBEncode (EHPayload m) = BE.toDict $ (BSC.pack "m") .=! m .: BE.endDict
    -- where
    --   extensionsDict = BE.toDict $ (BSC.pack "ut_metadata") .=? idx .: BE.endDict
  fromBEncode = fromDict (EHPayload <$>! (BSC.pack "m"))

data EHMPayload = EHMPayload { ehpMetadataIdx :: Maybe Word8 } deriving (Show, Read, Eq)

instance BE.BEncode EHMPayload where
  toBEncode (EHMPayload idx) = BE.toDict $ (BSC.pack "ut_metadata") .=? idx .: BE.endDict
  fromBEncode = fromDict (EHMPayload <$>? (BSC.pack "ut_metadata"))

recvExtensionHandshakeMessage :: Socket -> BS.ByteString -> IO (Maybe Word8, BS.ByteString)
recvExtensionHandshakeMessage sock previous = do
  (message, rest) <- recvExtensionMessage sock previous
  case message of
    EMHandshake (EHPayload payload) -> return (ehpMetadataIdx payload, rest)
    _ -> recvExtensionHandshakeMessage sock rest


----------------------
-- UT Metadata message
----------------------

data UTMetadata = UTMetadata 
  { emmType :: Int
  , emmPiece :: Int
  , emmTotalSize :: Maybe Int
  } deriving (Eq, Show)

instance BE.BEncode UTMetadata where
  toBEncode emm = BE.toDict $
    (BSC.pack "msg_type") .=! (emmType emm)
    .: (BSC.pack "piece" ).=! (emmPiece emm)
    .: (BSC.pack "total_size") .=? (emmTotalSize emm)
    .: BE.endDict
  fromBEncode = fromDict $ do
    UTMetadata <$>! (BSC.pack "msg_type")
               <*>! (BSC.pack "piece")
               <*>? (BSC.pack "total_size")


askPiece :: Socket -> Word8 -> Int -> IO Int
askPiece sock metadataIdx piece = sendExtensionMessage sock metadataIdx $ EMMetadata $ UTMetadata 0 piece Nothing

recvPiece :: Socket -> BS.ByteString -> IO (Int, Maybe Int)
recvPiece sock previous = do
    (em, rest) <- recvExtensionMessage sock previous
    case em of
      EMMetadata (UTMetadata 1 piece size) -> return (piece, size)
      EMMetadata (UTMetadata 2 _ _) -> ioError $ userError "peer was not able to give us the piece we asked for"
      _ -> do
          putStrLn "  recvPiece is recursing"
          recvPiece sock rest

