module Network.Bittorrent.Extension
  ( doExtensionHandshake
  , getMetadata
  ) where

import Data.BEncode
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Network.Bittorrent.LPMessage
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString

----------------------------------
-- Extension messages manipulation
----------------------------------
data ExtensionMessage
  = EMHandshake EHPayload
  | EMMetadata UTMetadata
  deriving (Eq, Show)

parseExtensionMessage :: BS.ByteString -> Maybe ExtensionMessage
parseExtensionMessage bs = do
  embs <-
    case BS.uncons bs of
      Just (20, rest) -> Just rest
      _ -> Nothing
  em <-
    case BS.uncons embs of
      Just (0, message) -> EMHandshake <$> parseEMHandshake message
      Just (1, message) -> EMMetadata <$> parseEMMetadata message
      _ -> Nothing
  return em

parseEMHandshake :: BS.ByteString -> Maybe EHPayload
parseEMHandshake bs =
  case decode bs of
    Left _ -> Nothing
    Right m -> Just m

parseEMMetadata :: BS.ByteString -> Maybe UTMetadata
parseEMMetadata bs =
  case decode bs of
    Left _ -> Nothing
    Right m -> Just m

-- Wait for the next extension message and returns it
-- Return the ExtensionMessage parsed, all the bytes of the associated
-- Bittorrent message and the unconsummed bytes
recvExtensionMessage ::
     Socket
  -> BS.ByteString
  -> IO (ExtensionMessage, BS.ByteString, BS.ByteString)
recvExtensionMessage sock previous = do
  (message, rest) <- recvLPMessage sock previous
  case parseExtensionMessage message of
    Just em -> return (em, message, rest)
    Nothing -> recvExtensionMessage sock rest

sendExtensionMessage :: Socket -> Word8 -> ExtensionMessage -> IO Int
sendExtensionMessage sock metadataIdx = sendLPMessage sock . encodeEM
  where
    encodeEM (EMHandshake p) = BSL.pack [20, 0] <> encode p
    encodeEM (EMMetadata p) = BSL.pack [20, metadataIdx] <> encode p

------------------------------
-- Extension Handshake message
------------------------------
metadataIdx = 1

extensionHandshakeMessage :: BS.ByteString
extensionHandshakeMessage = createLPMessage $ BSL.pack [20, 0] <> payload
  where
    payload = encode $ EHPayload (EHMPayload (Just metadataIdx)) Nothing

-- Extension Handshake Payload
data EHPayload = EHPayload
  { m :: EHMPayload
  , metadataSize :: Maybe Int
  } deriving (Show, Read, Eq)

instance BEncode EHPayload where
  toBEncode (EHPayload m _) = toDict $ (BSC.pack "m") .=! m .: endDict
  fromBEncode =
    fromDict (EHPayload <$>! (BSC.pack "m") <*>? (BSC.pack "metadata_size"))

data EHMPayload = EHMPayload
  { ehpMetadataIdx :: Maybe Word8
  } deriving (Show, Read, Eq)

instance BEncode EHMPayload where
  toBEncode (EHMPayload idx) =
    toDict $ (BSC.pack "ut_metadata") .=? idx .: endDict
  fromBEncode = fromDict (EHMPayload <$>? (BSC.pack "ut_metadata"))

recvExtensionHandshakeMessage ::
     Socket -> BS.ByteString -> IO (Maybe Word8, Int, BS.ByteString)
recvExtensionHandshakeMessage sock previous = do
  (message, _, rest) <- recvExtensionMessage sock previous
  case message of
    EMHandshake (EHPayload payload metadataSize) ->
      case metadataSize of
        Just size -> return (ehpMetadataIdx payload, size, rest)
        Nothing ->
          ioError $ userError "Metadatasize is not defined, cannot continue"
    _ -> recvExtensionHandshakeMessage sock rest

-- Do the extension handshake and return the index of the metadata extension of
-- the peer plus the unconsummed bytes
doExtensionHandshake :: Socket -> IO (Word8, Int, BS.ByteString)
doExtensionHandshake sock = do
  send sock extensionHandshakeMessage
  (idx, metadataSize, rest) <- recvExtensionHandshakeMessage sock BS.empty
  case idx of
    Just idx -> return (idx, metadataSize, rest)
    Nothing ->
      ioError $ userError "The peer doesn't support ut_metadata extension"

----------------------
-- UT Metadata message
----------------------
data UTMetadata = UTMetadata
  { emmType :: Int
  , emmPiece :: Int
  , emmTotalSize :: Maybe Int
  } deriving (Eq, Show)

instance BEncode UTMetadata where
  toBEncode emm =
    toDict $
    (BSC.pack "msg_type") .=! (emmType emm) .: (BSC.pack "piece") .=!
    (emmPiece emm) .:
    (BSC.pack "total_size") .=?
    (emmTotalSize emm) .:
    endDict
  fromBEncode =
    fromDict $ do
      UTMetadata <$>! (BSC.pack "msg_type") <*>! (BSC.pack "piece") <*>?
        (BSC.pack "total_size")

askPiece :: Socket -> Word8 -> Int -> IO Int
askPiece sock metadataIdx piece =
  sendExtensionMessage sock metadataIdx $
  EMMetadata $ UTMetadata 0 piece Nothing

recvPiece ::
     Socket -> BS.ByteString -> Int -> IO (Int, BS.ByteString, BS.ByteString)
recvPiece sock previous pieceSize = do
  (em, bytes, rest) <- recvExtensionMessage sock previous
  case em of
    EMMetadata (UTMetadata 1 piece _) ->
      let messageSize = BS.length bytes
          pieceBytes = BS.drop (messageSize - pieceSize) bytes
       in return (piece, pieceBytes, rest)
    EMMetadata (UTMetadata 2 _ _) ->
      ioError $ userError "peer was not able to give us the piece we asked for"
    _ -> do
      recvPiece sock rest pieceSize

-- Get a piece of metadata
-- Return the piece bytes and the unconsummed bytes
getMetadataPiece ::
     Socket
  -> Word8
  -> Int
  -> Int
  -> BS.ByteString
  -> IO (BS.ByteString, BS.ByteString)
getMetadataPiece sock metadataIdx piece pieceSize previousBS = do
  askPiece sock metadataIdx piece
  (receivedPiece, bytes, remaining) <- recvPiece sock previousBS pieceSize
  if piece == receivedPiece
    then return (bytes, remaining)
    else ioError $ userError "The piece received was not the one asked for"

getMetadata :: Socket -> Word8 -> Int -> BS.ByteString -> IO BS.ByteString
getMetadata sock metadataIdx metadataSize previousBS =
  doGet 0 metadataSize previousBS
  where
    doGet :: Int -> Int -> BS.ByteString -> IO BS.ByteString
    doGet piece size prevBytes =
      if size > 16384
        then do
          (pieceBytes, remainingBytes) <-
            getMetadataPiece sock metadataIdx piece 16384 prevBytes
          otherPieces <- doGet (piece + 1) (size - 16384) remainingBytes
          return $ BS.append pieceBytes otherPieces
        else fst <$> getMetadataPiece sock metadataIdx piece size prevBytes
