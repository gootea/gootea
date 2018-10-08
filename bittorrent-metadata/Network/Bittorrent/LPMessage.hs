module Network.Bittorrent.LPMessage
  ( recvLPMessage
  , sendLPMessage
  , extractLPMessage
  , createLPMessage
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString
import Data.Monoid

sendLPMessage :: Socket -> BSL.ByteString -> IO Int
sendLPMessage sock = send sock . createLPMessage

recvLPMessage :: Socket -> BS.ByteString -> IO (BS.ByteString, BS.ByteString)
recvLPMessage sock fullbs =
  case extractLPMessage fullbs of
    Just (mbs, restbs) -> return (mbs, restbs)
    Nothing -> do
      recvbs <- recv sock 2048
      if (BS.length recvbs == 0) then ioError $ userError "Peer closed its connection"
      else recvLPMessage sock (fullbs <> recvbs)

extractLPMessage :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
extractLPMessage fullbs = do
  (length, payload) <- extractLength fullbs
  splitMessage (fromInteger length) payload
  where
    extractLength bs =
      if BS.length lengthPrefix == 4
        then Just (len, payload)
        else Nothing
      where
        (lengthPrefix, payload) = BS.splitAt 4 bs
        len =
          BS.foldl (\total -> \w -> total * 256 + (toInteger w)) 0 lengthPrefix
    splitMessage l bs =
      if (BS.length bs >= l)
        then let (message, rest) = BS.splitAt l bs
              in Just (message, rest)
        else Nothing

-- create a length prefixed message
createLPMessage :: BSL.ByteString -> BS.ByteString
createLPMessage bs = BSL.toStrict (BSL.pack [0, 0, 0, length bs] <> bs)
  where
    length = fromIntegral . toInteger . BSL.length
