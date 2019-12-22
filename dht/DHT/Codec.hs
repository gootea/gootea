module DHT.Codec
  ( encode
  , decode
  ) where

import BEncode (BType(..))
import Control.Applicative
import Control.Monad
import DHT.Node
import DHT.NodeID
import qualified DHT.NodeID as N
import DHT.Peer
import DHT.Transactions (TransactionID(..), TransactionType(..))
import DHT.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M
import Data.Maybe

-- Encode
encode :: Packet -> BType
encode (Packet (TransactionID tid) message) =
  BDict $ M.fromList $ transactionID : (messageKeys message)
  where
    transactionID = (C.pack "t", BString tid)

messageKeys :: DHTMessage -> [(B.ByteString, BType)]
messageKeys (PingQuery nodeID) =
  keysForQuery "ping" [("id", encodeNodeID nodeID)]
messageKeys (FindNodeQuery nodeID targetID) =
  keysForQuery
    "find_node"
    [("id", encodeNodeID nodeID), ("target", encodeNodeID targetID)]
messageKeys (GetPeersQuery nodeID (InfoHash infoHash)) =
  keysForQuery
    "get_peers"
    [("id", encodeNodeID nodeID), ("info_hash", BString infoHash)]
messageKeys (AnnouncePeerQuery nodeID (InfoHash infoHash) port (Token token)) =
  keysForQuery
    "announce_peer"
    [ ("id", encodeNodeID nodeID)
    , ("info_hash", BString infoHash)
    , ("port", BInt port)
    , ("token", BString token)
    ]
messageKeys (PingResponse nodeID) =
  keysForResponse [("id", encodeNodeID nodeID)]
messageKeys (FindNodeResponse nodeID nodes) =
  keysForResponse [("id", encodeNodeID nodeID), ("nodes", encodeNodes nodes)]
messageKeys (GetPeersWithPeersResponse nodeID (Token token) peers) =
  keysForResponse
    [ ("id", encodeNodeID nodeID)
    , ("token", BString token)
    , ("values", encodePeers peers)
    ]
messageKeys (GetPeersWithNodesResponse nodeID (Token token) nodes) =
  keysForResponse
    [ ("id", encodeNodeID nodeID)
    , ("token", BString token)
    , ("nodes", encodeNodes nodes)
    ]
messageKeys (AnnouncePeerResponse nodeID) =
  keysForResponse [("id", encodeNodeID nodeID)]

encodeNodeID :: NodeID -> BType
encodeNodeID = BString . N.toByteString

encodeNodes :: [Node] -> BType
encodeNodes nodes = BString bytes
  where
    bytes = foldl B.append B.empty (fmap toCompactNodeInfo nodes)

encodePeers :: [Peer] -> BType
encodePeers peers =
  let p = BString . toCompactPeerInfo <$> peers
   in BList p

keysForQuery :: String -> [(String, BType)] -> [(B.ByteString, BType)]
keysForQuery name namedValues =
  [ (C.pack "y", BString $ C.pack "q")
  , (C.pack "q", BString $ C.pack name)
  , (C.pack "a", namedValuesToBDict namedValues)
  ]

keysForResponse :: [(String, BType)] -> [(C.ByteString, BType)]
keysForResponse namedValues =
  [ (C.pack "y", BString $ C.pack "r")
  , (C.pack "r", namedValuesToBDict namedValues)
  ]
  where


namedValuesToBDict :: [(String, BType)] -> BType
namedValuesToBDict namedValues =
  (BDict . M.fromList . (fmap packKey)) namedValues
  where
    packKey (k, v) = (C.pack k, v)

-- Decode
decode :: (TransactionID -> Maybe TransactionType) -> BType -> Maybe Packet
decode getType (BDict dict) = Just Packet <*> transactionID <*> dhtMessage
  where
    transactionID = fmap TransactionID $ getBString "t" dict
    dhtMessage =
      case transactionType of
        Just "q" -> join $ liftA2 decodeQuery queryType queryValues
        Just "r" -> join $ liftA2 decodeResponse responseType responseValues
        _ -> Nothing
    transactionType = getString "y" dict
    queryType = getString "q" dict
    queryValues = getBDict "a" dict
    responseType = transactionID >>= getType
    responseValues = getBDict "r" dict
decode _ _ = Nothing

decodeQuery :: String -> M.Map B.ByteString BType -> Maybe DHTMessage
decodeQuery "ping" dict = fmap PingQuery $ getNodeID "id" dict
decodeQuery "find_node" dict =
  liftA2 FindNodeQuery (getNodeID "id" dict) (getNodeID "target" dict)
decodeQuery "get_peers" dict =
  liftA2
    GetPeersQuery
    (getNodeID "id" dict)
    (fmap InfoHash $ getBString "info_hash" dict)
decodeQuery "announce_peer" dict =
  liftA3
    AnnouncePeerQuery
    (getNodeID "id" dict)
    (fmap InfoHash $ getBString "info_hash" dict)
    (getBInt "port" dict) <*>
  fmap Token (getBString "token" dict)
decodeQuery _ _ = Nothing

decodeResponse ::
     TransactionType -> M.Map B.ByteString BType -> Maybe DHTMessage
decodeResponse Ping dict = fmap PingResponse $ getNodeID "id" dict
decodeResponse FindNode dict =
  liftA2 FindNodeResponse (getNodeID "id" dict) (getNodes "nodes" dict)
decodeResponse AnnouncePeer dict =
  fmap AnnouncePeerResponse $ getNodeID "id" dict
decodeResponse GetPeers dict =
  let nodeID = getNodeID "id" dict
      token = fmap Token $ getBString "token" dict
      peers = getPeers "values" dict
      nodes = getNodes "nodes" dict
   in case (peers, nodes) of
        (Just p, _) -> liftA2 GetPeersWithPeersResponse nodeID token <*> pure p
        (_, Just n) -> liftA2 GetPeersWithNodesResponse nodeID token <*> pure n
        _ -> Nothing

getBString :: [Char] -> M.Map B.ByteString BType -> Maybe B.ByteString
getBString key m =
  case (M.lookup (C.pack key) m) of
    Just (BString bString) -> Just bString
    _ -> Nothing

getBInt :: [Char] -> M.Map B.ByteString BType -> Maybe Int
getBInt key m =
  case (M.lookup (C.pack key) m) of
    Just (BInt int) -> Just int
    _ -> Nothing

getBDict ::
     [Char] -> M.Map B.ByteString BType -> Maybe (M.Map B.ByteString BType)
getBDict key m =
  case (M.lookup (C.pack key) m) of
    Just (BDict dict) -> Just dict
    _ -> Nothing

getString :: [Char] -> M.Map B.ByteString BType -> Maybe [Char]
getString key m = fmap C.unpack $ getBString key m

getNodeID :: [Char] -> M.Map B.ByteString BType -> Maybe N.NodeID
getNodeID key m = getBString key m >>= N.fromByteString

getNodes :: [Char] -> M.Map B.ByteString BType -> Maybe [Node]
getNodes key m = getBString key m >>= cut >>= toNodes
  where
    cut bytes =
      case compare len 26 of
        EQ -> Just [bytes]
        LT ->
          if len == 0
            then Just []
            else Nothing
        GT -> cut (B.drop 26 bytes) >>= (\l -> Just ((B.take 26 bytes) : l))
      where
        len = B.length bytes
    toNodes = traverse fromCompactNodeInfo

getPeers :: [Char] -> M.Map B.ByteString BType -> Maybe [Peer]
getPeers key m =
  case M.lookup (C.pack key) m of
    Just (BList peers) -> Just $ catMaybes $ convert <$> peers
    _ -> Nothing
  where
    convert (BString peer) = fromCompactPeerInfo peer
    convert _ = Nothing
