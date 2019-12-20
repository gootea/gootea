{-# LANGUAGE DeriveGeneric #-}

module Chord.ChordMessage
  ( ChordMessage(..)
  , toCapnp
  , fromCapnp
  ) where

import qualified Capnp.Gen.Messages.Pure as C
import Chord.ID
import Chord.Node
import Common.Models.InfoHash
import qualified Data.ByteString as B
import Data.Int
import Data.Maybe
import qualified Data.Vector as V
import Network.Socket

import GHC.Generics
import Generic.Random
import Test.QuickCheck

-- | Messages used to communicate between nodes
data ChordMessage
  = FingerTableQuery
  | FingerTableResponse [Node]
  | GetValuesQuery ID
  | GetValuesResponse ID [InfoHash]
  | AddValueQuery ID InfoHash
  deriving (Eq, Show, Generic)

instance Arbitrary ChordMessage where
  arbitrary = genericArbitraryU

-- | Convert a ChordMessage to a Capn'proto one
toCapnp :: ChordMessage -> C.Message
toCapnp FingerTableQuery = C.Message C.Message'message'fingerTableQuery
toCapnp (FingerTableResponse nodes) =
  C.Message $
  C.Message'message'fingerTableResponse $
  C.FingerTableResponse $ V.fromList $ fmap nodeToCapnp nodes
toCapnp (GetValuesQuery target) =
  C.Message $
  C.Message'message'getValuesQuery $ C.GetValuesQuery $ idToCapnp target
toCapnp (GetValuesResponse target values) =
  C.Message $
  C.Message'message'getValuesResponse $
  C.GetValuesResponse
    (idToCapnp target)
    (V.fromList $ fmap infohashToCapnp values)
toCapnp (AddValueQuery key value) =
  C.Message $
  C.Message'message'addValueQuery $
  C.AddValueQuery (idToCapnp key) (infohashToCapnp value)

-- | Convert a Capn'proto message to a ChordMessage
fromCapnp :: C.Message -> Maybe ChordMessage
fromCapnp = decode . C.message
  where
    decode (C.Message'message'fingerTableQuery) = Just FingerTableQuery
    decode (C.Message'message'fingerTableResponse fingerTableResponse) =
      decodeFingerTableResponse fingerTableResponse
    decode (C.Message'message'getValuesQuery getValuesQuery) =
      decodeGetValuesQuery getValuesQuery
    decode (C.Message'message'getValuesResponse getValuesResponse) =
      decodeGetValuesResponse getValuesResponse
    decode (C.Message'message'addValueQuery addValueQuery) =
      decodeAddValueQuery addValueQuery
    decode (C.Message'message'unknown' _) = Nothing

decodeFingerTableResponse :: C.FingerTableResponse -> Maybe ChordMessage
decodeFingerTableResponse =
  Just .
  FingerTableResponse . (=<<) (maybeToList . nodeFromCapnp) . V.toList . C.nodes

decodeGetValuesQuery :: C.GetValuesQuery -> Maybe ChordMessage
decodeGetValuesQuery (C.GetValuesQuery key) = GetValuesQuery <$> idFromCapnp key

decodeGetValuesResponse :: C.GetValuesResponse -> Maybe ChordMessage
decodeGetValuesResponse (C.GetValuesResponse key values) =
  GetValuesResponse <$> idFromCapnp key <*>
  (Just $ (=<<) maybeToList $ fmap infohashFromCapnp $ V.toList values)

decodeAddValueQuery :: C.AddValueQuery -> Maybe ChordMessage
decodeAddValueQuery (C.AddValueQuery key value) =
  AddValueQuery <$> idFromCapnp key <*> infohashFromCapnp value

nodeToCapnp :: Node -> C.Node
nodeToCapnp node =
  C.Node {C.id = idToCapnp $ nodeId node, C.address = capAddr, C.port = capPort}
  where
    (capAddr, capPort) = nodeAddrToCapnp $ nodeAddr node

nodeAddrToCapnp :: SockAddr -> (C.Node'address, Int16)
nodeAddrToCapnp (SockAddrInet port addr) =
  let (b1, b2, b3, b4) = hostAddressToTuple addr
   in ( C.Node'address'ipv4Address $ C.IPV4Address b1 b2 b3 b4
      , fromInteger $ toInteger port)
nodeAddrToCapnp (SockAddrInet6 port _ addr _) =
  let (w1, w2, w3, w4, w5, w6, w7, w8) = hostAddress6ToTuple addr
   in ( C.Node'address'ipv6Address $ C.IPV6Address w1 w2 w3 w4 w5 w6 w7 w8
      , fromInteger $ toInteger port)
nodeAddrToCapnp _ = (C.Node'address'unknown' $ fromInteger 0, 0)

nodeFromCapnp :: C.Node -> Maybe Node
nodeFromCapnp (C.Node cid caddr cport) =
  newNode <$> (idFromCapnp cid) <*> (nodeAddrFromCapnp caddr cport)

nodeAddrFromCapnp :: C.Node'address -> Int16 -> Maybe SockAddr
nodeAddrFromCapnp (C.Node'address'ipv4Address addr) cport =
  let C.IPV4Address b1 b2 b3 b4 = addr
   in Just $
      SockAddrInet
        (fromInteger $ toInteger cport)
        (tupleToHostAddress (b1, b2, b3, b4))
nodeAddrFromCapnp (C.Node'address'ipv6Address addr) cport =
  let C.IPV6Address w1 w2 w3 w4 w5 w6 w7 w8 = addr
   in Just $
      SockAddrInet6
        (fromInteger $ toInteger cport)
        0
        (tupleToHostAddress6 (w1, w2, w3, w4, w5, w6, w7, w8))
        0
nodeAddrFromCapnp _ _ = Nothing

idToCapnp :: ID -> C.ID
idToCapnp = C.ID . idToByteString

idFromCapnp :: C.ID -> Maybe ID
idFromCapnp (C.ID value) = idFromByteString value

infohashToCapnp :: InfoHash -> B.ByteString
infohashToCapnp = infoHashToByteString

infohashFromCapnp :: B.ByteString -> Maybe InfoHash
infohashFromCapnp = infoHashFromByteString
