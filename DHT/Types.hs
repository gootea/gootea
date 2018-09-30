module DHT.Types
  ( Packet(..)
  , NodeID
  , DHTMessage(..)
  , DHTEvent(..)
  , TransactionType(..)
  , Transaction(..)
  , TransactionID(..)
  , Token(..)
  , Peer(..)
  , InfoHash(..)
  , ihToNodeID
  ) where

import DHT.Node
import DHT.NodeID
import DHT.Peer
import qualified Data.ByteString as B
import System.Random

newtype InfoHash =
  InfoHash B.ByteString
  deriving (Eq, Show)

instance Ord InfoHash where
  compare (InfoHash a) (InfoHash b) = compare a b

ihToNodeID :: InfoHash -> NodeID
ihToNodeID (InfoHash ih) = NodeID ih

data Packet =
  Packet TransactionID
         DHTMessage -- Message content
  deriving (Eq, Show)

data DHTMessage
  = PingQuery NodeID
  | FindNodeQuery NodeID
                  NodeID
  | GetPeersQuery NodeID
                  InfoHash
  | AnnouncePeerQuery NodeID
                      InfoHash
                      Int
                      Token
  | PingResponse NodeID
  | FindNodeResponse NodeID
                     [Node]
  | GetPeersWithPeersResponse NodeID
                              Token
                              [Peer]
  | GetPeersWithNodesResponse NodeID
                              Token
                              [Node]
  | AnnouncePeerResponse NodeID
  deriving (Eq, Show)

data DHTEvent
  = DHTNodeAdded Node
  | DHTPeerAdded InfoHash
                 Peer
  | DHTPeersReceived InfoHash
                     [Peer]
  | DHTInfoHashDiscovered InfoHash
  deriving (Eq, Show)

data TransactionType
  = Ping
  | FindNode
  | GetPeers
  | AnnouncePeer
  deriving (Eq, Show)

newtype TransactionID =
  TransactionID B.ByteString
  deriving (Eq, Ord, Show)

instance Random TransactionID where
  random g = (TransactionID $ B.pack bytes, finalGen)
    where
      (bytes, finalGen) = foldl addWord ([], g) [0 .. 10 :: Int]
      addWord (acc, gen) _ = (newElem : acc, newGen)
        where
          (newElem, newGen) = random gen
  randomR (_, _) g = random g

data Transaction =
  Transaction TransactionID
              TransactionType

newtype Token =
  Token B.ByteString
  deriving (Eq, Show)

instance Random Token where
  random g = (Token $ B.pack bytes, finalGen)
    where
      (bytes, finalGen) = foldl addWord ([], g) [0 .. 4 :: Int]
      addWord (acc, gen) _ = (newElem : acc, newGen)
        where
          (newElem, newGen) = random gen
  randomR (_, _) g = random g
