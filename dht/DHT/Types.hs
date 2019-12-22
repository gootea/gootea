module DHT.Types
  ( Packet(..)
  , NodeID
  , DHTMessage(..)
  , DHTEvent(..)
  , Token(..)
  , Peer(..)
  ) where

import DHT.Node
import DHT.NodeID
import DHT.Peer
import DHT.Transactions
import qualified Data.ByteString as B
import System.Random

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
