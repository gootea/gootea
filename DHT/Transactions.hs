module DHT.Transactions
  ( Transaction(..)
  , TransactionID(..)
  , Transactions(..)
  , TransactionType(..)
  , transactionType
  , newGetPeersTransaction
  , emptyTransactions
  , addTransaction
  , getTransaction
  , removeTransaction
  , addPeersToTransactionGetPeers
  , updateTransactionGetPeersWithNodes
  , getExpiredTransaction
  ) where

import Control.Concurrent.Chan
import DHT.Node
import DHT.NodeID
import DHT.Peer (Peer)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Time.Clock
import Data.Tuple
import Data.Maybe
import Network.Socket
import System.Random

import Debug.Trace

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

-----------------
-- Transaction --
-----------------
data Transaction
  = TransactionPing
  | TransactionFindNode
  | TransactionGetPeers [Peer] -- List of collected peers
                        NodeID -- Best Node contacted yet
                        InfoHash
                        UTCTime -- Expiration time of this transaction
                        (Chan [SockAddr]) -- Chan for the response
  | TransactionAnnouncePeer

transactionType :: Transaction -> TransactionType
transactionType TransactionPing = Ping
transactionType TransactionFindNode = FindNode
transactionType (TransactionGetPeers _ _ _ _ _) = GetPeers
transactionType TransactionAnnouncePeer = AnnouncePeer

newGetPeersTransaction ::
     InfoHash -> NodeID -> UTCTime -> Chan [SockAddr] -> Transaction
newGetPeersTransaction ih bestNode expire chan =
  TransactionGetPeers [] bestNode ih expire chan

-----------------------
-- Transaction Store --
-----------------------
data Transactions =
  Transactions (M.Map TransactionID Transaction)

emptyTransactions :: Transactions
emptyTransactions = Transactions M.empty

addTransaction :: Transactions -> TransactionID -> Transaction -> Transactions
addTransaction (Transactions a) tid t = Transactions $ M.insert tid t a

getTransaction :: Transactions -> TransactionID -> Maybe Transaction
getTransaction (Transactions a) tid = M.lookup tid a

removeTransaction :: Transactions -> TransactionID -> Transactions
removeTransaction (Transactions a) tid = Transactions (M.delete tid a)

addPeersToTransactionGetPeers ::
     Transactions -> TransactionID -> [Peer] -> Transactions
addPeersToTransactionGetPeers (Transactions a) tid peers =
  Transactions $ M.adjust addPeers tid a
  where
    addPeers (TransactionGetPeers existing nodes ih expire chan) =
      TransactionGetPeers (existing ++ peers) nodes ih expire chan
    addPeers other = other

getExpiredTransaction ::
     UTCTime -> Transactions -> (Transactions, [Transaction])
getExpiredTransaction now (Transactions a) =
  (Transactions ongoing, M.elems expired)
  where
    (expired, ongoing) = M.partition isExpired a
    isExpired (TransactionGetPeers _ _ _ expire _) = now > expire
    isExpired _ = False

-- Update a get peers transaction with a received list of nodes
-- It will return the updated transaction store and a subset of the input nodes
-- that can be contacted
updateTransactionGetPeersWithNodes ::
     Transactions
  -> TransactionID
  -> [Node]
  -> (Transactions, [(Node, InfoHash)])
updateTransactionGetPeersWithNodes (Transactions a) tid nodes =
  (swap . fmap Transactions . M.alterF update tid) a
  where
    update :: Maybe Transaction -> ([(Node, InfoHash)], Maybe Transaction)
    update (Just (TransactionGetPeers peers bestNodeYet ih expire chan)) =
      let 
        newNodes = S.fromList nodes
        nodesToContact = filter (isCloser ih bestNodeYet) nodes
        newBestNode = fromMaybe bestNodeYet (toNodeID <$> closest ih nodesToContact)
       in ( fmap (\n -> (n, ih)) nodesToContact
          , Just $
            TransactionGetPeers peers newBestNode ih expire chan)
    update t = ([], t)
