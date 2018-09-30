module DHT.Transactions
  ( Transaction(..)
  , TransactionID(..)
  , Transactions(..)
  , emptyTransactions
  , addTransaction
  , getTransaction
  , removeTransaction
  ) where

import DHT.Types
import qualified Data.Map.Strict as M

data Transactions =
  Transactions (M.Map TransactionID Transaction)

emptyTransactions :: Transactions
emptyTransactions = Transactions M.empty

addTransaction :: Transactions -> Transaction -> Transactions
addTransaction (Transactions a) t = Transactions $ M.insert tid t a
  where
    Transaction tid _ = t

getTransaction :: Transactions -> TransactionID -> Maybe Transaction
getTransaction (Transactions a) tid = M.lookup tid a

removeTransaction :: Transactions -> TransactionID -> Transactions
removeTransaction (Transactions a) tid = Transactions (M.delete tid a)
