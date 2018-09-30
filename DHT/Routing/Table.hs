module DHT.Routing.Table
  ( Table
  , emptyTable
  , insert
  , insertOrUpdate
  , findNode
  , findClosests
  , listNodes
  ) where

import Control.Monad
import DHT.Node
import DHT.NodeID
import qualified DHT.Routing.Bucket as DB
import qualified Data.ByteString as B
import Data.List (sortOn, uncons)

data Table =
  Table NodeID
        [DB.Bucket]
  deriving (Eq, Show)

emptyTable :: NodeID -> Table
emptyTable nodeID = Table nodeID $ replicate 20 DB.emptyBucket

insert :: Table -> Node -> Table
insert table node = updateNodeBucket table nodeID fnInsert
  where
    Node nodeID _ _ = node
    fnInsert bucket = DB.insert bucket node

insertOrUpdate :: Table -> Node -> (Node -> Node) -> NodeID -> Table
insertOrUpdate table newNode update nodeID =
  updateNodeBucket table nodeID fnInsertOrUpdate
  where
    fnInsertOrUpdate bucket = DB.insertOrUpdate bucket newNode update nodeID

findNode :: Table -> NodeID -> Maybe Node
findNode table nodeID = mapNodeBucket table nodeID getNode
  where
    getNode bucket = DB.findNode bucket nodeID

-- Find and return the 8 closest node to a given ID
findClosests :: Table -> NodeID -> [Node]
findClosests table nodeID = take 8 $ listNodesByDistance table nodeID

-- List all nodes on the Table
listNodes :: Table -> [Node]
listNodes (Table _ buckets) = join $ fmap DB.listNodes buckets

-- Update the bucket holding the given NodeID
updateNodeBucket :: Table -> NodeID -> (DB.Bucket -> DB.Bucket) -> Table
updateNodeBucket (Table tableID buckets) nodeID fn =
  Table tableID updatedBuckets
  where
    updatedBuckets = start ++ ((fn target) : end)
    (start, target, end) = splitBucketListForNodeID buckets tableID nodeID

-- Map the bucket holding the given NodeID
mapNodeBucket :: Table -> NodeID -> (DB.Bucket -> a) -> a
mapNodeBucket (Table tableID buckets) nodeID fn = fn target
  where
    (_, target, _) = splitBucketListForNodeID buckets tableID nodeID

-- List nodes ordered by the increasing distance from the target ID
listNodesByDistance :: Table -> NodeID -> [Node]
listNodesByDistance (Table _ buckets) targetID =
  (listSorted [target]) ++ (merge start end)
  where
    (start, target, end) = splitBucketListForNodeID buckets targetID targetID
    merge :: [DB.Bucket] -> [DB.Bucket] -> [Node]
    merge (ha:ta) (hb:tb) = (listSorted [ha, hb]) ++ (merge ta tb)
    merge [] b = listSorted b
    merge a [] = listSorted a
    listSorted = sortOn (distanceTo targetID) . join . fmap DB.listNodes

-- Split a list of buckets into three parts:
-- - buckets before the nodeID match
-- - bucket matching the nodeID
-- - buckets after the nodeID match
splitBucketListForNodeID ::
     [DB.Bucket] -> NodeID -> NodeID -> ([DB.Bucket], DB.Bucket, [DB.Bucket])
splitBucketListForNodeID buckets (NodeID tableID) (NodeID nodeID) =
  let bucketsWithIDs = zip3 buckets (B.unpack tableID) (B.unpack nodeID)
      (matching, rest) = span (\(_, t, n) -> t == n) bucketsWithIDs
      (start, target, end) =
        case uncons rest of
          Just (t, r) -> (matching, t, r)
          Nothing
                      -- This may happend if tableID == nodeID
           -> (init matching, last matching, [])
      keepBucket (b, _, _) = b
   in (fmap keepBucket start, keepBucket target, fmap keepBucket end)
