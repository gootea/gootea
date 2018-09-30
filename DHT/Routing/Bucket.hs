module DHT.Routing.Bucket
  ( Bucket
  , emptyBucket
  , insert
  , insertOrUpdate
  , findNode
  , listNodes
  ) where

import DHT.Node (Node(..))
import DHT.NodeID (NodeID)
import Data.Foldable

data Bucket =
  Bucket [Node]
  deriving (Eq, Show)

emptyBucket :: Bucket
emptyBucket = Bucket []

insert :: Bucket -> Node -> Bucket
insert (Bucket nodes) node =
  if length nodes < 8
    then Bucket $ node : nodes
    else Bucket nodes

insertOrUpdate :: Bucket -> Node -> (Node -> Node) -> NodeID -> Bucket
insertOrUpdate (Bucket nodes) newNode update nodeID =
  Bucket (newTarget : others)
  where
    (target, others) = span (\(Node nid _ _) -> nid == nodeID) nodes
    newTarget =
      case target of
        node:_ -> update node
        [] -> newNode

findNode :: Bucket -> NodeID -> Maybe Node
findNode (Bucket nodes) nodeID = find (\(Node nid _ _) -> nid == nodeID) nodes

listNodes :: Bucket -> [Node]
listNodes (Bucket nodes) = nodes
