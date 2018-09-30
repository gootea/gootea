module Collector.Collector
  ( Collection
  , newCollection
  -- , addToCollection
  -- , consumeItems
  , filterThroughCollection
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Item a =
  Item a -- Object hold on the collection
       Int -- Number of time this item has been seen

incItemSeenCount :: Item a -> Item a
incItemSeenCount (Item obj count) = Item obj (count + 1)

data Collection a = Collection
  { consumedItems :: S.Set a
  , collectedItems :: M.Map a (Item a)
  , minSeenCount :: Int -- number of time we need to see an object before it can go out
  }

-- Create a new collection that will hold object until they have been seen the
-- given number of times
newCollection :: Int -> Collection a
newCollection = Collection S.empty M.empty

-- Take a collection and an object and return an updated collection and
-- eventually the object if it's ready to go out
filterThroughCollection :: Ord a => Collection a -> a -> (Collection a, Maybe a)
filterThroughCollection c obj =
  if S.member obj (consumedItems c)
    then (c, Nothing)
    else case M.alterF updateCreateOrExtract obj (collectedItems c) of
           (Just out, updatedMap) ->
             ( c
                 { collectedItems = updatedMap
                 , consumedItems = S.insert out (consumedItems c)
                 }
             , Just out)
           (Nothing, updatedMap) -> (c {collectedItems = updatedMap}, Nothing)
  where
    updateCreateOrExtract Nothing = (Nothing, Just $ Item obj 1)
    updateCreateOrExtract (Just (Item o count))
      | count >= (minSeenCount c) = (Just o, Nothing)
    updateCreateOrExtract (Just i) = (Nothing, Just $ incItemSeenCount i)
