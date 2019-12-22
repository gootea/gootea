{-# LANGUAGE OverloadedStrings #-}

module Common.Models.InfoHash
  ( InfoHash
  , newInfoHash
  ) where

import qualified Data.ByteString as B

newtype InfoHash =
  InfoHash B.ByteString
  deriving (Show, Eq, Ord)

newInfoHash :: B.ByteString -> InfoHash
newInfoHash = InfoHash


