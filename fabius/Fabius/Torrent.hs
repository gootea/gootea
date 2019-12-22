{-# LANGUAGE OverloadedStrings #-}

module Fabius.Torrent
  ( Torrent(..)
  , newTorrent
  ) where

import qualified Data.Text as T
import Common.Models.InfoHash

data Torrent =
  Torrent
    { torrentInfoHash :: InfoHash
    , torrentName :: T.Text
    }
  deriving (Eq, Ord, Show)

-- | Create a new Torrent
newTorrent :: InfoHash -> T.Text -> Torrent
newTorrent = Torrent
