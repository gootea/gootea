module Data.Torrent
  ( Metainfo(..)
  ) where

import Data.BEncode
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

data Metainfo = Metainfo
  { mFiles :: [FileInfo]
  , mName :: Maybe BS.ByteString
  } deriving (Show)

instance BEncode Metainfo where
  toBEncode t = undefined
  fromBEncode =
    fromDict $ do Metainfo <$>! (BSC.pack "files") <*>? (BSC.pack "name")

data FileInfo = FileInfo
  { fPath :: [BS.ByteString]
  } deriving (Show)

instance BEncode FileInfo where
  toBEncode t = undefined
  fromBEncode = fromDict $ do FileInfo <$>! (BSC.pack "path")
