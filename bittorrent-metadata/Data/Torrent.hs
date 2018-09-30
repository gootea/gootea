module Data.Torrent
  ( Metainfo(..)
  , FileInfo(..)
  , listFiles
  ) where

import Data.BEncode
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

data Metainfo = Metainfo
  { mFiles :: Maybe [FileInfo]
  , mName :: Maybe BS.ByteString
  } deriving (Show, Eq)

instance BEncode Metainfo where
  toBEncode t = undefined
  fromBEncode =
    fromDict $ do Metainfo <$>? (BSC.pack "files") <*>? (BSC.pack "name")

data FileInfo = FileInfo
  { fPath :: [BS.ByteString]
  } deriving (Show, Eq)

instance BEncode FileInfo where
  toBEncode t = undefined
  fromBEncode = fromDict $ do FileInfo <$>! (BSC.pack "path")

listFiles :: Metainfo -> [BS.ByteString]
listFiles (Metainfo files name) =
  case (name, files) of
    (Just name, Nothing) -> [name]
    (Just name, Just []) -> [name]
    (Just name, Just files) ->
      fmap
        (BS.append name . BS.append delimiter . BS.intercalate delimiter . fPath)
        files
    (Nothing, Just files) -> fmap (BS.intercalate delimiter . fPath) files
    (Nothing, Nothing) -> []
  where
    delimiter = BSC.pack "/"
