module Store.Store
  ( Store
  , newStore
  , saveMetainfo
  ) where

import DHT.NodeID
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe
import Data.Torrent
import GHC.IO.Handle.FD
import System.IO

fieldSep :: BS.ByteString
fieldSep = BS.pack [32]

lineSep :: BS.ByteString
lineSep = BS.pack [10]

data Store =
  Store Handle -- namesHandle
        Handle -- nameHandle

newStore ::
     FilePath -- filename for torrent names storage
  -> FilePath -- filename for torrent files storage
  -> IO Store
newStore namesFilename filesFilename = do
  namesHandle <- openFile namesFilename AppendMode
  filesHandle <- openFile filesFilename AppendMode
  return $ Store namesHandle filesHandle

saveMetainfo :: Store -> InfoHash -> Metainfo -> IO ()
saveMetainfo (Store nameHandle fileHandle) ih metainfo = do
  let nameLine = toLine $ fromMaybe BS.empty $ mName metainfo
  BS.hPutStr nameHandle nameLine
  hFlush nameHandle
  let fileLines = toLine <$> listFiles metainfo
  BS.hPutStr fileHandle $ BS.concat fileLines
  hFlush fileHandle
  where
    toLine bs = BS.concat [BSC.pack . show $ ih, fieldSep, bs, lineSep]
