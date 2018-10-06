module Store.Store
  ( newStore
  , saveMetainfo
  ) where

import DHT.NodeID
import qualified Data.ByteString as BS
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
saveMetainfo (Store nameHandle fileHandle) (InfoHash ih) metainfo = do
  let nameLine = toLine $ fromMaybe BS.empty $ mName metainfo
  BS.hPutStr nameHandle nameLine
  let fileLines = toLine <$> listFiles metainfo
  BS.hPutStr fileHandle $ BS.concat fileLines
  where
    toLine bs = BS.concat [ih, fieldSep, bs, lineSep]
