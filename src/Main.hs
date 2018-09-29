module Main where

import Network.Socket hiding (send, recv)
import qualified Data.ByteString as BS
import System.IO.Error
import Network.Bittorrent.Client
import qualified Data.Torrent as T

main :: IO ()
main = do
  metainfo <- getMetainfo sockAddr infohash nodeID

  case metainfo of
    Right info -> putStrLn $ show $ T.listFiles info
    Left _ -> putStrLn "Failed to decode torrent metainfo"

sockAddr = SockAddrInet 6881 (tupleToHostAddress (5,39,91,197))
infohash = BS.pack [254,10,72,13,167,152,42,193,185,141,126,15,9,91,5,223,56,175,46,130] -- InfoHash of Chasing Ice
-- infohash = BS.pack [75,156,230,86,88,210,217,195,181,188,118,42,8,20,216,228,155,59,2,142] -- The Fall
nodeID = BS.pack [42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42]

