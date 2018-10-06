module Main where

import Collector.Collector
import Control.Concurrent
import Control.Monad
import DHT.NodeID (InfoHash(..))
import DHT.Server
import DHT.Types (DHTEvent(..))
import Data.List.Split
import Data.Torrent (Metainfo)
import Network.Socket hiding (recv, recvFrom)
import System.Console.GetOpt
import System.Environment
import Network.Bittorrent.Client
import Store.Store

main :: IO ()
main = do
  args <- getArgs
  let configuration = getConfiguration args
  if (_cShowHelp configuration)
    then putStrLn getUsageInfo
    else start configuration

start :: Configuration -> IO ()
start conf = do
  seeds <- resolveSeeds $ _cSeedHosts conf
  _ <-
    putStrLn $
    "The DHT server will be initialized with those seeds: " ++ show seeds
  dhtServer <- createServer (_cPort conf) seeds
  _ <- forkIO $ runServer dhtServer
  _ <- putStrLn "Server is running"
  let outputChan = _dOutputChan dhtServer
  let collection = newCollection 3
  store <- newStore "torrentNames.txt" "torrentFiles.txt"
  infohashChan <- newChan
  ihWithPeersChan <- newChan
  metainfoChan <- newChan
  forkIO $ stageFilter outputChan collection infohashChan
  forkIO $ stageResolvePeers infohashChan dhtServer ihWithPeersChan
  forkIO $ stageGetTorrentMetainfo ihWithPeersChan metainfoChan
  stageSaveToStore store metainfoChan


stageFilter ::
     Chan DHTOutputMessage -> Collection InfoHash -> Chan InfoHash -> IO ()
stageFilter chanIn collection chanOut = do
  event <- readChan chanIn
  let (newCollection, newIH) =
        case event of
          DHTOutputEvent (DHTInfoHashDiscovered ih) ->
            filterThroughCollection collection ih
          _ -> (collection, Nothing)
  stageFilter chanIn collection chanOut

stageResolvePeers ::
     Chan InfoHash -> DHTServer -> Chan (InfoHash, [SockAddr]) -> IO ()
stageResolvePeers chanIn dhtServer chanOut =
  forever (readChan chanIn >>= forkIO . doResolve)
  where
    doResolve infoHash =
      (,) infoHash <$> getPeers dhtServer infoHash >>= writeChan chanOut

stageGetTorrentMetainfo :: Chan (InfoHash, [SockAddr]) -> Chan (InfoHash, Metainfo) -> IO ()
stageGetTorrentMetainfo chanIn chanOut = forever (readChan chanIn >>= forkIO . doGet)
  where
    doGet :: (InfoHash, [SockAddr]) -> IO ()
    doGet (InfoHash ih, firstAddr : otherAddr) = do
      result <- getMetainfo firstAddr ih
      case result of
        Left _ -> doGet (InfoHash ih, otherAddr)
        Right metainfo -> writeChan chanOut (InfoHash ih, metainfo)
    doGet (ih, []) = putStrLn $ "Failed to get Metainfo for IH " ++ show ih

stageSaveToStore :: Store -> Chan (InfoHash, Metainfo) -> IO ()
stageSaveToStore store chan = forever $ do
  (ih, metainfo) <- readChan chan
  saveMetainfo store ih metainfo

resolveSeeds :: [(HostName, ServiceName)] -> IO [SockAddr]
resolveSeeds = fmap (fmap addrAddress . join) . mapM resolveTuple
  where
    resolveTuple (h, p) = getAddrInfo Nothing (Just h) (Just p)

data Configuration = Configuration
  { _cShowHelp :: Bool
  , _cPort :: PortNumber
  , _cSeedHosts :: [(HostName, ServiceName)]
  } deriving (Show)

defaultConfiguration :: Configuration
defaultConfiguration = Configuration False (fromInteger 6881) []

updateWithFlag :: Configuration -> Flag -> Configuration
updateWithFlag conf Help = conf {_cShowHelp = True}
updateWithFlag conf (Port pn) = conf {_cPort = pn}
updateWithFlag conf (Seeds seeds) = conf {_cSeedHosts = seeds}

getConfiguration :: [String] -> Configuration
getConfiguration = foldl updateWithFlag defaultConfiguration . parseFlags

--
-- Command line parsing
--
data Flag
  = Help
  | Port PortNumber
  | Seeds [(HostName, ServiceName)]

options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"] (NoArg Help) "print help"
  , Option ['p'] ["port"] (ReqArg parsePortNumber "PORTNUMBER") "port number"
  , Option
      ['s']
      ["seeds"]
      (ReqArg parseSeeds "host1:port1,host2:port2,...")
      "list of seeds to initiate the DHT"
  ]

parsePortNumber :: String -> Flag
parsePortNumber = Port . fromInteger . read

parseSeeds :: String -> Flag
parseSeeds = Seeds . join . fmap (toTuple . splitHostPort) . splitSeeds
  where
    splitSeeds = splitOn ","
    splitHostPort = splitOn ":"
    toTuple :: [String] -> [(HostName, ServiceName)]
    toTuple (host:port:_) = [(host, port)]
    toTuple (host:_) = [(host, "6881")]
    toTuple _ = []

parseFlags :: [String] -> [Flag]
parseFlags = first . getOpt Permute options
  where
    first (f, _, _) = f

getUsageInfo :: String
getUsageInfo = usageInfo "chuchichaschtli" options
