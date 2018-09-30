module DHT.Server
  ( createServer
  , createServerWithID
  , runServer
  , DHTServer(..)
  , DHTInputMessage(..)
  , DHTOutputMessage(..)
  ) where

import qualified BEncode
import Control.Concurrent
import Control.Concurrent.Chan
import qualified DHT.Codec
import DHT.DHT
import DHT.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.Socket hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import System.IO.Error
import System.Random

data DHTServer = DHTServer
  { _dInputChan :: Chan DHTInputMessage
  , _dOutputChan :: Chan DHTOutputMessage
  , _dDht :: DHT
  , _dInitialHosts :: [SockAddr]
  , _dPort :: PortNumber
  }

data DHTInputMessage =
  DHTInputCommand DHTCmd

data DHTOutputMessage =
  DHTOutputEvent DHTEvent
  deriving (Show)

runServer :: DHTServer -> IO ()
runServer (DHTServer inputChan outputChan dht initialHosts port) = do
  mainSocket <- createSocket
  mainInputChan <- newChan
  _ <- forkIO $ forwardChan mapInputToMainInput inputChan mainInputChan
  _ <- forkIO $ mainLoop mainInputChan mainSocket outputChan dht
  _ <- writeChan mainInputChan $ MainInputCommand $ DHTCmdAddHosts initialHosts
  _ <- writeChan mainInputChan $ MainInputCommand DHTCmdInit
  socketLoop mainSocket mainInputChan
  where
    mapInputToMainInput (DHTInputCommand cmd) = MainInputCommand cmd
    createSocket = do
      sock <- socket AF_INET Datagram defaultProtocol
      let sockAddr = SockAddrInet port $ tupleToHostAddress (0, 0, 0, 0)
      _ <- bind sock sockAddr
      return sock
    socketLoop s c = do
      (bytes, sa) <- recvFrom s 1500
      let input = MainInputNetworkBytes sa bytes
      _ <- writeChan c input
      socketLoop s c

createServer :: PortNumber -> [SockAddr] -> IO DHTServer
createServer port initialHosts =
  randomIO >>= createServerWithID port initialHosts

createServerWithID :: PortNumber -> [SockAddr] -> NodeID -> IO DHTServer
createServerWithID port initalHosts nodeID = do
  inputChan <- newChan
  outputChan <- newChan
  stdGen <- getStdGen
  return $
    DHTServer inputChan outputChan (emptyDHT nodeID stdGen) initalHosts port

data MainInputMessage
  = MainInputNetworkBytes SockAddr
                          B.ByteString
  | MainInputCommand DHTCmd

mainLoop ::
     (Chan MainInputMessage)
  -> Socket
  -> (Chan DHTOutputMessage)
  -> DHT
  -> IO ()
mainLoop inputChan sock outputChan dht = do
  message <- readChan inputChan
  let (newDHT, dhtOutputs) = handleInputMessage dht message
  _ <- sequence $ fmap (handleError . handleOutput) dhtOutputs
  mainLoop inputChan sock outputChan newDHT
  where
    handleError :: IO () -> IO ()
    handleError io = catchIOError io (\_ -> return ())
    handleOutput (DHTOutPacket addr packet) =
      sendTo sock (encodePacket packet) addr >> return ()
    handleOutput (DHTOutEvent event) =
      writeChan outputChan $ DHTOutputEvent event
    handleOutput DHTOutNoNodesToInit =
      forkIO (scheduleMessage inputChan 5000 (MainInputCommand DHTCmdInit)) >>
      return ()
    encodePacket :: DHT.Types.Packet -> B.ByteString
    encodePacket = BL.toStrict . BEncode.encode . DHT.Codec.encode

handleInputMessage :: DHT -> MainInputMessage -> (DHT, [DHTOutput])
handleInputMessage dht (MainInputNetworkBytes srcAddr inputBytes) =
  case packet of
    Just p -> handlePacket dht srcAddr p
    Nothing -> (dht, [])
  where
    packet =
      (BEncode.decode inputBytes) >>=
      (DHT.Codec.decode (getGetTransactionType dht))
handleInputMessage dht (MainInputCommand command) = handleCommand dht command

-- Forward message from inputChan to outpuChan and map then inbetween
forwardChan :: (a -> b) -> Chan a -> Chan b -> IO ()
forwardChan fn inputChan outputChan = do
  input <- readChan inputChan
  _ <- writeChan outputChan (fn input)
  forwardChan fn inputChan outputChan

-- Schedule a message to be sent later on a channel
-- sleepDuration is a number of milliseconds
scheduleMessage :: Chan a -> Int -> a -> IO ()
scheduleMessage chan sleepDuration message = do
  _ <- threadDelay $ sleepDuration * 1000
  _ <- writeChan chan message
  return ()