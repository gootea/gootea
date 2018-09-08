module Main where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import qualified Data.ByteString as BS

main :: IO ()
main = do
  sock <- socket AF_INET Stream defaultProtocol
  putStrLn "Socket created"
  connect sock sockAddr
  putStrLn "Socket connected"
  send sock handshakeMessage
  putStrLn "Handshake message sent"
  send sock extensionHandshakeMessage
  putStrLn "Extension handshake sent"
  handShakeResponse <- recv sock 68
  putStrLn $ show handShakeResponse

sockAddr = SockAddrInet 6881 (tupleToHostAddress (5,39,91,197))

handshakeMessage = BS.pack
  [19 -- Lenght prefix
  , 66,105,116,84,111,114,114,101,110,116,32,112,114,111,116,111,99,111,108 -- BitTorrent protocol
  ,0,0,16,0,0,0,0,0 -- Indicate that we support extensions
  , 254,10,72,13,167,152,42,193,185,141,126,15,9,91,5,223,56,175,46,130 -- InfoHash of Chasing Ice
  ,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42 -- NodeID
  ]


extensionHandshakeMessage = BS.pack
  [0,0,0,9 -- Length of the message
  ,20 -- Type: extension
  ,0 -- Extension type: handshake
  ,100,49,58,109,100,101,101 -- d1:mdee This is an empty 'm' dictionary
  ]
