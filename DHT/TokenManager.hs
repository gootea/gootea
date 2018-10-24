module DHT.TokenManager
  ( TokenManager
  , newTokenManager
  , newToken
  , checkToken
  , rotateSeeds
  ) where

import Crypto.Hash.SHA1
import DHT.Types
import qualified Data.ByteString as BS
import Network.Socket
import System.Random

newtype Seed =
  Seed BS.ByteString

instance Random Seed where
  random g = (Seed $ BS.pack bytes, finalGen)
    where
      (bytes, finalGen) = foldl addWord ([], g) [0 .. 4 :: Int]
      addWord (acc, gen) _ = (newElem : acc, newGen)
        where
          (newElem, newGen) = random gen
  randomR (_, _) g = random g

data TokenManager =
  TokenManager Seed
               Seed

newTokenManager :: StdGen -> (TokenManager, StdGen)
newTokenManager gen = (TokenManager seed1 seed2, gen2)
  where
    (seed1, gen1) = random gen
    (seed2, gen2) = random gen1

newToken :: TokenManager -> SockAddr -> Token
newToken (TokenManager seed _) addr = computeToken seed addr

addrToBytes :: SockAddr -> BS.ByteString
addrToBytes (SockAddrInet port addr) = BS.pack [p, a1, a2, a3, a4]
  where
    p = fromIntegral port
    (a1, a2, a3, a4) = hostAddressToTuple addr
addrToBytes (SockAddrInet6 port _ addr _) =
  BS.pack
    [ p
    , fromIntegral a1
    , fromIntegral a2
    , fromIntegral a3
    , fromIntegral a4
    , fromIntegral a5
    , fromIntegral a6
    , fromIntegral a7
    , fromIntegral a8
    ]
  where
    p = fromIntegral port
    (a1, a2, a3, a4, a5, a6, a7, a8) = hostAddress6ToTuple addr
addrToBytes _ = BS.pack [1, 2, 3, 4, 5, 6]

checkToken :: TokenManager -> Token -> SockAddr -> Bool
checkToken (TokenManager seed1 seed2) token addr =
  checkWithSeed seed1 || checkWithSeed seed2
  where
    checkWithSeed seed = computeToken seed addr == token

computeToken :: Seed -> SockAddr -> Token
computeToken (Seed s) addr = Token $ hash (s <> addrToBytes addr)

rotateSeeds :: TokenManager -> StdGen -> (TokenManager, StdGen)
rotateSeeds (TokenManager seed1 seed2) stdgen = (TokenManager newSeed seed1, newGen)
  where
    (newSeed, newGen) = random stdgen
