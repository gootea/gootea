module DHT.TokenManager
  ( TokenManager
  , newTokenManager
  , newToken
  , checkToken
  ) where

import DHT.Types
import Network.Socket
import System.Random

data IssuedToken =
  IssuedToken SockAddr
              Token

data TokenManager =
  TokenManager [IssuedToken]

newTokenManager :: TokenManager
newTokenManager = TokenManager []

newToken :: TokenManager -> StdGen -> SockAddr -> (Token, TokenManager, StdGen)
newToken (TokenManager existing) stdGen addr =
  (token, TokenManager $ issued : existing, gen)
  where
    issued = IssuedToken addr token
    (token, gen) = random stdGen

checkToken :: TokenManager -> Token -> SockAddr -> Bool
checkToken (TokenManager tokens) token addr = any match tokens
  where
    match (IssuedToken a t) = a == addr && t == token
