module DHTServerSpec where

import Control.Applicative (liftA2)
import Control.Concurrent.Chan
import Control.Monad
import DHT.NodeID
import DHT.Server
import qualified Data.ByteString.Char8 as BC
import Data.List ((\\))
import Network.Socket
import Prelude hiding (insert)
import System.Exit
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = return ()
    -- describe "DHT Server" $ do
-- Unit tests
