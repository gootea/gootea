module DHTPeerStoreSpec where

import Control.Applicative
import DHT.PeerStore
import DHT.Types
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import Network.Socket
import Test.HUnit hiding (Node)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "DHT Peer Store" $ do
    it "insert two peers and find them again" $ do testInsertTwoPeersAndFindThem
    prop "add and get peer into the store" $ propAddAndGetPeer

testInsertTwoPeersAndFindThem = assertEqual "found peers" expected res
  where
    expected = Just $ reverse peers
    res = getPeers store ih
    store = foldl (\s p -> addPeer s ih p) emptyPeerStore peers
    peers = [peer1, peer2]
    peer1 = Peer 42 $ tupleToHostAddress (42, 42, 42, 42)
    peer2 = Peer 6881 $ tupleToHostAddress (10, 10, 2, 1)
    ih =
      InfoHash $
      B.pack [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]

-- Property tests
instance Arbitrary Peer where
  arbitrary = liftA2 Peer (fmap fromInteger arbitrary) arbitrary

instance Arbitrary InfoHash where
  arbitrary = fmap (InfoHash . B.pack) $ vectorOf 20 arbitrary

propAddAndGetPeer :: [(InfoHash, [Peer])] -> Bool
propAddAndGetPeer peersForIH = all isFound peersForIH
  where
    isFound (ih, peers) = all (\e -> elem e peers) foundPeers
      where
        foundPeers =
          case getPeers store ih of
            Just peers -> peers
            Nothing -> []
    store = foldl insertPeers emptyPeerStore peersForIH
    insertPeers ps (ih, peers) = foldl (\s p -> addPeer s ih p) ps peers
