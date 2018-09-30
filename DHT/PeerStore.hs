module DHT.PeerStore
  ( PeerStore
  , emptyPeerStore
  , addPeer
  , getPeers
  , listAssocs
  ) where

import DHT.Types
import qualified Data.Map.Strict as M

data PeerStore =
  PeerStore (M.Map InfoHash [Peer])

emptyPeerStore :: PeerStore
emptyPeerStore = PeerStore M.empty

addPeer :: PeerStore -> InfoHash -> Peer -> PeerStore
addPeer (PeerStore ps) ih p = PeerStore $ M.insertWith (++) ih [p] ps

getPeers :: PeerStore -> InfoHash -> Maybe [Peer]
getPeers (PeerStore ps) ih = M.lookup ih ps

listAssocs :: PeerStore -> [(InfoHash, [Peer])]
listAssocs (PeerStore ps) = M.assocs ps
