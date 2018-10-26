import Criterion.Main
import DHT.Distance
import DHT.NodeID
import qualified Data.ByteString as B

main =
  defaultMain
    [ bgroup
        "XOR distance"
        [ bench "compare equal" $ whnf compareXORDistance (ref, a, a)
        , bench "compare when very different" $ whnf compareXORDistance (ref, a, b)
        ]
    ]


ref = NodeID (B.pack $ replicate 20 0)
a = NodeID (B.pack $ replicate 20 0)
b = NodeID (B.pack $ replicate 20 255)

compareXORDistance (ref, a, b) = compare (distanceTo ref a) (distanceTo ref a)
