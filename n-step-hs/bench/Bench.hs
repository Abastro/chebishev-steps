module Main (main) where

import Chebyshev
import Criterion.Main
import Data.Vector qualified as V

main :: IO ()
main =
  defaultMain
    [ bgroup
        "chebyNormal"
        [ bgroup
            "1/3"
            [ bench "[1..3]" $ whnf (chebyNormal (1 / 3)) (V.enumFromN 1 3)
            ],
          bgroup "2/7"
            $ let cheby = chebyNormal (2 / 7)
               in [bench (show vec) $ whnf cheby vec | k <- [2 .. 8], let vec = V.enumFromN 1 k]
        ],
      bgroup
        "slopeTermUB"
        [ bgroup
            "2/7"
            [ bench "[1..5]" $ whnf (chebyNormal (2 / 7)) (V.enumFromN 1 5)
            ]
        ]
    ]
