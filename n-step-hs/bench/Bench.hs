module Main (main) where

import Chebyshev
import Criterion.Main
import Data.Vector qualified as V

main :: IO ()
main =
  defaultMain
    [ bgroup
        "sPolyNormal"
        [ bench "1/3, [1,2,3]" $ whnf (uncurry sPolyNormal) (1 / 3, V.fromList [1, 2, 3]),
          bench "2/7, [7,3,1,2]" $ whnf (uncurry sPolyNormal) (2 / 7, V.fromList [7, 3, 1, 2]),
          bench "2/7, [7,3,1,2,3,4]" $ whnf (uncurry sPolyNormal) (2 / 7, V.fromList [7, 3, 1, 2, 3, 4]),
          bench "2/7, [7,3,1,2,3,4,5,6]" $ whnf (uncurry sPolyNormal) (2 / 7, V.fromList [7, 3, 1, 2, 3, 4, 5, 6])
        ]
    ]
