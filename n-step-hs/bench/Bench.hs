module Main (main) where

import Chebyshev.Base
import Chebyshev.Fraction (SearchPass (..))
import Chebyshev.Fraction qualified as Fraction
import Chebyshev.Linear qualified as Linear
import Test.Tasty.Bench

main :: IO ()
main =
  defaultMain
    [ bgroup
        "chebyNormal"
        [ bgroup
            "1/3"
            [ bench "[1..3]" $ whnf (chebyNormal (1 / 3)) [1 .. 3]
            ],
          bgroup "2/7"
            $ let cheby = chebyNormal (2 / 7)
               in [bench (show n_) $ whnf cheby n_ | k <- [2 .. 5], let n_ = [1 .. k]]
        ],
      bgroup
        "findChebyshev linear vs fraction"
        [ bench "Linear.findChebyshev 9/5" $ nf (findUntilCutoff 100 . Linear.chebyZero) (9 / 5),
          bench "Fraction.findChebyshev 9/5" $ nf (findUntilCutoff 100 . Fraction.chebyZero [Complete]) (9 / 5),
          bench "Linear.findChebyshev 8/5" $ nf (findUntilCutoff 100 . Linear.chebyZero) (8 / 5),
          bench "Fraction.findChebyshev 8/5" $ nf (findUntilCutoff 100 . Fraction.chebyZero [Complete]) (8 / 5)
        ]
    ]
