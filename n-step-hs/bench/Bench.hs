module Main (main) where

import Chebyshev.Base
import Chebyshev.Composite qualified as Composite
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
        [ bench "Linear.chebyZero 9/5" $ nf (findUntilCutoff 100 . Linear.chebyZero) (9 / 5),
          bench "Fraction.chebyZero 9/5" $ nf (findUntilCutoff 100 . Fraction.chebyZero Indefinite) (9 / 5),
          bench "Composite.chebyZero 9/5" $ nf (findUntilCutoff 100 . (findZeroStream . Composite.chebyNormalMin)) (9 / 5),
          bench "Linear.chebyZero 8/5" $ nf (findUntilCutoff 100 . Linear.chebyZero) (8 / 5),
          bench "Fraction.chebyZero 8/5" $ nf (findUntilCutoff 100 . Fraction.chebyZero Indefinite) (8 / 5),
          bench "Composite.chebyZero 8/5" $ nf (findUntilCutoff 100 . (findZeroStream . Composite.chebyNormalMin)) (8 / 5)
        ]
    ]
