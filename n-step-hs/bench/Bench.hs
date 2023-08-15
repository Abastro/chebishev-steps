module Main (main) where

import Chebyshev.Base
import Chebyshev.Fraction qualified as Fraction
import Chebyshev.Linear qualified as Linear
import Data.Functor.Identity
import Data.Vector qualified as V
import Streamly.Prelude qualified as Stream
import Test.Tasty.Bench

alternatingTo1 :: (Monad m, Num a, Stream.Enumerable a) => a -> Stream.SerialT m a
alternatingTo1 bnd = (*) <$> Stream.enumerateFromTo 1 bnd <*> Stream.fromList [1, -1]

alternatingTo2 :: (Monad m, Num a, Eq a, Stream.Enumerable a) => a -> Stream.SerialT m a
alternatingTo2 bnd = Stream.filter (/= 0) $ Stream.enumerateFromTo (-bnd) bnd

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
        "alternatingTo"
        [ bench "alternatingTo1 10" $ whnf (runIdentity . Stream.sum . alternatingTo1) (10 :: Integer),
          bench "alternatingTo2 10" $ whnf (runIdentity . Stream.sum . alternatingTo2) (10 :: Integer)
        ],
      bgroup
        "findChebyshev linear vs fraction"
        [ bench "Linear.findChebyshev 9/5" $ nf (`Linear.findChebyshev` 100) (9 / 5),
          bench "Fraction.findChebyshev 9/5" $ nf (`Fraction.findChebyshev` 100) (9 / 5),
          bench "Linear.findChebyshev 8/5" $ nf (`Linear.findChebyshev` 100) (8 / 5),
          bench "Fraction.findChebyshev 8/5" $ nf (`Fraction.findChebyshev` 100) (8 / 5)
        ]
    ]
