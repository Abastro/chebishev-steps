module Main (main) where

import Chebyshev (chebyNormal)
import Criterion.Main
import Data.Vector qualified as V
import Streamly.Prelude qualified as Stream
import Data.Functor.Identity

alternatingTo :: (Monad m, Num a, Stream.Enumerable a) => a -> Stream.SerialT m a
alternatingTo bnd = (*) <$> Stream.enumerateFromTo 1 bnd <*> Stream.fromList [1, -1]

alternatingTo_ :: (Monad m, Num a, Eq a, Stream.Enumerable a) => a -> Stream.SerialT m a
alternatingTo_ bnd = Stream.filter (/= 0) $ Stream.enumerateFromTo (-bnd) bnd

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
      bgroup "alternatingTo"
        [
          bench "alternatingTo 10" $ whnf (runIdentity . Stream.sum . alternatingTo) (10 :: Integer),
          bench "alternatingTo_ 10" $ whnf (runIdentity . Stream.sum . alternatingTo_) (10 :: Integer)
        ]
    ]
