module Chebyshev.Fraction.Base (
  FractionResult,
  findInftyStream,
) where

import Data.Function ((&))
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Streaming
import Streaming.Prelude qualified as Stream
import Util

type FractionResult = Arg (Projective Rational) (V.Vector Integer)

-- | Finds the infinity from the stream.
findInftyStream :: (Monad m) => (Int -> FractionResult) -> Stream (Of Int) m (Maybe (V.Vector Integer))
findInftyStream getMax =
  Stream.enumFrom (1 :: Int)
    & Stream.map detectInfty
    & Stream.partitionEithers
    & Stream.head_
 where
  detectInfty k = case getMax k of
    Arg Infinity arg -> Left arg
    _ -> Right k
