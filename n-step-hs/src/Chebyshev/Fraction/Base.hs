module Chebyshev.Fraction.Base (
  FractionResult,
  findInftyStream,
) where

import Chebyshev.Base
import Data.Function ((&))
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Streamly.Data.Stream qualified as Stream
import Util

type FractionResult = Arg (Projective Rational) (V.Vector Integer)

-- | Determines if the initiated case has s_k's zero.
findInftyStream :: (Monad m) => (Int -> FractionResult) -> Stream.Stream m (Either Int (V.Vector Integer))
findInftyStream getMax =
  Stream.enumerateFrom 1
    & fmap (\k -> (k, getMax k))
    & Stream.scanMaybe (untilCond $ \(_, Arg curMax _) -> curMax == Infinity)
    & fmap argInfinite
 where
  argInfinite = \case
    (_, Arg Infinity arg) -> Right arg
    (k, _) -> Left (k + 1) -- Related with s_(k+1)
