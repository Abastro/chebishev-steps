module Chebyshev.Fraction.Base (
  FractionResult,
  untilInfinity,
  chebyZeroOf,
) where

import Data.Function ((&))
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Util

type FractionResult = Arg (Projective Rational) (V.Vector Integer)

-- Stops when infinity is encountered
untilInfinity :: (Monad m) => Fold.Fold m FractionResult (Maybe FractionResult)
untilInfinity = Fold.takeEndBy (\(Arg curMax _) -> curMax == Infinity) Fold.latest

-- | Determines if the initiated case has s_k's zero.
chebyZeroOf :: (Monad m) => (Int -> FractionResult) -> Stream.Stream m (Either Int (V.Vector Integer))
chebyZeroOf getMax =
  Stream.enumerateFrom 0
    & fmap getMax
    & Stream.scanMaybe untilInfinity
    & Stream.indexed
    & fmap argInfinite
 where
  argInfinite = \case
    (_, Arg Infinity arg) -> Right arg
    (k_1, _) -> Left (k_1 + 1)
