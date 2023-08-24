module Chebyshev.Fraction.Base (
  FractionResult,
  initChebyRealFrac,
  chebyRealFraction,
  untilInfinity,
  chebyFracMaxOf,
  chebyZeroOf,
) where

import Data.ExtendedReal
import Data.Function ((&))
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Util

type FractionResult = Arg (Extended Rational) (V.Vector Integer)

-- | Initiate fraction computation of chebyshev polynomials.
initChebyRealFrac :: Rational -> InductiveEval Integer (Extended Rational)
initChebyRealFrac u2 = inductive induction 0
 where
  induction n_k g_k = infiRecip . knownFinite $ Finite u2 * (fromIntegral n_k - value g_k)

-- | Fraction of chebyshev polynomials, divided by u to make it real.
chebyRealFraction :: Rational -> [Integer] -> Extended Rational
chebyRealFraction u2 n_ = value $ nexts n_ (initChebyRealFrac u2)

-- Stops when infinity is encountered
untilInfinity :: (Monad m) => Fold.Fold m FractionResult (Maybe FractionResult)
untilInfinity = Fold.takeEndBy (\(Arg curMax _) -> Data.ExtendedReal.isInfinite curMax) Fold.latest

-- | Chebyshev fraction maximum given certain inductive initiation.
chebyFracMaxOf :: (Monad m) => InductiveEval () FractionResult -> Stream.Stream m FractionResult
chebyFracMaxOf initiate =
  Stream.iterate (next ()) initiate
    & fmap value
    & Stream.scanMaybe untilInfinity

-- | Determines if the initiated case has s_k's zero.
chebyZeroOf :: (Monad m) => InductiveEval () FractionResult -> Stream.Stream m (Either Int (V.Vector Integer))
chebyZeroOf initiate =
  chebyFracMaxOf initiate
    & Stream.indexed
    & fmap argInfinite
 where
  argInfinite = \case
    (_, Arg m arg) | Data.ExtendedReal.isInfinite m -> Right arg
    (k_1, _) -> Left (k_1 + 1)
