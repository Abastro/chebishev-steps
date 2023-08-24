module Chebyshev.Fraction.Base (
  FractionResult,
  initChebyRealFrac,
  chebyRealFraction,
  untilInfinity,
) where

import Data.ExtendedReal
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Streamly.Data.Fold qualified as Fold
import Util

type FractionResult = Arg (Extended Rational) (V.Vector Integer)

-- | Initiate fraction computation of chebyshev polynomials.
initChebyRealFrac :: Rational -> FractionEval
initChebyRealFrac u2 = inductive induction 0
 where
  induction n_k g_k = infiRecip . knownFinite $ Finite u2 * (fromIntegral n_k - value g_k)

-- | Fraction of chebyshev polynomials, divided by u to make it real.
chebyRealFraction :: Rational -> [Integer] -> Extended Rational
chebyRealFraction u2 n_ = value $ nexts n_ (initChebyRealFrac u2)

-- Stops when infinity is encountered
untilInfinity :: (Monad m) => Fold.Fold m (Arg (Extended r) b) (Maybe (Arg (Extended r) b))
untilInfinity = Fold.takeEndBy (\(Arg curMax _) -> Data.ExtendedReal.isInfinite curMax) Fold.latest
