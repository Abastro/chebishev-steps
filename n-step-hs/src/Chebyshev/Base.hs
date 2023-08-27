module Chebyshev.Base (
  initChebyNormal,
  chebyNormal,
  initContinuedFrac,
  continuedFraction,
) where

import Data.ExtendedReal
import Inductive
import Util

-- | InductiveEval for normalized chebyshev; Starts at s_1.
--
-- This is to ensure that the value stays being a quotient.
initChebyNormal :: (Integral a, Fractional v) => v -> InductiveEval a v
initChebyNormal u2 = inductive induction 1
 where
  induction n_k s_k = case previous s_k of
    Nothing -> value s_k -- s_0 = 0
    Just (n_k_1, s_k_1) -> value s_k - value s_k_1 / (u2 * fromIntegral (n_k * n_k_1))

-- | Normalized chebyshev polynomial.
--
-- >>> chebyNormal (1/3) [1, 2, 3]
-- (-1) % 1
--
-- >>> chebyNormal 1 [1, 2, 2]
-- 1 % 4
chebyNormal :: Rational -> [Integer] -> Rational
chebyNormal u2 n_ = value $ nexts n_ (initChebyNormal u2)

-- | InductiveEval for continued fraction divided by u.
initContinuedFrac :: Rational -> InductiveEval Integer (Extended Rational)
initContinuedFrac u2 = inductive induction 0
 where
  induction n_k g_k = infiRecip . knownFinite $ Finite u2 * (fromIntegral n_k - value g_k)

-- | Continued fraction, divided by u to make it real.
continuedFraction :: Rational -> [Integer] -> Extended Rational
continuedFraction u2 n_ = value $ nexts n_ (initContinuedFrac u2)
