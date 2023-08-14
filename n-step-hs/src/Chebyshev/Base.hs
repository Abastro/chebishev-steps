module Chebyshev.Base (
  initChebyNormal,
  chebyNormal,
) where

import Data.Vector qualified as V
import Inductive

-- | InductiveEval for normalized chebyshev; Starts at s_1.
initChebyNormal :: (Integral a, Fractional v) => v -> InductiveEval a v
initChebyNormal u2 = inductive induction 1
 where
  induction n_k s_k = case previous s_k of
    Nothing -> value s_k -- s_0 = 0
    Just (n_k_1, s_k_1) -> value s_k - value s_k_1 / (u2 * fromIntegral (n_k * n_k_1))

-- | Normalized chebyshev polynomial.
--
-- >>> chebyNormal (1/3) (V.fromList [1, 2, 3])
-- (-1) % 1
--
-- >>> chebyNormal 1 (V.fromList [1, 2, 2])
-- 1 % 4
chebyNormal :: Rational -> V.Vector Integer -> Rational
chebyNormal u2 n_ = value $ nexts (V.toList n_) (initChebyNormal u2)
