-- | Instead of finding real maximum, settle for easy-to-find ones.
module Chebyshev.Fraction.Naive (
  initChebyRealFrac,
  chebyRealFraction,
  chebyRealFractionMax,
  findChebyshev,
) where

import Data.ExtendedReal
import Data.MemoTrie
import Data.Monoid (First (..))
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Util

-- | Initiate fraction computation of chebyshev polynomials.
initChebyRealFrac :: (RealFrac v, Integral a) => v -> InductiveEval a (Extended v)
initChebyRealFrac u2 = inductive induction 0
 where
  induction n_k g_k = infiRecip . knownFinite $ Finite u2 * (fromIntegral n_k - value g_k)

-- | Fraction of chebyshev polynomials, divided by u to make it real.
chebyRealFraction :: Rational -> [Integer] -> Extended Rational
chebyRealFraction u2 n_ = value $ nexts n_ (initChebyRealFrac u2)

-- >>> chebyRealFractionMax 3 3
-- Arg (Finite (2 % 3)) [1,1,1]

-- >>> chebyRealFractionMax 3 5
-- Arg PosInf [1,1,1,1,1]

-- >>> chebyRealFractionMax (8/3) 5
-- Arg PosInf [6,1,1,1,1]

-- TODO Just "naive" is useless, need some naive computation

-- Stops when infinity is encountered

-- | Compute maximal real-fraction G_k given k and steps.
chebyRealFractionMax :: Rational -> Word -> Arg (Extended Rational) (V.Vector Integer)
chebyRealFractionMax u2 = computeMax
 where
  maxCandidate :: Word -> Arg (Extended Rational) (V.Vector Integer)
  maxCandidate k = Arg (abs . chebyRealFraction u2 $ V.toList n_) n_
   where
    n_ = V.cons n_1 argmax_R

    Arg _ argmax_R = computeMax (k - 1)
    g_R_rev = knownFinite $ chebyRealFraction u2 (V.toList $ V.reverse argmax_R)
    -- Opposite direction of truncate
    n_1 = case properFraction g_R_rev of
      (btwn, r)
        | r < 0 -> btwn - 1
        | r > 0 -> btwn + 1
        | otherwise -> btwn

  computeMax :: Word -> Arg (Extended Rational) (V.Vector Integer)
  computeMax = memo $ \case
    0 -> Arg 0 V.empty -- s0 / s1 = 0
    1 -> Arg (Finite $ 1 / abs u2) (V.singleton 1) -- s1 / s2 = x1 / u^2
    k -> maxCandidate k

-- Slow numbers:
-- 7: 17/6, 23/6
-- 8: 23/8, 31/8

-- >>> findChebyshev (7/3) 8
-- Just [3,1,2,2,1,1,1]

-- >>> findChebyshev (8/3) 8
-- Just [6,1,1,1,1]

-- | Given a root, find a chebyshev polynomial of minimal degree.
findChebyshev :: Rational -> Word -> Maybe (V.Vector Integer)
findChebyshev u2 cutoff = getFirst $ foldMap (First . argForInfinite) [2 .. cutoff]
 where
  getMax = chebyRealFractionMax u2
  -- 'chebyFractionMax u2 k' is infinite when 'chebyNormal u2 (k+1)' is 0.
  argForInfinite k = case getMax (pred k) of
    Arg m arg | Data.ExtendedReal.isInfinite m -> Just arg
    _ -> Nothing
