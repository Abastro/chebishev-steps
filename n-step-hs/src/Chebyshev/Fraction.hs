-- | Properties of minimal chebyshev polynomial in terms of fractions.
module Chebyshev.Fraction (
  chebyFraction,
  chebyPartSlope,
  chebyFractionMax,
  findChebyshev,
) where

import Chebyshev.Base
import Control.Monad.State
import Data.Bifunctor (Bifunctor (..))
import Data.ExtendedReal
import Data.Maybe
import Data.MemoTrie
import Data.Monoid (First (..))
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Streamly.Data.Fold qualified as Fold
import Streamly.Prelude qualified as Stream
import Util

-- | Normalized chebyshev into fraction.
chebyFraction :: (Fractional v, Eq v) => InductiveEval a v -> Extended v
chebyFraction s_k1 = case previous s_k1 of
  Nothing -> Finite 0
  Just (_, s_k) -> value s_k `infiDiv` value s_k1

-- | Part of the chebyshev slope fraction.
chebyPartSlope :: (Fractional v, Eq v, Integral a) => v -> InductiveEval a v -> Extended v
chebyPartSlope u2 s_k1 = case previous s_k1 of
  Nothing -> Finite 0
  Just (n_k, s_k) -> value s_k `infiDiv` (u2 * fromIntegral n_k * value s_k1)

-- >>> chebyFractionMax 3 3
-- Arg (Finite (2 % 1)) [-1,-1,-1]

-- >>> chebyFractionMax 3 5
-- Arg PosInf [-1,-1,-1,-1,-1]

-- >>> chebyFractionMax (8/3) 5
-- Arg PosInf [-6,-1,-1,-1,-1]

chebyFractionMax :: Rational -> Word -> Arg (Extended Rational) (V.Vector Integer)
chebyFractionMax u2 = computeMax
 where
  getMax :: Word -> Extended Rational
  getMax k = case computeMax k of
    Arg v _ -> v

  -- Initial max candidate.
  maxCandidate :: Word -> Arg (Extended Rational) (V.Vector Integer)
  maxCandidate k = Arg maxCand n_
   where
    n_ = V.cons n_1 argmax_R
    maxCand = abs . chebyFraction $ nexts (V.toList n_) (initChebyNormal u2)

    Arg _ argmax_R = computeMax (k - 1)
    s_R_rev = nexts (V.toList $ V.reverse argmax_R) (initChebyNormal u2)
    g_R = knownFinite $ chebyPartSlope u2 s_R_rev
    n_1 = case properFraction g_R of
      (btwn, r)
        | r < 0 -> btwn - 1
        | r > 0 -> btwn + 1
        | otherwise -> btwn

  boundsFor :: Word -> Word -> Rational -> Rational -> Extended Rational -> (Rational, Rational)
  boundsFor k i g_L maxG_R curMax
    | i < k = (g_L - boundRadius, g_L + boundRadius)
    | otherwise = if g_L < 0 then (g_L * upperOnK, g_L * lowerOnK) else (g_L * lowerOnK, g_L * upperOnK)
   where
    boundMultiple = knownFinite $ (1 + getMax (k - i - 1) / curMax) / (1 - getMax (k - i) / curMax)
    boundRadius = maxG_R * boundMultiple
    lowerOnK = knownFinite $ 1 / (1 + 1 / curMax)
    upperOnK = knownFinite $ 1 / (1 - 1 / curMax)

  -- TODO Bound for n_1 is often horrible; Remedy that part.
  computeMax :: Word -> Arg (Extended Rational) (V.Vector Integer)
  computeMax = memo $ \case
    0 -> Arg 0 V.empty -- s0 / s1 = 0
    1 -> Arg 1 (V.singleton 1) -- s1 / s2 = 1
    k -> flip evalState 0 $ do
      fromMaybe (error "maximal entry not found") <$> Stream.fold untilInfinity maxFinding
     where
      -- Setup: F(x) = F(x_L, x_i, x_R), |x| = k, |x_L| = i-1, |x_R| = k-i
      maxCandArg@(Arg _ _) = maxCandidate k

      chooseN_i ::
        InductiveEval Integer Rational ->
        Word ->
        Stream.SerialT (State (Extended Rational)) (InductiveEval Integer Rational)
      chooseN_i s_L i = do
        let knownG_L = knownFinite $ chebyPartSlope u2 s_L
        let maxG_R = knownFinite (getMax (k - i)) / abs u2
        rateBounds <- Stream.fromEffect $ gets (boundsFor k i knownG_L maxG_R)
        let (minBnd, maxBnd) = bimap ceiling floor rateBounds
        -- curMax <- get
        -- when (i <= 2) $ traceShowM (k, i, inputs s_L, knownG_L, maxG_R, curMax, minBnd, maxBnd)
        n_i <- Stream.filter (/= 0) $ Stream.enumerateFromTo minBnd maxBnd
        pure (next n_i s_L)

      -- Stops when infinity is encountered
      untilInfinity :: (Monad m) => Fold.Fold m (Arg (Extended r) b) (Maybe (Arg (Extended r) b))
      untilInfinity = Fold.takeEndBy (\(Arg curMax _) -> Data.ExtendedReal.isInfinite curMax) Fold.last

      maxFinding :: Stream.SerialT (State (Extended Rational)) (Arg (Extended Rational) (V.Vector Integer))
      maxFinding = do
        curMaxArg@(Arg curMax _) <- Stream.scanl' max maxCandArg $ do
          s_k1 <- foldM chooseN_i (initChebyNormal u2) [1 .. k]
          let c_k = chebyFraction s_k1
          pure $ Arg (abs <$> c_k) (V.fromList $ inputs s_k1)
        curMaxArg <$ Stream.fromEffect (put curMax)

-- TODO Slow for some numbers.
-- TODO 6: 17/6
-- TODO 8: 23/8, 31/8
-- 9:
-- 10:

-- >>> findChebyshev (7/3) 8
-- Just [-3,-1,-1,-1,-3]

-- >>> findChebyshev (8/3) 8
-- Just [-6,-1,-1,-1,-1]

-- | Given a root, find a chebyshev polynomial of minimal degree.
findChebyshev :: Rational -> Word -> Maybe (V.Vector Integer)
findChebyshev u2 cutoff = getFirst $ foldMap (First . argForInfinite) [2 .. cutoff]
 where
  getMax = chebyFractionMax u2
  -- 'chebyFractionMax u2 k' is infinite when 'chebyNormal u2 (k+1)' is 0.
  argForInfinite k = case getMax (pred k) of
    Arg m arg | Data.ExtendedReal.isInfinite m -> Just arg
    _ -> Nothing
