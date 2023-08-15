-- | Properties of minimal chebyshev polynomial in terms of fractions.
module Chebyshev.Fraction (
  chebyFraction,
  chebyPartSlope,
  chebyFractionMax,
  findChebyshev,
) where

import Chebyshev.Base
import Control.Monad.State
import Data.ExtendedReal
import Data.Maybe
import Data.MemoTrie
import Data.Monoid (First (..))
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
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
  maxCandidate :: Word -> Extended Rational
  maxCandidate k = abs . chebyFraction $ nexts (n_1 : V.toList argmax_R) (initChebyNormal u2)
   where
    Arg _ argmax_R = computeMax (k - 1)
    s_R_rev = nexts (V.toList $ V.reverse argmax_R) (initChebyNormal u2)
    g_R = knownFinite $ chebyPartSlope u2 s_R_rev
    n_1 = case properFraction g_R of
      (btwn, r)
        | r < 0 -> btwn - 1
        | r > 0 -> btwn + 1
        | otherwise -> btwn

  computeMax :: Word -> Arg (Extended Rational) (V.Vector Integer)
  computeMax = memo $ \case
    0 -> Arg 0 V.empty -- s0 / s1 = 0
    1 -> Arg 1 (V.singleton 1) -- s1 / s2 = 1
    k -> evalState maxFinding (maxCandidate k)
     where
      -- Setup: F(x) = F(x_L, x_i, x_R), |x| = k, |x_L| = i-1, |x_R| = k-i
      boundMultiple i curMax =
        knownFinite
          $ (1 + getMax (k - i - 1) / curMax)
          / (1 - getMax (k - i) / curMax)

      maxRadiusN_i i maxG_L maxG_R curMax =
        if i < k
          then maxG_R * boundMultiple i curMax
          else maxG_L * knownFinite ((1 / curMax) / (1 - 1 / curMax))

      chooseN_i ::
        InductiveEval Integer Rational ->
        Word ->
        Stream.SerialT (State (Extended Rational)) (InductiveEval Integer Rational)
      chooseN_i s_L i = do
        let knownG_L = knownFinite $ chebyPartSlope u2 s_L
        let maxG_R = knownFinite (getMax (k - i)) / abs u2
        maxRadius <- Stream.fromEffect $ gets (maxRadiusN_i i (abs knownG_L) maxG_R)
        let (minBnd, maxBnd) = (ceiling $ knownG_L - maxRadius, floor $ knownG_L + maxRadius)
        -- traceShowM (k, i, knownG_L, maxG_R, maxRadius, curMax)
        n_i <- Stream.filter (/= 0) $ Stream.enumerateFromTo minBnd maxBnd
        pure (next n_i s_L)

      -- TODO Exit when current maximum is infinite
      maxFinding :: State (Extended Rational) (Arg (Extended Rational) (V.Vector Integer))
      maxFinding = fmap fromJust . Stream.last $ do
        curMaxArg@(Arg curMax _) <- Stream.scanl1' max $ do
          s_k1 <- foldM chooseN_i (initChebyNormal u2) [1 .. k]
          let c_k = chebyFraction s_k1
          pure $ Arg (abs <$> c_k) (V.fromList $ inputs s_k1)

        Stream.fromEffect (put curMax)
        pure curMaxArg

-- >>> findChebyshev (7/3) 8
-- Just [-3,-1,-1,-1,-3]

-- >>> findChebyshev (8/3) 8
-- Just [-6,-1,-1,-1,-1]

-- | Given a root, find a chebyshev polynomial of minimal degree.
findChebyshev :: Rational -> Word -> Maybe (V.Vector Integer)
findChebyshev u2 cutoff = getFirst $ foldMap (First . argForInfinite) [2 .. cutoff]
 where
  getMax = chebyFractionMax u2
  argForInfinite k = case getMax k of
    Arg m arg | Data.ExtendedReal.isInfinite m -> Just arg
    _ -> Nothing
