-- | Properties of minimal chebyshev polynomial in terms of linearity in each coefficient.
module Chebyshev.Linear (
  constTerm,
  slopeTerm,
  chebyNormalMin,
  chebyZero,
) where

import Chebyshev.Base
import Data.Function ((&))
import Data.MemoTrie
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream

-- | Constant term in the normalized chebyshev.
constTerm :: Rational -> V.Vector Integer -> V.Vector Integer -> Rational
constTerm u2 n_L n_R = chebyNormal u2 (V.toList n_L) * chebyNormal u2 (V.toList n_R)

-- | (Negative) slope term in the normalized chebyshev.
slopeTerm :: Rational -> V.Vector Integer -> V.Vector Integer -> Rational
slopeTerm u2 n_L n_R = (s_L.value * s_R_1_part + s_L_1_part * s_R.value) / u2
 where
  s_L = nexts (initChebyNormal u2) (V.toList n_L)
  s_R = nexts (initChebyNormal u2) (V.toList $ V.reverse n_R)  -- Reversed to remove "initial" input
  s_L_1_part = case s_L.previous of
    Nothing -> 0
    Just (n_i_1, s_n_L_1) -> s_n_L_1.value / fromIntegral n_i_1
  s_R_1_part = case s_R.previous of
    Nothing -> 0
    Just (n_i1, s_n_R_1) -> s_n_R_1.value / fromIntegral n_i1

-- Assumption: u^2 is rational

chebyNormalMin :: Rational -> Int -> RatioResult
chebyNormalMin u2 = memoFix $ \getMin -> \case
  0 -> Arg 0 V.empty -- (placeholder)
  1 -> Arg 1 V.empty -- normalized s1 = 1
  2 -> Arg 1 (V.singleton 1) -- normalized s2 = 1
  k -> searchMinWith (chebyNormalMinSearch u2 getMin) (k - 1)

chebyNormalMinSearch :: Rational -> (Int -> RatioResult) -> MinSearch Rational
chebyNormalMinSearch u2 getMin =
  MinSearch
    { computeInd = chebyNormalInd u2,
      minA,
      computeB = slopeTerm u2,
      minAwith,
      maxBwith,
      size = abs
    }
 where
  minA k_1 i =
    let Arg s_Lmin n_L = getMin i
        Arg s_Rmin n_R = getMin (k_1 + 1 - i)
     in Arg (s_Lmin * s_Rmin) (n_L, n_R)

  -- A = constTerm
  minAwith s_L k_1 =
    let i_1 = inductNum s_L
        Arg s_Rmin _ = getMin (k_1 - i_1)
     in abs s_L.value * s_Rmin

  -- B = slopeTerm / constTerm
  maxBwith s_L k_1 = slopeUBAt s_L (k_1 + 1) (inductNum s_L + 1) / minAwith s_L k_1

  -- Upper bound of slope term where left portion is decided.
  slopeUBAt :: IntFnInd Rational -> Int -> Int -> Rational
  slopeUBAt s_L k i = (leftBiased + rightBiased) / abs u2
   where
    s_L_1_part = case s_L.previous of
      Nothing -> 0
      Just (n_i_1, s_L_1) -> s_L_1.value / fromIntegral n_i_1
    leftBiased = abs s_L.value * chebyNormalUB (k - i - 1)
    rightBiased = abs s_L_1_part * chebyNormalUB (k - i)

  -- Upper bound of normalized chebyshev.
  chebyNormalUB :: Int -> Rational
  chebyNormalUB = memo $ \case
    0 -> 0
    1 -> 1
    k -> chebyNormalUB (k - 1) + chebyNormalUB (k - 2) / abs u2

-- Stops when 0 is encountered
untilZero :: (Monad m, Eq r, Num r) => Fold.Fold m (Arg r b) (Maybe (Arg r b))
untilZero = Fold.takeEndBy (\(Arg curMin _) -> curMin == 0) Fold.latest

-- | Determines if u2 is a s_k's zero.
chebyZero :: (Monad m) => Rational -> Stream.Stream m (Either Int (V.Vector Integer))
chebyZero u2 =
  Stream.enumerateFrom 1
    & fmap getMin
    & Stream.scanMaybe untilZero
    & Stream.indexed
    & fmap argZero
 where
  getMin = chebyNormalMin u2
  argZero = \case
    (_, Arg m arg) | m == 0 -> Right arg
    (k_1, _) -> Left (k_1 + 1)
