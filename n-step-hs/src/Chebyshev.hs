{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Chebyshev (
  chebyNormal,
  constTerm,
  slopeTerm,
  chebyNormalMin,
  boundaryMinAt,
  findMinimalChebyshev,
) where

import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import Data.MemoTrie
import Data.Ratio
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Streamly.Prelude qualified as Stream
import Util

instance (HasTrie a, Integral a) => HasTrie (Ratio a) where
  data Ratio a :->: b = RatioTrie (a :->: (a :->: b))
  trie :: (HasTrie a, Integral a) => (Ratio a -> b) -> Ratio a :->: b
  trie f = RatioTrie $ trie $ \x -> trie $ \y -> f (x % y)
  untrie :: (HasTrie a, Integral a) => (Ratio a :->: b) -> Ratio a -> b
  untrie (RatioTrie t) n = untrie (untrie t (numerator n)) (denominator n)
  enumerate :: (HasTrie a, Integral a) => (Ratio a :->: b) -> [(Ratio a, b)]
  enumerate (RatioTrie tt) = [(x % y, z) | (x, t) <- enumerate tt, (y, z) <- enumerate t]

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

-- | Constant term in the normalized chebyshev.
constTerm :: Rational -> V.Vector Integer -> V.Vector Integer -> Rational
constTerm u2 n_L n_R = chebyNormal u2 n_L * chebyNormal u2 n_R

-- | (Negative) slope term in the normalized chebyshev.
slopeTerm :: Rational -> V.Vector Integer -> V.Vector Integer -> Rational
slopeTerm u2 n_L n_R = (value s_L * s_R_1_part + s_L_1_part * value s_R) / u2
 where
  s_L = nexts (V.toList n_L) (initChebyNormal u2)
  s_R = nexts (V.toList $ V.reverse n_R) (initChebyNormal u2) -- Reversed to remove "initial" input
  s_L_1_part = case previous s_L of
    Nothing -> 0
    Just (n_i_1, s_n_L_1) -> value s_n_L_1 / fromIntegral n_i_1
  s_R_1_part = case previous s_R of
    Nothing -> 0
    Just (n_i1, s_n_R_1) -> value s_n_R_1 / fromIntegral n_i1

-- | Minimum of constant term where left portion is decided.
constMinAt :: Rational -> Int -> InductiveEval a Rational -> Int -> Rational
constMinAt u2 k s_L i = value s_L * s_R_min
 where
  Arg s_R_min _ = chebyNormalMin u2 (k - i)

-- | Upper bound of slope term where left portion is decided.
slopeUBAt :: Rational -> Int -> InductiveEval Integer Rational -> Int -> Rational
slopeUBAt u2 k s_L i = (leftBiased + rightBiased) / abs u2
 where
  s_L_1_part = case previous s_L of
    Nothing -> 0
    Just (n_i_1, s_L_1) -> value s_L_1 / fromIntegral n_i_1
  leftBiased = abs (value s_L) * chebyNormalUB u2 (k - i - 1)
  rightBiased = abs s_L_1_part * chebyNormalUB u2 (k - i)

-- Assumption: u^2 is rational

-- >>> chebyNormalMin (1/2) 3
-- Arg (0 % 1) [2,1]

-- >>> chebyNormalMin (2/3) 3
-- Arg (1 % 4) [2,1]

-- >>> chebyNormalMin (2/3) 4
-- Arg (0 % 1) [3,2,1]

-- >>> chebyNormalMin (4/5) 4
-- Arg (0 % 1) [-5,1,1]

alternating :: (Monad m, Num a, Stream.Enumerable a) => Stream.SerialT m a
alternating = (*) <$> Stream.enumerateFrom 1 <*> Stream.fromList [1, -1]

-- TODO Profiling
-- TODO Try parallelization

-- | Minimum value of normalized chebyshev, along with the ns for the minimum.
chebyNormalMin :: Rational -> Int -> Arg Rational (V.Vector Integer)
chebyNormalMin = memo2 $ \u2 -> \case
  1 -> Arg 1 V.empty -- normalized s1 = 1
  2 -> Arg 1 (V.singleton 1) -- normalized s2 = 1
  k -> fromJust (evalState minFinding initMin)
   where
    constArgMins = boundaryMinAt u2 k <$> V.enumFromTo 1 (k - 1)
    Arg _ bndArg = minimum constArgMins
    initMinArg@(Arg initMin _) = initialMinCand u2 bndArg

    boundAt :: InductiveEval Integer Rational -> Int -> Rational -> Integer
    boundAt s_L i curMin = floor $ slopeUBAt u2 k s_L i / (constMinAt u2 k s_L i - curMin)

    chooseNi ::
      InductiveEval Integer Rational -> Int -> Stream.SerialT (State Rational) (InductiveEval Integer Rational)
    chooseNi s_L i = do
      bound <- Stream.fromEffect $ gets (boundAt s_L i)
      ni <- Stream.takeWhile (\n_i -> abs n_i <= bound) alternating
      pure (next ni s_L)

    minFinding :: State Rational (Maybe (Arg Rational (V.Vector Integer)))
    minFinding = Stream.last $ do
      curMinArg@(Arg curMin _) <- Stream.scanl' min initMinArg $ do
        s_k <- foldM chooseNi (initChebyNormal u2) [1 .. k - 1]
        pure $ Arg (abs $ value s_k) (V.fromList $ inputs s_k)
      curMinArg <$ Stream.fromEffect (put curMin)

-- | Upper bound of normalized chebyshev.
chebyNormalUB :: Rational -> Int -> Rational
chebyNormalUB = memo2 $ \u2 -> \case
  0 -> 0
  1 -> 1
  k -> chebyNormalUB u2 (k - 1) + chebyNormalUB u2 (k - 2) / abs u2

-- | minimum of |s_i| |s_{k-i}| for fixed i.
boundaryMinAt :: Rational -> Int -> Int -> Arg Rational (V.Vector Integer, V.Vector Integer)
boundaryMinAt u2 k i = Arg (lv * rv) (left, right)
 where
  Arg lv left = chebyNormalMin u2 i
  Arg rv right = chebyNormalMin u2 (k - i)

-- | Initial minimum candidate derived from the boundary minimum.
initialMinCand :: Rational -> (V.Vector Integer, V.Vector Integer) -> Arg Rational (V.Vector Integer)
initialMinCand u2 (n_L, n_R) = Arg (abs minCand) minN_
 where
  iConst = constTerm u2 n_L n_R
  iSlope = slopeTerm u2 n_L n_R
  n_i = closestToInv (iConst / iSlope)

  minN_ = n_L <> V.singleton n_i <> n_R
  minCand = iConst - iSlope / fromIntegral n_i

-- >>> findMinimalChebyshev (1/2) 8
-- Just [2,1]

-- >>> findMinimalChebyshev (2/3) 8
-- Just [3,2,1]

-- >>> findMinimalChebyshev 3 8
-- Just [1,1,1,1,1]

-- >>> chebyNormalMin (10/3) 8
-- Arg (1 % 400) [2,1,1,1,1,1,1]

-- >>> findMinimalChebyshev (7/3) 8
-- Just [3,1,1,1,3]

-- >>> findMinimalChebyshev (4/5) 8
-- Just [-5,1,1]

-- >>> findMinimalChebyshev (8/3) 8
-- Just [6,1,1,1,1]

findMinimalChebyshev :: Rational -> Int -> Maybe (V.Vector Integer)
findMinimalChebyshev u2 cutoff = case mayFound of
  Nothing -> Nothing
  Just (Arg _ n_) -> Just n_
 where
  mayFound = find (== Arg 0 undefined) $ getMin <$> [1 .. cutoff]
  getMin = chebyNormalMin u2
