{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Properties of minimal chebyshev polynomial in terms of linearity in each coefficient.
module Chebyshev.Linear (
  constTerm,
  slopeTerm,
  chebyNormalMin,
  findChebyshev,
) where

import Chebyshev.Base
import Control.Monad.State.Strict
import Data.Maybe
import Data.MemoTrie
import Data.Monoid (First (..))
import Data.Ratio
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Streamly.Internal.Data.Stream.StreamK qualified as StreamK
import Util

instance (HasTrie a, Integral a) => HasTrie (Ratio a) where
  data Ratio a :->: b = RatioTrie (a :->: (a :->: b))
  trie :: (HasTrie a, Integral a) => (Ratio a -> b) -> Ratio a :->: b
  trie f = RatioTrie $ trie $ \x -> trie $ \y -> f (x % y)
  untrie :: (HasTrie a, Integral a) => (Ratio a :->: b) -> Ratio a -> b
  untrie (RatioTrie t) n = untrie (untrie t (numerator n)) (denominator n)
  enumerate :: (HasTrie a, Integral a) => (Ratio a :->: b) -> [(Ratio a, b)]
  enumerate (RatioTrie tt) = [(x % y, z) | (x, t) <- enumerate tt, (y, z) <- enumerate t]

-- | Constant term in the normalized chebyshev.
constTerm :: Rational -> V.Vector Integer -> V.Vector Integer -> Rational
constTerm u2 n_L n_R = chebyNormal u2 (V.toList n_L) * chebyNormal u2 (V.toList n_R)

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

-- Assumption: u^2 is rational

-- >>> chebyNormalMin (1/2) 3
-- Arg (0 % 1) [2,1]

-- >>> chebyNormalMin (2/3) 3
-- Arg (1 % 4) [2,1]

-- >>> chebyNormalMin (2/3) 4
-- Arg (0 % 1) [3,2,1]

-- >>> chebyNormalMin (4/5) 4
-- Arg (0 % 1) [-5,1,1]

-- Bottleneck 1 (?)
alternatingTo :: (Monad m, Eq a, Num a, Stream.Enumerable a) => a -> Stream.Stream m a
alternatingTo bnd = Stream.filter (/= 0) $ Stream.enumerateFromTo (-bnd) bnd

-- | Minimum value of normalized chebyshev, along with the ns for the minimum.
chebyNormalMin :: Rational -> Int -> Arg Rational (V.Vector Integer)
chebyNormalMin u2 = computeMin
 where
  computeMin = memo $ \case
    1 -> Arg 1 V.empty -- normalized s1 = 1
    2 -> Arg 1 (V.singleton 1) -- normalized s2 = 1
    k -> fromJust (evalState minFinding initMin)
     where
      constArgMins = boundaryMinAt k <$> V.enumFromTo 1 (k - 1)
      Arg _ bndArg = minimum constArgMins
      initMinArg@(Arg initMin _) = initialMinCand u2 bndArg

      -- Bottleneck 2
      boundAt :: InductiveEval Integer Rational -> Int -> Rational -> Integer
      boundAt s_L i curMin = floor $ slopeUBAt k s_L i / (constMinAt k s_L i - curMin)

      chooseNi ::
        InductiveEval Integer Rational -> Int -> StreamK.CrossStreamK (State Rational) (InductiveEval Integer Rational)
      chooseNi s_L i = do
        bound <- StreamK.mkCross . StreamK.fromEffect $ gets (boundAt s_L i)
        n_i <- StreamK.mkCross . StreamK.fromStream $ alternatingTo bound
        pure (next n_i s_L)

      withArgPut argValue@(Arg val _) = argValue <$ put val
      puttingMin oldMin new = if oldMin > new then withArgPut new else pure oldMin

      minFinding :: State Rational (Maybe (Arg Rational (V.Vector Integer)))
      minFinding = Stream.fold Fold.latest $ do
        Stream.scan (Fold.foldlM' puttingMin $ withArgPut initMinArg) . StreamK.toStream . StreamK.unCross $ do
          s_k <- foldM chooseNi (initChebyNormal u2) [1 .. k - 1]
          pure $ Arg (abs $ value s_k) (V.fromList $ inputs s_k)

  -- Minimum of constant term where left portion is decided.
  constMinAt :: Int -> InductiveEval a Rational -> Int -> Rational
  constMinAt k s_L i = abs (value s_L) * s_R_min
   where
    Arg s_R_min _ = computeMin (k - i)

  -- Upper bound of slope term where left portion is decided.
  slopeUBAt :: Int -> InductiveEval Integer Rational -> Int -> Rational
  slopeUBAt k s_L i = (leftBiased + rightBiased) / abs u2
   where
    s_L_1_part = case previous s_L of
      Nothing -> 0
      Just (n_i_1, s_L_1) -> value s_L_1 / fromIntegral n_i_1
    leftBiased = abs (value s_L) * chebyNormalUB (k - i - 1)
    rightBiased = abs s_L_1_part * chebyNormalUB (k - i)

  -- Upper bound of normalized chebyshev.
  chebyNormalUB :: Int -> Rational
  chebyNormalUB = memo $ \case
    0 -> 0
    1 -> 1
    k -> chebyNormalUB (k - 1) + chebyNormalUB (k - 2) / abs u2

  -- Minimum of |s_i| |s_{k-i}| for fixed i.
  boundaryMinAt :: Int -> Int -> Arg Rational (V.Vector Integer, V.Vector Integer)
  boundaryMinAt k i = Arg (lv * rv) (left, right)
   where
    Arg lv left = computeMin i
    Arg rv right = computeMin (k - i)

-- | Initial minimum candidate derived from the boundary minimum.
initialMinCand :: Rational -> (V.Vector Integer, V.Vector Integer) -> Arg Rational (V.Vector Integer)
initialMinCand u2 (n_L, n_R) = Arg (abs minCand) minN_
 where
  iConst = constTerm u2 n_L n_R
  iSlope = slopeTerm u2 n_L n_R
  n_i = closestToInv (iConst / iSlope)

  minN_ = n_L <> V.singleton n_i <> n_R
  minCand = iConst - iSlope / fromIntegral n_i

-- >>> findChebyshev (1/2) 8
-- Just [2,1]

-- >>> findChebyshev (2/3) 8
-- Just [3,2,1]

-- >>> findChebyshev 3 8
-- Just [1,1,1,1,1]

-- >>> chebyNormalMin (10/3) 8
-- Arg (1 % 400) [2,1,1,1,1,1,1]

-- >>> findChebyshev (7/3) 8
-- Just [3,1,1,1,3]

-- >>> findChebyshev (4/5) 8
-- Just [-5,1,1]

-- >>> findChebyshev (8/3) 8
-- Just [6,1,1,1,1]

findChebyshev :: Rational -> Word -> Maybe (V.Vector Integer)
findChebyshev u2 cutoff = getFirst $ foldMap (First . argForZero) [3 .. fromIntegral cutoff]
 where
  getMin = chebyNormalMin u2
  argForZero k = case getMin k of
    Arg 0 arg -> Just arg
    _ -> Nothing
