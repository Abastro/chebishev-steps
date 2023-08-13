{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Chebyshev (
  chebyNormal,
  constTerm,
  slopeTerm,
  slopeTermUB,
  sPolyMinimum,
  boundaryMinAt,
  findMinimalChebyshev,
) where

import Control.Monad.State.Strict
import Data.IntSet qualified as IS
import Data.List
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.MemoTrie
import Data.Ratio
import Data.Semigroup (Arg (..))
import Data.Traversable
import Data.Vector qualified as V
import Inductive
import MultilinPoly
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

-- | Continuant as a polynomial.
--
-- >>> continuant 3
-- MultilinPoly (fromList [(fromList [],-1),(fromList [1,2],1)])
--
-- >>> continuant 4
-- MultilinPoly (fromList [(fromList [1],-1),(fromList [1,2,3],1),(fromList [3],-1)])
continuant :: Int -> MultilinPoly Int
continuant = memo $ \case
  1 -> single IS.empty
  2 -> single (IS.singleton 1)
  k -> multVar (k - 1) (continuant (k - 1)) <> scale (-1) (continuant (k - 2))

-- | Normalized chebyshev polynomial.
--
-- >>> chebyNormal (1/3) (V.fromList [1, 2, 3])
-- (-1) % 1
--
-- >>> chebyNormal 1 (V.fromList [1, 2, 2])
-- 1 % 4
chebyNormal :: Rational -> V.Vector Integer -> Rational
chebyNormal u2 n_ = value $ nexts n_ (initChebyNormal u2)

-- | Constant term in the normalized chebyshev polynomial.
constTerm :: Rational -> V.Vector Integer -> V.Vector Integer -> Rational
constTerm u2 n_L n_R = chebyNormal u2 n_L * chebyNormal u2 n_R

-- | (Negative) slope term in the normalized chebyshev polynomial.
slopeTerm :: Rational -> V.Vector Integer -> V.Vector Integer -> Rational
slopeTerm u2 n_L n_R = (value s_L * s_R_1_part + s_L_1_part * value s_R) / u2
 where
  s_L = nexts n_L (initChebyNormal u2)
  s_R = nexts (V.reverse n_R) (initChebyNormal u2) -- Reversed to remove "initial" input
  s_L_1_part = case previous s_L of
    Nothing -> 0
    Just (n_i_1, s_n_L_1) -> value s_n_L_1 / fromIntegral n_i_1
  s_R_1_part = case previous s_R of
    Nothing -> 0
    Just (n_i1, s_n_R_1) -> value s_n_R_1 / fromIntegral n_i1

-- | An upper bound of the slope term of chebyshev polynomial.
--
-- >>> slopeTermUB (1/3) 4 1
-- 3 % 1
--
-- >>> slopeTermUB (4/5) 4 <$> [1..3]
-- [5 % 4,5 % 2,5 % 4]
slopeTermUB :: Rational -> Int -> Int -> Rational
slopeTermUB u2 k i = absSum
 where
  absSum = sum [1 / abs u2 ^ (exc `div` 2) | monomial <- M.keys monos, let exc = k - 1 - IS.size monomial]
  MultilinPoly monos = substZero i $ continuant k

-- Assumption: u^2 is rational

-- >>> sPolyMinimum (1/2) 3
-- Arg (0 % 1) [2,1]

-- >>> sPolyMinimum (2/3) 3
-- Arg (1 % 4) [2,1]

-- >>> sPolyMinimum (2/3) 4
-- Arg (0 % 1) [3,2,1]

-- >>> sPolyMinimum (4/5) 4
-- Arg (0 % 1) [-5,1,1]

nsToArg :: Rational -> V.Vector Integer -> Arg Rational (V.Vector Integer)
nsToArg u2 ns = Arg (abs $ chebyNormal u2 ns) ns

alternating :: (Monad m, Num a, Stream.Enumerable a) => Stream.SerialT m a
alternating = do
  ni <- Stream.enumerateFrom 1
  sign <- Stream.fromList [1, -1]
  pure (sign * ni)

--  * Pros: It works.
--  * Cons: Horribly slow.

-- | Minimum value of normalized chebyshev polynomial, along with the ns for the minimum.
sPolyMinimum :: Rational -> Int -> Arg Rational (V.Vector Integer)
sPolyMinimum = memo2 $ \u2 -> \case
  1 -> Arg 1 V.empty -- normalized s1 = 1
  2 -> Arg 1 (V.singleton 1) -- normalized s2 = 1
  k -> fromJust (evalState minFinding initMin)
   where
    constArgMins = boundaryMinAt u2 k <$> V.enumFromTo 1 (k - 1)
    constMins = (\(Arg r _) -> r) <$> constArgMins
    -- Max. of normalized s_k where n_i = 0, i = 1 to k-1.
    slopeMaxs = slopeTermUB u2 k <$> V.enumFromTo 1 (k - 1)

    Arg _ bndArg = minimum constArgMins
    initMinArg@(Arg initMin _) = initialMinCand u2 bndArg

    boundAt i curMin = floor $ (slopeMaxs V.! pred i) / (constMins V.! pred i - curMin)
    boundCond i ni = do
      bound <- gets (boundAt i)
      -- curMin <- get
      -- when (ni == 1) $ traceShowM (curMin, i, bound)
      pure (abs ni <= bound)

    chooseNs :: Stream.SerialT (State Rational) (V.Vector Integer)
    chooseNs = for (V.enumFromTo 1 (k - 1)) $ \i -> do
      Stream.takeWhileM (boundCond i) alternating

    minFinding :: State Rational (Maybe (Arg Rational (V.Vector Integer)))
    minFinding = Stream.last $ do
      curMinArg@(Arg curMin _) <- Stream.scanl' min initMinArg $ do
        nsToArg u2 <$> chooseNs
      curMinArg <$ put curMin

-- | minimum of |s_i| |s_{k-i}| for fixed i.
boundaryMinAt :: Rational -> Int -> Int -> Arg Rational (V.Vector Integer, V.Vector Integer)
boundaryMinAt u2 k i = Arg (lv * rv) (left, right)
 where
  Arg lv left = sPolyMinimum u2 i
  Arg rv right = sPolyMinimum u2 (k - i)

-- | Initial minimum candidate derived from the boundary minimum.
initialMinCand :: Rational -> (V.Vector Integer, V.Vector Integer) -> Arg Rational (V.Vector Integer)
initialMinCand u2 (left, right) = nsToArg u2 minNs
 where
  xiConst = chebyNormal u2 left * chebyNormal u2 right
  xiSlope = slopeTerm u2 left right
  ni = closestToInv (xiConst / xiSlope)
  minNs = left <> V.singleton ni <> right

-- >>> findMinimalChebyshev (1/2)
-- [2,1]

-- >>> findMinimalChebyshev (2/3)
-- [3,2,1]

-- >>> findMinimalChebyshev 3

-- >>> findMinimalChebyshev (7/3)

-- >>> findMinimalChebyshev (4/5)
-- [-5,1,1]

-- >>> findMinimalChebyshev (8/3)
-- [6,1,1,1,1]

findMinimalChebyshev :: Rational -> V.Vector Integer
findMinimalChebyshev u2 = found
 where
  Arg _ found = fromJust $ find (== Arg 0 undefined) $ getMin <$> [1 ..]
  getMin = sPolyMinimum u2
