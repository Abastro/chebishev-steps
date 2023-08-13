{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Chebyshev where

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

-- | Compute "normalized" value with u2 involved.
u2Normalized :: Rational -> Int -> IS.IntSet -> (Var -> Int) -> MultilinPoly Int -> Rational
u2Normalized u2 k allVars subst = evalWith evalMonomial . fmap fromIntegral
 where
  evalMonomial monomial =
    product [1 / fromIntegral (subst i) | i <- IS.toList (allVars `IS.difference` monomial)]
      / u2
      ^ ((k - 1 - IS.size monomial) `div` 2)

-- Issue: sPolyNormal is O(F_k) now.

-- | Normalized chebyshev polynomial.
--
-- >>> sPolyNormal (1/3) (V.fromList [1, 2, 3])
-- (-1) % 1
--
-- >>> sPolyNormal 1 (V.fromList [1, 2, 2])
-- 1 % 4
sPolyNormal :: Rational -> V.Vector Int -> Rational
sPolyNormal u2 ns = u2Normalized u2 k (IS.fromList [1 .. k - 1]) (\j -> ns V.! pred j) (continuant k)
 where
  k = V.length ns + 1

-- | "Normalized" chebyshev polynomial when n_i = 0.
sPolyZeroNormal :: Rational -> V.Vector Int -> Int -> Rational
sPolyZeroNormal u2 ns i = u2Normalized u2 k vars (\j -> ns V.! pred j) (substZero i $ continuant k)
 where
  k = V.length ns + 1
  vars = IS.delete i $ IS.fromList [1 .. k - 1]

-- | An upper bound of "normalized" chebyshev polynomial when n_i = 0.
--
-- >>> sPolyZeroUB (1/3) 4 1
-- 3 % 1
--
-- >>> sPolyZeroUB (4/5) 4 <$> [1..3]
-- [5 % 4,5 % 2,5 % 4]
sPolyZeroUB :: Rational -> Int -> Int -> Rational
sPolyZeroUB u2 k i = absSum
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

nsToArg :: Rational -> V.Vector Int -> Arg Rational (V.Vector Int)
nsToArg u2 ns = Arg (abs $ sPolyNormal u2 ns) ns

alternating :: (Monad m) => Stream.SerialT m Int
alternating = do
  ni <- Stream.enumerateFrom 1
  sign <- Stream.fromList [1, -1]
  pure (sign * ni)

--  * Pros: It works.
--  * Cons: Horribly slow.

-- | Minimum value of normalized chebyshev polynomial, along with the ns for the minimum.
sPolyMinimum :: Rational -> Int -> Arg Rational (V.Vector Int)
sPolyMinimum = memo2 $ \u2 -> \case
  1 -> Arg 1 V.empty -- normalized s1 = 1
  2 -> Arg 1 (V.singleton 1) -- normalized s2 = 1
  k -> fromJust (evalState minFinding initMin)
   where
    constArgMins = boundaryMinAt u2 k <$> V.enumFromTo 1 (k - 1)
    constMins = (\(Arg r _) -> r) <$> constArgMins
    -- Max. of normalized s_k where n_i = 0, i = 1 to k-1.
    slopeMaxs = sPolyZeroUB u2 k <$> V.enumFromTo 1 (k - 1)

    Arg _ bndArg = minimum constArgMins
    initMinArg@(Arg initMin _) = initialMinCand u2 bndArg

    boundAt i curMin = floor $ (slopeMaxs V.! pred i) / (constMins V.! pred i - curMin)
    boundCond i ni = do
      bound <- gets (boundAt i)
      -- curMin <- get
      -- when (ni == 1) $ traceShowM (curMin, i, bound)
      pure (abs ni <= bound)

    chooseNs :: Stream.SerialT (State Rational) (V.Vector Int)
    chooseNs = for (V.enumFromTo 1 (k - 1)) $ \i -> do
      Stream.takeWhileM (boundCond i) alternating

    minFinding :: State Rational (Maybe (Arg Rational (V.Vector Int)))
    minFinding = Stream.last $ do
      curMinArg@(Arg curMin _) <- Stream.scanl' min initMinArg $ do
        nsToArg u2 <$> chooseNs
      curMinArg <$ put curMin

-- | minimum of |s_i| |s_{k-i}| for fixed i.
boundaryMinAt :: Rational -> Int -> Int -> Arg Rational (V.Vector Int, V.Vector Int)
boundaryMinAt u2 k i = Arg (lv * rv) (left, right)
 where
  Arg lv left = sPolyMinimum u2 i
  Arg rv right = sPolyMinimum u2 (k - i)

-- | Initial minimum candidate derived from the boundary minimum.
initialMinCand :: Rational -> (V.Vector Int, V.Vector Int) -> Arg Rational (V.Vector Int)
initialMinCand u2 (left, right) = nsToArg u2 minNs
 where
  xiConst = sPolyNormal u2 left * sPolyNormal u2 right
  xiSlope = sPolyZeroNormal u2 (left <> V.singleton 0 <> right) (V.length left + 1)
  ni = closestToInv (-xiConst / xiSlope)
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

findMinimalChebyshev :: Rational -> V.Vector Int
findMinimalChebyshev u2 = found
 where
  Arg _ found = fromJust $ find (== Arg 0 undefined) $ getMin <$> [1 ..]
  getMin = sPolyMinimum u2
