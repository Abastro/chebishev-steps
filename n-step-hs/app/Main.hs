{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.State.Strict
import Data.Bifunctor (Bifunctor (..))
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.MemoTrie
import Data.Ratio
import Data.Semigroup (Arg (..))
import Data.Traversable
import Data.Vector qualified as V
import Streamly.Prelude qualified as Stream
import Data.List

safeDiv :: (Eq a, Fractional a) => a -> a -> [a]
safeDiv n m = [n / m | m /= 0]

type Var = Int

-- | Polynomial that is at most linear in each variable.
--
-- Each variable is denoted by a number.
newtype MultilinPoly n = MultilinPoly (M.Map IS.IntSet n)
  deriving (Show, Functor)

instance (Num n) => Semigroup (MultilinPoly n) where
  (<>) :: (Num n) => MultilinPoly n -> MultilinPoly n -> MultilinPoly n
  MultilinPoly poly <> MultilinPoly poly' = MultilinPoly (M.unionWith (+) poly poly')
instance (Num n) => Monoid (MultilinPoly n) where
  mempty :: (Num n) => MultilinPoly n
  mempty = MultilinPoly M.empty

single :: (Num n) => IS.IntSet -> MultilinPoly n
single monomial = MultilinPoly (M.singleton monomial 1)

scale :: (Num n) => n -> MultilinPoly n -> MultilinPoly n
scale s poly = (s *) <$> poly

-- | Multiply by a new variable.
multVar :: Var -> MultilinPoly n -> MultilinPoly n
multVar var (MultilinPoly poly) = MultilinPoly $ M.mapKeys (IS.insert var) poly

-- | Substitute zero to certain variable.
substZero :: (Num n) => Var -> MultilinPoly n -> MultilinPoly n
substZero var (MultilinPoly poly) =
  MultilinPoly $ M.filterWithKey (\monomial _ -> var `IS.notMember` monomial) poly

evalWith :: (Num n) => (IS.IntSet -> n) -> MultilinPoly n -> n
evalWith evalMono (MultilinPoly poly) = sum $ uncurry (*) . first evalMono <$> M.toList poly

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
      / u2 ^ ((k - 1 - IS.size monomial) `div` 2)

-- | Normalized chebyshev polynomial.
--
-- >>> sPolyNormal (1 / 3) (V.fromList [1, 2, 3])
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
-- >>> sPolyZeroUB (1 / 3) 4 1
-- 3 % 1
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

-- >>> sPolyMinimum 3 6
-- Arg (0 % 1) [1,1,1,1,1]

-- >>> sPolyNormal 3 (V.fromList [1, 1, 1, 1, 1])
-- 0 % 1

nsToArg :: Rational -> V.Vector Int -> Arg Rational (V.Vector Int)
nsToArg u2 ns = Arg (abs $ sPolyNormal u2 ns) ns

-- | Minimum value of normalized chebyshev polynomial, along with the ns for the minimum.
sPolyMinimum :: Rational -> Int -> Arg Rational (V.Vector Int)
sPolyMinimum u2 = computeMinimum
 where
  computeMinimum :: Int -> Arg Rational (V.Vector Int)
  computeMinimum = memo $ \case
    1 -> Arg 1 V.empty -- normalized s1 = 1
    2 -> Arg 1 (V.singleton 1) -- normalized s2 = 1
    k -> fromJust (evalState minFinding initMin)
     where
      bndMinArg@(Arg bndMin _) = boundaryMinimum k
      initMinArg@(Arg initMin _) = initialMinCand u2 bndMinArg
      -- Max. of normalized s_k where n_i = 0, i = 1 to k-1.
      sZeroMaxes = sPolyZeroUB u2 k <$> V.enumFromTo 1 (k - 1)

      boundAt i curMin = floor $ (bndMin - curMin) / (sZeroMaxes V.! pred i)
      boundCond i ni = do
        bound <- gets (boundAt i)
        pure (ni <= bound)

      chooseNs :: Stream.SerialT (State Rational) (V.Vector Int)
      chooseNs = for (V.enumFromTo 1 (k - 1)) $ \i -> do
        Stream.takeWhileM (boundCond i) $ do
          ni <- Stream.enumerateFrom 1
          sign <- Stream.fromList [-1, 1]
          pure (sign * ni)

      minFinding :: State Rational (Maybe (Arg Rational (V.Vector Int)))
      minFinding = Stream.last $ do
        curMinArg@(Arg curMin _) <- Stream.scanl' min initMinArg $ do
          nsToArg u2 <$> chooseNs
        curMinArg <$ put curMin

  -- min_i |s_i| |s_{k-i}|, with the args
  boundaryMinimum :: Int -> Arg Rational (V.Vector Int, V.Vector Int)
  boundaryMinimum k = minimum $ do
    i <- [1 .. k `div` 2]
    let Arg lv left = computeMinimum i
        Arg rv right = computeMinimum (k - i)
    pure $ Arg (lv * rv) (left, right)

-- | Initial minimum candidate derived from the boundary-minimums.
initialMinCand :: Rational -> Arg Rational (V.Vector Int, V.Vector Int) -> Arg Rational (V.Vector Int)
initialMinCand u2 (Arg xiConst (left, right)) = nsToArg u2 minNs
 where
  xiSlope = sPolyZeroNormal u2 (left <> V.singleton 0 <> right) (V.length left + 1)
  ni = closestToInv (-xiConst / xiSlope)
  minNs = left <> V.singleton ni <> right

-- | closestToInv r is 1/n closest to r where n is integer.
--
-- >>> closestToInv (1/3)
-- 3
--
-- >>> closestToInv (2/7)
-- 4
--
-- >>> closestToInv (3/8)
-- 3
closestToInv :: Rational -> Int
closestToInv r = goal
 where
  Arg _ goal = minimum $ do
    let cand = floor (1 / r)
    k <- filter (/= 0) [cand, cand + 1]
    pure $ Arg (abs $ r - 1 % fromIntegral k) k

-- >>> findMinimalChebyshev (1/2)
-- [2,1]

-- >>> findMinimalChebyshev (2/3)
-- [3,2,1]

-- >>> findMinimalChebyshev 3
-- [1,1,1,1,1]

-- >>> findMinimalChebyshev (7/3)
-- [3,1,2,2,1,1,1]

-- >>> findMinimalChebyshev (4/5)
-- [1,1,-10,1,1]

findMinimalChebyshev :: Rational -> V.Vector Int
findMinimalChebyshev u2 = found
 where
  Arg _ found = fromJust $ find (== Arg 0 undefined) $ getMin <$> [1 ..]
  getMin = sPolyMinimum u2

main :: IO ()
main = putStrLn "Hello, Haskell!"
