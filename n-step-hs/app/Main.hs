{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.State.Strict
import Data.IntMap.Lazy qualified as IM
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Ratio
import Data.Semigroup (Arg (..))
import Data.Traversable
import Data.Vector qualified as V
import Streamly.Prelude qualified as Stream

safeDiv :: (Eq a, Fractional a) => a -> a -> [a]
safeDiv n m = [n / m | m /= 0]

-- | Polynomial that is at most linear in each variable.
--
-- Each variable is denoted by a number.
newtype MultilinPoly n = MultilinPoly (M.Map IS.IntSet n)

instance (Num n) => Semigroup (MultilinPoly n) where
  (<>) :: (Num n) => MultilinPoly n -> MultilinPoly n -> MultilinPoly n
  MultilinPoly poly <> MultilinPoly poly' = MultilinPoly (M.unionWith (+) poly poly')
instance (Num n) => Monoid (MultilinPoly n) where
  mempty :: (Num n) => MultilinPoly n
  mempty = MultilinPoly M.empty

-- Assumption: u^2 is rational

-- | Record of the minimum of normalized chebyshev polynomial.
type SMinimums = IM.IntMap (Arg (V.Vector Int) Rational)

makeSPolyMinimum :: Rational -> Int -> SMinimums
makeSPolyMinimum u2 maxK = allMins
 where
  allMins = IM.fromSet (sPolyMinimum u2 allMins) $ IS.fromList [1 .. maxK]

-- | Minimum value of normalized chebyshev polynomial, with the location achieving the bound.
sPolyMinimum :: Rational -> SMinimums -> Int -> Arg (V.Vector Int) Rational
sPolyMinimum _ _ 1 = Arg V.empty 1 -- normalized s1 = 1
sPolyMinimum _ _ 2 = Arg (V.singleton 1) 1 -- normalized s2 = 1
sPolyMinimum u2 allMins k = fromJust mayResult
 where
  mayResult = evalState minFinding initialMin

  bndMinArg@(Arg _ bndMin) = boundaryMin allMins k
  initialMin = initialMinCand u2 bndMinArg
  -- Max. of normalized s_k where n_i = 0, i = 1 to k-1.
  sZeroMaxes = sPolyZeroMax u2 k <$> V.enumFromTo 1 (k - 1)

  boundAt i curMin = floor $ (bndMin - curMin) / (sZeroMaxes V.! pred i)
  boundCond i ni = do
    bound <- gets (boundAt i)
    pure (ni <= bound)

  chooseNs :: Stream.SerialT (State Rational) (V.Vector Int)
  chooseNs = for (V.enumFromTo 1 (k - 1)) $ \i -> do
    Stream.takeWhileM (boundCond i) (Stream.enumerateFrom 1)

  minFinding :: State Rational (Maybe (Arg (V.Vector Int) Rational))
  minFinding = Stream.last $ do
    -- minArg: Current minimum.
    minArg@(Arg _ curMin) <- Stream.scanl' min (Arg (error "unreachable") initialMin) $ do
      ns <- chooseNs
      pure (Arg ns $ sPoly u2 ns / sPolyLead ns)
    minArg <$ put curMin

-- | min_i |s_i| |s_{k-i}|, with the args
boundaryMin :: SMinimums -> Int -> Arg (V.Vector Int, V.Vector Int) Rational
boundaryMin allMins k = minimum $ do
  i <- [1 .. k `div` 2]
  let Arg left lv = allMins IM.! i
      Arg right rv = allMins IM.! (k - i)
  pure $ Arg (left, right) (lv * rv)

-- | Initial minimum candidate derived from the boundary-minimums.
initialMinCand :: Rational -> Arg (V.Vector Int, V.Vector Int) Rational -> Rational
initialMinCand u2 (Arg (left, right) xiConst) = sPoly u2 (left <> V.singleton ni <> right)
 where
  xiSlope = sPoly u2 (left <> V.singleton 0 <> right) / sPolyLead (left <> right)
  ni = closestToInv (-xiConst / xiSlope)

-- | closestToInv r is 1/n closest to r where n is integer.
closestToInv :: Rational -> Int
closestToInv r = goal
 where
  Arg goal _ = minimum $ do
    let cand = floor (1 / r)
    k <- filter (/= 0) [cand, cand + 1]
    pure $ Arg k $ abs (r - 1 % fromIntegral k)

-- * NOTE: Following "polynomials" are of form divided by u^{k-1}.

-- >>> sPoly 1 (V.fromList [1, 2])
-- 1 % 2

-- >>> sPoly 1 (V.fromList [1, 2, 2])
-- 1 % 4

-- TODO Verify sPoly

-- | Typical chebyshev polynomial.
sPoly :: Rational -> V.Vector Int -> Rational
sPoly u2 ns = V.last results
 where
  results = V.fromList (map sAt [1 .. V.length ns + 1])
  ni j = fromIntegral $ ns V.! pred j
  sAtPrev k = results V.! pred k
  sAt = \case
    1 -> 1
    2 -> 1
    k -> sAtPrev (k - 1) - sAtPrev (k - 2) / (u2 * ni (k - 1) * ni (k - 2))

-- | Leading coefficient of the chebyshev polynomial.
sPolyLead :: V.Vector Int -> Rational
sPolyLead ns = product (fromIntegral <$> ns)

-- TODO Verify sPolyMax

-- >>> sPolyMax 2 <$> [1..5]
-- [1 % 1,1 % 1,3 % 2,7 % 4,19 % 8]

-- | Maximum of normalized k-th chebyshev polynomial.
sPolyMax :: Rational -> Int -> Rational
sPolyMax u2 = \i -> maxes !! (i - 1)
 where
  mkOf mk_1 mk_2 = mk_1 + abs (1 / u2) * mk_2
  maxes = 1 : 1 : zipWith mkOf maxes (tail maxes)

-- | Maximum of k-th chebyshev polynomial with zero at certain index.
sPolyZeroMax :: Rational -> Int -> Int -> Rational
sPolyZeroMax _u2 _k _i = undefined

main :: IO ()
main = putStrLn "Hello, Haskell!"
