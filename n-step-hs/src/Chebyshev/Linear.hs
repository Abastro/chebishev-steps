-- | Properties of minimal chebyshev polynomial in terms of linearity in each coefficient.
module Chebyshev.Linear (
  constTerm,
  slopeTerm,
  initChebyNormalMin,
  chebyNormalMins,
  chebyZero,
  findChebyshev,
) where

import Chebyshev.Base
import Control.Monad.Identity (Identity (..))
import Data.Function ((&))
import Data.Maybe
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

-- alternatingTo :: (Monad m, Eq a, Num a, Stream.Enumerable a) => a -> Stream.Stream m a
-- alternatingTo bnd = Stream.filter (/= 0) $ Stream.enumerateFromTo (-bnd) bnd

initChebyNormalMin :: Rational -> InductiveEval () RatioResult
initChebyNormalMin u2 =
  inductive
    (const $ \prev -> chebyMinInduction u2 prev (inductNum prev + 1))
    (Arg (-1) V.empty)

chebyNormalMinSearch :: Rational -> InductiveEval () RatioResult -> MinSearch Rational
chebyNormalMinSearch u2 prev =
  MinSearch
    { initTerm = initChebyNormal u2,
      minA,
      computeB = slopeTerm u2,
      minAwith,
      maxBwith,
      size = abs
    }
 where
  minA k_1 i =
    let Arg s_Lmin n_L = fromJust (valueAt i prev)
        Arg s_Rmin n_R = fromJust (valueAt (k_1 + 1 - i) prev)
     in Arg (s_Lmin * s_Rmin) (n_L, n_R)

  -- A = constTerm
  minAwith s_L k_1 =
    let i_1 = inductNum s_L
        Arg s_Rmin _ = fromJust (valueAt (k_1 - i_1) prev)
     in abs (value s_L) * s_Rmin

  -- B = slopeTerm / constTerm
  maxBwith s_L k_1 = slopeUBAt s_L (k_1 + 1) (inductNum s_L + 1) / minAwith s_L k_1

  -- Upper bound of slope term where left portion is decided.
  slopeUBAt :: IntFnEval Rational -> Int -> Int -> Rational
  slopeUBAt s_L k i = (leftBiased + rightBiased) / abs u2
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

chebyMinInduction :: Rational -> InductiveEval () RatioResult -> Int -> RatioResult
chebyMinInduction u2 prev = \case
  1 -> Arg 1 V.empty -- normalized s1 = 1
  2 -> Arg 1 (V.singleton 1) -- normalized s2 = 1
  k -> searchMinWith (chebyNormalMinSearch u2 prev) (k - 1)

-- Stops when 0 is encountered
untilZero :: (Monad m, Eq r, Num r) => Fold.Fold m (Arg r b) (Maybe (Arg r b))
untilZero = Fold.takeEndBy (\(Arg curMin _) -> curMin == 0) Fold.latest

-- | Gives a stream of minimums until 0.
chebyNormalMins :: (Monad m) => Rational -> Stream.Stream m RatioResult
chebyNormalMins u2 =
  Stream.iterate (next ()) (initChebyNormalMin u2)
    & Stream.drop 1 -- Initial element does not mean anything
    & fmap value
    & Stream.scanMaybe untilZero

-- | Determines if u2 is a s_k's zero.
chebyZero :: (Monad m) => Rational -> Stream.Stream m (Either Int (V.Vector Integer))
chebyZero u2 =
  chebyNormalMins u2
    & Stream.indexed
    & fmap argZero
 where
  argZero = \case
    (_, Arg m arg) | m == 0 -> Right arg
    (k_1, _) -> Left (k_1 + 1)

-- >>> findChebyshev (1/2) 8
-- Just [2,1]

-- >>> findChebyshev (2/3) 8
-- Just [3,2,1]

-- >>> findChebyshev 3 8
-- Just [1,1,1,1,1]

-- >>> findChebyshev (7/3) 8
-- Just [-3,-1,-1,-1,-3]

-- >>> findChebyshev (4/5) 8
-- Just [-5,1,1]

-- >>> findChebyshev (8/3) 5
-- Nothing

-- >>> findChebyshev (8/3) 6
-- Just [6,1,1,1,1]

findChebyshev :: Rational -> Word -> Maybe (V.Vector Integer)
findChebyshev u2 cutoff =
  Stream.iterate (next ()) (initChebyNormalMin u2)
    & Stream.take (fromIntegral cutoff + 1)
    & fmap value
    & Stream.fold (Fold.mapMaybe argZero Fold.one)
    & runIdentity
 where
  argZero = \case
    Arg m arg | m == 0 -> Just arg
    _ -> Nothing
