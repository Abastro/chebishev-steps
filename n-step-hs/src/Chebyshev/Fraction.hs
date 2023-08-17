-- | Properties of minimal chebyshev polynomial in terms of fractions.
module Chebyshev.Fraction (
  chebyNormalFraction,
  chebyRealFraction,
  chebyRealFractionMax,
  findChebyshev,
) where

import Chebyshev.Base
import Control.Monad.State
import Data.ExtendedReal
import Data.List qualified as List
import Data.Maybe
import Data.MemoTrie
import Data.Monoid (First (..))
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Unfold qualified as Unfold
import Streamly.Prelude qualified as Stream
import Util

-- | Normalized chebyshev into fraction.
chebyNormalFraction :: (Fractional v, Eq v) => InductiveEval a v -> Extended v
chebyNormalFraction s_k1 = case previous s_k1 of
  Nothing -> Finite 0
  Just (_, s_k) -> value s_k `infiDiv` value s_k1

-- | Fraction of chebyshev polynomials, divided by u to make it real.
chebyRealFraction :: (Fractional v, Eq v, Integral a) => v -> InductiveEval a v -> Extended v
chebyRealFraction u2 s_k1 = case previous s_k1 of
  Nothing -> Finite 0
  Just (n_k, s_k) -> value s_k `infiDiv` (u2 * fromIntegral n_k * value s_k1)

-- | Initiate fraction computation of chebyshev polynomials.
initChebyRealFrac :: (RealFrac v, Integral a) => v -> InductiveEval a (Extended v)
initChebyRealFrac u2 = inductive induction 0
 where
  induction n_k g_k = 1 `infiDiv` knownFinite (Finite u2 * (fromIntegral n_k - value g_k))

-- >>> chebyRealFractionMax 3 3
-- Arg (Finite (2 % 3)) [1,1,1]

-- >>> chebyRealFractionMax 3 5
-- Arg PosInf [1,1,1,1,1]

-- >>> chebyRealFractionMax (8/3) 5
-- Arg PosInf [6,1,1,1,1]

type FractionEval = InductiveEval Integer (Extended Rational)

-- | Compute maximal real-fraction G_k given k and steps.
chebyRealFractionMax :: Rational -> Word -> Arg (Extended Rational) (V.Vector Integer)
chebyRealFractionMax u2 = computeMax
 where
  getMax :: Word -> Extended Rational
  getMax k = case computeMax k of
    Arg v _ -> v

  -- Initial max candidate.
  maxCandidate :: Word -> Arg (Extended Rational) (V.Vector Integer)
  maxCandidate k = Arg maxCand n_
   where
    n_ = V.cons n_1 argmax_R
    maxCand = abs . chebyRealFraction u2 $ nexts (V.toList n_) (initChebyNormal u2)

    Arg _ argmax_R = computeMax (k - 1)
    s_R_rev = nexts (V.toList $ V.reverse argmax_R) (initChebyNormal u2)
    g_R_rev = knownFinite $ chebyRealFraction u2 s_R_rev
    -- Opposite direction of truncate
    n_1 = case properFraction g_R_rev of
      (btwn, r)
        | r < 0 -> btwn - 1
        | r > 0 -> btwn + 1
        | otherwise -> btwn

  boundRadiusFor :: Word -> Word -> Extended Rational -> Rational
  boundRadiusFor k i curMax
    | i == k = knownFinite (1 / curMax) / u2 -- Typically, very small
    | otherwise = maxG_R * boundMultiple
   where
    maxG_R = knownFinite $ getMax (k - i)
    maxG_R_1 = knownFinite $ getMax (k - i - 1)
    boundMultiple = case curMax of
      Finite cmax -> (cmax + maxG_R_1) / (cmax - maxG_R)
      _ -> 1

  boundRadiusVec :: Word -> Extended Rational -> V.Vector Rational
  boundRadiusVec k curMax = V.generate (fromIntegral k) $ \i_1 ->
    boundRadiusFor k (fromIntegral i_1 + 1) curMax

  computeMax :: Word -> Arg (Extended Rational) (V.Vector Integer)
  computeMax = memo $ \case
    0 -> Arg 0 V.empty -- s0 / s1 = 0
    1 -> Arg (Finite $ 1 / abs u2) (V.singleton 1) -- s1 / s2 = x1 / u^2
    k -> flip evalState (error "initial state exposed") $ do
      fromMaybe (error "maximal entry not found") <$> Stream.fold untilInfinity maxFinding
     where
      -- Setup: F(x) = F(x_L, x_i, x_R), |x| = k, |x_L| = i-1, |x_R| = k-i
      maxCandArg = maxCandidate k

      chooseN_i ::
        Word ->
        Unfold.Unfold (State (V.Vector Rational)) FractionEval FractionEval
      chooseN_i i =
        flip Unfold.lmapM Unfold.fromStream $ \g_L -> do
          boundRadius <- gets (V.! (fromIntegral i - 1))
          let vG_L = knownFinite $ value g_L
              center = floor vG_L
              minBnd = floor $ vG_L - boundRadius
              maxBnd = ceiling $ vG_L + boundRadius
          pure $ (`next` g_L) <$> do
            if i == 1
              then Stream.enumerateFromTo (max 1 minBnd) maxBnd -- Only take n_1 > 0
              else Stream.filter (/= 0) $ rangeFromCenter center minBnd maxBnd

      withArgPut argValue@(Arg val _) = argValue <$ put (boundRadiusVec k val)
      -- Puts max to state and return.
      puttingMax oldMax new = if oldMax < new then withArgPut new else pure oldMax

      inductAsArg g_k = Arg (abs <$> value g_k) (V.fromList $ inputs g_k)

      maxFinding :: Stream.SerialT (State (V.Vector Rational)) (Arg (Extended Rational) (V.Vector Integer))
      maxFinding =
        Stream.scanlM' puttingMax (withArgPut maxCandArg) $ inductAsArg <$> do
          List.foldl'
            (\stream i -> Stream.unfoldMany (chooseN_i i) stream)
            (Stream.fromPure $ initChebyRealFrac u2)
            [1 .. k]

-- Stops when infinity is encountered
untilInfinity :: (Monad m) => Fold.Fold m (Arg (Extended r) b) (Maybe (Arg (Extended r) b))
untilInfinity = Fold.takeEndBy (\(Arg curMax _) -> Data.ExtendedReal.isInfinite curMax) Fold.last

-- >>> Stream.toList $ rangeFromCenter (0 :: Int) (-3) 6
-- [0,1,-1,2,-2,3,-3,4,5,6]

rangeFromCenter :: (Enum a, Stream.Enumerable a, Monad m) => a -> a -> a -> Stream.SerialT m a
rangeFromCenter center lower higher =
  Stream.enumerateFromThenTo center (pred center) lower
    `Stream.wSerial` Stream.enumerateFromTo (succ center) higher

-- Do not know what is problem now.
-- 7: 17/6, 23/6
-- 8: 23/8, 31/8

-- >>> findChebyshev (7/3) 8
-- Just [3,1,1,1,3]

-- >>> findChebyshev (8/3) 8
-- Just [6,1,1,1,1]

-- | Given a root, find a chebyshev polynomial of minimal degree.
findChebyshev :: Rational -> Word -> Maybe (V.Vector Integer)
findChebyshev u2 cutoff = getFirst $ foldMap (First . argForInfinite) [2 .. cutoff]
 where
  getMax = chebyRealFractionMax u2
  -- 'chebyFractionMax u2 k' is infinite when 'chebyNormal u2 (k+1)' is 0.
  argForInfinite k = case getMax (pred k) of
    Arg m arg | Data.ExtendedReal.isInfinite m -> Just arg
    _ -> Nothing
