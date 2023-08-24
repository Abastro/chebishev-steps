-- | Properties of minimal chebyshev polynomial in terms of fractions.
module Chebyshev.Fraction.Reverse (
  initChebyRealFrac,
  chebyRealFraction,
  initChebyReverseFrac,
  chebyRealFractionMax,
  findChebyshev,
) where

import Control.Monad.ST
import Data.Bifunctor (Bifunctor (..))
import Data.ExtendedReal
import Data.Foldable
import Data.Function ((&))
import Data.Maybe
import Data.MemoTrie
import Data.Monoid (First (..))
import Data.STRef
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Streamly.Data.StreamK qualified as StreamK
import Streamly.Internal.Data.Stream qualified as Stream
import Streamly.Internal.Data.Stream.StreamK qualified as StreamK
import Util

-- | Initiate fraction computation of chebyshev polynomials.
initChebyRealFrac :: (RealFrac v, Integral a) => v -> InductiveEval a (Extended v)
initChebyRealFrac u2 = inductive induction 0
 where
  induction n_k g_k = infiRecip . knownFinite $ Finite u2 * (fromIntegral n_k - value g_k)

-- | Fraction of chebyshev polynomials, divided by u to make it real.
chebyRealFraction :: Rational -> [Integer] -> Extended Rational
chebyRealFraction u2 n_ = value $ nexts n_ (initChebyRealFrac u2)

-- | Gives G_k along with its reverse direction version.
initChebyReverseFrac :: (RealFrac v, Integral a) => v -> InductiveEval a (Extended v, Extended v)
initChebyReverseFrac u2 = inductive induction (0, 0)
 where
  induction n_k g_ls =
    let (g_l, g_l_rev) = bimap knownFinite knownFinite $ value g_ls
        g = infiRecip $ u2 * (fromIntegral n_k - g_l)
        g_rev = case previous g_ls of
          Just (_, g_lls)
            | (_, g_ll_rev) <- value g_lls ->
                (g_l_rev * fromIntegral n_k - knownFinite g_ll_rev * g_l) `infiDiv` (fromIntegral n_k - g_l)
          Nothing -> Finite $ recip (fromIntegral n_k * u2)
     in (g, g_rev)

-- >>> chebyRealFractionMax 3 3
-- Arg (Finite (2 % 3)) [1,1,1]

-- >>> chebyRealFractionMax 3 5
-- Arg PosInf [1,1,1,1,1]

-- >>> chebyRealFractionMax (8/3) 5
-- Arg PosInf [6,1,1,1,1]

type FractionEval = InductiveEval Integer (Extended Rational, Extended Rational)

-- Value of delta
deltaRight :: Rational -> FractionEval -> Rational
deltaRight g g_Rs = (g - g_Rr) / (g - g_R) * g_R_rev
 where
  (g_R_rev, g_R) = bimap knownFinite knownFinite $ value g_Rs
  g_Rr = knownFinite $ case previous g_Rs of
    Just (_, g_Rrs) -> snd $ value g_Rrs
    Nothing -> 0

-- Stops when infinity is encountered
untilInfinity :: (Monad m) => Fold.Fold m (Arg (Extended r) b) (Maybe (Arg (Extended r) b))
untilInfinity = Fold.takeEndBy (\(Arg curMax _) -> Data.ExtendedReal.isInfinite curMax) Fold.latest

-- | Compute maximal real-fraction G_k given k and steps.
chebyRealFractionMax :: Rational -> Word -> Arg (Extended Rational) (V.Vector Integer)
chebyRealFractionMax u2 = computeMax
 where
  getMax :: Word -> Extended Rational
  getMax k = case computeMax k of
    Arg v _ -> v

  -- Initial max candidate.
  maxCandidate :: Word -> Arg (Extended Rational) (V.Vector Integer)
  maxCandidate k = Arg (abs . chebyRealFraction u2 $ V.toList n_) n_
   where
    n_ = V.cons n_1 argmax_R

    Arg _ argmax_R = computeMax (k - 1)
    g_R_rev = knownFinite $ chebyRealFraction u2 (V.toList $ V.reverse argmax_R)
    -- Opposite direction of truncate
    n_1 = case properFraction g_R_rev of
      (btwn, r)
        | r < 0 -> btwn - 1
        | r > 0 -> btwn + 1
        | otherwise -> btwn

  computeMax :: Word -> Arg (Extended Rational) (V.Vector Integer)
  computeMax = memo $ \case
    0 -> Arg 0 V.empty -- s0 / s1 = 0
    1 -> Arg (Finite $ 1 / abs u2) (V.singleton 1) -- s1 / s2 = x1 / u^2
    k -> runST $ do
      fromMaybe (error "maximal entry not found") <$> Stream.fold untilInfinity maxFinding
     where
      -- Setup: F(x) = F(x_L, x_i, x_R), |x| = k, |x_L| = i-1, |x_R| = k-i
      chooseN_i ::
        STRef s (Extended Rational) ->
        FractionEval ->
        Int ->
        StreamK.CrossStreamK (ST s) FractionEval
      chooseN_i maxCandRef g_Rs i = StreamK.mkCross . StreamK.concatEffect $ do
        maxCand <- knownFinite <$> readSTRef maxCandRef
        -- traceShowM (k, i, maxCand, g_Rs)
        -- Note that we accumulate in the reverse direction.
        let g_L_maxabs = knownFinite $ getMax (fromIntegral i - 1)
            g_R_rev = knownFinite . fst $ value g_Rs
            -- Possible range of delta
            (delta1, delta2) = (deltaRight (-maxCand) g_Rs, deltaRight maxCand g_Rs)
            minBnd = max (floor $ g_R_rev - g_L_maxabs) $ ceiling (min delta1 delta2 - g_L_maxabs)
            maxBnd = min (ceiling $ g_R_rev + g_L_maxabs) $ floor (max delta1 delta2 + g_L_maxabs)
            positives = Stream.takeWhile (<= maxBnd) $ Stream.enumerateFromStepIntegral (max 1 minBnd) 1
            negatives = Stream.takeWhile (>= minBnd) $ Stream.enumerateFromStepIntegral (min (-1) maxBnd) (-1)
        pure $ (`next` g_Rs) <$> do
          if i == 1
            then StreamK.fromStream positives -- Only take n_1 > 0 (Maybe this does not work)
            else StreamK.fromStream positives `StreamK.interleave` StreamK.fromStream negatives

      puttingMax maxCandRef oldMax new@(Arg newV _) =
        if oldMax < new then new <$ writeSTRef maxCandRef newV else pure oldMax

      maxFinding :: Stream.Stream (ST s) (Arg (Extended Rational) (V.Vector Integer))
      maxFinding = Stream.concatEffect $ do
        let maxCandArg@(Arg newCand _) = maxCandidate k
        maxCandRef <- newSTRef newCand
        let chosens = foldlM (chooseN_i maxCandRef) (initChebyReverseFrac u2) $ fromIntegral <$> [k, k - 1 .. 1]
        pure
          $ StreamK.toStream (StreamK.unCross chosens)
          & fmap (\g_ks -> Arg (abs . snd $ value g_ks) (V.fromList . reverse $ inputs g_ks))
          & Stream.scan (Fold.foldlM' (puttingMax maxCandRef) $ pure maxCandArg)

-- Slow numbers:
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
