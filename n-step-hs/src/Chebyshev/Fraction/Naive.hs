-- | Instead of finding real maximum, settle for "good enough".
--
-- Currently, limits n to be <= denominator/numerator.
module Chebyshev.Fraction.Naive (
  initChebyRealFrac,
  chebyRealFraction,
  chebyFracMaxs,
  findChebyshev,
) where

import Control.Monad.Identity (Identity (..))
import Control.Monad.ST
import Data.ExtendedReal
import Data.Foldable
import Data.Function ((&))
import Data.Maybe
import Data.Ratio (denominator, numerator)
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

type FractionResult = Arg (Extended Rational) (V.Vector Integer)
type FractionEval = InductiveEval Integer (Extended Rational)

-- Stops when infinity is encountered
untilInfinity :: (Monad m) => Fold.Fold m (Arg (Extended r) b) (Maybe (Arg (Extended r) b))
untilInfinity = Fold.takeEndBy (\(Arg curMax _) -> Data.ExtendedReal.isInfinite curMax) Fold.latest

-- | Compute maximal real-fraction given u^2.
initChebyRealFracMax :: Rational -> InductiveEval () FractionResult
initChebyRealFracMax u2 =
  inductive
    (const $ \prev -> fracMaxInduction u2 prev (inductNum prev + 1))
    (fracMaxInduction u2 (error "unreachable") 0)

fracMaxInduction :: Rational -> InductiveEval () FractionResult -> Int -> FractionResult
fracMaxInduction u2 prev = \case
  0 -> Arg (Finite 0) V.empty -- s0 / s1 = 0
  1 -> Arg (Finite $ 1 / abs u2) (V.singleton 1) -- s1 / s2 = x1 / u^2
  k -> runST $ fromMaybe (error "maximal entry not found") <$> Stream.fold untilInfinity maxFinding
   where
    getMax :: Int -> Extended Rational
    getMax i = case valueAt i prev of
      Just (Arg v _) -> v
      Nothing -> error "not yet available"

    -- Initial max candidate.
    maxCandidate :: FractionResult
    maxCandidate = Arg (abs . chebyRealFraction u2 $ V.toList n_) n_
     where
      n_ = V.cons n_1 argmax_R

      Arg _ argmax_R = fromJust $ valueAt (k - 1) prev
      g_R_rev = knownFinite $ chebyRealFraction u2 (V.toList $ V.reverse argmax_R)
      -- Opposite direction of truncate
      n_1 = case properFraction g_R_rev of
        (btwn, r)
          | r < 0 -> btwn - 1
          | r > 0 -> btwn + 1
          | otherwise -> btwn

    boundRadiusFor :: Int -> Extended Rational -> Rational
    boundRadiusFor i curMax = min (maxG_R + 1) (maxG_R * boundMultiple) -- Minimum is obtained near G_R.
     where
      maxG_R = knownFinite $ getMax (k - i)
      maxG_R_1 = knownFinite $ getMax (k - i - 1)
      boundMultiple = case curMax of
        Finite cmax -> (cmax + maxG_R_1) / (cmax - maxG_R)
        _ -> 1

    boundRadiusVec :: FractionResult -> V.Vector Rational
    boundRadiusVec (Arg curMax _) = V.generate (k - 1) $ \i_1 ->
      boundRadiusFor (i_1 + 1) curMax

    bound = fromIntegral $ max (denominator u2) (numerator u2)

    -- Setup: F(x) = F(x_L, x_i, x_R), |x| = k, |x_L| = i-1, |x_R| = k-i
    chooseN_i ::
      STRef s (V.Vector Rational) ->
      FractionEval ->
      Int ->
      StreamK.CrossStreamK (ST s) FractionEval
    chooseN_i radiusRef g_L i = StreamK.mkCross . StreamK.concatEffect $ do
      boundRadius <- min bound . (V.! pred i) <$> readSTRef radiusRef
      let vG_L = knownFinite $ value g_L
          maxG_R = knownFinite $ getMax (k - i)
          minBnd = max (ceiling $ vG_L - boundRadius) (floor $ vG_L - maxG_R)
          maxBnd = min (floor $ vG_L + boundRadius) (ceiling $ vG_L + maxG_R)
          positives = Stream.takeWhile (<= maxBnd) $ Stream.enumerateFromStepIntegral (max 1 minBnd) 1
          negatives = Stream.takeWhile (>= minBnd) $ Stream.enumerateFromStepIntegral (min (-1) maxBnd) (-1)
      pure $ (`next` g_L) <$> do
        if i == 1
          then StreamK.fromStream positives -- Only take n_1 > 0
          else StreamK.fromStream positives `StreamK.interleave` StreamK.fromStream negatives

    puttingMax radiusRef oldMax new =
      if oldMax < new then new <$ writeSTRef radiusRef (boundRadiusVec new) else pure oldMax

    lastStep :: FractionEval -> Maybe FractionResult
    lastStep g_L =
      let n_k = round . knownFinite $ value g_L
          g_k = next n_k g_L
       in if n_k == 0 then Nothing else Just $ Arg (abs <$> value g_k) (V.fromList $ inputs g_k)

    maxFinding :: Stream.Stream (ST s) FractionResult
    maxFinding = Stream.concatEffect $ do
      radiusRef <- newSTRef $ boundRadiusVec maxCandidate
      let chosens = foldlM (chooseN_i radiusRef) (initChebyRealFrac u2) [1 .. k - 1]
      pure
        $ StreamK.toStream (StreamK.unCross chosens)
        & Stream.mapMaybe lastStep
        & Stream.scan (Fold.foldlM' (puttingMax radiusRef) $ pure maxCandidate)

-- | Gives a stream of maximums until infinity.
chebyFracMaxs :: Rational -> Stream.Stream Identity (Arg (Extended Rational) (V.Vector Integer))
chebyFracMaxs u2 =
  Stream.iterate (next ()) (initChebyRealFracMax u2)
    & fmap value
    & Stream.scanMaybe untilInfinity

-- >>> chebyFracMaxs (7/3)
-- fromList [Arg (Finite (0 % 1)) [],Arg (Finite (3 % 7)) [1],Arg (Finite (3 % 4)) [1,1],Arg (Finite (12 % 7)) [1,1,1],Arg (Finite (15 % 2)) [2,1,1,1],Arg PosInf [3,1,1,1,3]]

-- >>> findChebyshev (7/3) 8
-- Just [3,1,1,1,3]

-- >>> findChebyshev (8/3) 8
-- Just [6,1,1,1,1]

-- | Given a root, find a chebyshev polynomial of minimal degree.
findChebyshev :: Rational -> Word -> Maybe (V.Vector Integer)
findChebyshev u2 cutoff =
  Stream.iterate (next ()) (initChebyRealFracMax u2)
    & Stream.take (fromIntegral cutoff)
    & fmap value
    & Stream.fold (Fold.mapMaybe argInfinite Fold.one)
    & runIdentity
 where
  -- 'chebyFractionMax u2 k' is infinite when 'chebyNormal u2 (k+1)' is 0.
  argInfinite = \case
    Arg m arg | Data.ExtendedReal.isInfinite m -> Just arg
    _ -> Nothing
