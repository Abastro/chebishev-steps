module Chebyshev.TFrac (
  tfunFraction,
  tfunFracMax,
  tfnZero,
) where

import Chebyshev.Base
import Chebyshev.Fraction qualified as Fraction
import Chebyshev.Fraction.Base
import Control.Monad.ST
import Data.Foldable
import Data.Function ((&))
import Data.MemoTrie
import Data.STRef
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Streamly.Data.Stream qualified as Stream
import Streamly.Data.StreamK qualified as StreamK
import Streamly.Internal.Data.Stream qualified as Stream
import Streamly.Internal.Data.Stream.StreamK qualified as StreamK
import Util

-- | Induction for fraction of Reverse-T function.
tfunFracInd :: Rational -> Induction Integer (Projective Rational)
tfunFracInd u2 =
  Induction
    { base = 0,
      step = \n_k h_k -> case seqPrev h_k of
        Nothing -> 2 / (Finite u2 * fromIntegral n_k) -- H_1 = 2 / u^2 n_1
        Just _ -> recip $ Finite u2 * (fromIntegral n_k - seqValue h_k) -- Otherwise, same as G_k
    }

-- | Fraction of Reverse-T function.
tfunFraction :: Rational -> [Integer] -> Projective Rational
tfunFraction u2 n = (nexts (inductive $ tfunFracInd u2) n).value

type FractionEval = IntFnInd (Projective Rational)

-- | Maximum of fraction of Reverse-T function given u^2.
tfunFracMax :: Rational -> Int -> FractionResult
tfunFracMax u2 = memo $ \case
  0 -> Arg (Finite 0) V.empty -- H_0 = 0
  1 -> Arg (Finite $ 2 / abs u2) (V.singleton 1) -- H_1 = 2 / u^2 n_1
  k -> runST $ do
    maxRef <- newSTRef maxCandidate
    Stream.drain $ runPass maxRef
    readSTRef maxRef
   where
    getContFracMaxArg = Fraction.continuedFractionMax [Fraction.Complete] u2
    getContFracMax i = case getContFracMaxArg i of Arg v _ -> v

    runPass :: STRef s FractionResult -> Stream.Stream (ST s) FractionResult
    runPass maxRef =
      foldlM (chooseN_i maxRef) (inductive $ tfunFracInd u2) [1 .. k - 1]
        & (StreamK.toStream . StreamK.unCross)
        & Stream.mapMaybe lastStep
        & Stream.mapM (updateAndGetMax maxRef)
        & Stream.scanMaybe untilInfinity

    updateAndGetMax :: STRef s FractionResult -> FractionResult -> ST s FractionResult
    updateAndGetMax maxRef new = do
      old <- readSTRef maxRef
      if old >= new
        then pure old
        else new <$ writeSTRef maxRef new

    lastStep :: FractionEval -> Maybe FractionResult
    lastStep h_L =
      let n_k = round . knownFinite $ h_L.value
          h_k = h_L.next n_k
       in if n_k == 0 then Nothing else Just $ Arg (abs h_k.value) (V.fromList $ inputs h_k)

    chooseN_i ::
      STRef s FractionResult ->
      FractionEval ->
      Int ->
      StreamK.CrossStreamK (ST s) FractionEval
    chooseN_i maxRef h_L i = StreamK.mkCross . StreamK.concatEffect $ do
      let vH_L = knownFinite h_L.value
          maxG_R = knownFinite $ getContFracMax (k - i)
      Arg curMax _ <- readSTRef maxRef
      let boundRadius = boundRadiusFor i curMax
          factor = if i == 1 then 2 else 1 -- See below
          minBnd = max (ceiling $ vH_L - boundRadius) (floor $ vH_L - factor * maxG_R)
          maxBnd = min (floor $ vH_L + boundRadius) (ceiling $ vH_L + factor * maxG_R)
          positives = Stream.takeWhile (<= maxBnd) $ Stream.enumerateFromStepIntegral (max 1 minBnd) 1
          negatives = Stream.takeWhile (>= minBnd) $ Stream.enumerateFromStepIntegral (min (-1) maxBnd) (-1)
      pure $ h_L.next <$> do
        if i == 1
          then StreamK.fromStream positives -- Only take n_1 > 0
          else StreamK.fromStream positives `StreamK.interleave` StreamK.fromStream negatives

    boundRadiusFor :: Int -> Projective Rational -> Rational
    boundRadiusFor i curMax = min (factor * maxG_R + 1) (factor * maxG_R * boundMultiple) -- Minimum is obtained near G_R.
     where
      maxG_R = knownFinite $ getContFracMax (k - i)
      maxG_R_1 = knownFinite $ getContFracMax (k - i - 1)
      boundMultiple = case curMax of
        Finite cmax -> (cmax + maxG_R_1) / (cmax - maxG_R)
        _ -> 1
      -- a_1 / 2 should be used instead of a_1
      factor = if i == 1 then 2 else 1

    -- Setup: H(a) = H(a_L, a_i, a_R), |a| = k, |a_L| = i-1, |a_R| = k-i
    -- Initial max candidate.
    maxCandidate :: FractionResult
    maxCandidate = Arg (abs . tfunFraction u2 $ V.toList n) n
     where
      n = V.cons n_1 argmax_R
      -- Only need to determine the right side, which is just continued fractions.
      Arg _ argmax_R = getContFracMaxArg (k - 1)
      g_R_rev = knownFinite $ continuedFraction u2 (V.toList $ V.reverse argmax_R)
      -- Opposite direction of truncate. Note that 'n_1 / 2' should be used here.
      n_1 = case properFraction (2 * g_R_rev) of
        (btwn, r)
          | r < 0 -> btwn - 1
          | r > 0 -> btwn + 1
          | otherwise -> btwn

-- Slow numbers:
-- 7: 17/6, 23/6
-- 8: 23/8, 31/8

-- | Zeros of T function.
tfnZero :: (Monad m) => Rational -> Stream.Stream m (Either Int (V.Vector Integer))
tfnZero u2 = chebyZeroOf (tfunFracMax u2)
