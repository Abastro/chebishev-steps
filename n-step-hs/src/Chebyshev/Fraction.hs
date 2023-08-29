-- | Properties of minimal chebyshev polynomial in terms of fractions.
module Chebyshev.Fraction (
  module Chebyshev.Fraction.Base,
  SearchPass (..),
  continuedFractionMax,
  chebyZero,
) where

import Chebyshev.Base
import Chebyshev.Fraction.Base
import Control.Monad.ST
import Data.Foldable
import Data.Function ((&))
import Data.MemoTrie
import Data.Ratio (denominator)
import Data.STRef
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Streamly.Data.Stream qualified as Stream
import Streamly.Data.StreamK qualified as StreamK
import Streamly.Internal.Data.Stream qualified as Stream
import Streamly.Internal.Data.Stream.StreamK qualified as StreamK
import Util

type FractionEval = IntFnInd (Projective Rational)

data SearchPass = Narrow | Complete
  deriving (Show)

-- | Maximum of continued fraction given u^2.
continuedFractionMax :: [SearchPass] -> Rational -> Int -> FractionResult
continuedFractionMax passes u2 = memoFix $ \getMaxArg -> \case
  0 -> Arg (Finite 0) V.empty -- s0 / s1 = 0
  1 -> Arg (Finite $ 1 / abs u2) (V.singleton 1) -- s1 / s2 = x1 / u^2
  k -> runST $ do
    maxRef <- newSTRef maxCandidate
    traverse_ (Stream.drain . runPass maxRef) passes
    readSTRef maxRef
   where
    getMax i = case getMaxArg i of Arg v _ -> v

    runPass :: STRef s FractionResult -> SearchPass -> Stream.Stream (ST s) FractionResult
    runPass maxRef pass =
      foldlM (chooseN_i pass maxRef) (initContinuedFrac u2) [1 .. k - 1]
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
    lastStep g_L =
      let n_k = round . knownFinite $ g_L.value
          g_k = g_L.next n_k
       in if n_k == 0 then Nothing else Just $ Arg (abs g_k.value) (V.fromList $ inputs g_k)

    narrowSearchRadius = fromIntegral $ denominator u2

    chooseN_i ::
      SearchPass ->
      STRef s FractionResult ->
      FractionEval ->
      Int ->
      StreamK.CrossStreamK (ST s) FractionEval
    chooseN_i pass maxRef g_L i = StreamK.mkCross . StreamK.concatEffect $ do
      let vG_L = knownFinite g_L.value
          maxG_R = knownFinite $ getMax (k - i)
      Arg curMax _ <- readSTRef maxRef
      let boundRadius = boundRadiusFor i curMax
          checkRadius = case pass of
            Narrow -> min narrowSearchRadius boundRadius
            Complete -> boundRadius
          minBnd = max (ceiling $ vG_L - checkRadius) (floor $ vG_L - maxG_R)
          maxBnd = min (floor $ vG_L + checkRadius) (ceiling $ vG_L + maxG_R)
          positives = Stream.takeWhile (<= maxBnd) $ Stream.enumerateFromStepIntegral (max 1 minBnd) 1
          negatives = Stream.takeWhile (>= minBnd) $ Stream.enumerateFromStepIntegral (min (-1) maxBnd) (-1)
      pure $ g_L.next <$> do
        if i == 1
          then StreamK.fromStream positives -- Only take n_1 > 0
          else StreamK.fromStream positives `StreamK.interleave` StreamK.fromStream negatives

    boundRadiusFor :: Int -> Projective Rational -> Rational
    boundRadiusFor i curMax = min (maxG_R + 1) (maxG_R * boundMultiple) -- Minimum is obtained near G_R.
     where
      maxG_R = knownFinite $ getMax (k - i)
      maxG_R_1 = knownFinite $ getMax (k - i - 1)
      boundMultiple = case curMax of
        Finite cmax -> (cmax + maxG_R_1) / (cmax - maxG_R)
        _ -> 1

    -- Setup: F(x) = F(x_L, x_i, x_R), |x| = k, |x_L| = i-1, |x_R| = k-i
    -- Initial max candidate.
    maxCandidate :: FractionResult
    maxCandidate = Arg (abs . continuedFraction u2 $ V.toList n_) n_
     where
      n_ = V.cons n_1 argmax_R

      Arg _ argmax_R = getMaxArg (k - 1)
      g_R_rev = knownFinite $ continuedFraction u2 (V.toList $ V.reverse argmax_R)
      -- Opposite direction of truncate
      n_1 = case properFraction g_R_rev of
        (btwn, r)
          | r < 0 -> btwn - 1
          | r > 0 -> btwn + 1
          | otherwise -> btwn

-- Slow numbers:
-- 7: 17/6, 23/6
-- 8: 23/8, 31/8

chebyZero :: Monad m => [SearchPass] -> Rational -> Stream.Stream m (Either Int (V.Vector Integer))
chebyZero passes u2 = chebyZeroOf (continuedFractionMax passes u2)
