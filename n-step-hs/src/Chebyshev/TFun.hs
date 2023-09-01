module Chebyshev.TFun (
  tfunNormal,
  tfunFraction,
  tfunFracMax,
  tfunZero,
) where

import Chebyshev.Base
import Chebyshev.Fraction qualified as Fraction
import Chebyshev.Fraction.Base
import Control.Monad
import Control.Monad.ST
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable
import Data.MemoTrie
import Data.Ratio (denominator)
import Data.STRef
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Util

-- | Compute the normalized Reverse-T function.
tfunNormal :: Rational -> [Integer] -> Rational
tfunNormal u2 = \case
  [] -> 2 -- Somehow this fits
  [n_1] -> chebyNormal u2 [n_1]
  n@(n_1 : n_2 : n_R) -> chebyNormal u2 n - chebyNormal u2 n_R / (u2 * fromIntegral (n_1 * n_2))

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

-- | Maximum of fraction of Reverse-T function given u^2.
tfunFracMax :: [Fraction.SearchPass] -> Rational -> Int -> FractionResult
tfunFracMax passes u2 = memo $ \case
  0 -> Arg (Finite 0) V.empty -- H_0 = 0
  1 -> Arg (Finite $ 2 / abs u2) (V.singleton 1) -- H_1 = 2 / u^2 n_1
  k -> runST $ do
    maxRef <- newSTRef maxCandidate
    let searchPass pass = searchRanges (tfunFracSearch pass u2 getFracMax maxRef) k
    traverse_ searchPass passes
    readSTRef maxRef
   where
    getFracMaxArg = Fraction.continuedFracMax passes u2
    getFracMax i = case getFracMaxArg i of Arg v _ -> knownFinite v

    -- Setup: H(a) = H(a_L, a_i, a_R), |a| = k, |a_L| = i-1, |a_R| = k-i
    -- Initial max candidate.
    maxCandidate :: FractionResult
    maxCandidate = Arg (abs . tfunFraction u2 $ V.toList n) n
     where
      n = V.cons n_1 argmax_R
      -- Only need to determine the right side, which is just continued fractions.
      Arg _ argmax_R = getFracMaxArg (k - 1)
      g_R_rev = knownFinite $ continuedFraction u2 (V.toList $ V.reverse argmax_R)
      -- Opposite direction of truncate. Note that 'n_1 / 2' should be used here.
      n_1 = case properFraction (2 * g_R_rev) of
        (btwn, r)
          | r < 0 -> btwn - 1
          | r > 0 -> btwn + 1
          | otherwise -> btwn

tfunFracSearch ::
  Fraction.SearchPass ->
  Rational ->
  (Int -> Rational) ->
  STRef s FractionResult ->
  SearchIntFn (ST s) (Projective Rational) ()
tfunFracSearch pass u2 fracMax maxRef =
  SearchIntFn
    { fnInduct = tfunFracInd u2,
      getBounds = getBounds,
      summarize =
        Fold.lmap (\g_k -> Arg (abs g_k.value) (V.fromList $ inputs g_k))
          . Fold.lmapM updateAndGetMax
          $ (void . Fold.find $ \(Arg curMax _) -> curMax == Infinity)
    }
 where
  narrowSearchRadius = fromIntegral $ denominator u2

  -- len = k here
  getBounds h_L k i = do
    let vH_L = knownFinite h_L.value
        maxG_R = fracMax (k - i)
        maxG_R_1 = fracMax (k - i - 1)

    Arg curMax _ <- readSTRef maxRef
    let boundMultiple = case curMax of
          Finite cmax -> (cmax + maxG_R_1) / (cmax - maxG_R)
          Infinity -> 1
        boundRadius = maxG_R * boundMultiple
        checkRadius = case pass of
          Fraction.Narrow -> min narrowSearchRadius boundRadius
          Fraction.Complete -> boundRadius

    let minBnd = max (ceiling $ vH_L - checkRadius) (floor $ vH_L - maxG_R)
        maxBnd = min (floor $ vH_L + checkRadius) (ceiling $ vH_L + maxG_R)
    pure $ case i of
      -- Only check positive values. (vH_L = 0 here)
      -- In addition, effectively we are using n_1 / 2 here.
      1 -> (1, max (ceiling $ vH_L + 2 * boundRadius) (floor $ vH_L + 2 * maxG_R))
      _ | i == k -> let n_k = round . knownFinite $ h_L.value in (n_k, n_k)
      _ -> (minBnd, maxBnd)

  updateAndGetMax new = do
    old <- readSTRef maxRef
    if old >= new
      then pure old
      else new <$ writeSTRef maxRef new

-- | Zeroes of Reverse-T function. (Subtract 1 because it was added..)
tfunZero :: (Monad m) => [Fraction.SearchPass] -> Rational -> Stream.Stream m (Either Int (V.Vector Integer))
tfunZero passes u2 = first (subtract 1) <$> findInftyStream (tfunFracMax passes u2)
