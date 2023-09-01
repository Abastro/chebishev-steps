-- | Properties of minimal chebyshev polynomial in terms of fractions.
module Chebyshev.Fraction (
  module Chebyshev.Fraction.Base,
  SearchPass (..),
  continuedFracMax,
  chebyZero,
) where

import Chebyshev.Base
import Chebyshev.Fraction.Base
import Control.Monad
import Control.Monad.ST
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

data SearchPass = Narrow | Complete
  deriving (Show)

-- | Maximum of continued fraction given u^2.
continuedFracMax :: [SearchPass] -> Rational -> Int -> FractionResult
continuedFracMax passes u2 = memoFix $ \getMaxArg -> \case
  0 -> Arg (Finite 0) V.empty -- G0 = 0
  1 -> Arg (Finite $ 1 / abs u2) (V.singleton 1) -- G1 = 1 / (n_1 * u^2)
  k -> runST $ do
    maxRef <- newSTRef maxCandidate
    let searchPass pass = searchRanges (continuedFracSearch pass u2 getMax maxRef) k
    traverse_ searchPass passes
    readSTRef maxRef
   where
    getMax i = case getMaxArg i of Arg v _ -> knownFinite v

    -- Setup: F(x) = F(x_L, x_i, x_R), |x| = k, |x_L| = i-1, |x_R| = k-i
    -- Initial max candidate.
    maxCandidate :: FractionResult
    maxCandidate = Arg (abs . continuedFraction u2 $ V.toList n) n
     where
      n = V.cons n_1 argmax_R

      Arg _ argmax_R = getMaxArg (k - 1)
      g_R_rev = knownFinite $ continuedFraction u2 (V.toList $ V.reverse argmax_R)
      -- Opposite direction of truncate
      n_1 = case properFraction g_R_rev of
        (btwn, r)
          | r < 0 -> btwn - 1
          | r > 0 -> btwn + 1
          | otherwise -> btwn

continuedFracSearch ::
  SearchPass ->
  Rational ->
  (Int -> Rational) ->
  STRef s FractionResult ->
  SearchIntFn (ST s) (Projective Rational) ()
continuedFracSearch pass u2 fracMax maxRef =
  SearchIntFn
    { fnInduct = inductive $ continuedFracInd u2,
      getBounds = getBounds,
      summarize =
        Fold.lmap (\g_k -> Arg (abs g_k.value) (V.fromList $ inputs g_k))
          . Fold.lmapM updateAndGetMax
          $ (void . Fold.find $ \(Arg curMax _) -> curMax == Infinity)
    }
 where
  narrowSearchRadius = fromIntegral $ denominator u2

  -- len = k here
  getBounds g_L k i = do
    let vG_L = knownFinite g_L.value
        maxG_R = fracMax (k - i)
        maxG_R_1 = fracMax (k - i - 1)

    Arg curMax _ <- readSTRef maxRef
    let boundMultiple = case curMax of
          Finite cmax -> (cmax + maxG_R_1) / (cmax - maxG_R)
          Infinity -> 1
        boundRadius = maxG_R * boundMultiple
        checkRadius = case pass of
          Narrow -> min narrowSearchRadius boundRadius
          Complete -> boundRadius

    let minBnd = max (ceiling $ vG_L - checkRadius) (floor $ vG_L - maxG_R)
        maxBnd = min (floor $ vG_L + checkRadius) (ceiling $ vG_L + maxG_R)
    pure $ case i of
      1 -> (1, maxBnd) -- Only check positive values (vG_L = 0 here)
      _ | i == k -> let n_k = round . knownFinite $ g_L.value in (n_k, n_k)
      _ -> (minBnd, maxBnd)

  updateAndGetMax new = do
    old <- readSTRef maxRef
    if old >= new
      then pure old
      else new <$ writeSTRef maxRef new

-- Slow numbers:
-- 7: 17/6, 23/6
-- 8: 23/8, 31/8

chebyZero :: (Monad m) => [SearchPass] -> Rational -> Stream.Stream m (Either Int (V.Vector Integer))
chebyZero passes u2 = findInftyStream (continuedFracMax passes u2)
