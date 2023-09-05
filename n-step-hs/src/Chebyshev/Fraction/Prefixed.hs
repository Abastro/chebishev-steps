module Chebyshev.Fraction.Prefixed (
  prefixedFracMax,
  chebyZero,
) where

import Chebyshev.Base
import Chebyshev.Fraction qualified as Fraction
import Chebyshev.Fraction.Base
import Control.Monad
import Control.Monad.ST
import Data.MemoTrie
import Data.STRef
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Range
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Util

-- | Maximum of prefixed fraction given u^2.
prefixedFracMax :: Breadth -> [Integer] -> Rational -> Int -> FractionResult
prefixedFracMax breadth prefix u2 = memo $ \case
  0 -> Arg (Finite 0) V.empty -- G0 = 0
  1 -> Arg (Finite $ 1 / abs u2) (V.singleton 1) -- G1 = 1 / (n_1 * u^2)
  k -> runST $ do
    maxRef <- newSTRef (Arg 0 V.empty) -- fallback
    searchRanges (prefixedFracSearch breadth prefix u2 getMax maxRef) k
    readSTRef maxRef
   where
    fracMaxArg = Fraction.continuedFracMax breadth u2
    getMax i = case fracMaxArg i of Arg v _ -> knownFinite v

-- | Searches infinity when given prefix.
prefixedFracSearch ::
  Breadth ->
  [Integer] ->
  Rational ->
  (Int -> Rational) ->
  STRef s FractionResult ->
  SearchIntFn (ST s) (Projective Rational) ()
prefixedFracSearch pass prefix u2 fracMax maxRef =
  SearchIntFn
    { fnInduct = nexts (inductive $ continuedFracInd u2) prefix,
      getBounds = getBounds,
      summarize =
        Fold.lmap (\g_k -> Arg (abs g_k.value) (V.fromList $ inputs g_k))
          . Fold.lmapM updateAndGetMax
          $ (void . Fold.find $ \(Arg curMax _) -> curMax == Infinity)
    }
 where
  -- len = k here. We only need to find infinity, so we assume infinity.
  getBounds g_L k i = do
    let vG_L = knownFinite g_L.value
        maxG_R = fracMax (k - i)

    let checkRadius = case pass of
          Indefinite -> maxG_R
          MaxBr n -> min (fromIntegral n) maxG_R

    let minBnd = ceiling $ vG_L - checkRadius
        maxBnd = floor $ vG_L + checkRadius
    pure $ case i of
      _ | i == k -> let n_k = round . knownFinite $ g_L.value in Range n_k n_k
      _ -> Range minBnd maxBnd

  updateAndGetMax new = do
    old <- readSTRef maxRef
    if old >= new
      then pure old
      else new <$ writeSTRef maxRef new

chebyZero :: (Monad m) => Breadth -> [Integer] -> Rational -> Stream.Stream m (Either Int (V.Vector Integer))
chebyZero passes prefix u2 = findInftyStream (prefixedFracMax passes prefix u2)
