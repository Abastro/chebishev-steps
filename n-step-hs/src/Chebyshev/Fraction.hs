-- | Properties of minimal chebyshev polynomial in terms of fractions.
module Chebyshev.Fraction (
  module Chebyshev.Fraction.Base,
  continuedFracMax,
  chebyZero,
) where

import Chebyshev.Base
import Chebyshev.Fraction.Base
import Control.Category ((>>>))
import Control.Monad.ST
import Data.MemoTrie
import Data.STRef
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Range
import Streaming
import Streaming.Prelude qualified as Stream
import Util

-- | Maximum of continued fraction given u^2.
continuedFracMax :: Breadth -> Rational -> Int -> FractionResult
continuedFracMax breadth u2 = memoFix $ \getMaxArg -> \case
  0 -> Arg (Finite 0) V.empty -- G0 = 0
  1 -> Arg (Finite $ 1 / abs u2) (V.singleton 1) -- G1 = 1 / (n_1 * u^2)
  k -> runST $ do
    maxRef <- newSTRef maxCandidate
    searchRanges (continuedFracSearch breadth u2 getMax maxRef) k
    readSTRef maxRef
   where
    getMax i = case getMaxArg i of Arg v _ -> knownFinite v

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
  Breadth ->
  Rational ->
  (Int -> Rational) ->
  STRef s FractionResult ->
  SearchIntFn (ST s) (Projective Rational) ()
continuedFracSearch breadth u2 fracMax maxRef =
  SearchIntFn
    { fnInduct = inductive $ continuedFracInd u2,
      selectNext = selectFromBounds getBounds,
      summarize =
        Stream.map (\g_k -> Arg (abs g_k.value) (V.fromList $ inputs g_k))
          >>> Stream.mapM updateAndGetMax
          >>> Stream.takeWhile (\v -> argValue v /= Infinity)
          >>> Stream.effects
    }
 where
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
        checkRadius = case breadth of
          Indefinite -> boundRadius
          MaxBr n -> min (fromIntegral n) boundRadius
        depBounds = innerInt $ deltaFrom vG_L checkRadius
        indepBounds = outerInt $ deltaFrom vG_L maxG_R
        bounds = depBounds `intersect` indepBounds

    pure $ case i of
      _ | i == k -> let n_k = round . knownFinite $ g_L.value in Range n_k n_k
      1 -> higherThan 1 bounds -- Only check positive values (vG_L = 0 here)
      _ -> bounds

  updateAndGetMax new = do
    old <- readSTRef maxRef
    if old >= new
      then pure old
      else new <$ writeSTRef maxRef new

-- Slow numbers:
-- 7: 17/6, 23/6
-- 8: 23/8, 31/8

chebyZero :: (Monad m) => Breadth -> Rational -> Stream (Of Int) m (Maybe (V.Vector Integer))
chebyZero passes u2 = findInftyStream (continuedFracMax passes u2)
