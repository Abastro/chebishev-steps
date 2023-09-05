module Chebyshev.Fraction.ChooseOne (
  naiveContinuedFracInfty,
  chebyZero,
) where

import Chebyshev.Base
import Chebyshev.Fraction qualified as Fraction
import Control.Monad.Identity (Identity (..))
import Data.MemoTrie
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Util

-- | Infinity of continued fraction given u^2.
-- Snd of the second parameter is the depth limit to apply selection.
naiveContinuedFracInfty :: Breadth -> Rational -> (Int, Int) -> Maybe (V.Vector Integer)
naiveContinuedFracInfty breadth u2 = memo $ \case
  (0, _) -> Nothing -- G0 = 0
  (k, depth) -> runIdentity $ searchRanges (naiveContinuedFracSearch depth u2 fracMax) k
 where
  fracMaxArg = Fraction.continuedFracMax breadth u2
  -- Cutoff max when it is bigger than search breadth.
  fracMax = case breadth of
    Indefinite -> knownFinite . argValue . fracMaxArg
    MaxBr br -> \i -> case maxes V.!? i of
      Just v -> knownFinite v
      Nothing -> fromIntegral br
     where
      maxes = V.fromList . takeWhile (<= fromIntegral br) $ argValue . fracMaxArg <$> [0 ..]

naiveContinuedFracSearch ::
  Int ->
  Rational ->
  (Int -> Rational) ->
  SearchIntFn Identity (Projective Rational) (Maybe (V.Vector Integer))
naiveContinuedFracSearch depth u2 fracMax =
  SearchIntFn
    { fnInduct = inductive $ continuedFracInd u2,
      getBounds = getBounds,
      summarize =
        Fold.lmap (\g_k -> Arg (abs g_k.value) (V.fromList $ inputs g_k))
          $ Fold.mapMaybe emitWhenInfty Fold.one
    }
 where
  -- len = k here
  getBounds g_L k i = do
    let vG_L = knownFinite g_L.value
        maxG_R = fracMax (k - i)
    let radius = maxG_R

    let minBnd = ceiling $ vG_L - radius
        maxBnd = floor $ vG_L + radius
    -- when (i <= 1 && i < k) $ traceShowM (k, i, depth, inputs g_L, minBnd, maxBnd)

    pure $ case i of
      _ | i == k -> let n_k = round vG_L in (n_k, n_k)
      -- Over the select depth
      _ | i > depth -> let n_k = roundExceptZero vG_L in (n_k, n_k)
      1 -> (1, maxBnd) -- Only check positive values (vG_L = 0 here)
      _ -> (minBnd, maxBnd)

  emitWhenInfty = \case
    Arg Infinity n -> Just n
    _ -> Nothing

  roundExceptZero v = case (round v, floor $ signum v) of
    (0, 0) -> 1
    (0, sign) -> sign
    (n, _) -> n

-- Slow numbers:
-- 7: 17/6, 23/6
-- 8: 23/8, 31/8

chebyZero :: (Monad m) => Breadth -> Int -> Rational -> Stream.Stream m (Either Int (V.Vector Integer))
chebyZero breadth depth u2 = findJustStream (\k -> compute (k, depth))
 where
  compute = naiveContinuedFracInfty breadth u2
