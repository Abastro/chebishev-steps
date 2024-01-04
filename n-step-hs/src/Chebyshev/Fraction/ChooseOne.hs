module Chebyshev.Fraction.ChooseOne (
  naiveContinuedFracInfty,
) where

import Chebyshev.Base
import Chebyshev.Fraction qualified as Fraction
import Data.Function ((&))
import Data.MemoTrie
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Range
import Util
import Streaming
import qualified Streaming.Prelude as Stream
import Control.Category ((>>>))

-- | Infinity of continued fraction given u^2.
-- Snd of the second parameter is the depth limit to apply selection.
naiveContinuedFracInfty :: Breadth -> Rational -> Int -> Int -> Maybe (V.Vector Integer)
naiveContinuedFracInfty breadth u2 = memo2 $ \maxK depth ->
  -- Select until depth, then search in stream in next phase.
  runIdentity $ searchRanges (naiveContinuedFracSearch maxK u2 fracMax) (depth + 1)
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

-- TODO Terminate when 1/2
naiveContinuedFracSearch ::
  Int ->
  Rational ->
  (Int -> Rational) ->
  SearchIntFn Identity (Projective Rational) (Maybe (V.Vector Integer))
naiveContinuedFracSearch maxK u2 fracMax =
  SearchIntFn
    { fnInduct = inductive $ continuedFracInd u2,
      selectNext,
      summarize =
        Stream.map (\g_k -> Arg (abs g_k.value) (V.fromList $ inputs g_k))
          >>> Stream.mapMaybe emitWhenInfty
          >>> Stream.head_
    }
 where
  -- Meaningful select until len is reached
  -- ? Perhaps this should be in summarize phase
  selectNext g_L len i = case i `compare` len of
    EQ -> diveFracInfty maxK g_L i
    _ -> selectFromBounds getBounds g_L len i

  -- len = k here
  getBounds g_L _len i = do
    let vG_L = knownFinite g_L.value
        maxG_R = fracMax (maxK - i)
    let bounds = innerInt $ deltaFrom vG_L maxG_R
    -- when (i <= 1 && i < k) $ traceShowM (k, i, depth, inputs g_L, minBnd, maxBnd)

    pure $ case i of
      1 -> higherThan 1 bounds -- Only check positive values (vG_L = 0 here)
      _ -> bounds

  emitWhenInfty = \case
    Arg Infinity n -> Just n
    _ -> Nothing

-- | "Dive" into the potential infinity by selecting only the rounding value.
diveFracInfty ::
  Int ->
  IntFnInd (Projective Rational) ->
  Int ->
  Stream (Of (IntFnInd (Projective Rational))) Identity ()
diveFracInfty cut g_L i =
  Stream.iterate (\g_k -> g_k.next $ roundExceptZero (knownFinite g_k.value)) g_L
    & Stream.take (cut - i)
 where
  roundExceptZero v = case (round v, floor $ signum v) of
    (0, 0) -> 1
    (0, sign) -> sign
    (n, _) -> n
