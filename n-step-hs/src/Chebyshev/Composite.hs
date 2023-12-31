-- | Composite chebyshevs.
module Chebyshev.Composite (
  chebyNormalMin,
  tildeZero,
  hatZero,
) where

import Chebyshev.Base
import Chebyshev.Fraction
import Control.Category ((>>>))
import Control.Monad.Identity
import Data.MemoTrie
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Range
import Streaming.Prelude qualified as Stream
import Util

-- >>> chebyNormalMin (7/3) 6
-- Arg (0 % 1) [3,1,1,1,3]

-- >>> chebyNormalMin (8/3) 6
-- Arg (0 % 1) [6,1,1,1,1]

-- | Minimum of normalized chebyshev using fraction maximum.
chebyNormalMin ::
  Rational ->
  Int ->
  RatioResult
chebyNormalMin u2 = memoFix $ \getChebyMin -> \case
  1 -> Arg 1 V.empty -- normalized s1 = 1
  2 -> Arg 1 (V.singleton 1) -- normalized s2 = 1
  k -> searchMinWith (chebyNormalMinSearch u2 getFracMax getChebyMin) (k - 1)
 where
  getFracMax = continuedFracMax Indefinite u2

chebyNormalMinSearch ::
  Rational ->
  (Int -> FractionResult) ->
  (Int -> RatioResult) ->
  MinSearch (Rational, Projective Rational)
chebyNormalMinSearch u2 fracMax chebyMin =
  MinSearch
    { computeInd = zipInduction (chebyNormalInd u2) (continuedFracInd u2),
      minA,
      computeB,
      minAwith,
      maxBwith,
      representative = \sG_k -> abs (fst sG_k.value)
    }
 where
  minA k_1 i =
    let Arg s_Lmin n_L = chebyMin i
        Arg s_Rmin n_R = chebyMin (k_1 + 1 - i)
     in Arg (s_Lmin * s_Rmin) (n_L, n_R)

  minAwith sG_L k_1 =
    let i_1 = inductNum sG_L
        (s_L, _) = sG_L.value
        Arg s_Rmin _ = chebyMin (k_1 - i_1)
     in abs s_L * s_Rmin

  computeB n_L n_R =
    let g_L = continuedFraction u2 (V.toList n_L)
        g_R_rev = continuedFraction u2 (V.toList $ V.reverse n_R)
     in knownFinite (g_L + g_R_rev)

  maxBwith sG_L k_1 =
    let i_1 = inductNum sG_L
        (_, g_L) = sG_L.value
        -- g_L = continuedFraction u2 (inputs s_L)
        Arg g_R_rev _ = fracMax (k_1 - i_1 - 1)
     in knownFinite $ abs g_L + g_R_rev

-- | Induction for normalized chebyshev along with its shifted version; starts at k = 1.
--
-- The shifted version is on the back.
chebyWithShiftedInd :: Rational -> Induction Integer (Rational, Rational)
chebyWithShiftedInd u2 = zipInduction (chebyNormalInd u2) (addBase 0 $ chebyNormalInd u2)

-- >>> cheby = inductive (chebyWithShiftedInd 1)
-- >>> (nexts cheby [1,2,3,4,5]).evaluated
-- InductSeq {initial = (1 % 1,0 % 1), result = fromList [(1,(1 % 1,1 % 1)),(2,(1 % 2,1 % 1)),(3,(1 % 3,5 % 6)),(4,(7 % 24,3 % 4)),(5,(11 % 40,17 % 24))]}

-- | Compute T-tilde from shifted inductive.
tildeFromShifted :: Rational -> IntFnInd (Rational, Rational) -> Rational
tildeFromShifted u2 ss_k = s_k - shift_k / (u2 * fromIntegral (n_1 * n_k))
 where
  n = V.fromList (inputs ss_k)
  (n_1, n_k) = (V.head n, V.last n)
  s_k = fst ss_k.value
  shift_k = case ss_k.previous of
    Nothing -> 0
    Just (_, ss_k_1) -> snd ss_k_1.value

-- >>> tildeZero 2 3
-- Just [1,1,2]

-- | Finds zero of tilde.
tildeZero :: Breadth -> Rational -> Int -> Maybe (V.Vector Integer)
tildeZero breadth u2 = memo $ \case
  1 -> Nothing -- s_2 - s_0 = 1, normalized
  k -> runIdentity $ searchRanges (tildeZeroSearch u2 getFracMax) k
 where
  getFracMax = continuedFracMax breadth u2

tildeZeroSearch ::
  Rational ->
  (Int -> FractionResult) ->
  SearchIntFn Identity (Rational, Rational) (Maybe (V.Vector Integer))
tildeZeroSearch u2 fracMax =
  SearchIntFn
    { fnInduct = inductive $ chebyWithShiftedInd u2,
      selectNext = selectFromBounds $ \_ k _ ->
        Identity $
          let Arg maxB _ = fracMax (k - 1)
              boundRadius = knownFinite (2 * maxB)
           in innerInt $ deltaFrom 0 boundRadius,
      summarize =
        Stream.map (\ind -> Arg (tildeFromShifted u2 ind) (V.fromList $ inputs ind))
          >>> Stream.mapMaybe emitWhenZero
          >>> Stream.head_
    }
 where
  emitWhenZero = \case
    Arg 0 n -> Just n
    _ -> Nothing

-- | Compute T-hat from shifted inductive.
hatFromShifted :: Rational -> IntFnInd (Rational, Rational) -> Rational
hatFromShifted u2 ss_k = s_k + shift_k / (u2 * fromIntegral (n_1 * n_k))
 where
  n = V.fromList (inputs ss_k)
  (n_1, n_k) = (V.head n, V.last n)
  s_k = fst ss_k.value
  shift_k = case ss_k.previous of
    Nothing -> 0
    Just (_, ss_k_1) -> snd ss_k_1.value

-- >>> hatZero 2 3
-- Nothing

-- | Finds zero of T-hat.
hatZero :: Breadth -> Rational -> Int -> Maybe (V.Vector Integer)
hatZero breadth u2 = memo $ \case
  1 -> Nothing -- s_2 - s_0 = 1, normalized
  k -> runIdentity $ searchRanges (hatZeroSearch u2 getFracMax) k
 where
  getFracMax = continuedFracMax breadth u2

hatZeroSearch ::
  Rational ->
  (Int -> FractionResult) ->
  SearchIntFn Identity (Rational, Rational) (Maybe (V.Vector Integer))
hatZeroSearch u2 fracMax =
  SearchIntFn
    { fnInduct = inductive $ chebyWithShiftedInd u2,
      selectNext = selectFromBounds $ \_ k _ ->
        Identity $
          let Arg maxB _ = fracMax (k - 1)
              boundRadius = knownFinite (2 * maxB)
           in innerInt $ deltaFrom 0 boundRadius,
      summarize =
        Stream.map (\ind -> Arg (hatFromShifted u2 ind) (V.fromList $ inputs ind))
          >>> Stream.mapMaybe emitWhenZero
          >>> Stream.head_
    }
 where
  emitWhenZero = \case
    Arg 0 n -> Just n
    _ -> Nothing
