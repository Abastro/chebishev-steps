-- | Composite chebyshevs.
module Chebyshev.Composite (
  chebyNormalMin,
  tildeNormalMin,
) where

import Chebyshev.Base
import Chebyshev.Fraction
import Data.MemoTrie
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Util
import Debug.Trace

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
  getFracMax = continuedFractionMax [Complete] u2

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
_tildeFromShifted :: IntFnInd (Rational, Rational) -> Rational
_tildeFromShifted ss_k = s_k - shift_k
 where
  s_k = fst ss_k.value
  shift_k = case ss_k.previous of
    Nothing -> 0
    Just (_, ss_k_1) -> snd ss_k_1.value

-- >>> tildeNormalMin 1 3
-- Prelude.minimum: empty list

-- | Minimum of tilde.
tildeNormalMin ::
  Rational ->
  Int ->
  RatioResult
tildeNormalMin u2 = memo $ \case
  1 -> Arg 1 V.empty -- s_2 - s_0, normalized
  k -> searchMinWith (_tildeMinSearch u2 getFracMax getChebyMin) k
 where
  getFracMax = continuedFractionMax [Complete] u2
  getChebyMin = chebyNormalMin u2

_tildeMinSearch ::
  Rational ->
  (Int -> FractionResult) ->
  (Int -> RatioResult) ->
  MinSearch (Rational, Rational)
_tildeMinSearch u2 fracMax chebyMin =
  MinSearch
    { computeInd = chebyWithShiftedInd u2,
      minA,
      computeB,
      minAwith,
      maxBwith,
      representative = abs . _tildeFromShifted
    }
 where
  -- A (n_L, n_R) is normalized chebyshev with (n_R, -n_L).
  minA k i = case chebyMin k of
    Arg v vec | (n_R, neg_n_L) <- V.splitAt (i - 1) vec -> Arg v (negate <$> neg_n_L, n_R)

  -- Known n_L values do not improve the minimum of A.
  minAwith _ k = case chebyMin k of
    Arg v _ -> v

  -- B (n_L, n_R) is linear combination of continued fractions.
  computeB n_L n_R =
    let neg_n_L = negate <$> n_L
        normal = continuedFraction u2 (V.toList $ n_R <> neg_n_L)
        reversed = continuedFraction u2 (V.toList . V.reverse $ n_R <> neg_n_L)
     in traceShow (n_L, n_R, normal, reversed) $ knownFinite (-normal + reversed)

  maxBwith _ k = case fracMax (k - 1) of
    Arg v _ -> knownFinite (2 * v)
