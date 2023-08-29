-- | Composite chebyshevs.
module Chebyshev.Composite (
  chebyNormalMin,
) where

import Chebyshev.Base
import Chebyshev.Fraction
import Data.MemoTrie
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
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
      size = \(s_k, _) -> abs s_k
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

-- | normalized chebyshev along with its shifted version; starts at 1.
--
-- Unshifted is first.
-- initChebyShifted :: Rational -> IntFnInd (Rational, Rational)
-- initChebyShifted u2 = inductive induction (1, 0)
--  where
--   induction n_k ss_k = case previous ss_k of
--     Nothing -> (1, 1) -- (s_2, shift_2)
--     Just (n_k_1, ss_k_1) ->
--       let (s_k, s_k_1) = value ss_k
--        in error ""

--   nextOf n_k n_k_1 s_k s_k_1 = s_k - s_k_1 / (u2 * fromIntegral (n_k * n_k_1))
