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
  MinSearch Rational
chebyNormalMinSearch u2 fracMax chebyMin =
  MinSearch
    { initTerm = initChebyNormal u2,
      minA,
      computeB,
      minAwith,
      maxBwith,
      size = abs
    }
 where
  minA k_1 i =
    let Arg s_Lmin n_L = chebyMin i
        Arg s_Rmin n_R = chebyMin (k_1 + 1 - i)
     in Arg (s_Lmin * s_Rmin) (n_L, n_R)

  minAwith s_L k_1 =
    let i_1 = inductNum s_L
        Arg s_Rmin _ = chebyMin (k_1 - i_1)
     in abs (value s_L) * s_Rmin

  computeB n_L n_R =
    let g_L = continuedFraction u2 (V.toList n_L)
        g_R_rev = continuedFraction u2 (V.toList $ V.reverse n_R)
     in knownFinite (g_L + g_R_rev)

  maxBwith s_L k_1 =
    let i_1 = inductNum s_L
        g_L = continuedFraction u2 (inputs s_L)
        Arg g_R_rev _ = fracMax (k_1 - i_1 - 1)
     in knownFinite $ g_L + g_R_rev
