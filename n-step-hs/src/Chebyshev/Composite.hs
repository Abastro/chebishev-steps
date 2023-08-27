-- | Composite chebyshevs.
module Chebyshev.Composite (

initChebyNormalMin) where

import Chebyshev.Base
import Chebyshev.Fraction (FractionResult)
import Data.Maybe
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Util

-- TODO How to pass maximum of fraction?
-- Easier with monad.

-- | Minimum of normalized chebyshev using maximum of continued fractions.
initChebyNormalMin :: Rational -> InductiveEval () RatioResult
initChebyNormalMin u2 =
  inductive
    (const $ \prev -> chebyMinInduction u2 undefined prev (inductNum prev + 1))
    (Arg (-1) V.empty)

chebyMinInduction ::
  Rational ->
  InductiveEval () FractionResult ->
  InductiveEval () RatioResult ->
  Int ->
  RatioResult
chebyMinInduction u2 fracMax chebyPrev = \case
  1 -> Arg 1 V.empty -- normalized s1 = 1
  2 -> Arg 1 (V.singleton 1) -- normalized s2 = 1
  k -> searchMinWith (chebyNormalMinSearch u2 fracMax chebyPrev) (k - 1)

chebyNormalMinSearch ::
  Rational ->
  InductiveEval () FractionResult ->
  InductiveEval () RatioResult ->
  MinSearch Rational
chebyNormalMinSearch u2 fracMax chebyPrev =
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
    let Arg s_Lmin n_L = fromJust (valueAt i chebyPrev)
        Arg s_Rmin n_R = fromJust (valueAt (k_1 + 1 - i) chebyPrev)
     in Arg (s_Lmin * s_Rmin) (n_L, n_R)

  minAwith s_L k_1 =
    let i_1 = inductNum s_L
        Arg s_Rmin _ = fromJust (valueAt (k_1 - i_1) chebyPrev)
     in abs (value s_L) * s_Rmin

  computeB n_L n_R =
    let g_L = continuedFraction u2 (V.toList n_L)
        g_R_rev = continuedFraction u2 (V.toList $ V.reverse n_R)
     in knownFinite (g_L + g_R_rev)

  maxBwith s_L k_1 =
    let i_1 = inductNum s_L
        g_L = continuedFraction u2 (inputs s_L)
        Arg g_R_rev _ = fromJust (valueAt (k_1 - i_1 - 1) fracMax)
     in knownFinite $ g_L + g_R_rev
