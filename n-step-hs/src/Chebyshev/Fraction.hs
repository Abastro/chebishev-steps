-- | Properties of minimal chebyshev polynomial in terms of fractions.
module Chebyshev.Fraction (
  chebyFraction,
  chebyPartSlope,
  chebyFractionMax,
) where

import Chebyshev.Base
import Control.Monad.State
import Data.ExtendedReal
import Data.Maybe
import Data.MemoTrie
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Streamly.Prelude qualified as Stream
import Util

-- | Normalized chebyshev into fraction.
chebyFraction :: (Fractional v, Eq v) => InductiveEval a v -> Extended v
chebyFraction s_k1 = case previous s_k1 of
  Nothing -> Finite 0
  Just (_, s_k) -> value s_k `infiDiv` value s_k1

-- | Part of the chebyshev slope fraction.
chebyPartSlope :: (Fractional v, Eq v, Integral a) => v -> InductiveEval a v -> Extended v
chebyPartSlope u2 s_k1 = case previous s_k1 of
  Nothing -> Finite 0
  Just (n_k, s_k) -> value s_k `infiDiv` (u2 * fromIntegral n_k * value s_k1)

chebyFractionMax :: Rational -> Word -> Arg (Extended Rational) (V.Vector Integer)
chebyFractionMax u2 = computeMax
 where
  getFiniteMax :: Word -> Rational
  getFiniteMax k = case computeMax k of
    Arg (Finite v) _ -> v
    _ -> error "the maximum is infinite"

  computeMax :: Word -> Arg (Extended Rational) (V.Vector Integer)
  computeMax = memo $ \case
    0 -> Arg 0 V.empty -- s0 / s1 = 0
    1 -> Arg 1 (V.singleton 1) -- s1 / s2 = 1
    k -> evalState maxFinding (error "initMin")
     where
      -- F(x) = F(x_L, x_i, x_R), |x| = k, |x_L| = i-1, |x_R| = k-i

      boundMultiple i curMax =
        (1 + getFiniteMax (k - i - 1) / curMax)
          / (1 - getFiniteMax (k - i) / curMax)

      chooseN_i ::
        InductiveEval Integer Rational ->
        Word ->
        Stream.SerialT (State Rational) (InductiveEval Integer Rational)
      chooseN_i s_L i = do
        let max_R = getFiniteMax (k - i) / u2
        boundMult <- Stream.fromEffect $ gets (boundMultiple i)
        let maxDist = max_R * boundMult
            -- By induction, known to give finite values.
            known_L = knownFinite $ chebyPartSlope u2 s_L
            (minBnd, maxBnd) = (floor $ known_L - maxDist, floor $ known_L + maxDist)
        n_i <- Stream.filter (/= 0) $ Stream.enumerateFromTo minBnd maxBnd
        pure (next n_i s_L)

      -- TODO Exit when current maximum == 0
      maxFinding :: State Rational (Arg (Extended Rational) (V.Vector Integer))
      maxFinding = fmap fromJust . Stream.last $ do
        curMaxArg@(Arg curMax _) <- Stream.scanl1' max $ do
          s_k1 <- foldM chooseN_i (initChebyNormal u2) [1 .. k - 1]
          let c_k = chebyFraction s_k1
          pure $ Arg (abs <$> c_k) (V.fromList $ inputs s_k1)

        case curMax of
          Finite finMax -> Stream.fromEffect (put finMax)
          _ -> pure ()
        pure curMaxArg
