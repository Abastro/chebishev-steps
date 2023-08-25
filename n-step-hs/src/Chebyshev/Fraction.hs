-- | Properties of minimal chebyshev polynomial in terms of fractions.
module Chebyshev.Fraction (
  module Chebyshev.Fraction.Base,
  SearchPass (..),
  initChebyRealFracMax,
  findChebyshev,
) where

import Chebyshev.Fraction.Base
import Control.Monad.Identity (Identity (..))
import Control.Monad.ST
import Data.ExtendedReal
import Data.Foldable
import Data.Function ((&))
import Data.Maybe
import Data.Ratio (denominator)
import Data.STRef
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Streamly.Data.StreamK qualified as StreamK
import Streamly.Internal.Data.Stream qualified as Stream
import Streamly.Internal.Data.Stream.StreamK qualified as StreamK
import Util

-- | Compute maximal real-fraction given u^2.
initChebyRealFracMax :: [SearchPass] -> Rational -> InductiveEval () FractionResult
initChebyRealFracMax passes u2 =
  let induct = fracMaxInduction passes u2
   in inductive
        (const $ \prev -> induct prev (inductNum prev + 1))
        (induct (error "unreachable") 0)

type FractionEval = InductiveEval Integer (Extended Rational)

data SearchPass = Narrow | Complete
  deriving (Show)

newtype Refs s = Refs {maxRef :: STRef s FractionResult}

fracMaxInduction :: [SearchPass] -> Rational -> InductiveEval () FractionResult -> Int -> FractionResult
fracMaxInduction passes u2 prev = \case
  0 -> Arg (Finite 0) V.empty -- s0 / s1 = 0
  1 -> Arg (Finite $ 1 / abs u2) (V.singleton 1) -- s1 / s2 = x1 / u^2
  k -> runST $ do
    refs <- Refs <$> newSTRef maxCandidate
    traverse_ (Stream.drain . runPass refs) passes
    readSTRef refs.maxRef
   where
    runPass :: Refs s -> SearchPass -> Stream.Stream (ST s) FractionResult
    runPass refs pass =
      foldlM (chooseN_i pass refs) (initChebyRealFrac u2) [1 .. k - 1]
        & (StreamK.toStream . StreamK.unCross)
        & Stream.mapMaybe lastStep
        & Stream.mapM (updateAndGetMax refs)
        & Stream.scanMaybe untilInfinity

    updateAndGetMax :: Refs s -> FractionResult -> ST s FractionResult
    updateAndGetMax refs new = do
      old <- readSTRef refs.maxRef
      if old >= new
        then pure old
        else new <$ writeSTRef refs.maxRef new

    lastStep :: FractionEval -> Maybe FractionResult
    lastStep g_L =
      let n_k = round . knownFinite $ value g_L
          g_k = next n_k g_L
       in if n_k == 0 then Nothing else Just $ Arg (abs <$> value g_k) (V.fromList $ inputs g_k)

    narrowSearchRadius = fromIntegral $ denominator u2

    chooseN_i ::
      SearchPass ->
      Refs s ->
      FractionEval ->
      Int ->
      StreamK.CrossStreamK (ST s) FractionEval
    chooseN_i pass refs g_L i = StreamK.mkCross . StreamK.concatEffect $ do
      let vG_L = knownFinite $ value g_L
          maxG_R = knownFinite $ getMax (k - i)
      Arg curMax _ <- readSTRef refs.maxRef
      let boundRadius = boundRadiusFor i curMax
          checkRadius = case pass of
            Narrow -> min narrowSearchRadius boundRadius
            Complete -> boundRadius
          minBnd = max (ceiling $ vG_L - checkRadius) (floor $ vG_L - maxG_R)
          maxBnd = min (floor $ vG_L + checkRadius) (ceiling $ vG_L + maxG_R)
          positives = Stream.takeWhile (<= maxBnd) $ Stream.enumerateFromStepIntegral (max 1 minBnd) 1
          negatives = Stream.takeWhile (>= minBnd) $ Stream.enumerateFromStepIntegral (min (-1) maxBnd) (-1)
      pure $ (`next` g_L) <$> do
        if i == 1
          then StreamK.fromStream positives -- Only take n_1 > 0
          else StreamK.fromStream positives `StreamK.interleave` StreamK.fromStream negatives

    getMax :: Int -> Extended Rational
    getMax i = case valueAt i prev of
      Just (Arg v _) -> v
      Nothing -> error "not yet available"

    boundRadiusFor :: Int -> Extended Rational -> Rational
    boundRadiusFor i curMax = min (maxG_R + 1) (maxG_R * boundMultiple) -- Minimum is obtained near G_R.
     where
      maxG_R = knownFinite $ getMax (k - i)
      maxG_R_1 = knownFinite $ getMax (k - i - 1)
      boundMultiple = case curMax of
        Finite cmax -> (cmax + maxG_R_1) / (cmax - maxG_R)
        _ -> 1

    -- Setup: F(x) = F(x_L, x_i, x_R), |x| = k, |x_L| = i-1, |x_R| = k-i
    -- Initial max candidate.
    maxCandidate :: FractionResult
    maxCandidate = Arg (abs . chebyRealFraction u2 $ V.toList n_) n_
     where
      n_ = V.cons n_1 argmax_R

      Arg _ argmax_R = fromJust $ valueAt (k - 1) prev
      g_R_rev = knownFinite $ chebyRealFraction u2 (V.toList $ V.reverse argmax_R)
      -- Opposite direction of truncate
      n_1 = case properFraction g_R_rev of
        (btwn, r)
          | r < 0 -> btwn - 1
          | r > 0 -> btwn + 1
          | otherwise -> btwn

-- >>> chebyFracMaxs (7/3)
-- fromList [Arg (Finite (0 % 1)) [],Arg (Finite (3 % 7)) [1],Arg (Finite (3 % 4)) [1,1],Arg (Finite (12 % 7)) [1,1,1],Arg (Finite (15 % 2)) [2,1,1,1],Arg PosInf [3,1,1,1,3]]

-- Slow numbers:
-- 7: 17/6, 23/6
-- 8: 23/8, 31/8

-- >>> findChebyshev (7/3) 8
-- Just [3,1,1,1,3]

-- >>> findChebyshev (8/3) 8
-- Just [6,1,1,1,1]

-- | Given a root, find a chebyshev polynomial of minimal degree.
findChebyshev :: Rational -> Word -> Maybe (V.Vector Integer)
findChebyshev u2 cutoff =
  Stream.iterate (next ()) (initChebyRealFracMax [Complete] u2)
    & Stream.take (fromIntegral cutoff)
    & fmap value
    & Stream.fold (Fold.mapMaybe argInfinite Fold.one)
    & runIdentity
 where
  -- 'chebyFractionMax u2 k' is infinite when 'chebyNormal u2 (k+1)' is 0.
  argInfinite = \case
    Arg m arg | Data.ExtendedReal.isInfinite m -> Just arg
    _ -> Nothing
