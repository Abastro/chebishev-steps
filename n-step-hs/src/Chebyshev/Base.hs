module Chebyshev.Base (
  initChebyNormal,
  chebyNormal,
  initContinuedFrac,
  continuedFraction,
  IntFnEval,
  RatioResult,
  MinSearch (..),
  searchMinWith,
) where

import Control.Monad.ST
import Data.Foldable
import Data.Function ((&))
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

-- | InductiveEval for normalized chebyshev; Starts at s_1.
initChebyNormal :: Rational -> InductiveEval Integer Rational
initChebyNormal u2 = inductive induction 1
 where
  induction n_k s_k = case previous s_k of
    Nothing -> value s_k -- s_2 = s_1
    Just (n_k_1, s_k_1) -> value s_k - value s_k_1 / (u2 * fromIntegral (n_k * n_k_1))

-- | Normalized chebyshev polynomial.
--
-- >>> chebyNormal (1/3) [1, 2, 3]
-- (-1) % 1
--
-- >>> chebyNormal 1 [1, 2, 2]
-- 1 % 4
chebyNormal :: Rational -> [Integer] -> Rational
chebyNormal u2 n_ = value $ nexts n_ (initChebyNormal u2)

-- | InductiveEval for continued fraction divided by u.
initContinuedFrac :: Rational -> InductiveEval Integer (Projective Rational)
initContinuedFrac u2 = inductive induction 0
 where
  induction n_k g_k = recip $ Finite u2 * (fromIntegral n_k - value g_k)

-- | Continued fraction, divided by u to make it real.
continuedFraction :: Rational -> [Integer] -> Projective Rational
continuedFraction u2 n_ = value $ nexts n_ (initContinuedFrac u2)

type IntFnEval = InductiveEval Integer
type RatioResult = Arg Rational (V.Vector Integer)

-- | Folds latest until certain condition is encountered.
untilCond :: (Monad m) => (r -> Bool) -> Fold.Fold m r (Maybe r)
untilCond cond = Fold.takeEndBy cond Fold.latest

data MinSearch v = MinSearch
  { initTerm :: IntFnEval v,
    minA :: Int -> Int -> Arg Rational (V.Vector Integer, V.Vector Integer),
    computeB :: V.Vector Integer -> V.Vector Integer -> Rational,
    minAwith :: IntFnEval v -> Int -> Rational,
    maxBwith :: IntFnEval v -> Int -> Rational,
    size :: v -> Rational
  }

-- | Search the minimum of a form V = A_i (1 - B_i / n_i).
searchMinWith ::
  forall v.
  MinSearch v ->
  Int ->
  RatioResult
searchMinWith minSearch len = runST $ do
  minRef <- newSTRef minCandidate
  Stream.drain $ searchThrough minRef
  readSTRef minRef
 where
  searchThrough minRef =
    foldlM (selectN_i minRef) minSearch.initTerm [0 .. pred len]
      & (StreamK.toStream . StreamK.unCross)
      & fmap (\ev -> Arg (minSearch.size $ value ev) (V.fromList $ inputs ev))
      & Stream.mapM (updateAndGetMin minRef)
      & Stream.scanMaybe (untilCond $ \(Arg curMin _) -> curMin == 0)

  updateAndGetMin minRef new = do
    old <- readSTRef minRef
    if old <= new
      then pure old
      else new <$ writeSTRef minRef new

  selectN_i ::
    STRef s RatioResult ->
    IntFnEval v ->
    Int ->
    StreamK.CrossStreamK (ST s) (IntFnEval v)
  selectN_i minRef v_L _ = StreamK.mkCross . StreamK.concatEffect $ do
    let minA = minSearch.minAwith v_L len
        maxB = minSearch.maxBwith v_L len
    Arg curMin _ <- readSTRef minRef
    let boundRadius = maxB / (1 - curMin / minA)
        minBnd = max (ceiling (-boundRadius)) (floor (-maxB))
        maxBnd = min (floor boundRadius) (ceiling maxB)
    let positives = Stream.takeWhile (<= maxBnd) $ Stream.enumerateFromStepIntegral (max 1 minBnd) 1
        negatives = Stream.takeWhile (>= minBnd) $ Stream.enumerateFromStepIntegral (min (-1) maxBnd) (-1)
        selected = StreamK.fromStream positives `StreamK.interleave` StreamK.fromStream negatives
    pure ((`next` v_L) <$> selected)

  minCandidate :: RatioResult
  minCandidate = minimum $ do
    i <- [1 .. len]
    let Arg vA (n_L, n_R) = minSearch.minA len i
        vB = minSearch.computeB n_L n_R
    n_i <- filter (/= 0) [floor vB, ceiling vB]
    let vV = vA * (1 - vB / fromIntegral n_i)
    pure $ Arg (abs vV) (n_L <> V.singleton n_i <> n_R)
