module Chebyshev.Base (
  chebyNormalInd,
  initChebyNormal,
  chebyNormal,
  continuedFracInd,
  initContinuedFrac,
  continuedFraction,
  untilCond,
  IntFnInd,
  RatioResult,
  findZeroStream,
  findUntilCutoff,
  MinSearch (..),
  searchMinWith,
) where

import Control.Monad.Identity
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

type IntFnInd = Inductive Integer

-- | Inductive for normalized chebyshev; Starts at s_1.
initChebyNormal :: Rational -> IntFnInd Rational
initChebyNormal u2 = inductive (chebyNormalInd u2)

chebyNormalInd :: Rational -> Induction Integer Rational
chebyNormalInd u2 =
  Induction
    { base = 1,
      step = \n_k s_k -> case seqPrev s_k of
        Nothing -> 1 -- s_2 = 1
        Just (n_k_1, s_k_1) -> seqValue s_k - seqValue s_k_1 / (u2 * fromIntegral (n_k * n_k_1))
    }

-- | Normalized chebyshev polynomial.
--
-- >>> chebyNormal (1/3) [1, 2, 3]
-- (-1) % 1
--
-- >>> chebyNormal 1 [1, 2, 2]
-- 1 % 4
chebyNormal :: Rational -> [Integer] -> Rational
chebyNormal u2 n_ = (nexts (inductive $ chebyNormalInd u2) n_).value

continuedFracInd :: Rational -> Induction Integer (Projective Rational)
continuedFracInd u2 =
  Induction
    { base = 0,
      step = \n_k g_k -> recip $ Finite u2 * (fromIntegral n_k - seqValue g_k)
    }

-- | InductiveEval for continued fraction divided by u.
initContinuedFrac :: Rational -> IntFnInd (Projective Rational)
initContinuedFrac u2 = inductive (continuedFracInd u2)

-- | Continued fraction, divided by u to make it real.
continuedFraction :: Rational -> [Integer] -> Projective Rational
continuedFraction u2 n_ = (nexts (initContinuedFrac u2) n_).value

-- | Folds latest until certain condition is encountered.
untilCond :: (Monad m) => (r -> Bool) -> Fold.Fold m r (Maybe r)
untilCond cond = Fold.takeEndBy cond Fold.latest

type RatioResult = Arg Rational (V.Vector Integer)

-- | Stream of some computation which goes on until zero is found.
--
-- Assumes that it starts from 1.
findZeroStream :: (Monad m) => (Int -> RatioResult) -> Stream.Stream m (Either Int (V.Vector Integer))
findZeroStream getMin =
  Stream.enumerateFrom 1
    & fmap (\k -> (k, getMin k))
    & Stream.scanMaybe (untilCond $ \(_, Arg curMin _) -> curMin == 0)
    & fmap argZero
 where
  argZero = \case
    (_, Arg m arg) | m == 0 -> Right arg
    (k, _) -> Left k

-- | Finds in stream until cutoff is reached.
findUntilCutoff :: Int -> Stream.Stream Identity (Either Int (V.Vector Integer)) -> Maybe (V.Vector Integer)
findUntilCutoff cutoff stream =
  stream
    & Stream.fold (Fold.mapMaybe findEnd $ join <$> Fold.one)
    & runIdentity
 where
  findEnd = \case
    Left k | k <= cutoff -> Nothing
    Left _ -> Just Nothing
    Right res -> Just (Just res)

data MinSearch v = MinSearch
  { computeInd :: Induction Integer v,
    minA :: Int -> Int -> Arg Rational (V.Vector Integer, V.Vector Integer),
    computeB :: V.Vector Integer -> V.Vector Integer -> Rational,
    minAwith :: IntFnInd v -> Int -> Rational,
    maxBwith :: IntFnInd v -> Int -> Rational,
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
    foldlM (selectN_i minRef) (inductive minSearch.computeInd) [0 .. pred len]
      & (StreamK.toStream . StreamK.unCross)
      & fmap (\ev -> Arg (minSearch.size ev.value) (V.fromList $ inputs ev))
      & Stream.mapM (updateAndGetMin minRef)
      & Stream.scanMaybe (untilCond $ \(Arg curMin _) -> curMin == 0)

  updateAndGetMin minRef new = do
    old <- readSTRef minRef
    if old <= new
      then pure old
      else new <$ writeSTRef minRef new

  selectN_i ::
    STRef s RatioResult ->
    IntFnInd v ->
    Int ->
    StreamK.CrossStreamK (ST s) (IntFnInd v)
  selectN_i minRef v_L _i = StreamK.mkCross . StreamK.concatEffect $ do
    let minA = minSearch.minAwith v_L len
        maxB = minSearch.maxBwith v_L len
    Arg curMin _ <- readSTRef minRef
    let boundRadius = maxB / (1 - curMin / minA)
        minBnd = max (ceiling (-boundRadius)) (floor (-maxB))
        maxBnd = min (floor boundRadius) (ceiling maxB)
    -- traceShowM (len, _i, maxB, curMin / minA, boundRadius, minBnd, maxBnd)
    let positives = Stream.takeWhile (<= maxBnd) $ Stream.enumerateFromStepIntegral (max 1 minBnd) 1
        negatives = Stream.takeWhile (>= minBnd) $ Stream.enumerateFromStepIntegral (min (-1) maxBnd) (-1)
        selected = StreamK.fromStream positives `StreamK.interleave` StreamK.fromStream negatives
    pure (v_L.next <$> selected)

  minCandidate :: RatioResult
  minCandidate = minimum $ do
    i <- [1 .. len]
    let Arg vA (n_L, n_R) = minSearch.minA len i
        vB = minSearch.computeB n_L n_R
    n_i <- filter (/= 0) [floor vB, ceiling vB]
    let vV = vA * (1 - vB / fromIntegral n_i)
    pure $ Arg (abs vV) (n_L <> V.singleton n_i <> n_R)
