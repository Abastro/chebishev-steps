module Chebyshev.Base (
  chebyNormalInd,
  initChebyNormal,
  chebyNormal,
  continuedFracInd,
  initContinuedFrac,
  continuedFraction,
  -- untilCond,
  Breadth (..),
  IntFnInd,
  RatioResult,
  findZeroStream,
  findJustStream,
  findUntilCutoff,
  MinSearch (..),
  searchMinWith,
  SearchIntFn (..),
  searchRanges,
  selectFromBounds,
  rangeNonzeroStream,
) where

import Control.Category ((>>>))
import Control.Monad.Identity
import Control.Monad.ST
import Data.Foldable
import Data.Function ((&))
import Data.Maybe
import Data.STRef
import Data.Semigroup (Arg (..))
import Data.Vector qualified as V
import Inductive
import Range
import Streaming
import Streaming.Prelude qualified as Stream
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
-- untilCond :: (Monad m) => (r -> Bool) -> Fold.Fold m r (Maybe r)
-- untilCond cond = Fold.takeEndBy cond Fold.latest
data Breadth = Indefinite | MaxBr Int
  deriving (Show)

type RatioResult = Arg Rational (V.Vector Integer)

-- | Stream of some computation which goes on until zero is found, and returns the argument if found.
--
-- Assumes that it starts from 1.
findZeroStream :: (Monad m) => (Int -> RatioResult) -> Stream (Of Int) m (Maybe (V.Vector Integer))
findZeroStream getMin =
  Stream.enumFrom (1 :: Int)
    & Stream.map detectZero
    & Stream.partitionEithers
    & Stream.head_
 where
  detectZero k = case getMin k of
    Arg 0 arg -> Left arg
    _ -> Right k

-- | Stream of some computation which goes on until Nothing is found, and returns the argument if found.
--
-- Assumes that it starts from 1.
findJustStream :: (Monad m) => (Int -> Maybe a) -> Stream (Of Int) m (Maybe a)
findJustStream fn =
  Stream.enumFrom (1 :: Int)
    & Stream.map detectJust
    & Stream.partitionEithers
    & Stream.head_
 where
  detectJust k = case fn k of
    Just v -> Left v
    Nothing -> Right k

-- | Finds in stream until cutoff is reached.
findUntilCutoff :: Int -> Stream (Of Int) Identity (Maybe (V.Vector Integer)) -> Maybe (V.Vector Integer)
findUntilCutoff cut stream =
  stream
    & Stream.dropWhile (<= cut)
    & Stream.head
    & runIdentity
    & Stream.snd'

data MinSearch v = MinSearch
  { computeInd :: Induction Integer v,
    minA :: Int -> Int -> Arg Rational (V.Vector Integer, V.Vector Integer),
    computeB :: V.Vector Integer -> V.Vector Integer -> Rational,
    minAwith :: IntFnInd v -> Int -> Rational,
    maxBwith :: IntFnInd v -> Int -> Rational,
    -- | The representative to take minimum of.
    representative :: IntFnInd v -> Rational
  }

-- | Search the minimum of a form V = A_i (1 - B_i / n_i).
--
-- The last parameter is length.
searchMinWith ::
  forall v.
  MinSearch v ->
  Int ->
  RatioResult
searchMinWith minSearch len = runST $ do
  minRef <- newSTRef $ fromMaybe (error "error while finding minimal candidate") minCandidate
  searchRanges (minSearcher minRef) len
  readSTRef minRef
 where
  minSearcher :: STRef s RatioResult -> SearchIntFn (ST s) v ()
  minSearcher minRef =
    SearchIntFn
      { fnInduct = inductive minSearch.computeInd,
        selectNext = selectFromBounds $ \v_L len_ _i -> do
          let minA = minSearch.minAwith v_L len_
              maxB = minSearch.maxBwith v_L len_
          Arg curMin _ <- readSTRef minRef
          let boundRadius = maxB / (1 - curMin / minA)
              depBounds = innerInt $ deltaFrom 0 boundRadius
              indepBounds = outerInt $ deltaFrom 0 maxB -- can be slightly bigger
          pure (depBounds `intersect` indepBounds),
        summarize =
          Stream.map (\ev -> Arg (minSearch.representative ev) (V.fromList $ inputs ev))
            >>> Stream.mapM (updateAndGetMin minRef)
            >>> Stream.takeWhile (\v -> argValue v /= 0)
            >>> Stream.effects
      }

  updateAndGetMin minRef new = do
    old <- readSTRef minRef
    if old <= new
      then pure old
      else new <$ writeSTRef minRef new

  minCandidate :: Maybe RatioResult
  minCandidate =
    runIdentity . Stream.minimum_ $ Stream.for (Stream.each [1 .. len]) $ \i -> do
      let Arg vA (n_L, n_R) = minSearch.minA len i
          vB = minSearch.computeB n_L n_R
      Stream.for (Stream.filter (/= 0) $ Stream.each [floor vB, ceiling vB]) $ \n_i -> do
        let vV = vA * (1 - vB / fromIntegral n_i)
        Stream.yield $ Arg (abs vV) (n_L <> V.singleton n_i <> n_R)

data SearchIntFn m v a = SearchIntFn
  { fnInduct :: Inductive Integer v,
    selectNext :: IntFnInd v -> Int -> Int -> Stream (Of (IntFnInd v)) m (),
    summarize :: Stream (Of (IntFnInd v)) m () -> m a
  }

-- | Search through integer-valued functions.
searchRanges :: forall m v a. (Monad m) => SearchIntFn m v a -> Int -> m a
searchRanges search len =
  search.summarize $ foldl' selectN_i (Stream.yield search.fnInduct) [1 .. len]
 where
  selectN_i stream_L i = Stream.for stream_L $ \v_L -> search.selectNext v_L len i

-- | Select nonzero integer and feed it in the given range.
selectFromBounds ::
  (Monad m) =>
  (IntFnInd v -> Int -> Int -> m (Range Integer)) ->
  IntFnInd v ->
  Int ->
  Int ->
  Stream (Of (IntFnInd v)) m ()
selectFromBounds getBnds v_L len i = effect $ do
  Stream.map v_L.next . rangeNonzeroStream <$> getBnds v_L len i

-- >>> Stream.toList_ $ rangeNonzeroStream (Range (-3) 6)
-- [1,-1,2,-2,3,-3,4,5,6]

-- >>> Stream.toList_ $ rangeNonzeroStream (Range 3 6)
-- [3,4,5,6]

rangeNonzeroStream :: (Monad m, Integral a) => Range a -> Stream (Of a) m ()
rangeNonzeroStream (Range minBnd maxBnd) =
  void $ Stream.mergeOn abs positives negatives
 where
  positives = Stream.takeWhile (<= maxBnd) $ Stream.iterate succ (max 1 minBnd)
  negatives = Stream.takeWhile (>= minBnd) $ Stream.iterate pred (min (-1) maxBnd)
