module Chebyshev.Enumerative (
  inorder,
  fareyTree,
  fareySeqCat,
  enumerateChebyZeros,
) where

import Chebyshev.Base
import Chebyshev.Fraction (continuedFracMax)
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.Function ((&))
import Data.Ratio
import Data.Semigroup (Arg (..))
import Debug.Trace
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Streamly.Data.StreamK qualified as StreamK
import Util

-- | Infinite binary tree
data Tree a = Node !a (Tree a) (Tree a)

foldTree :: ((a, c, c) -> c) -> Tree a -> c
foldTree f (Node a x y) = f (a, foldTree f x, foldTree f y)

unfoldTree :: (t -> (a, t, t)) -> t -> Tree a
unfoldTree f x = let (a, y, z) = f x in Node a (unfoldTree f y) (unfoldTree f z)

-- Farey tree does not work well..
fareyTree :: Tree Rational
fareyTree = unfoldTree step (0, 1)
 where
  step (l, r) = let m = adj l r in (m, (l, m), (m, r))
  adj l r = (numerator l + numerator r) % (denominator l + denominator r)

inorder :: (Monad m) => Tree a -> Stream.Stream m a
inorder = StreamK.toStream . concatRows . foldTree glue
 where
  concatRows = StreamK.concatMapWith StreamK.append (StreamK.fromStream . Stream.fromList)
  glue (m, lrows, rrows) = StreamK.cons [m] $ StreamK.zipWith (++) lrows rrows

-- >>> Stream.take 24 $ inorder @Identity fareyTree
-- fromList [1 % 2,1 % 3,2 % 3,1 % 4,2 % 5,3 % 5,3 % 4,1 % 5,2 % 7,3 % 8,3 % 7,4 % 7,5 % 8,5 % 7,4 % 5,1 % 6,2 % 9,3 % 11,3 % 10,4 % 11,5 % 13,5 % 12,4 % 9,5 % 9]

-- | Farey sequences concatenated, with duplicates removed
fareySeqCat :: (Monad m) => Stream.Stream m Rational
fareySeqCat =
  Stream.enumerateFrom 1
    & Stream.concatMap (\q -> Stream.filter (\r -> denominator r == q) $ (% q) <$> Stream.enumerateFromTo 1 (q - 1))

-- >>> Stream.take 24 $ fareySeqCat @Identity
-- fromList [1 % 2,1 % 3,2 % 3,1 % 4,3 % 4,1 % 5,2 % 5,3 % 5,4 % 5,1 % 6,5 % 6,1 % 7,2 % 7,3 % 7,4 % 7,5 % 7,6 % 7,1 % 8,3 % 8,5 % 8,7 % 8,1 % 9,2 % 9,4 % 9]

-- Third is the currently checked degree.
type ExploreResult = (Rational, Int -> Projective Rational, Int)

-- | Enumerate zeros of chebyshev, along with its degrees.
enumerateChebyZeros :: Stream.Stream Identity (Int, Rational)
enumerateChebyZeros =
  Stream.concatMap (const pass) (Stream.repeat ())
    & Stream.runStateT (pure $ beginExplore . (* 4) <$> fareySeqCat)
    & fmap snd
 where
  -- onPass pending = let (found, newPend) = pass pending in Just (found, newPend)
  argVal (Arg m _) = m
  beginExplore u2 = (u2, argVal . continuedFracMax Indefinite u2, 1)

  -- Describes certain pass until retrying from the former unsolved rational again.
  -- The parameter stores the maximums for those where zeros are not yet found.
  pass ::
    Stream.Stream (State (Stream.Stream Identity ExploreResult)) (Int, Rational)
  pass = Stream.concatEffect $ do
    pending <- get
    pure
      $ pending
      & Stream.liftInner
      & Stream.takeWhileM condition
      & Stream.mapM decision
      & Stream.runStateT (pure Infinity)
      & fmap snd
      & Stream.liftInner
      -- Yet to filter
      & Stream.scanMaybe (Fold.teeWith (\v -> fmap (v,)) (Fold.tee Fold.length (Fold.catRights Fold.toList)) Fold.latest)
      -- Apply update
      & Stream.mapM (\((len, st), elt) -> elt <$ put (Stream.fromList st `Stream.append` Stream.drop len pending))
      -- Filter is the last step
      & Stream.mapMaybe
        ( \case
            Left emit -> Just emit
            Right _ -> Nothing
        )
   where
    -- Problem: If a pass did not yield a new element,
    -- The state fails to update.
    condition (_, compute, k) = do
      b <- get
      pure (compute k <= b)

    decision (u2, compute, k) = case compute (k + 1) of
      Infinity -> pure $ Left (k + 1, u2)
      Finite v -> do
        get >>= \case
          Infinity -> put (Finite v)
          Finite _ -> pure ()
        pure $ Right (u2, compute, k + 1)
