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
import Streaming
import Streaming.Prelude qualified as Stream
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

inorder :: (Monad m) => Tree a -> Stream (Of a) m ()
inorder = Stream.concat . foldTree glue
 where
  glue (m, lrows, rrows) = Stream.cons [m] $ Stream.zipWith (++) lrows rrows

-- >>> Stream.toList_ $ Stream.take 24 $ inorder @Identity fareyTree
-- Identity [1 % 2,1 % 3,2 % 3,1 % 4,2 % 5,3 % 5,3 % 4,1 % 5,2 % 7,3 % 8,3 % 7,4 % 7,5 % 8,5 % 7,4 % 5,1 % 6,2 % 9,3 % 11,3 % 10,4 % 11,5 % 13,5 % 12,4 % 9,5 % 9]

-- | Farey sequences concatenated, with duplicates removed
fareySeqCat :: (Monad m) => Stream (Of Rational) m ()
fareySeqCat =
  Stream.for (Stream.enumFrom 1) $ \q -> do
    Stream.filter (\r -> denominator r == q) $ Stream.map (% q) $ Stream.each [1 .. (q - 1)]

-- >>> Stream.toList_ $ Stream.take 24 $ fareySeqCat @Identity
-- Identity [1 % 2,1 % 3,2 % 3,1 % 4,3 % 4,1 % 5,2 % 5,3 % 5,4 % 5,1 % 6,5 % 6,1 % 7,2 % 7,3 % 7,4 % 7,5 % 7,6 % 7,1 % 8,3 % 8,5 % 8,7 % 8,1 % 9,2 % 9,4 % 9]

-- Third is the currently checked degree.
type ExploreResult = (Rational, Int -> Projective Rational, Int)

-- | Enumerate zeros of chebyshev, along with its degrees.
enumerateChebyZeros :: Stream (Of (Int, Rational)) Identity ()
enumerateChebyZeros =
  Stream.effects $ Stream.iterateM pass (pure $ Stream.map (beginExplore . (* 4)) fareySeqCat)
 where
  argVal (Arg m _) = m
  beginExplore u2 = (u2, argVal . continuedFracMax Indefinite u2, 1)

  -- Describes certain pass until retrying from the former unsolved rational again.
  -- The parameter stores the maximums for those where zeros are not yet found.
  pass ::
    Stream (Of ExploreResult) Identity () ->
    Stream (Of (Int, Rational)) Identity (Stream (Of ExploreResult) Identity ())
  pass pending =
    pending
      & hoist lift
      & Stream.mapM (\elt -> (elt,) <$> get)
      & Stream.span condition -- Just wiring ;/
      & Stream.map fst
      & fmap (hoist (`evalStateT` Infinity) . Stream.map fst)
      & Stream.mapM decision
      & (`evalStateT` Infinity) . distribute
      & Stream.partitionEithers
      & Stream.toList
      & fmap (\(left :> out) -> Stream.each left <> out)

  condition ((_, compute, k), b) = compute k <= b

  decision (u2, compute, k) = case compute (k + 1) of
    Infinity -> pure $ Right (k + 1, u2)
    Finite v -> do
      get >>= \case
        Infinity -> put (Finite v)
        Finite _ -> pure ()
      pure $ Left (u2, compute, k + 1)
