module Inductive (
  InductiveEval,
  inductive,
  next,
  nexts,
  previous,
  value,
  input,
  inputs,
) where

import Data.List
import Data.Sequence qualified as Seq

data InductiveResult a v
  = Initial !v
  | Next !a !v (InductiveResult a v)
  deriving (Show)

-- | Datatype for an inductive evaluation of [a] -> v.
data InductiveEval a v = InductiveEval
  { induction :: a -> InductiveEval a v -> v,
    initial :: v,
    evaluated :: Seq.Seq (a, v)
  }

instance (Show a, Show v) => Show (InductiveEval a v) where
  show :: InductiveEval a v -> String
  show eval = show eval.evaluated

inductive :: (a -> InductiveEval a v -> v) -> v -> InductiveEval a v
inductive induction initValue = InductiveEval{induction, initial = initValue, evaluated = Seq.empty}

next :: a -> InductiveEval a v -> InductiveEval a v
next inp prev =
  prev{evaluated = prev.evaluated Seq.|> (inp, prev.induction inp prev)}

-- Consecutive calls to 'next'
nexts :: [a] -> InductiveEval a v -> InductiveEval a v
nexts inps prev = foldl' (flip next) prev inps

previous :: InductiveEval a v -> Maybe (a, InductiveEval a v)
previous cur = case cur.evaluated of
  Seq.Empty -> Nothing
  prev Seq.:|> (inp, _) -> Just (inp, cur{evaluated = prev})

value :: InductiveEval a v -> v
value eval = case eval.evaluated of
  Seq.Empty -> eval.initial
  _ Seq.:|> (_, v) -> v

input :: InductiveEval a v -> Maybe a
input eval = case eval.evaluated of
  Seq.Empty -> Nothing
  _ Seq.:|> (inp, _) -> Just inp

-- |
-- > inputs (nexts inps (inductive ind v0)) == inps
inputs :: InductiveEval a v -> [a]
inputs = reverse . unfoldr previous
