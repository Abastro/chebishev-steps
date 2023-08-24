module Inductive (
  InductiveEval,
  inductive,
  next,
  nexts,
  previous,
  value,
  valueAt,
  input,
  inputAt,
  inputs,
) where

import Data.Foldable (Foldable (..))
import Data.List
import Data.Sequence qualified as Seq

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

-- | Note that the initial value is 0.
valueAt :: Int -> InductiveEval a v -> Maybe v
valueAt 0 eval = Just eval.initial
valueAt i eval = snd <$> eval.evaluated Seq.!? (i - 1)

input :: InductiveEval a v -> Maybe a
input eval = case eval.evaluated of
  Seq.Empty -> Nothing
  _ Seq.:|> (inp, _) -> Just inp

-- | To match with valueAt, input starts at 1.
inputAt :: Int -> InductiveEval a v -> Maybe a
inputAt i eval = fst <$> eval.evaluated Seq.!? (i - 1)

-- |
-- > inputs (nexts inps (inductive ind v0)) == inps
inputs :: InductiveEval a v -> [a]
inputs eval = fst <$> toList eval.evaluated
