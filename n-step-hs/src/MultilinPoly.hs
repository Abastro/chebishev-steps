module MultilinPoly (
  Var,
  MultilinPoly (..),
  single,
  scale,
  multVar,
  substZero,
  evalWith,
) where

import Data.Bifunctor (Bifunctor (..))
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as M

type Var = Int

-- | Polynomial that is at most linear in each variable.
--
-- Each variable is denoted by a number.
newtype MultilinPoly n = MultilinPoly (M.Map IS.IntSet n)
  deriving (Show, Functor)

instance (Num n) => Semigroup (MultilinPoly n) where
  (<>) :: (Num n) => MultilinPoly n -> MultilinPoly n -> MultilinPoly n
  MultilinPoly poly <> MultilinPoly poly' = MultilinPoly (M.unionWith (+) poly poly')
instance (Num n) => Monoid (MultilinPoly n) where
  mempty :: (Num n) => MultilinPoly n
  mempty = MultilinPoly M.empty

single :: (Num n) => IS.IntSet -> MultilinPoly n
single monomial = MultilinPoly (M.singleton monomial 1)

scale :: (Num n) => n -> MultilinPoly n -> MultilinPoly n
scale s poly = (s *) <$> poly

-- | Multiply by a new variable.
multVar :: Var -> MultilinPoly n -> MultilinPoly n
multVar var (MultilinPoly poly) = MultilinPoly $ M.mapKeys (IS.insert var) poly

-- | Substitute zero to certain variable.
substZero :: (Num n) => Var -> MultilinPoly n -> MultilinPoly n
substZero var (MultilinPoly poly) =
  MultilinPoly $ M.filterWithKey (\monomial _ -> var `IS.notMember` monomial) poly

evalWith :: (Num n) => (IS.IntSet -> n) -> MultilinPoly n -> n
evalWith evalMono (MultilinPoly poly) = sum $ uncurry (*) . first evalMono <$> M.toList poly
