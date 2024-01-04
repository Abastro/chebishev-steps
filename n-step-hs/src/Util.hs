module Util (
  argValue,
  purely,
  impurely,
  Projective (..),
  knownFinite,
  closestToInv,
) where

import Data.Semigroup (Arg (..))
import GHC.Stack
import Fold.Pure.Type as Pure
import Fold.Effectful.Type as Eff

argValue :: Arg v a -> v
argValue (Arg v _) = v

purely :: (forall x. (x -> a -> x) -> x -> (x -> b) -> r) -> Fold a b -> r
purely cont (Fold{Pure.initial, Pure.step, Pure.extract}) = cont step initial extract

impurely :: (forall x. (x -> a -> m x) -> m x -> (x -> m b) -> r) -> EffectfulFold m a b -> r
impurely cont EffectfulFold{Eff.initial, Eff.step, Eff.extract} = cont step initial extract

-- | Describes a projective space over a number field.
--
-- Operations follow convention of riemann sphere,
-- which means this is not a proper field.
--
-- 
data Projective a = Finite a | Infinity
  deriving (Eq, Show)

instance (Num a) => Num (Projective a) where
  (+) :: (Num a) => Projective a -> Projective a -> Projective a
  Finite x + Finite y = Finite (x + y)
  Infinity + _ = Infinity
  _ + Infinity = Infinity
  (*) :: (Num a) => Projective a -> Projective a -> Projective a
  Finite x * Finite y = Finite (x * y)
  Infinity * _ = Infinity
  _ * Infinity = Infinity
  (-) :: (Num a) => Projective a -> Projective a -> Projective a
  Finite x - Finite y = Finite (x - y)
  Infinity - Finite _ = Infinity
  Finite _ - Infinity = Infinity
  Infinity - Infinity = error "cannot compute infinity - infinity"

  fromInteger :: (Num a) => Integer -> Projective a
  fromInteger n = Finite (fromInteger n)
  abs :: (Num a) => Projective a -> Projective a
  abs (Finite x) = Finite (abs x)
  abs Infinity = Infinity
  signum :: (Num a) => Projective a -> Projective a
  signum (Finite x) = Finite (signum x)
  signum Infinity = Infinity -- hell is sign of infinity point

instance (Eq a, Fractional a) => Fractional (Projective a) where
  fromRational :: (Fractional a) => Rational -> Projective a
  fromRational m = Finite (fromRational m)

  (/) :: (Eq a, Fractional a) => Projective a -> Projective a -> Projective a
  Finite 0 / Finite 0 = error "cannot compute 0 / 0"
  _ / Finite 0 = Infinity -- division by 0 gives infinity
  Infinity / Infinity = error "cannot compute infinity / infinity"
  _ / Infinity = Finite 0 -- division by infinity gives 0
  Finite x / Finite y = Finite (x / y)
  Infinity / Finite _ = Infinity

-- | Be cautious with this instance,
-- for order is not well-defined on projective numbers.
-- Specifically, -infinity = infinity.
deriving instance (Ord a) => Ord (Projective a)

knownFinite :: (HasCallStack) => Projective a -> a
knownFinite = \case
  Finite a -> a
  Infinity -> error "unexpected infinity"

-- | closestToInv r is 1/n closest to r where n is integer.
--
-- >>> closestToInv (1/3)
-- 3
--
-- >>> closestToInv (2/7)
-- 4
--
-- >>> closestToInv (3/8)
-- 3
--
-- >>> closestToInv (-1/4)
-- -4
--
-- >>> closestToInv (-2/7)
-- -4
--
-- >>> closestToInv (-2)
-- -1
closestToInv :: (Integral n) => Rational -> n
closestToInv r = goal
 where
  Arg _ goal = minimum $ do
    let cand = floor (1 / r)
    k <- filter (/= 0) [cand, cand + 1]
    pure $ Arg (abs $ r - 1 / fromIntegral k) k
