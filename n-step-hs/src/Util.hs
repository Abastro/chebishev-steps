module Util (
  infiDiv,
  infiRecip,
  knownFinite,
  closestToInv,
) where

import Data.ExtendedReal
import Data.Semigroup (Arg (..))
import GHC.Stack

infiDiv :: (Eq a, Fractional a) => a -> a -> Extended a
infiDiv nom denom = if denom == 0 then PosInf else Finite (nom / denom)
infixl 7 `infiDiv`

infiRecip :: (Eq a, Fractional a) => a -> Extended a
infiRecip v = if v == 0 then PosInf else Finite (recip v)

knownFinite :: (HasCallStack) => Extended a -> a
knownFinite = \case
  Finite a -> a
  NegInf -> error "Unexpected negative infinity"
  PosInf -> error "Unexpected positive infinity"

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
