module Util (closestToInv) where

import Data.Semigroup (Arg (..))

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
--
closestToInv :: Rational -> Int
closestToInv r = goal
 where
  Arg _ goal = minimum $ do
    let cand = floor (1 / r)
    k <- filter (/= 0) [cand, cand + 1]
    pure $ Arg (abs $ r - 1 / fromIntegral k) k
