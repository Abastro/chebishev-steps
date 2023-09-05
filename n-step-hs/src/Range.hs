module Range (
  Range (..),
  inRange,
  intersect,
  deltaFrom,
  innerInt,
  outerInt,
  higherThan,
) where

-- | Denotes inclusive range from lower bound to higher bound.
data Range a = Range !a !a
  deriving (Eq, Show, Functor)

inRange :: (Ord a) => a -> Range a -> Bool
inRange v (Range low high) = v >= low && v <= high

intersect :: (Ord a) => Range a -> Range a -> Range a
intersect (Range l1 h1) (Range l2 h2) = Range (max l1 l2) (min h1 h2)

deltaFrom :: (Num a) => a -> a -> Range a
deltaFrom origin delta = Range (origin - delta) (origin + delta)

innerInt :: (RealFrac a, Integral b) => Range a -> Range b
innerInt (Range low high) = Range (ceiling low) (floor high)

outerInt :: (RealFrac a, Integral b) => Range a -> Range b
outerInt (Range low high) = Range (floor low) (ceiling high)

higherThan :: (Ord a) => a -> Range a -> Range a
higherThan lb (Range low high) = Range (max lb low) high
