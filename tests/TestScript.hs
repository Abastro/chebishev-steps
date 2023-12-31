module TestScript where

import Control.Monad
import Data.Maybe
import Data.Ratio
import Data.Semigroup

inversesOver :: Rational -> [Rational]
inversesOver val = undefined

safeDiv :: (Eq a, Fractional a) => a -> a -> [a]
safeDiv n m = [n / m | m /= 0]

s6LHS, s6RHS :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational
s6LHS (v :: Rational) m1 m2 m3 m4 = (v - m1 - m2) * (v - m3 - m4)
s6RHS (v :: Rational) m1 m2 m3 m4 = m2 * m3

closest1Step :: Rational -> Arg (Rational, Rational) Rational
closest1Step v =
  if 1 % lessK == v
    then Arg (1 % lessK, 0) 0
    else minimum @[] $ do
      k1 <- [1 .. pred $ (lessK + 1) * 2]
      let Arg k2Inv delta = closestDelta $ abs (v - 1 % k1)
      pure $ Arg (1 % k1, signum (v - 1 % k1) * k2Inv) delta
 where
  -- v is in (1 / (lessK + 1), 1 / lessK].
  -- k1 should be smaller than (lessK + 1) * 2.
  lessK = floor (1 / v)

step2a2a3 :: Rational -> [Rational]
step2a2a3 v = do
  undefined

-- * Constraint: lambda > 0

inverses :: [Rational]
inverses = [1 % m | m <- [1 ..]]

-- TODO Finish and prepare s_6

-- >>> take 5 inverses
-- [1 % 1,1 % 2,1 % 3,1 % 4,1 % 5]

s5Poly :: Rational -> Rational -> Rational -> Rational -> Rational
s5Poly (lam :: Rational) m1 m2 m3 = (lam - m1) * (lam - m3) - lam * m2

s5Diff :: Rational -> Rational -> Rational -> Rational -> Rational
s5Diff (lam :: Rational) m1 m2 m3 = abs (m2 * lam) - abs (lam - m1) * abs (lam - m3)

closestDelta :: Rational -> Arg Rational Rational
closestDelta (lam :: Rational) =
  minimum $ do
    k <- [less, less + 1]
    guard (k /= 0)
    invK <- safeDiv 1 (fromIntegral k)
    let delta = abs $ lam - invK
    guard (delta /= 0)
    pure $ Arg (1 % k) delta
 where
  less = floor (1 / lam)

-- >>> closestDelta (3/2)
-- Arg (1 % 1) (1 % 2)
-- >>> closestDelta (2/7)
-- Arg (1 % 4) (1 % 28)
-- >>> closestDelta (1/2)
-- Arg (1 % 3) (1 % 6)

m2Cands :: Rational -> [Rational]
m2Cands lam = takeWhile (\m2 -> s5Diff lam mMin m2 mMin >= 0) inverses
 where
  Arg mMin _ = closestDelta lam

-- >>> m2Cands (3/2)
-- [1 % 1,1 % 2,1 % 3,1 % 4,1 % 5,1 % 6]
-- >>> m2Cands (5/3)
-- [1 % 1,1 % 2,1 % 3]
-- >>> m2Cands 3
-- []

rationalsUntil :: Integer -> [Rational]
rationalsUntil (n :: Integer) = [num % denom | denom <- [1 .. n], num <- [1 .. n], gcd denom num == 1]

-- >>> rationalsUntil 5
-- [1 % 1,2 % 1,3 % 1,4 % 1,5 % 1,1 % 2,3 % 2,5 % 2,1 % 3,2 % 3,4 % 3,5 % 3,1 % 4,3 % 4,5 % 4,1 % 5,2 % 5,3 % 5,4 % 5]

-- >>> m2Cands <$> rationalsUntil 5
-- [[1 % 1,1 % 2,1 % 3,1 % 4],[1 % 1,1 % 2],[],[],[],[1 % 1,1 % 2,1 % 3,1 % 4,1 % 5,1 % 6,1 % 7,1 % 8,1 % 9,1 % 10,1 % 11,1 % 12,1 % 13,1 % 14,1 % 15,1 % 16,1 % 17,1 % 18],[1 % 1,1 % 2,1 % 3,1 % 4,1 % 5,1 % 6],[1 % 1],[1 % 1,1 % 2,1 % 3,1 % 4,1 % 5,1 % 6,1 % 7,1 % 8,1 % 9,1 % 10,1 % 11,1 % 12,1 % 13,1 % 14,1 % 15,1 % 16,1 % 17,1 % 18,1 % 19,1 % 20,1 % 21,1 % 22,1 % 23,1 % 24,1 % 25,1 % 26,1 % 27,1 % 28,1 % 29,1 % 30,1 % 31,1 % 32,1 % 33,1 % 34,1 % 35,1 % 36,1 % 37,1 % 38,1 % 39,1 % 40,1 % 41,1 % 42,1 % 43,1 % 44,1 % 45,1 % 46,1 % 47,1 % 48],[1 % 1,1 % 2,1 % 3,1 % 4,1 % 5,1 % 6,1 % 7,1 % 8,1 % 9,1 % 10,1 % 11,1 % 12,1 % 13,1 % 14,1 % 15,1 % 16,1 % 17,1 % 18,1 % 19,1 % 20,1 % 21,1 % 22,1 % 23,1 % 24],[1 % 1,1 % 2,1 % 3,1 % 4,1 % 5,1 % 6,1 % 7,1 % 8,1 % 9,1 % 10,1 % 11,1 % 12],[1 % 1,1 % 2,1 % 3],[1 % 1,1 % 2,1 % 3,1 % 4,1 % 5,1 % 6,1 % 7,1 % 8,1 % 9,1 % 10,1 % 11,1 % 12,1 % 13,1 % 14,1 % 15,1 % 16,1 % 17,1 % 18,1 % 19,1 % 20,1 % 21,1 % 22,1 % 23,1 % 24,1 % 25,1 % 26,1 % 27,1 % 28,1 % 29,1 % 30,1 % 31,1 % 32,1 % 33,1 % 34,1 % 35,1 % 36,1 % 37,1 % 38,1 % 39,1 % 40,1 % 41,1 % 42,1 % 43,1 % 44,1 % 45,1 % 46,1 % 47,1 % 48,1 % 49,1 % 50,1 % 51,1 % 52,1 % 53,1 % 54,1 % 55,1 % 56,1 % 57,1 % 58,1 % 59,1 % 60,1 % 61,1 % 62,1 % 63,1 % 64,1 % 65,1 % 66,1 % 67,1 % 68,1 % 69,1 % 70,1 % 71,1 % 72,1 % 73,1 % 74,1 % 75,1 % 76,1 % 77,1 % 78,1 % 79,1 % 80,1 % 81,1 % 82,1 % 83,1 % 84,1 % 85,1 % 86,1 % 87,1 % 88,1 % 89,1 % 90,1 % 91,1 % 92,1 % 93,1 % 94,1 % 95,1 % 96,1 % 97,1 % 98,1 % 99,1 % 100],[1 % 1,1 % 2,1 % 3,1 % 4,1 % 5,1 % 6,1 % 7,1 % 8,1 % 9,1 % 10,1 % 11,1 % 12],[1 % 1,1 % 2,1 % 3,1 % 4,1 % 5,1 % 6,1 % 7,1 % 8,1 % 9,1 % 10,1 % 11,1 % 12,1 % 13,1 % 14,1 % 15,1 % 16,1 % 17,1 % 18,1 % 19,1 % 20],[1 % 1,1 % 2,1 % 3,1 % 4,1 % 5,1 % 6,1 % 7,1 % 8,1 % 9,1 % 10,1 % 11,1 % 12,1 % 13,1 % 14,1 % 15,1 % 16,1 % 17,1 % 18,1 % 19,1 % 20,1 % 21,1 % 22,1 % 23,1 % 24,1 % 25,1 % 26,1 % 27,1 % 28,1 % 29,1 % 30,1 % 31,1 % 32,1 % 33,1 % 34,1 % 35,1 % 36,1 % 37,1 % 38,1 % 39,1 % 40,1 % 41,1 % 42,1 % 43,1 % 44,1 % 45,1 % 46,1 % 47,1 % 48,1 % 49,1 % 50,1 % 51,1 % 52,1 % 53,1 % 54,1 % 55,1 % 56,1 % 57,1 % 58,1 % 59,1 % 60,1 % 61,1 % 62,1 % 63,1 % 64,1 % 65,1 % 66,1 % 67,1 % 68,1 % 69,1 % 70,1 % 71,1 % 72,1 % 73,1 % 74,1 % 75,1 % 76,1 % 77,1 % 78,1 % 79,1 % 80,1 % 81,1 % 82,1 % 83,1 % 84,1 % 85,1 % 86,1 % 87,1 % 88,1 % 89,1 % 90,1 % 91,1 % 92,1 % 93,1 % 94,1 % 95,1 % 96,1 % 97,1 % 98,1 % 99,1 % 100,1 % 101,1 % 102,1 % 103,1 % 104,1 % 105,1 % 106,1 % 107,1 % 108,1 % 109,1 % 110,1 % 111,1 % 112,1 % 113,1 % 114,1 % 115,1 % 116,1 % 117,1 % 118,1 % 119,1 % 120,1 % 121,1 % 122,1 % 123,1 % 124,1 % 125,1 % 126,1 % 127,1 % 128,1 % 129,1 % 130,1 % 131,1 % 132,1 % 133,1 % 134,1 % 135,1 % 136,1 % 137,1 % 138,1 % 139,1 % 140,1 % 141,1 % 142,1 % 143,1 % 144,1 % 145,1 % 146,1 % 147,1 % 148,1 % 149,1 % 150,1 % 151,1 % 152,1 % 153,1 % 154,1 % 155,1 % 156,1 % 157,1 % 158,1 % 159,1 % 160,1 % 161,1 % 162,1 % 163,1 % 164,1 % 165,1 % 166,1 % 167,1 % 168,1 % 169,1 % 170,1 % 171,1 % 172,1 % 173,1 % 174,1 % 175,1 % 176,1 % 177,1 % 178,1 % 179,1 % 180],[1 % 1,1 % 2,1 % 3,1 % 4,1 % 5,1 % 6,1 % 7,1 % 8,1 % 9,1 % 10,1 % 11,1 % 12,1 % 13,1 % 14,1 % 15,1 % 16,1 % 17,1 % 18,1 % 19,1 % 20,1 % 21,1 % 22,1 % 23,1 % 24,1 % 25,1 % 26,1 % 27,1 % 28,1 % 29,1 % 30,1 % 31,1 % 32,1 % 33,1 % 34,1 % 35,1 % 36,1 % 37,1 % 38,1 % 39,1 % 40,1 % 41,1 % 42,1 % 43,1 % 44,1 % 45,1 % 46,1 % 47,1 % 48,1 % 49,1 % 50,1 % 51,1 % 52,1 % 53,1 % 54,1 % 55,1 % 56,1 % 57,1 % 58,1 % 59,1 % 60,1 % 61,1 % 62,1 % 63,1 % 64,1 % 65,1 % 66,1 % 67,1 % 68,1 % 69,1 % 70,1 % 71,1 % 72,1 % 73,1 % 74,1 % 75,1 % 76,1 % 77,1 % 78,1 % 79,1 % 80,1 % 81,1 % 82,1 % 83,1 % 84,1 % 85,1 % 86,1 % 87,1 % 88,1 % 89,1 % 90],[1 % 1,1 % 2,1 % 3,1 % 4,1 % 5,1 % 6,1 % 7,1 % 8,1 % 9,1 % 10,1 % 11,1 % 12,1 % 13,1 % 14,1 % 15,1 % 16,1 % 17,1 % 18,1 % 19,1 % 20,1 % 21,1 % 22,1 % 23,1 % 24,1 % 25,1 % 26,1 % 27,1 % 28,1 % 29,1 % 30,1 % 31,1 % 32,1 % 33,1 % 34,1 % 35,1 % 36,1 % 37,1 % 38,1 % 39,1 % 40,1 % 41,1 % 42,1 % 43,1 % 44,1 % 45,1 % 46,1 % 47,1 % 48,1 % 49,1 % 50,1 % 51,1 % 52,1 % 53,1 % 54,1 % 55,1 % 56,1 % 57,1 % 58,1 % 59,1 % 60],[1 % 1,1 % 2,1 % 3,1 % 4,1 % 5,1 % 6,1 % 7,1 % 8]]

m1Cands :: Rational -> Rational -> [Rational]
m1Cands lam m2 = case lam `compare` abs m2 of
  GT -> takeWhile (\m1 -> s5Diff lam m1 m2 m1 >= 0) inverses
  LT -> takeWhile (\m1 -> s5Diff lam m1 m2 m1 <= 0) $ map negate inverses
  EQ -> [1 % k1 | let k2 = numerator $ 1 / lam, k1 <- [1 .. k2 - 1]] -- known case
  -- m2 < 0 case not covered

allMs :: Rational -> [(Rational, Rational, Rational)]
allMs lam = do
  m2 <- m2Cands lam
  m1 <- m1Cands lam m2
  m3 <- (lam -) <$> safeDiv (m2 * lam) (lam - m1)
  guard $ abs (numerator m3) == 1
  pure (m1, m2, m3)

-- >>> allMs (2/3)
-- [(1 % 3,1 % 2,(-1) % 3),(1 % 5,1 % 2,(-1) % 21),(1 % 7,1 % 2,1 % 33),(1 % 9,1 % 2,1 % 15),(1 % 2,1 % 4,(-1) % 3),(1 % 3,1 % 4,1 % 6),(1 % 1,1 % 6,1 % 1),(1 % 3,1 % 6,1 % 3)]
-- >>> allMs (3/2)
-- [(1 % 1,1 % 3,1 % 2),(1 % 1,1 % 6,1 % 1)]
-- >>> allMs 2
-- [(1 % 1,1 % 2,1 % 1)]

-- >>> allMs 1
-- [(1 % 3,1 % 2,1 % 4),(1 % 2,1 % 3,1 % 3),(1 % 2,1 % 4,1 % 2)]
-- >>> allMs (1/2)
-- [((-1) % 1,1 % 1,1 % 6),((-1) % 3,1 % 1,(-1) % 10),((-1) % 4,1 % 1,(-1) % 6),(1 % 1,1 % 2,1 % 1)]
-- >>> allMs (1/3)
-- [((-1) % 1,1 % 1,1 % 12),((-1) % 2,1 % 1,(-1) % 15),((-1) % 3,1 % 1,(-1) % 6),((-1) % 3,1 % 2,1 % 12),((-1) % 4,1 % 2,1 % 21),((-1) % 5,1 % 2,1 % 48),((-1) % 7,1 % 2,(-1) % 60),((-1) % 8,1 % 2,(-1) % 33),((-1) % 9,1 % 2,(-1) % 24),((-1) % 12,1 % 2,(-1) % 15),(1 % 1,1 % 3,1 % 2),(1 % 2,1 % 3,1 % 1)]

verifyAllMs :: Rational -> Bool
verifyAllMs lam = all (verify lam) (allMs lam)
 where
  verify (lam :: Rational) (m1, m2, m3) = lam ^ 2 - (m1 + m2 + m3) * lam + m1 * m3 == 0

-- >>> verifyAllMs <$> rationalsUntil 10
-- [True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]

-- TODO Finding the matching n1, n2, n3, n4
-- TODO Will be better to approach as 1/k1, 1/k2, 1/k3

listings :: Integer -> [(Rational, (Rational, Rational, Rational))]
listings bound = do
  lam <- rationalsUntil bound
  ms : _ <- pure $ allMs lam
  pure (lam, ms)
