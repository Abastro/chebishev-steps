module Main (main) where

import Chebyshev
import Data.Vector qualified as V
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

newtype SmallSized a = SmallSized a
  deriving (Show)
instance (Arbitrary a) => Arbitrary (SmallSized a) where
  arbitrary :: Gen (SmallSized a)
  arbitrary = scale (min 10) $ SmallSized <$> arbitrary

  shrink :: (Arbitrary a) => SmallSized a -> [SmallSized a]
  shrink (SmallSized a) = SmallSized <$> shrink a

main :: IO ()
main = hspec $ do
  describe "Chebyshev" $ do
    prop "sPolyNormal has inductive relationships with sPoly" $
      \(NonZero u2) (SmallSized leftNZ) (NonZero ni) (SmallSized rightNZ) -> do
        let lefts = getNonZero <$> leftNZ
            rights = getNonZero <$> rightNZ
        let combined = V.fromList lefts <> V.singleton ni <> V.fromList rights
            sk = sPolyNormal u2 combined
            si = sPolyNormal u2 (V.fromList lefts)
            sk_i = sPolyNormal u2 (V.fromList rights)
            sk0 = sPolyZeroNormal u2 combined (1 + length lefts)
        si * sk_i + sk0 / fromIntegral ni `shouldBe` sk
