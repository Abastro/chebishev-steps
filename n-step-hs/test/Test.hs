module Main (main) where

import Chebyshev
import Data.Vector qualified as V
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Chebyshev" $ do
    describe "chebyNormal" $ do
      prop "chebyNormal is symmetric" $ \(NonZero u2) nzs -> do
        let n_ = V.fromList $ getNonZero <$> nzs
        chebyNormal u2 n_ `shouldBe` chebyNormal u2 (V.reverse n_)

      prop "chebyNormal is constTerm + slopeTerm / n_i"
        $ \(NonZero u2) leftNZ (NonZero n_i) rightNZ -> do
          let n_L = V.fromList $ getNonZero <$> leftNZ
              n_R = V.fromList $ getNonZero <$> rightNZ
              n_ = n_L <> V.singleton n_i <> n_R
          constTerm u2 n_L n_R - (slopeTerm u2 n_L n_R / fromIntegral n_i)
            `shouldBe` chebyNormal u2 n_
