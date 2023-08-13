module Main (main) where

import Chebyshev
import Data.Vector qualified as V
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Chebyshev" $ do
    prop "chebyNormal is symmetric" $ \(NonZero u2) nzs -> do
      let n_ = V.fromList $ getNonZero <$> nzs
      chebyNormal u2 n_ `shouldBe` chebyNormal u2 (V.reverse n_)

    prop "chebyNormal is constTerm + slopeTerm / n_i"
      $ \(NonZero u2) leftNZ (NonZero ni) rightNZ -> do
        let n_L = V.fromList $ getNonZero <$> leftNZ
            n_R = V.fromList $ getNonZero <$> rightNZ
            n_ = n_L <> V.singleton ni <> n_R
        constTerm u2 n_L n_R - (slopeTerm u2 n_L n_R / fromIntegral ni)
          `shouldBe` chebyNormal u2 n_
