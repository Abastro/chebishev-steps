module Main (main) where

import Chebyshev.Base
import Chebyshev.Linear
import Data.Ratio
import Data.Vector qualified as V
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "n-step-hs" [properties]

properties :: TestTree
properties = testGroup "properties" [testGroup "Chebyshev" [chebyshevBase, chebyshevLinear]]

chebyshevBase :: TestTree
chebyshevBase =
  testGroup
    "Base"
    [ testProperty "chebyNormal is symmetric"
        $ \(NonZero u2) nzs ->
          let n_ = V.fromList $ getNonZero <$> nzs
           in chebyNormal u2 n_ == chebyNormal u2 (V.reverse n_)
    ]

chebyshevLinear :: TestTree
chebyshevLinear =
  testGroup
    "Linear"
    [ testProperty "chebyNormal is constTerm + slopeTerm / n_i"
        $ \(NonZero u2) leftNZ (NonZero n_i) rightNZ ->
          let n_L = V.fromList $ getNonZero <$> leftNZ
              n_R = V.fromList $ getNonZero <$> rightNZ
              n_ = n_L <> V.singleton n_i <> n_R
           in constTerm u2 n_L n_R
                - (slopeTerm u2 n_L n_R / fromIntegral n_i)
                == chebyNormal u2 n_,
      testProperty "findMinimalChebyshev must give a root"
        $ withMaxSuccess 20
        $ \(NonZero (Small p)) (NonZero (Small q)) ->
          discardAfter 200000
            $ let u2 = p % q
               in case findMinimalChebyshev u2 100 of
                    Just n_ -> chebyNormal u2 n_ == 0
                    Nothing -> discard,
      testProperty "findMinimalChebyshev must give s_3 for roots of s_3"
        $ \(NonZero (Small q)) ->
          let u2 = 1 % q
           in maybe (-1) length (findMinimalChebyshev u2 5) == 2,
      testProperty "findMinimalChebyshev must give less than s_4 for roots of s_4"
        $ withMaxSuccess 30
        $ \(NonZero (Small a)) (NonZero (Small b)) ->
          discardAfter 200000
            $ let u2 = 1 % a + 1 % b
               in (u2 /= 0) ==> maybe (-1) length (findMinimalChebyshev u2 6) <= 3
    ]
