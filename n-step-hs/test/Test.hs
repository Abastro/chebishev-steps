module Main (main) where

import Chebyshev.Base
import Chebyshev.Fraction qualified as Fraction
import Chebyshev.Fraction.Reverse qualified as Reverse
import Chebyshev.Linear qualified as Linear
import Data.Ratio
import Data.Vector qualified as V
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "n-step-hs" [properties]

properties :: TestTree
properties =
  testGroup
    "properties"
    [ testGroup
        "Chebyshev"
        [ chebyshevBase,
          chebyshevLinear,
          chebyshevFraction,
          chebyshevFractionReverse
        ]
    ]

chebyshevBase :: TestTree
chebyshevBase =
  testGroup
    "Base"
    [ testProperty "chebyNormal is symmetric"
        $ \(NonZero u2) nzs ->
          let n_ = getNonZero <$> nzs
           in chebyNormal u2 n_ == chebyNormal u2 n_
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
           in Linear.constTerm u2 n_L n_R
                - (Linear.slopeTerm u2 n_L n_R / fromIntegral n_i)
                == chebyNormal u2 (V.toList n_),
      testProperty "findChebyshev must give a root"
        $ withMaxSuccess 20
        $ mapSize (`div` 3)
        $ \(NonZero u2) ->
          discardAfter 200000
            $ case Linear.findChebyshev u2 100 of
              Just n_ -> chebyNormal u2 (V.toList n_) == 0
              Nothing -> discard,
      testProperty "findChebyshev must give s_3 for roots of s_3"
        $ \(NonZero (Small q)) ->
          let u2 = 1 % q
           in maybe (-1) length (Linear.findChebyshev u2 5) == 2,
      testProperty "findChebyshev must give less than s_4 for roots of s_4"
        $ withMaxSuccess 30
        $ \(NonZero (Small a)) (NonZero (Small b)) ->
          discardAfter 200000
            $ let u2 = 1 % a + 1 % b
               in (u2 /= 0) ==> maybe (-1) length (Linear.findChebyshev u2 6) <= 3
    ]

chebyshevFraction :: TestTree
chebyshevFraction =
  testGroup
    "Fraction"
    [ testProperty "findChebyshev must give a root"
        $ withMaxSuccess 20
        $ mapSize (`div` 3)
        $ \(NonZero u2) ->
          discardAfter 200000
            $ case Fraction.findChebyshev u2 100 of
              Just n_ -> chebyNormal u2 (V.toList n_) == 0
              Nothing -> discard,
      testProperty "findChebyshev must give s_3 for roots of s_3"
        $ \(NonZero (Small q)) ->
          let u2 = 1 % q
           in maybe (-1) length (Fraction.findChebyshev u2 5) == 2,
      testProperty "findChebyshev must give less than s_4 for roots of s_4"
        $ withMaxSuccess 30
        $ \(NonZero (Small a)) (NonZero (Small b)) ->
          discardAfter 200000
            $ let u2 = 1 % a + 1 % b
               in (u2 /= 0) ==> maybe (-1) length (Fraction.findChebyshev u2 6) <= 3
    ]

chebyshevFractionReverse :: TestTree
chebyshevFractionReverse =
  testGroup
    "Fraction.Reverse"
    [ testProperty "findChebyshev must give a root"
        $ withMaxSuccess 20
        $ mapSize (`div` 3)
        $ \(NonZero u2) ->
          discardAfter 200000
            $ case Reverse.findChebyshev u2 100 of
              Just n_ -> chebyNormal u2 (V.toList n_) == 0
              Nothing -> discard,
      testProperty "findChebyshev must give s_3 for roots of s_3"
        $ \(NonZero (Small q)) ->
          let u2 = 1 % q
           in maybe (-1) length (Reverse.findChebyshev u2 5) == 2,
      testProperty "findChebyshev must give less than s_4 for roots of s_4"
        $ withMaxSuccess 30
        $ \(NonZero (Small a)) (NonZero (Small b)) ->
          discardAfter 200000
            $ let u2 = 1 % a + 1 % b
               in (u2 /= 0) ==> maybe (-1) length (Reverse.findChebyshev u2 6) <= 3
    ]
