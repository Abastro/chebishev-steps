module Main (main) where

import Chebyshev.Base
import Chebyshev.Composite qualified as Composite
import Chebyshev.Fraction (SearchPass (..))
import Chebyshev.Fraction qualified as Fraction
import Chebyshev.Linear qualified as Linear
import Chebyshev.TFrac qualified as TFun
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
          chebyshevFractionNaive,
          chebyshevComposite,
          tfunZero
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
      testProperty "chebyZero must give a root"
        $ withMaxSuccess 20
        $ mapSize (`div` 3)
        $ \(NonZero u2) ->
          discardAfter 200000
            $ case findUntilCutoff 100 (Linear.chebyZero u2) of
              Just n_ -> chebyNormal u2 (V.toList n_) == 0
              Nothing -> discard,
      testProperty "chebyZero must give s_3 for roots of s_3"
        $ \(NonZero (Small q)) ->
          let u2 = 1 % q
           in maybe (-1) length (findUntilCutoff 5 $ Linear.chebyZero u2) == 2,
      testProperty "chebyZero must give less than s_4 for roots of s_4"
        $ withMaxSuccess 30
        $ \(NonZero (Small a)) (NonZero (Small b)) ->
          discardAfter 200000
            $ let u2 = 1 % a + 1 % b
               in (u2 /= 0) ==> maybe (-1) length (findUntilCutoff 6 $ Linear.chebyZero u2) <= 3
    ]

chebyshevFraction :: TestTree
chebyshevFraction =
  testGroup
    "Fraction"
    [ testProperty "chebyZero must give a root"
        $ withMaxSuccess 20
        $ mapSize (`div` 3)
        $ \(NonZero u2) ->
          discardAfter 200000
            $ case findUntilCutoff 100 (Fraction.chebyZero [Complete] u2) of
              Just n_ -> chebyNormal u2 (V.toList n_) == 0
              Nothing -> discard,
      testProperty "chebyZero must give s_3 for roots of s_3"
        $ \(NonZero (Small q)) ->
          let u2 = 1 % q
           in maybe (-1) length (findUntilCutoff 5 $ Fraction.chebyZero [Complete] u2) == 2,
      testProperty "Fraction.chebyZero should give equal length to Linear.chebyZero"
        $ withMaxSuccess 20
        $ mapSize (`div` 3)
        $ \(NonZero u2) ->
          discardAfter 200000
            $ let linearSol = findUntilCutoff 100 (Linear.chebyZero u2)
                  fractionSol = findUntilCutoff 100 (Fraction.chebyZero [Complete] u2)
               in fmap length linearSol == fmap length fractionSol
    ]

chebyshevFractionNaive :: TestTree
chebyshevFractionNaive =
  testGroup
    "Fraction.Naive"
    [ testProperty "chebyZero must give a root"
        $ withMaxSuccess 20
        $ mapSize (`div` 3)
        $ \(NonZero u2) ->
          discardAfter 200000
            $ case findUntilCutoff 100 (Fraction.chebyZero [Narrow] u2) of
              Just n_ -> chebyNormal u2 (V.toList n_) == 0
              Nothing -> discard,
      testProperty "chebyZero must give s_3 for roots of s_3"
        $ \(NonZero (Small q)) ->
          let u2 = 1 % q
           in maybe (-1) length (findUntilCutoff 5 $ Fraction.chebyZero [Narrow] u2) == 2
    ]

chebyshevComposite :: TestTree
chebyshevComposite =
  testGroup
    "Composite"
    [ testProperty "chebyZero must give a root"
        $ withMaxSuccess 20
        $ mapSize (`div` 3)
        $ \(NonZero u2) ->
          discardAfter 200000
            $ case findUntilCutoff 100 (findZeroStream $ Composite.chebyNormalMin u2) of
              Just n_ -> chebyNormal u2 (V.toList n_) == 0
              Nothing -> discard,
      testProperty "Composite.chebyZero should give equal length to Fraction.chebyZero"
        $ withMaxSuccess 20
        $ mapSize (`div` 3)
        $ \(NonZero u2) ->
          discardAfter 200000
            $ let fractionSol = findUntilCutoff 100 (Fraction.chebyZero [Complete] u2)
                  compositeSol = findUntilCutoff 100 (findZeroStream $ Composite.chebyNormalMin u2)
               in fmap length fractionSol == fmap length compositeSol
    ]

tfunZero :: TestTree
tfunZero =
  testGroup
    "TFrac"
    [ testProperty "tfunZero must give a root"
        $ withMaxSuccess 20
        $ mapSize (`div` 3)
        $ \(NonZero u2) ->
          discardAfter 200000
            $ case findUntilCutoff 100 (TFun.tfunZero u2) of
              Just n_ -> TFun.tfunNormal u2 (V.toList n_) == 0
              Nothing -> discard
    ]
