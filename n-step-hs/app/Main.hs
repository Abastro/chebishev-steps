module Main (main) where

import Chebyshev
import Control.Monad
import Data.Ratio
import Streamly.Prelude qualified as Stream
import System.Environment
import Text.Printf
import Text.Read

computeAndPrint :: Int -> Rational -> IO ()
computeAndPrint cutoff u2 = case findMinimalChebyshev u2 cutoff of
  Just n_ -> printf "%s: root for %s\n" (show u2) (show n_)
  Nothing -> printf "%s: not a root under s_%d\n" (show u2) cutoff

main :: IO ()
main =
  getArgs >>= \case
    [arg]
      | Just u2 <- readMaybe @Rational arg ->
          computeAndPrint 100 u2
    [arg1, arg2]
      | Just cutoff <- readMaybe @Int arg1,
        Just denom <- readMaybe @Integer arg2 -> do
          Stream.drain $ Stream.fromAsync $ do
            nom <- Stream.enumerateFromTo 1 (pred $ denom * 4)
            let u2 = nom % denom
            when (denominator u2 == denom)
              $ Stream.fromEffect
              $ computeAndPrint cutoff (nom % denom)
    _ -> putStrLn "pass the cutoff and denominator to check for."
