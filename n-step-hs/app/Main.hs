module Main (main) where

import Chebyshev
import Control.Monad
import Data.Ratio
import Options.Applicative
import Streamly.Prelude qualified as Stream
import Text.Printf

computeAndPrint :: Int -> Rational -> IO ()
computeAndPrint cutoff u2 = case findMinimalChebyshev u2 cutoff of
  Just n_ -> printf "%s: root for %s\n" (show u2) (show n_)
  Nothing -> printf "%s: not a root under s_%d\n" (show u2) cutoff

data Command
  = ComputeFor !Rational
  | ExhaustDenominator !Int !Integer !(Maybe FilePath)

parseCommands :: Parser Command
parseCommands =
  subparser
    $ mconcat
      [ command "compute"
          $ info (ComputeFor <$> argument auto (metavar "u^2"))
          $ progDesc "compute for a certain number",
        command "exhaust"
          $ info
            ( ExhaustDenominator
                <$> argument auto (metavar "STEPS")
                <*> argument auto (metavar "u^2")
                <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE"))
            )
          $ progDesc "exhaustively compute for given denominator"
      ]

opts :: ParserInfo Command
opts = info (parseCommands <**> helper) fullDesc

-- TODO Output
main :: IO ()
main = do
  execParser opts >>= \case
    ComputeFor u2 -> computeAndPrint 100 u2
    ExhaustDenominator cutoff denom _ -> do
      Stream.drain $ Stream.fromAsync $ do
        nom <- Stream.enumerateFromTo 1 (pred $ denom * 4)
        let u2 = nom % denom
        when (denominator u2 == denom)
          $ Stream.fromEffect
          $ computeAndPrint cutoff (nom % denom)
