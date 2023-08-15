module Main (main) where

import Chebyshev.Fraction qualified as Fraction
import Chebyshev.Fraction qualified as Linear
import Control.Monad
import Data.Ratio
import Options.Applicative
import Streamly.Prelude qualified as Stream
import Text.Printf

data Method = Linear | Fraction deriving (Show)

computeAndPrint :: Method -> Word -> Rational -> IO ()
computeAndPrint method cutoff u2 = case findChebyshev u2 cutoff of
  Just n_ -> printf "%s: root for %s\n" (show u2) (show n_)
  Nothing -> printf "%s: not a root under s_%d\n" (show u2) cutoff
 where
  findChebyshev = case method of
    Linear -> Linear.findChebyshev
    Fraction -> Fraction.findChebyshev

data Opts = Opts
  { command :: !Command,
    method :: !Method
  }

data Command
  = ComputeFor !Rational
  | ExhaustDenominator !Word !Integer !(Maybe FilePath)

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
                <*> argument auto (metavar "DENOMINATOR")
                <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE"))
            )
          $ progDesc "exhaustively compute for given denominator"
      ]

parseOptions :: ParserInfo Opts
parseOptions = info ((Opts <$> parseCommands <*> methodFlag) <**> helper) fullDesc
 where
  methodFlag = flag Fraction Linear (long "linear" <> help "evaluate in linear method")

-- TODO Output
main :: IO ()
main = do
  opts <- execParser parseOptions
  printf "Method: %s\n" (show opts.method)
  case opts.command of
    ComputeFor u2 -> computeAndPrint opts.method 100 u2
    ExhaustDenominator cutoff denom _ -> do
      Stream.drain $ Stream.fromAsync $ do
        nom <- Stream.enumerateFromTo 1 (pred $ denom * 4)
        let u2 = nom % denom
        when (denominator u2 == denom)
          $ Stream.fromEffect
          $ computeAndPrint opts.method cutoff (nom % denom)
