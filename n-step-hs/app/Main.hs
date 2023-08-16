module Main (main) where

import Chebyshev.Fraction qualified as Fraction
import Chebyshev.Linear qualified as Linear
import Control.Concurrent
import Control.Exception (evaluate)
import Data.Maybe
import Data.Ratio
import Data.Set qualified as S
import Data.Vector qualified as V
import Options.Applicative
import Streamly.Prelude qualified as Stream
import System.Console.ANSI
import System.IO
import Text.Printf

data Method = Linear | Fraction deriving (Show)
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

-- TODO Output file
main :: IO ()
main = do
  consoleLock <- newMVar ()
  opts <- execParser parseOptions
  printf "Method: %s\n" (show opts.method)
  case opts.command of
    ComputeFor u2 -> printResult stdout 100 u2 (finder opts.method u2 100)
    ExhaustDenominator cutoff denom outFile -> withFileMay outFile $ \outHandle -> do
      let fracts = fractions denom
      remaining <- newMVar fracts
      Stream.drain $ do
        (u2, result) <- Stream.fromAhead $ do
          u2 <- Stream.fromFoldable fracts
          Stream.fromEffect $ do
            result <- evaluate (finder opts.method u2 cutoff)
            withMVar consoleLock $ \_ -> do
              clearFromCursorToScreenEnd
              printResult stdout cutoff u2 result
              curRemains <- modifyMVar remaining $ \old ->
                let new = S.delete u2 old in pure (new, new)
              saveCursor
              printf "Remaining: %s\n" (show $ S.toList curRemains)
              restoreCursor
            pure (u2, result)
        handle <- Stream.fromFoldable outHandle
        Stream.fromEffect $ do
          printResult handle cutoff u2 result
          hFlush handle
 where
  withFileMay = \case
    Nothing -> \act -> act Nothing
    Just path -> \act -> withFile path WriteMode (act . Just)

  finder = \case
    Linear -> Linear.findChebyshev
    Fraction -> Fraction.findChebyshev

  asFraction nom denom =
    let u2 = nom % denom
     in if denominator u2 == denom then Just u2 else Nothing

  fractions denom = S.fromList $ mapMaybe (`asFraction` denom) [1 .. pred $ denom * 4]

printResult :: Handle -> Word -> Rational -> Maybe (V.Vector Integer) -> IO ()
printResult handle cutoff u2 = \case
  Just n_ -> hPrintf handle "%s, s_%d, \"%s\"\n" (showFraction u2) (length n_ + 1) (show n_)
  Nothing -> hPrintf handle "%s, > s_%d\n" (showFraction u2) cutoff
  where
    showFraction frac = show (numerator frac) <> "/" <> show (denominator frac)
