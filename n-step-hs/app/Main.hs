module Main (main) where

import Chebyshev.Fraction qualified as Fraction
import Chebyshev.Fraction.Reverse qualified as Reverse
import Chebyshev.Linear qualified as Linear
import Control.Concurrent
import Control.Exception (evaluate)
import Control.Monad.Identity
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Ratio
import Data.Set qualified as S
import Data.Vector qualified as V
import Inductive qualified
import Options.Applicative
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Streamly.Data.Stream.Prelude qualified as Stream
import Streamly.Data.StreamK qualified as StreamK
import System.Console.ANSI
import System.IO
import Text.Printf
import Util

data Method = Linear | Fraction | Reverse deriving (Show)
readMethod :: String -> Maybe Method
readMethod = \case
  "linear" -> Just Linear
  "fraction" -> Just Fraction
  "reverse" -> Just Reverse
  _ -> Nothing

data RootConvention = U2 | NegU2 deriving (Show)
data Opts = Opts
  { command :: !Command,
    method :: !Method,
    convention :: !RootConvention
  }

data Command
  = ComputeFor !Rational
  | ExhaustDenominator !Word !Integer !(Maybe FilePath)
  | AfterOnes !Word !Integer

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
          $ progDesc "exhaustively compute for given denominator",
        command "after-ones"
          $ info
            ( AfterOnes
                <$> argument auto (metavar "MAX_K")
                <*> argument auto (metavar "MAX_DENOMINATOR")
            )
          $ progDesc "compute cases after consecutive ones"
      ]

parseOptions :: ParserInfo Opts
parseOptions = info ((Opts <$> parseCommands <*> methodFlag <*> conventionFlag) <**> helper) fullDesc
 where
  methodFlag =
    option (maybeReader readMethod)
      $ value Fraction
      <> long "method"
      <> short 'm'
      <> help "evaluation method, linear|fraction|reverse"
  conventionFlag = flag NegU2 U2 (long "u2-conv" <> help "use u2 as passed root, instead of -u2")

main :: IO ()
main = do
  consoleLock <- newMVar ()
  opts <- execParser parseOptions
  printf "Method: %s\n" (show opts.method)
  printf "Convention: %s\n" (show opts.convention)
  case opts.command of
    ComputeFor root -> printResult stdout 100 root (finder opts.method (convertRoot root opts.convention) 100)
    ExhaustDenominator cutoff denom outFile -> withFileMay outFile $ \outHandle -> do
      let fracts = fractions denom
      remaining <- newMVar fracts
      for_ outHandle $ \handle -> hPutStrLn handle "rel #, k, seq n"

      -- Why do they make me do this
      StreamK.toStream (StreamK.fromFoldable fracts)
        & Stream.parConcatMap (Stream.ordered True) (Stream.fromEffect . evaluateAndPrint remaining)
        & Stream.mapM (`emitToFile` outHandle)
        & Stream.fold Fold.drain
     where
      evaluateAndPrint remaining root = do
        result <- evaluate (finder opts.method (convertRoot root opts.convention) cutoff)
        withMVar consoleLock $ \_ -> do
          clearFromCursorToScreenEnd
          printResult stdout cutoff root result
          curRemains <- modifyMVar remaining $ \old ->
            let new = S.delete root old in pure (new, new)
          printf "Remaining: %s\n" (show $ S.toList curRemains)
          cursorUpLine 1
        pure (root, result)

      emitToFile (root, result) = traverse_ $ \handle -> do
        printResult handle cutoff root result
        hFlush handle
    AfterOnes maxK maxDenom -> do
      let fractsFor denom = fractions denom <> S.map negate (fractions denom)
      Stream.enumerateFromTo 1 maxDenom
        & Stream.concatMap (StreamK.toStream . StreamK.fromFoldable . fractsFor)
        & Stream.concatMap (Stream.fromEffect . evaluateAndPrint)
        & Stream.fold Fold.drain
     where
      asInteger x = if denominator x == 1 then Just $ numerator x else Nothing
      checkAndReturn ev = case asInteger (knownFinite $ Inductive.value ev) of
        Just n -> Just (V.fromList $ Inductive.inputs ev <> [n])
        Nothing -> Nothing

      findN root =
        Stream.replicate (fromIntegral maxK - 2) (1 :: Integer)
          & Stream.scan
            ( Fold.foldl'
                (flip Inductive.next)
                (Fraction.initChebyRealFrac $ convertRoot root opts.convention)
            )
          & Stream.drop 1 -- First is always 0
          & Stream.fold (Fold.mapMaybe checkAndReturn Fold.one)

      evaluateAndPrint root = printResult stdout maxK root (runIdentity $ findN root)
 where
  withFileMay = \case
    Nothing -> \act -> act Nothing
    Just path -> \act -> withFile path WriteMode (act . Just)

  finder = \case
    Linear -> Linear.findChebyshev
    Fraction -> Fraction.findChebyshev
    Reverse -> Reverse.findChebyshev

  convertRoot root = \case
    U2 -> root
    NegU2 -> -root

  asFraction nom denom =
    let u2 = nom % denom
     in if denominator u2 == denom then Just u2 else Nothing

  fractions denom = S.fromList $ mapMaybe (`asFraction` denom) [1 .. pred $ denom * 4]

showFraction :: (Show a) => Ratio a -> String
showFraction frac = show (numerator frac) <> "/" <> show (denominator frac)

printResult :: Handle -> Word -> Rational -> Maybe (V.Vector Integer) -> IO ()
printResult handle cutoff u2 = \case
  Just n_ -> hPrintf handle "%s, \"s_%d\", \"%s\"\n" (showFraction u2) (length n_ + 1) (show n_)
  Nothing -> hPrintf handle "%s, \"> s_%d\", \"n/a\"\n" (showFraction u2) cutoff
