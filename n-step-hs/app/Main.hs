module Main (main) where

import Chebyshev.Base
import Chebyshev.Composite qualified as Composite
import Chebyshev.Fraction qualified as Fraction
import Chebyshev.Linear qualified as Linear
import Chebyshev.TFun qualified as TFun
import Control.Concurrent
import Control.Exception (ErrorCall (..), evaluate, handle)
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

data Method
  = Linear
  | Fraction Fraction.SearchPass
  | TFun Fraction.SearchPass
  | TTilde
  | THat
  deriving (Show)

readMethod :: String -> Maybe Method
readMethod = \case
  "linear" -> Just Linear
  "fraction" -> Just (Fraction Fraction.Complete)
  "fraction-narrow" -> Just (Fraction Fraction.Narrow)
  "tfun" -> Just (TFun Fraction.Complete)
  "tfun-narrow" -> Just (TFun Fraction.Narrow)
  "ttilde" -> Just TTilde
  "that" -> Just THat
  _ -> Nothing

data RootConvention = U2 | NegU2 deriving (Show)
data Opts = Opts
  { command :: !Command,
    method :: !Method,
    convention :: !RootConvention,
    timeOut :: Maybe Int
  }

data Command
  = ComputeFor !Int !Rational
  | ExhaustDenominator !Int !Integer !(Maybe FilePath)
  | AfterOnes !Int !Integer

parseCommands :: Parser Command
parseCommands =
  subparser
    $ mconcat
      [ command "compute"
          $ info
            ( ComputeFor
                <$> argument auto (metavar "MAX_K")
                <*> argument auto (metavar "u^2")
            )
          $ progDesc "compute for a certain number",
        command "exhaust"
          $ info
            ( ExhaustDenominator
                <$> argument auto (metavar "MAX_K")
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
parseOptions = info (helper <*> (Opts <$> parseCommands <*> methodOpt <*> conventionFlag <*> timeoutOpt)) fullDesc
 where
  methodOpt =
    option (maybeReader readMethod)
      $ value (Fraction Fraction.Complete)
      <> long "method"
      <> short 'm'
      <> help "evaluation method"
  conventionFlag = flag NegU2 U2 (long "u2-conv" <> help "use u2 as passed root, instead of -u2")
  timeoutOpt = optional . option auto $ long "timeout" <> short 't' <> help "timeout deadline for evaluation"

main :: IO ()
main = do
  consoleLock <- newMVar ()
  opts <- execParser parseOptions
  printf "Method: %s\n" (show opts.method)
  printf "Convention: %s\n" (show opts.convention)
  case opts.command of
    -- "compute MAX_K ROOT"
    ComputeFor maxK root -> do
      Just result <- takeResult maxK opts.timeOut $ finder opts.method (convertRoot root opts.convention)
      printResult stdout root result

    -- "exhaust MAX_K DENOMINATOR"
    ExhaustDenominator maxK denom outFile -> withFileMay outFile $ \outHandle -> do
      let fracts = fractions denom
      remaining <- newMVar fracts
      for_ outHandle $ \out -> hPutStrLn out "rel #, k, seq n"

      -- Why do they make me do this
      StreamK.toStream (StreamK.fromFoldable fracts)
        & Stream.parConcatMap (Stream.ordered True) (Stream.fromEffect . evaluateAndPrint remaining)
        & Stream.mapM (`emitToFile` outHandle)
        & Stream.fold Fold.drain
     where
      evaluateAndPrint remaining root = handle (\(ErrorCall _) -> pure (root, Left (-1))) $ do
        Just res <- takeResult maxK opts.timeOut $ finder opts.method (convertRoot root opts.convention)
        result <- evaluate res
        withMVar consoleLock $ \_ -> do
          clearFromCursorToScreenEnd
          printResult stdout root result
          curRemains <- modifyMVar remaining $ \old ->
            let new = S.delete root old in pure (new, new)
          printf "Remaining: %s\n" (show $ S.toList curRemains)
          cursorUpLine 1
        pure (root, result)

      emitToFile (root, result) = traverse_ $ \out -> do
        printResult out root result
        hFlush out

    -- "after-ones MAX_K MAX_DENOMINATOR"
    AfterOnes maxK maxDenom -> do
      let fractsFor denom = fractions denom <> S.map negate (fractions denom)
      Stream.enumerateFromTo 1 maxDenom
        & Stream.concatMap (StreamK.toStream . StreamK.fromFoldable . fractsFor)
        & Stream.concatMap (Stream.fromEffect . evaluateAndPrint)
        & Stream.fold Fold.drain
     where
      asInteger x = if denominator x == 1 then Just $ numerator x else Nothing
      checkAndReturn ev = case asInteger (knownFinite ev.value) of
        Just n -> Just (V.fromList $ Inductive.inputs ev <> [n])
        Nothing -> Nothing

      findN root =
        Stream.replicate (fromIntegral maxK - 2) (1 :: Integer)
          & Stream.scan
            ( Fold.foldl'
                (\ev -> ev.next)
                (initContinuedFrac $ convertRoot root opts.convention)
            )
          & Stream.drop 1 -- First is always 0
          & Stream.fold (Fold.mapMaybe checkAndReturn Fold.one)

      evaluateAndPrint root = printResult stdout root $ do
        maybe (Left maxK) Right . runIdentity $ findN root
 where
  withFileMay = \case
    Nothing -> \act -> act Nothing
    Just path -> \act -> withFile path WriteMode (act . Just)

  finder = \case
    Linear -> Linear.chebyZero
    Fraction pass -> Fraction.chebyZero [pass]
    TFun pass -> TFun.tfunZero [pass]
    TTilde -> Composite.findJustStream . Composite.tildeZero [Fraction.Narrow]
    THat -> Composite.findJustStream . Composite.hatZero [Fraction.Narrow]
  takeResult maxK = \case
    Nothing -> Stream.fold Fold.latest . Stream.take maxK
    Just timeout ->
      Stream.fold Fold.one
        . Stream.sampleIntervalEnd (fromIntegral timeout)
        . Stream.take maxK

  convertRoot root = \case
    U2 -> root
    NegU2 -> -root

  asFraction nom denom =
    let u2 = nom % denom
     in if denominator u2 == denom then Just u2 else Nothing

  fractions denom = S.fromList $ mapMaybe (`asFraction` denom) [1 .. pred $ denom * 4]

showFraction :: Rational -> String
showFraction frac = show (numerator frac) <> "/" <> show (denominator frac)

printResult :: Handle -> Rational -> Either Int (V.Vector Integer) -> IO ()
printResult out u2 = \case
  Right n_ -> hPrintf out "%s, \"s_%d\", \"%s\"\n" (showFraction u2) (length n_ + 1) (show n_)
  Left triedK -> hPrintf out "%s, \"> s_%d\", \"n/a\"\n" (showFraction u2) triedK
