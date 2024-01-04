module Main (main) where

import Chebyshev.Base
import Chebyshev.Composite qualified as Composite
import Chebyshev.Fraction qualified as Fraction
import Chebyshev.Fraction.ChooseOne qualified as ChooseOne
import Chebyshev.Linear qualified as Linear
import Chebyshev.TFun qualified as TFun
import Control.Concurrent
import Control.Exception (ErrorCall (..), evaluate, handle)
import Control.Monad.Identity
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Set qualified as S
import Data.Vector qualified as V
import Options.Applicative
import Streaming.Prelude qualified as Stream
import System.Console.ANSI
import System.IO
import Text.Printf
import Text.Read
import Streaming

data Method
  = Linear
  | Fraction
  | TFun
  | TTilde
  | THat
  | Naive !Int
  deriving (Show)

readMethod :: String -> Maybe Method
readMethod = \case
  "linear" -> Just Linear
  "fraction" -> Just Fraction
  "tfun" -> Just TFun
  "ttilde" -> Just TTilde
  "that" -> Just THat
  m | Just d <- stripPrefix "naive" m -> Naive <$> readMaybe d
  _ -> Nothing

data RootConvention = U2 | NegU2 deriving (Show)
data Opts = Opts
  { command :: !Command,
    method :: !Method,
    convention :: !RootConvention,
    breadth :: Breadth,
    timeOut :: Maybe Int
  }

data Command
  = ComputeFor !Int !Rational
  | ExhaustDenominator !Int !Integer !(Maybe FilePath)

parseCommands :: Parser Command
parseCommands =
  subparser $
    mconcat
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
          $ progDesc "exhaustively compute for given denominator"
      ]

parseOptions :: ParserInfo Opts
parseOptions = info (helper <*> (Opts <$> parseCommands <*> methodOpt <*> conventionFlag <*> breadthOpt <*> timeoutOpt)) fullDesc
 where
  methodOpt =
    option (maybeReader readMethod) $
      value Fraction
        <> long "method"
        <> short 'm'
        <> help "evaluation method"
  conventionFlag = flag NegU2 U2 (long "u2-conv" <> help "use u2 as passed root, instead of -u2")
  breadthOpt = option (MaxBr <$> auto) $ value Indefinite <> long "breadth" <> short 'b' <> help "specify restriction on search breadth"
  timeoutOpt = optional . option auto $ long "timeout" <> short 't' <> help "timeout deadline for evaluation"

main :: IO ()
main = do
  consoleLock <- newMVar ()
  opts <- execParser parseOptions
  printf "Method: %s\n" (show opts.method)
  printf "Convention: %s\n" (show opts.convention)
  printf "Breadth: %s\n" (show opts.breadth)
  case opts.command of
    -- "compute MAX_K ROOT"
    ComputeFor maxK root -> do
      Just k :> ns <- takeResult maxK opts.timeOut $ do
        finder opts.breadth maxK opts.method (convertRoot root opts.convention)
      printResult stdout root (maybe (Left k) Right ns)

    -- "exhaust MAX_K DENOMINATOR"
    ExhaustDenominator maxK denom outFile -> withFileMay outFile $ \outHandle -> do
      let fracts = fractions denom
      remaining <- newMVar fracts
      for_ outHandle $ \out -> hPutStrLn out "rel #, k, seq n"

      -- TODO Parallel execution
      Stream.each fracts
        & Stream.mapM (evaluateAndPrint remaining)
        & Stream.mapM (`emitToFile` outHandle)
        & Stream.effects
     where
      evaluateAndPrint remaining root = handle (\(ErrorCall _) -> pure (root, Left (-1))) $ do
        Just k :> ns <- takeResult maxK opts.timeOut $ do
          finder opts.breadth maxK opts.method (convertRoot root opts.convention)
        result <- evaluate $ maybe (Left k) Right ns
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
 where
  withFileMay = \case
    Nothing -> \act -> act Nothing
    Just path -> \act -> withFile path WriteMode (act . Just)

  finder breadth maxK = \case
    Linear -> Linear.chebyZero
    Fraction -> Fraction.chebyZero breadth
    TFun -> TFun.tfunZero breadth
    TTilde -> findJustStream . Composite.tildeZero breadth
    THat -> findJustStream . Composite.hatZero breadth
    Naive depth -> \u2 ->
      let found = ChooseOne.naiveContinuedFracInfty breadth u2 maxK depth
       in maybe (Nothing <$ Stream.yield maxK) (pure . Just) found
  takeResult maxK = \case
    Nothing -> Stream.last . fmap join . cutoff maxK
    Just _ -> Stream.last . fmap join . cutoff maxK -- TODO Implement timeout

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
