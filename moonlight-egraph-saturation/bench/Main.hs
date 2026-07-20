{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Applicative ((<|>))
import Control.Exception (bracket)
import Data.Foldable (traverse_)
import GHC.Clock (getMonotonicTimeNSec)
import Moonlight.EGraph.Saturation.Bench.Egglog
  ( EgglogPrepared (..),
    cleanupEgglogProgram,
    discoverEgglogBinary,
    discoverEgglogEngineBenchBinary,
    prepareEgglogProgram,
    runEgglogEngineBench,
    runPreparedEgglog,
  )
import Moonlight.EGraph.Saturation.Bench.Front
  ( MoonlightFrontObservedSummary (..),
    MoonlightFrontSummary (..),
    runMoonlightFrontCompileOnly,
    runMoonlightFrontSaturation,
    runMoonlightFrontSaturationObserved,
    runMoonlightFrontSeedOnly,
  )
import Moonlight.Saturation.Context.Runtime.Engine
  ( RuntimeIOTiming (..),
  )
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout)
import Test.Tasty (defaultMainWithIngredients, testGroup)
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    benchIngredients,
    bgroup,
    whnfIO,
  )
import Text.Printf (printf)
import Text.Read (readMaybe)

data FrontScale = FrontScale
  { fsName :: !String,
    fsTermCount :: !Int
  }

data OneShotCommand
  = CompareMoonlightEgglog !Int
  | RunEgglogOnly !Int
  | RunMoonlightFront !Int
  deriving stock (Eq, Show)

data OneShotLookup = OneShotLookup !String !(Int -> OneShotCommand)

data OneShotLookupResult
  = InvalidOneShotScale !String !String
  | ValidOneShotCommand !OneShotCommand
  deriving stock (Eq, Show)

main :: IO ()
main = do
  oneShotRequest <- discoverOneShotCommand
  maybe runBenchmarkSuite runOneShotCommand oneShotRequest

discoverOneShotCommand :: IO (Maybe OneShotLookupResult)
discoverOneShotCommand =
  firstPresent <$> traverse lookupOneShotCommand oneShotLookups

lookupOneShotCommand :: OneShotLookup -> IO (Maybe OneShotLookupResult)
lookupOneShotCommand (OneShotLookup envName mkCommand) =
  fmap (parseOneShotCommand envName mkCommand) <$> lookupEnv envName

parseOneShotCommand :: String -> (Int -> OneShotCommand) -> String -> OneShotLookupResult
parseOneShotCommand envName mkCommand rawScale =
  maybe
    (InvalidOneShotScale envName rawScale)
    (ValidOneShotCommand . mkCommand)
    (readMaybe rawScale)

firstPresent :: [Maybe value] -> Maybe value
firstPresent =
  foldr (<|>) Nothing

oneShotLookups :: [OneShotLookup]
oneShotLookups =
  [ OneShotLookup compareEgglogOneShotScaleEnv CompareMoonlightEgglog,
    OneShotLookup egglogOneShotScaleEnv RunEgglogOnly,
    OneShotLookup frontOneShotScaleEnv RunMoonlightFront
  ]

runOneShotCommand :: OneShotLookupResult -> IO ()
runOneShotCommand =
  \case
    InvalidOneShotScale envName rawScale ->
      putStrLn ("Invalid " <> envName <> ": " <> rawScale)
    ValidOneShotCommand command ->
      case command of
        CompareMoonlightEgglog termCount ->
          runOneShotMoonlightVsEgglog termCount
        RunEgglogOnly termCount ->
          runOneShotEgglog termCount
        RunMoonlightFront termCount ->
          runOneShotMoonlightFront termCount

runBenchmarkSuite :: IO ()
runBenchmarkSuite = do
  enable1K <- optInEnv enable1KEnv
  defaultMainWithIngredients benchIngredients
    ( testGroup
        "All"
        [ bgroup
            "moonlight-front"
            (fmap frontScaleGroup (frontScales enable1K))
        ]
    )

frontOneShotScaleEnv :: String
frontOneShotScaleEnv =
  "MOONLIGHT_EGRAPH_SATURATION_BENCH_FRONT_ONESHOT_SCALE"

egglogOneShotScaleEnv :: String
egglogOneShotScaleEnv =
  "MOONLIGHT_EGRAPH_SATURATION_BENCH_EGGLOG_ONESHOT_SCALE"

compareEgglogOneShotScaleEnv :: String
compareEgglogOneShotScaleEnv =
  "MOONLIGHT_EGRAPH_SATURATION_BENCH_COMPARE_EGGLOG_SCALE"

enable1KEnv :: String
enable1KEnv =
  "MOONLIGHT_EGRAPH_SATURATION_BENCH_ENABLE_1K"

frontScales :: Bool -> [FrontScale]
frontScales enable1K =
  [FrontScale "100" 100]
    <> optionalWhen enable1K (FrontScale "1K" 1000)

frontScaleGroup :: FrontScale -> Benchmark
frontScaleGroup scale =
  bgroup
    (fsName scale)
    [ bench
        "compile-only"
        (whnfIO (runMoonlightFrontCompileOnly (fsTermCount scale))),
      bench
        "seed-only"
        (whnfIO (runMoonlightFrontSeedOnly (fsTermCount scale))),
      bench
        "saturate"
        (whnfIO (runMoonlightFrontSaturation (fsTermCount scale)))
    ]

runOneShotMoonlightFront :: Int -> IO ()
runOneShotMoonlightFront termCount = do
  compileResult <-
    timed
      "moonlight.front.compile"
      (runMoonlightFrontCompileOnly termCount)
  either fail pure compileResult
  seedOnlyResult <-
    timed
      "moonlight.front.seed-only"
      (runMoonlightFrontSeedOnly termCount)
  case seedOnlyResult of
    Left obstruction ->
      fail obstruction
    Right summary ->
      printMoonlightFrontSummary "moonlight.front.seed-only" summary
  moonlightResult <-
    timed
      "moonlight.front.run"
      (runMoonlightFrontSaturationObserved termCount)
  case moonlightResult of
    Left obstruction ->
      fail obstruction
    Right observed -> do
      printMoonlightFrontSummary "moonlight.front" (mfoSummary observed)
      traverse_ (printRuntimeTiming "moonlight.front.runtime") (mfoTimings observed)

runOneShotEgglog :: Int -> IO ()
runOneShotEgglog termCount = do
  maybeEgglogBinary <- discoverEgglogBinary
  case maybeEgglogBinary of
    Nothing ->
      putStrLn "egglog binary not found; set EGGLOG_BIN or install egglog"
    Just egglogBinary ->
      bracket
        (timed "egglog.cli.prepare-program" (prepareEgglogProgram egglogBinary termCount))
        cleanupEgglogProgram
        ( \prepared -> do
            putStrLn ("egglog.cli.program=" <> epFilePath prepared)
            result <-
              timed
                "egglog.cli.run-script"
                (runPreparedEgglog prepared)
            case result of
              Left obstruction ->
                fail obstruction
              Right () ->
                putStrLn ("egglog.cli scale=" <> show termCount <> " status=ok")
        )
  maybeEngineBench <- discoverEgglogEngineBenchBinary
  case maybeEngineBench of
    Nothing ->
      putStrLn "egglog.engine-api status=unavailable set EGGLOG_ENGINE_BENCH_BIN for engine-style comparison"
    Just engineBench -> do
      result <-
        timed
          "egglog.engine-api.run"
          (runEgglogEngineBench engineBench termCount)
      case result of
        Left obstruction ->
          fail obstruction
        Right () ->
          putStrLn ("egglog.engine-api scale=" <> show termCount <> " status=ok")

runOneShotMoonlightVsEgglog :: Int -> IO ()
runOneShotMoonlightVsEgglog termCount = do
  runOneShotMoonlightFront termCount
  runOneShotEgglog termCount

printMoonlightFrontSummary :: String -> MoonlightFrontSummary -> IO ()
printMoonlightFrontSummary label summary =
  putStrLn
    ( label
        <> " scale="
        <> show (mfsTermCount summary)
        <> " classes="
        <> show (mfsClassCount summary)
        <> " nodes="
        <> show (mfsNodeCount summary)
        <> " iterations="
        <> show (mfsIterations summary)
        <> " matches="
        <> show (mfsMatchesApplied summary)
    )

printRuntimeTiming :: String -> RuntimeIOTiming -> IO ()
printRuntimeTiming label timing =
  putStrLn
    ( label
        <> " round_build_ms="
        <> nanosToMillis (ritRoundBuildNanoseconds timing)
        <> " apply_ms="
        <> nanosToMillis (ritApplyNanoseconds timing)
        <> " rebuild_ms="
        <> nanosToMillis (ritRebuildNanoseconds timing)
        <> " commit_ms="
        <> nanosToMillis (ritCommitNanoseconds timing)
    )

nanosToMillis :: Integral natural => natural -> String
nanosToMillis =
  printf "%.3f" . ((/ 1000000) :: Double -> Double) . fromIntegral

timed :: String -> IO value -> IO value
timed label action = do
  start <- getMonotonicTimeNSec
  value <- action
  end <- getMonotonicTimeNSec
  printf "%s_ms=%.3f\n" label (fromIntegral (end - start) / 1000000 :: Double)
  hFlush stdout
  pure value

optionalWhen :: Bool -> value -> [value]
optionalWhen enabled value =
  if enabled
    then [value]
    else []

optInEnv :: String -> IO Bool
optInEnv envName =
  fmap (maybe False envEnables) (lookupEnv envName)

envEnables :: String -> Bool
envEnables value =
  value `elem` ["1", "true", "TRUE", "yes", "YES", "on", "ON"]
