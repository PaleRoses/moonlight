{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module BenchSupport
  ( BenchmarkLane (..),
    BenchmarkObstruction (..),
    PopulationDigest (..),
    populationDigest,
    forceEither,
    requireBenchmarkFixture,
    validatedPureBenchmark,
    validatedBenchmarkGroup,
    validatedBenchmarkFamily,
    runValidatedBenchmark,
    runValidatedBenchmarks,
    benchmarkCaseLabel,
    sumFromOne,
    sumFromZero,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (first)
import GHC.Generics (Generic)
import Moonlight.Pale.Bench.Runner (runBenchmark, runBenchmarks)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, nf)

data BenchmarkLane
  = CoreBenchmarkLane
  | ProtocolBenchmarkLane
  | ContextBenchmarkLane
  | ObstructionBenchmarkLane
  deriving stock (Eq, Ord, Show)

data BenchmarkObstruction
  = BenchmarkFixtureRejected
      { benchmarkObstructionLane :: !BenchmarkLane,
        benchmarkObstructionCase :: !String,
        benchmarkObstructionDetail :: !String
      }
  | BenchmarkSemanticMismatch
      { benchmarkObstructionLane :: !BenchmarkLane,
        benchmarkObstructionCase :: !String,
        benchmarkObstructionExpected :: !String,
        benchmarkObstructionActual :: !String
      }
  deriving stock (Eq, Show)

data PopulationDigest = PopulationDigest
  { populationCount :: !Int,
    populationChecksum :: !Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Semigroup PopulationDigest where
  PopulationDigest leftCount leftChecksum <> PopulationDigest rightCount rightChecksum =
    PopulationDigest (leftCount + rightCount) (leftChecksum + rightChecksum)

instance Monoid PopulationDigest where
  mempty = PopulationDigest 0 0

populationDigest :: Foldable collection => (element -> Int) -> collection element -> PopulationDigest
populationDigest weight = foldMap (PopulationDigest 1 . weight)

forceEither :: Show obstruction => (result -> ()) -> Either obstruction result -> ()
forceEither forceResult = either (rnf . show) forceResult

data Forced value = Forced
  { forceValue :: value -> (),
    forcedValue :: value
  }

instance NFData (Forced value) where
  rnf forced = forceValue forced (forcedValue forced)

requireBenchmarkFixture :: Show obstruction => BenchmarkLane -> String -> Either obstruction value -> Either BenchmarkObstruction value
requireBenchmarkFixture lane caseName =
  first (\obstruction -> BenchmarkFixtureRejected lane caseName (show obstruction))

validatedPureBenchmark ::
  (Eq result, Show result) =>
  BenchmarkLane ->
  String ->
  result ->
  (input -> ()) ->
  (result -> ()) ->
  (input -> result) ->
  input ->
  Either BenchmarkObstruction Benchmark
validatedPureBenchmark lane caseName expected forceInput forceResult measure input
  | actual /= expected = Left (BenchmarkSemanticMismatch lane caseName (show expected) (show actual))
  | otherwise =
      Right
        ( env
            (pure (Forced forceInput input))
            (\forced -> bench caseName (nf (Forced forceResult . measure) (forcedValue forced)))
        )
  where
    actual = measure input

validatedBenchmarkGroup :: String -> [Either BenchmarkObstruction Benchmark] -> Either BenchmarkObstruction Benchmark
validatedBenchmarkGroup groupName = fmap (bgroup groupName) . sequence

validatedBenchmarkFamily :: String -> (scale -> Either BenchmarkObstruction Benchmark) -> [scale] -> Either BenchmarkObstruction Benchmark
validatedBenchmarkFamily groupName build = validatedBenchmarkGroup groupName . fmap build

runValidatedBenchmark :: Either BenchmarkObstruction Benchmark -> IO ()
runValidatedBenchmark = either reportBenchmarkObstruction runBenchmark

runValidatedBenchmarks :: [Either BenchmarkObstruction Benchmark] -> IO ()
runValidatedBenchmarks = either reportBenchmarkObstruction runBenchmarks . sequence

reportBenchmarkObstruction :: BenchmarkObstruction -> IO result
reportBenchmarkObstruction obstruction =
  hPutStrLn stderr ("benchmark validation obstructed: " <> show obstruction) >> exitFailure

benchmarkCaseLabel :: String -> Int -> String
benchmarkCaseLabel operation size = operation <> " n=" <> show size

sumFromZero :: Int -> Int
sumFromZero size = size * (size - 1) `div` 2

sumFromOne :: Int -> Int
sumFromOne size = size * (size + 1) `div` 2
