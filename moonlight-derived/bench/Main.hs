module Main
  ( main
  ) where

import Fixture
  ( BenchmarkFixture
  , loadBenchmarkFixtures
  )
import Functor qualified as FunctorBench
import Gluing qualified as GluingBench
import Matrix qualified as MatrixBench
import Morse qualified as MorseBench
import Presentation qualified as PresentationBench
import Pruning qualified as PruningBench
import Site qualified as SiteBench
import Structural qualified as StructuralBench
import Triangulated qualified as TriangulatedBench
import System.Environment (lookupEnv)
import Test.Tasty.Bench (Benchmark, defaultMain)

main :: IO ()
main = do
  includeLarge <- shouldIncludeLargeBenchmarks
  includeHostile <- shouldIncludeHostileBenchmarks
  fixtures <- loadBenchmarkFixtures includeLarge
  defaultMain (benchmarkFamilies includeHostile fixtures)

benchmarkFamilies :: Bool -> [BenchmarkFixture] -> [Benchmark]
benchmarkFamilies includeHostile fixtures =
  [ SiteBench.benchmarks fixtures
  , MatrixBench.benchmarks fixtures
  , StructuralBench.benchmarks fixtures
  , GluingBench.benchmarks fixtures
  , FunctorBench.benchmarks includeHostile fixtures
  , MorseBench.benchmarks fixtures
  , PruningBench.benchmarks fixtures
  , TriangulatedBench.benchmarks fixtures
  , PresentationBench.benchmarks fixtures
  ]

shouldIncludeLargeBenchmarks :: IO Bool
shouldIncludeLargeBenchmarks =
  fmap parseTruthy (lookupEnv "MOONLIGHT_DERIVED_BENCH_ENABLE_LARGE")

shouldIncludeHostileBenchmarks :: IO Bool
shouldIncludeHostileBenchmarks =
  fmap parseTruthy (lookupEnv "MOONLIGHT_DERIVED_BENCH_INCLUDE_HOSTILE")

parseTruthy :: Maybe String -> Bool
parseTruthy =
  maybe False (`elem` ["1", "true", "TRUE", "yes", "YES"])
