module Registry
  ( BenchCase
  , benchCase
  , preparedBenchCase
  , familyBenchmarks
  , hostileProbeCases
  ) where

import Fixture
  ( BenchmarkFixture (..)
  , BenchmarkResult
  , ProbeCase
  , ProbeFamily
  , mkHostileProbeCase
  , probeBudgetClassForFamily
  , probeRunFromBenchmarkResult
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

data BenchCase = BenchCase
  { bcId :: !String
  , bcRun :: BenchmarkFixture -> BenchmarkResult
  , bcBench :: BenchmarkFixture -> Benchmark
  }

benchCase :: String -> (BenchmarkFixture -> BenchmarkResult) -> BenchCase
benchCase caseIdValue runValue =
  BenchCase
    { bcId = caseIdValue
    , bcRun = runValue
    , bcBench = \fixture -> bench (bfLabel fixture) (nf runValue fixture)
    }

preparedBenchCase ::
  String ->
  (BenchmarkFixture -> prepared) ->
  (prepared -> Int) ->
  (prepared -> BenchmarkResult) ->
  BenchCase
preparedBenchCase caseIdValue prepareValue forcePrepared queryPrepared =
  BenchCase
    { bcId = caseIdValue
    , bcRun = queryPrepared . prepareValue
    , bcBench =
        \fixture ->
          let preparedValue = prepareValue fixture
              forcedPrepared = forcePrepared preparedValue
           in forcedPrepared `seq` bench (bfLabel fixture) (nf queryPrepared preparedValue)
    }

familyBenchmarks :: String -> [BenchCase] -> [BenchmarkFixture] -> Benchmark
familyBenchmarks familyName cases fixtures =
  bgroup
    familyName
    (fmap (caseBenchmarks fixtures) cases)

hostileProbeCases :: String -> ProbeFamily -> [BenchCase] -> [BenchmarkFixture] -> [ProbeCase]
hostileProbeCases namespace probeFamily cases fixtures =
  concatMap
    ( \fixture ->
        fmap
          ( \caseValue ->
              mkHostileProbeCase
                ("hostile/" <> namespace <> "/" <> bcId caseValue <> "/" <> bfLabel fixture)
                (probeBudgetClassForFamily probeFamily fixture)
                (pure (probeRunFromBenchmarkResult (bcRun caseValue fixture)))
          )
          cases
    )
    fixtures

caseBenchmarks :: [BenchmarkFixture] -> BenchCase -> Benchmark
caseBenchmarks fixtures caseValue =
  bgroup
    (bcId caseValue)
    (fmap (bcBench caseValue) fixtures)
