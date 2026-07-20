module Morse
  ( benchmarks
  , probeCases
  ) where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Fixture
  ( BenchmarkCaseClass (..)
  , BenchmarkChecksum (..)
  , BenchmarkResult
  , BenchmarkFixture (..)
  , ProbeFamily (..)
  , ProbeBudgetClass (..)
  , ProbeCase
  , benchmarkFailure
  , benchmarkSuccess
  , mkHostileProbeCase
  , probeRunFromBenchmarkResult
  , probeBudgetClassForFamily
  )
import Moonlight.Derived.Pure.Morse.Hypercohomology
  ( hypercohomologyDimsWith
  )
import Moonlight.Derived.Pure.Morse.Support
  ( microSupportBangOnWith
  )
import Moonlight.Derived.Pure.Pipeline
  ( MicrosupportResult (..)
  , computeMicrosupportWith
  , preparedMicrosupportPullbacks
  )
import Moonlight.Derived.Pure.LinAlg.Interpreter
  ( fieldRankBackend
  , gf2PackedRankBackend
  )
import Moonlight.Derived.Morse (microsupportOfDifferential)
import Moonlight.Derived.Pure.LinAlg.Rank (RankBackend)
import Moonlight.Derived.Pure.Site.Microsupport
  ( Criticality (..)
  , LocalClosed
  , localClosedNodes
  )
import Moonlight.Category (FinObjectId (..))
import Moonlight.LinAlg (GF2)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

benchmarks :: [BenchmarkFixture] -> Benchmark
benchmarks fixtures =
  bgroup
    "morse"
    (fmap (benchmarkFamily fixtures) safeCases)
  where
    safeCases =
      filter ((== SafeMicro) . mfClass) morseCases

probeCases :: [BenchmarkFixture] -> [ProbeCase]
probeCases fixtures =
  concatMap (fixtureProbeCases hostileCases) fixtures
  where
    hostileCases =
      filter ((== HostileProbe) . mfClass) morseCases

data MorseCase = MorseCase
  { mfClass :: !BenchmarkCaseClass
  , mfId :: !String
  , mfRun :: BenchmarkFixture -> BenchmarkResult
  }

morseCases :: [MorseCase]
morseCases =
  [ MorseCase SafeMicro "hypercohomology/field-rank" runHypercohomologyField
  , MorseCase SafeMicro "hypercohomology/gf2-packed-rank" runHypercohomologyPacked
  , MorseCase SafeMicro "microsupport-prepared/field-rank" runMicroSupportField
  , MorseCase SafeMicro "microsupport-prepared/gf2-packed-rank" runMicroSupportPacked
  , MorseCase SafeMicro "compute-microsupport-prepared/field-rank" runComputeMicrosupportField
  , MorseCase SafeMicro "compute-microsupport-prepared/gf2-packed-rank" runComputeMicrosupportPacked
  , MorseCase SafeMicro "microsupport-of-differential" runMicrosupportOfDifferential
  ]

fixtureProbeCases :: [MorseCase] -> BenchmarkFixture -> [ProbeCase]
fixtureProbeCases cases fixture =
  fmap
    (\morseCase -> mkHostileProbeCase (hostileProbeId morseCase fixture) (probeBudgetClass fixture) (pure (probeRunFromBenchmarkResult (mfRun morseCase fixture))))
    cases

hostileProbeId :: MorseCase -> BenchmarkFixture -> String
hostileProbeId morseCase fixture =
  "hostile/morse/" <> mfId morseCase <> "/" <> bfLabel fixture

probeBudgetClass :: BenchmarkFixture -> ProbeBudgetClass
probeBudgetClass =
  probeBudgetClassForFamily ProbeFamilyMorse

benchmarkFamily :: [BenchmarkFixture] -> MorseCase -> Benchmark
benchmarkFamily fixtures morseCase =
  bgroup
    (mfId morseCase)
    (fmap (\fixture -> bench (bfLabel fixture) (nf (mfRun morseCase) fixture)) fixtures)

runHypercohomologyField :: BenchmarkFixture -> BenchmarkResult
runHypercohomologyField =
  runHypercohomology fieldRankBackend

runHypercohomologyPacked :: BenchmarkFixture -> BenchmarkResult
runHypercohomologyPacked =
  runHypercohomology gf2PackedRankBackend

runHypercohomology :: RankBackend GF2 -> BenchmarkFixture -> BenchmarkResult
runHypercohomology rankBackend fixture =
  eitherBenchmark
    dimsChecksum
    (hypercohomologyDimsWith rankBackend (bfSourceDerived fixture))

runMicroSupportField :: BenchmarkFixture -> BenchmarkResult
runMicroSupportField =
  runMicroSupport fieldRankBackend

runMicroSupportPacked :: BenchmarkFixture -> BenchmarkResult
runMicroSupportPacked =
  runMicroSupport gf2PackedRankBackend

runMicroSupport :: RankBackend GF2 -> BenchmarkFixture -> BenchmarkResult
runMicroSupport rankBackend fixture =
  eitherBenchmark
    localClosedListChecksum
    ( microSupportBangOnWith
        rankBackend
        (preparedMicrosupportPullbacks (bfPreparedMicrosupport fixture))
    )

runComputeMicrosupportField :: BenchmarkFixture -> BenchmarkResult
runComputeMicrosupportField =
  runComputeMicrosupport fieldRankBackend

runComputeMicrosupportPacked :: BenchmarkFixture -> BenchmarkResult
runComputeMicrosupportPacked =
  runComputeMicrosupport gf2PackedRankBackend

runComputeMicrosupport :: RankBackend GF2 -> BenchmarkFixture -> BenchmarkResult
runComputeMicrosupport rankBackend fixture =
  eitherBenchmark
    microsupportResultChecksum
    ( computeMicrosupportWith
        rankBackend
        (bfPreparedMicrosupport fixture)
    )

runMicrosupportOfDifferential :: BenchmarkFixture -> BenchmarkResult
runMicrosupportOfDifferential fixture =
  eitherBenchmark
    microsupportResultChecksum
    (microsupportOfDifferential (bfAmbientPoset fixture) (bfMaterializationBlocked fixture))

benchmarkInt :: Int -> BenchmarkResult
benchmarkInt =
  benchmarkSuccess . BenchmarkChecksum

eitherBenchmark :: Show errorValue => (value -> Int) -> Either errorValue value -> BenchmarkResult
eitherBenchmark checksumValue =
  either (benchmarkFailure . show) (benchmarkInt . checksumValue)

dimsChecksum :: IntMap.IntMap Int -> Int
dimsChecksum =
  IntMap.foldlWithKey'
    (\checksum degreeValue dimensionValue -> checksum * 37 + degreeValue * 11 + dimensionValue)
    7

localClosedListChecksum :: [LocalClosed] -> Int
localClosedListChecksum =
  foldl'
    (\checksum localClosedValue -> checksum * 41 + localClosedChecksum localClosedValue)
    7

localClosedChecksum :: LocalClosed -> Int
localClosedChecksum localClosedValue =
  IntSet.foldl'
    (\checksum nodeValue -> checksum * 29 + nodeValue + 1)
    5
    (localClosedNodes localClosedValue)

microsupportResultChecksum :: MicrosupportResult -> Int
microsupportResultChecksum microsupportResult =
  let criticalFiberChecksum =
        foldl'
          ( \checksum (FinObjectId nodeValue, criticalityValue) ->
              checksum * 43 + nodeValue * 13 + criticalityChecksum criticalityValue
          )
          11
          (mrCriticalFibers microsupportResult)
   in 101 * mrCriticalCount microsupportResult
        + 53 * mrNoncriticalCount microsupportResult
        + localClosedListChecksum (mrMicrosupport microsupportResult)
        + criticalFiberChecksum

criticalityChecksum :: Criticality -> Int
criticalityChecksum criticalityValue =
  case criticalityValue of
    Critical -> 3
    NonCritical -> 1
