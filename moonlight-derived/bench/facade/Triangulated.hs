module Triangulated
  ( benchmarks
  , probeCases
  ) where

import Data.IntMap.Strict qualified as IntMap
import Fixture
  ( BenchmarkChecksum (..)
  , BenchmarkFixture (..)
  , BenchmarkResult
  , ProbeCase
  , ProbeFamily (..)
  , benchmarkEitherWith
  , benchmarkSuccess
  , checksumDerivedGF2
  )
import Registry
  ( BenchCase
  , benchCase
  , familyBenchmarks
  , hostileProbeCases
  )
import Moonlight.Derived.Triangulated
  ( canonicalTruncateAtLeast
  , canonicalTruncateAtMost
  , canonicalTruncationPair
  , cone
  , derivedMapComponents
  , identityMap
  , mkDerivedMapChecked
  , mkTriangleOf
  , quasiIsoTo
  , shift
  , triC
  , zeroMap
  )
import Test.Tasty.Bench (Benchmark)

benchmarks :: [BenchmarkFixture] -> Benchmark
benchmarks =
  familyBenchmarks "triangulated" triangulatedFamilies

probeCases :: [BenchmarkFixture] -> [ProbeCase]
probeCases =
  hostileProbeCases "triangulated" ProbeFamilyStructural triangulatedFamilies

triangulatedFamilies :: [BenchCase]
triangulatedFamilies =
  [ benchCase "map-validate" runMapValidate
  , benchCase "shift" runShift
  , benchCase "cone-identity" runConeIdentity
  , benchCase "cone-zero" runConeZero
  , benchCase "triangle-of" runTriangleOf
  , benchCase "quasi-iso-identity" runQuasiIsoIdentity
  , benchCase "tau-le" runCanonicalTruncateAtMost
  , benchCase "tau-ge" runCanonicalTruncateAtLeast
  , benchCase "tau-pair" runCanonicalTruncationPair
  ]

runMapValidate :: BenchmarkFixture -> BenchmarkResult
runMapValidate fixture =
  benchmarkEitherWith
    (BenchmarkChecksum . IntMap.size . derivedMapComponents)
    ( mkDerivedMapChecked
        (bfSourceDerived fixture)
        (bfSourceDerived fixture)
        (derivedMapComponents (identityMap (bfSourceDerived fixture)))
    )

runShift :: BenchmarkFixture -> BenchmarkResult
runShift =
  benchmarkSuccess . checksumDerivedGF2 . shift 1 . bfSourceDerived

runConeIdentity :: BenchmarkFixture -> BenchmarkResult
runConeIdentity fixture =
  benchmarkEitherWith
    checksumDerivedGF2
    (cone (identityMap (bfSourceDerived fixture)))

runConeZero :: BenchmarkFixture -> BenchmarkResult
runConeZero fixture =
  benchmarkEitherWith
    checksumDerivedGF2
    (cone (zeroMap (bfSourceDerived fixture) (bfSecondaryDerived fixture)))

runTriangleOf :: BenchmarkFixture -> BenchmarkResult
runTriangleOf fixture =
  benchmarkEitherWith
    (checksumDerivedGF2 . triC)
    (mkTriangleOf (identityMap (bfSourceDerived fixture)))

runQuasiIsoIdentity :: BenchmarkFixture -> BenchmarkResult
runQuasiIsoIdentity fixture =
  benchmarkEitherWith
    (BenchmarkChecksum . fromEnum)
    (quasiIsoTo (identityMap (bfSourceDerived fixture)))

runCanonicalTruncateAtMost :: BenchmarkFixture -> BenchmarkResult
runCanonicalTruncateAtMost fixture =
  benchmarkEitherWith
    checksumDerivedGF2
    (canonicalTruncateAtMost 0 (bfSourceDerived fixture))

runCanonicalTruncateAtLeast :: BenchmarkFixture -> BenchmarkResult
runCanonicalTruncateAtLeast fixture =
  benchmarkEitherWith
    checksumDerivedGF2
    (canonicalTruncateAtLeast 1 (bfSourceDerived fixture))

runCanonicalTruncationPair :: BenchmarkFixture -> BenchmarkResult
runCanonicalTruncationPair fixture =
  benchmarkEitherWith
    ( \(lowerValue, upperValue) ->
        BenchmarkChecksum
          ( unBenchmarkChecksum (checksumDerivedGF2 lowerValue) * 16777619
              + unBenchmarkChecksum (checksumDerivedGF2 upperValue)
          )
    )
    (canonicalTruncationPair 0 (bfSourceDerived fixture))
