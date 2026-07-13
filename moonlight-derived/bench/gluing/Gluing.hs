module Gluing
  ( benchmarks
  , probeCases
  ) where

import Data.Vector qualified as Vector
import Fixture
  ( BenchmarkFixture (..)
  , BenchmarkResult
  , ProbeCase
  , ProbeFamily (..)
  , benchmarkEitherWith
  , benchmarkFailure
  , checksumBlockedMatGF2
  , checksumInjectiveComplexGF2
  )
import Registry
  ( BenchCase
  , benchCase
  , familyBenchmarks
  , hostileProbeCases
  )
import Moonlight.Derived.Complex (derivedInjectiveComplex)
import Moonlight.Derived.Gluing
  ( completeDifferential
  , makeExact
  , minimizeComplex
  , resolutionStep
  )
import Moonlight.Derived.Matrix
  ( BlockedMat
  , blockedMatRows
  , emptyAxis
  , zeroBlocked
  )
import Moonlight.Derived.Site
  ( derivedPosetTopoDesc
  )
import Test.Tasty.Bench (Benchmark)

benchmarks :: [BenchmarkFixture] -> Benchmark
benchmarks fixtures =
  familyBenchmarks "gluing" gluingFamilies fixtures

probeCases :: [BenchmarkFixture] -> [ProbeCase]
probeCases =
  hostileProbeCases "gluing" ProbeFamilyStructural gluingFamilies

gluingFamilies :: [BenchCase]
gluingFamilies =
  [ benchCase "make-exact-first-node" runMakeExactFirstNode
  , benchCase "complete-differential" runCompleteDifferential
  , benchCase "resolution-step" runResolutionStep
  , benchCase "minimize-complex" runMinimizeComplex
  ]

runMakeExactFirstNode :: BenchmarkFixture -> BenchmarkResult
runMakeExactFirstNode fixture =
  case derivedPosetTopoDesc (bfAmbientPoset fixture) Vector.!? 0 of
    Nothing ->
      benchmarkFailure "empty poset has no node for makeExact"
    Just nodeValue ->
      benchmarkEitherWith
        checksumBlockedMatGF2
        ( makeExact
            (bfAmbientPoset fixture)
            nodeValue
            (bfMaterializationBlocked fixture)
            (emptyResolutionTarget (bfMaterializationBlocked fixture))
        )

runCompleteDifferential :: BenchmarkFixture -> BenchmarkResult
runCompleteDifferential fixture =
  benchmarkEitherWith
    checksumBlockedMatGF2
    ( completeDifferential
        (bfAmbientPoset fixture)
        (Vector.toList (derivedPosetTopoDesc (bfAmbientPoset fixture)))
        (bfMaterializationBlocked fixture)
        (emptyResolutionTarget (bfMaterializationBlocked fixture))
    )

runResolutionStep :: BenchmarkFixture -> BenchmarkResult
runResolutionStep fixture =
  benchmarkEitherWith
    checksumBlockedMatGF2
    (resolutionStep (bfAmbientPoset fixture) (bfMaterializationBlocked fixture))

runMinimizeComplex :: BenchmarkFixture -> BenchmarkResult
runMinimizeComplex fixture =
  benchmarkEitherWith
    checksumInjectiveComplexGF2
    (minimizeComplex (derivedInjectiveComplex (bfSourceDerived fixture)))

emptyResolutionTarget :: Num a => BlockedMat a -> BlockedMat a
emptyResolutionTarget previousDifferential =
  zeroBlocked emptyAxis (blockedMatRows previousDifferential)
