module Structural
  ( benchmarks
  , probeCases
  ) where

import Fixture
  ( BenchmarkResult
  , BenchmarkFixture (..)
  , ProbeFamily (..)
  , ProbeCase
  , checksumBlockedMatGF2
  , checksumDenseMatGF2
  , benchmarkEitherWith
  , benchmarkSuccess
  )
import Registry
  ( BenchCase
  , benchCase
  , familyBenchmarks
  , hostileProbeCases
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( collapseBlockedDense
  , relabelBlocked
  , restrictBlocked
  )
import Moonlight.Derived.Pure.Site.Microsupport (localClosedNodes)
import Test.Tasty.Bench (Benchmark)

benchmarks :: [BenchmarkFixture] -> Benchmark
benchmarks fixtures =
  familyBenchmarks "structural" structuralFamilies fixtures

probeCases :: [BenchmarkFixture] -> [ProbeCase]
probeCases =
  hostileProbeCases "structural" ProbeFamilyStructural structuralFamilies

structuralFamilies :: [BenchCase]
structuralFamilies =
  [ benchCase "restrict-blocked" runRestrictBlocked
  , benchCase "relabel-blocked" runRelabelBlocked
  , benchCase "collapse-blocked-dense" runCollapseBlockedDense
  ]

runRestrictBlocked :: BenchmarkFixture -> BenchmarkResult
runRestrictBlocked fixture =
  benchmarkSuccess
    ( checksumBlockedMatGF2
        (restrictBlocked (localClosedNodes (bfOuterLocalClosed fixture)) (bfMaterializationBlocked fixture))
    )

runRelabelBlocked :: BenchmarkFixture -> BenchmarkResult
runRelabelBlocked fixture =
  benchmarkEitherWith
    checksumBlockedMatGF2
    (relabelBlocked (Right . bfMapToTarget fixture) (bfMaterializationBlocked fixture))

runCollapseBlockedDense :: BenchmarkFixture -> BenchmarkResult
runCollapseBlockedDense fixture =
  benchmarkSuccess
    (checksumDenseMatGF2 (collapseBlockedDense (bfMaterializationBlocked fixture)))
