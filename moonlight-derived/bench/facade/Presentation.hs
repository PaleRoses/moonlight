module Presentation
  ( benchmarks
  , probeCases
  ) where

import qualified Data.Vector as V
import Fixture
  ( BenchmarkChecksum (..)
  , BenchmarkFixture (..)
  , BenchmarkResult
  , ProbeCase
  , ProbeFamily (..)
  , benchmarkEitherWith
  , checksumDenseMatGF2
  , checksumDerivedGF2
  )
import Registry
  ( BenchCase
  , familyBenchmarks
  , hostileProbeCases
  , preparedBenchCase
  )
import Moonlight.Derived.Presentation.Builder
  ( derivedObject
  , differentialDense
  , objectsFrom
  )
import Moonlight.Derived.Pure.Gluing.Cone (RawComplex (..), rawFromDerived)
import Moonlight.Derived.Pure.Site.LabeledMatrix (DenseMat)
import Moonlight.Derived.Site (DerivedPoset, FinObjectId)
import Moonlight.LinAlg (GF2)
import Test.Tasty.Bench (Benchmark)

benchmarks :: [BenchmarkFixture] -> Benchmark
benchmarks =
  familyBenchmarks "presentation" presentationFamilies

probeCases :: [BenchmarkFixture] -> [ProbeCase]
probeCases =
  hostileProbeCases "presentation" ProbeFamilyStructural presentationFamilies

presentationFamilies :: [BenchCase]
presentationFamilies =
  [ preparedBenchCase "builder-object" prepareAuthoring forcePreparedAuthoring runBuilderObject
  ]

type PreparedAuthoring = (DerivedPoset, Int, [[FinObjectId]], [DenseMat GF2])

prepareAuthoring :: BenchmarkFixture -> PreparedAuthoring
prepareAuthoring fixture =
  (bfAmbientPoset fixture, rcStart rawValue, objectLists, denseDiffs)
  where
    rawValue = rawFromDerived (bfSourceDerived fixture)
    denseDiffs = V.toList (rcDiffs rawValue)
    declaredLists = fmap V.toList (V.toList (rcLabels rawValue))
    objectLists
      | length denseDiffs == length declaredLists = declaredLists <> [[]]
      | otherwise = declaredLists

forcePreparedAuthoring :: PreparedAuthoring -> Int
forcePreparedAuthoring (_, startDegree, objectLists, denseDiffs) =
  startDegree
    + sum (fmap length objectLists)
    + sum (fmap (unBenchmarkChecksum . checksumDenseMatGF2) denseDiffs)

runBuilderObject :: PreparedAuthoring -> BenchmarkResult
runBuilderObject (posetValue, startDegree, objectLists, denseDiffs) =
  benchmarkEitherWith
    checksumDerivedGF2
    ( derivedObject posetValue $ do
        declaredObjects <- objectsFrom startDegree objectLists
        sequence_
          ( zipWith3
              (\(sourceRef, _) (targetRef, _) denseValue -> differentialDense sourceRef targetRef denseValue)
              declaredObjects
              (drop 1 declaredObjects)
              denseDiffs
          )
    )
