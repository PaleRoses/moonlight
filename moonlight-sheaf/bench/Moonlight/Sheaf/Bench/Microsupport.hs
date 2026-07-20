{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Bench.Microsupport
  ( microsupportBenchmarks,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Sheaf.Site.Analysis.Microsupport (localMicrosupportPairwiseMeets)
import Moonlight.Sheaf.Site.Context.GeneratorCover (ContextGeneratorCover (..))
import Moonlight.Sheaf.Site.Interface.Types
  ( InterfaceDirectionEstimate (..),
    MorphismInterface (..),
  )
import Moonlight.Sheaf.Site.System
  ( AnalyzableSystem (..),
    ContextOrdinalSystem (..),
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)

microsupportBenchmarks :: Benchmark
microsupportBenchmarks =
  bgroup
    "microsupport pairwise meets"
    [ bench "generators-12" (whnf localMicrosupportPairwiseMeets (benchmarkSystem 12)),
      bench "generators-24" (whnf localMicrosupportPairwiseMeets (benchmarkSystem 24)),
      bench "generators-36" (whnf localMicrosupportPairwiseMeets (benchmarkSystem 36))
    ]

data PairwiseMeetBenchmarkSystem = PairwiseMeetBenchmarkSystem
  { benchmarkContexts :: ![Set Int],
    benchmarkGenerators :: ![Set Int]
  }

data BenchmarkTag

data BenchmarkObject = BenchmarkObject
  deriving stock (Eq, Ord)

data BenchmarkMorphism = BenchmarkIdentity
  deriving stock (Eq, Ord)

data BenchmarkMismatch

instance AnalyzableSystem PairwiseMeetBenchmarkSystem where
  type SystemTag PairwiseMeetBenchmarkSystem = BenchmarkTag
  type SystemOb PairwiseMeetBenchmarkSystem = BenchmarkObject
  type SystemMor PairwiseMeetBenchmarkSystem = BenchmarkMorphism
  type SystemCtx PairwiseMeetBenchmarkSystem = Set Int
  type SystemMismatch PairwiseMeetBenchmarkSystem = BenchmarkMismatch

  allContexts =
    benchmarkContexts

  contextLeq _ =
    Set.isSubsetOf

  systemObjectsInContext _ _ =
    [BenchmarkObject]

  systemMorphismsInContext _ _ =
    []

  restrictObject _ sourceContext targetContext objectValue
    | targetContext `Set.isSubsetOf` sourceContext = Just objectValue
    | otherwise = Nothing

  restrictMorphism _ sourceContext targetContext morphismValue
    | targetContext `Set.isSubsetOf` sourceContext = Just morphismValue
    | otherwise = Nothing

  identityMorphism _ _ _ =
    BenchmarkIdentity

  morphismSource _ _ =
    BenchmarkObject

  morphismTarget _ _ =
    BenchmarkObject

  composeMorphisms _ _ BenchmarkIdentity BenchmarkIdentity =
    Right BenchmarkIdentity

  morphismInterface _ _ =
    MorphismInterface
      { miBoundNames = Set.empty,
        miDeletedNames = Set.empty,
        miCreatedNames = Set.empty,
        miGuarded = False,
        miDirectionEstimate = InterfaceDirectionEstimate 0
      }

  normalizeMorphism _ _ =
    id

instance ContextOrdinalSystem PairwiseMeetBenchmarkSystem where
  contextOrdinal _ =
    Set.foldl' (\ordinal atom -> ordinal * 41 + atom) 0

instance ContextGeneratorCover PairwiseMeetBenchmarkSystem where
  contextGenerators =
    benchmarkGenerators

  contextIsBottom _ =
    Set.null

benchmarkSystem :: Int -> PairwiseMeetBenchmarkSystem
benchmarkSystem generatorCount =
  PairwiseMeetBenchmarkSystem
    { benchmarkContexts = generators,
      benchmarkGenerators = generators
    }
  where
    universe = Set.fromDistinctAscList [0 .. generatorCount]
    generators = fmap (`Set.delete` universe) [0 .. generatorCount - 1]
