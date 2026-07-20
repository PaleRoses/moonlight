{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ObstructionBench
  ( obstructionBenchmarks,
  )
where

import BenchSupport
import Control.DeepSeq (NFData (..))
import Control.Foldl qualified as Foldl
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Moonlight.Saturation.Obstruction.Cohomological.Aggregate
import Moonlight.Saturation.Obstruction.Cohomological.LivePruning
import Moonlight.Saturation.Obstruction.Cohomological.Prepared
import Moonlight.Saturation.Obstruction.Cohomological.Region
import Moonlight.Saturation.Obstruction.Cohomological.Search
import Moonlight.Saturation.Test.ObstructionFixture
import Test.Tasty.Bench (Benchmark)

data AggregateDigest = AggregateDigest
  { aggregateDigestResolutions :: !PopulationDigest,
    aggregateDigestExactResolutionCount :: !Int,
    aggregateDigestRootSupport :: !PopulationDigest,
    aggregateDigestInverseSupport :: !PopulationDigest,
    aggregateDigestInverseMembershipCount :: !Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data LiveRefreshDigest = LiveRefreshDigest
  { liveRefreshDigestRequests :: !PopulationDigest,
    liveRefreshDigestReusableRequests :: !PopulationDigest,
    liveRefreshDigestRootCount :: !Int,
    liveRefreshDigestSupportCount :: !Int,
    liveRefreshDigestInverseMembershipCount :: !Int,
    liveRefreshDigestExactScopeCount :: !Int,
    liveRefreshDigestHasObstruction :: !Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data SearchDigest = SearchDigest
  { searchDigestContexts :: !PopulationDigest,
    searchDigestSections :: !PopulationDigest,
    searchDigestComplete :: !Bool,
    searchDigestSatisfied :: !Bool,
    searchDigestCost :: !Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

obstructionBenchmarks :: Either BenchmarkObstruction Benchmark
obstructionBenchmarks =
  validatedBenchmarkGroup
    "obstruction"
    [ validatedBenchmarkFamily "merge-sparse-replacement" (mergeBenchmark SparseReplacement) aggregateScales,
      validatedBenchmarkFamily "merge-half-replacement" (mergeBenchmark HalfReplacement) aggregateScales,
      validatedBenchmarkFamily "lookup-sparse-keys" (lookupBenchmark SparseImpactedKeys) aggregateScales,
      validatedBenchmarkFamily "lookup-dense-keys" (lookupBenchmark DenseImpactedKeys) aggregateScales,
      validatedBenchmarkFamily "refresh-live-pruning" liveRefreshBenchmark liveRefreshScales,
      validatedBenchmarkFamily "search-exhaustive" (searchBenchmark ExhaustiveSearch) exhaustiveSearchScales,
      validatedBenchmarkFamily "search-lower-bound-pruned" (searchBenchmark LowerBoundPrunedSearch) prunedSearchScales,
      validatedBenchmarkFamily "fold-regions-mixed" regionBenchmark aggregateScales
    ]

aggregateScales :: [Int]
aggregateScales = [64, 512, 4096]

liveRefreshScales :: [(Int, Int)]
liveRefreshScales = [(4, 64), (16, 256), (64, 1024)]

exhaustiveSearchScales :: [Int]
exhaustiveSearchScales = [4, 6, 8]

prunedSearchScales :: [Int]
prunedSearchScales = [8, 16, 32]

mergeBenchmark :: AggregateReplacementProfile -> Int -> Either BenchmarkObstruction Benchmark
mergeBenchmark profile size =
  let caseName = benchmarkCaseLabel "roots" size
      fixture = aggregateMergeFixture profile size
   in validatedPureBenchmark
        ObstructionBenchmarkLane
        caseName
        (aggregateDigest (aggregateMergeExpected fixture))
        (rnf . show)
        rnf
        mergeAggregateDigest
        fixture

lookupBenchmark :: AggregateLookupProfile -> Int -> Either BenchmarkObstruction Benchmark
lookupBenchmark profile size =
  let caseName = benchmarkCaseLabel "roots" size
      fixture = aggregateLookupFixture profile size
   in validatedPureBenchmark
        ObstructionBenchmarkLane
        caseName
        (populationDigest id (aggregateLookupExpectedRoots fixture))
        (rnf . show)
        rnf
        lookupAggregateDigest
        fixture

liveRefreshBenchmark :: (Int, Int) -> Either BenchmarkObstruction Benchmark
liveRefreshBenchmark (requestCount, rootsPerRequest) =
  let caseName =
        "requests=" <> show requestCount <> " roots/request=" <> show rootsPerRequest
      fixture = liveRefreshFixture requestCount rootsPerRequest
   in validatedPureBenchmark
        ObstructionBenchmarkLane
        caseName
        (Right (liveRefreshDigest (liveRefreshExpectedState fixture)))
        (rnf . show)
        (forceEither rnf)
        refreshLiveDigest
        fixture

searchBenchmark :: SearchProfile -> Int -> Either BenchmarkObstruction Benchmark
searchBenchmark profile contextCount =
  let caseName = benchmarkCaseLabel "contexts" contextCount
      input = searchInput profile contextCount
      expectedSections = expectedSearchSections input
      expected =
        Just
          SearchDigest
            { searchDigestContexts = PopulationDigest contextCount (sumFromZero contextCount),
              searchDigestSections = PopulationDigest contextCount (sum expectedSections),
              searchDigestComplete = True,
              searchDigestSatisfied = True,
              searchDigestCost = sum expectedSections
            }
   in validatedPureBenchmark
        ObstructionBenchmarkLane
        caseName
        expected
        (rnf . show)
        rnf
        searchDigest
        input

regionBenchmark :: Int -> Either BenchmarkObstruction Benchmark
regionBenchmark size =
  let caseName = benchmarkCaseLabel "regions" size
      acceptedCount = size `div` 2
      expected = PopulationDigest acceptedCount (acceptedCount * (acceptedCount + 1))
   in validatedPureBenchmark
        ObstructionBenchmarkLane
        caseName
        expected
        rnf
        rnf
        foldRegionDigest
        (regionValues size)

mergeAggregateDigest :: AggregateMergeFixture -> AggregateDigest
mergeAggregateDigest fixture =
  aggregateDigest
    ( mergeRequestAggregateSummaries
        (aggregateMergeAffectedRoots fixture)
        (aggregateMergePrior fixture)
        (aggregateMergeUpdated fixture)
    )

lookupAggregateDigest :: AggregateLookupFixture -> PopulationDigest
lookupAggregateDigest fixture =
  populationDigest id
    ( rootsSupportedByKeys
        (aggregateLookupImpactedKeys fixture)
        (aggregateLookupSummary fixture)
    )

aggregateDigest :: ProbeAggregateSummary -> AggregateDigest
aggregateDigest summary =
  let resolutions = rasRootResolutions summary
      inverseSupport = rasSupportRoots summary
   in AggregateDigest
        { aggregateDigestResolutions =
            populationDigest
              (\(root, resolution) -> root + IntSet.foldl' (+) 0 (rootResolutionCoverage resolution))
              (Map.toAscList resolutions),
          aggregateDigestExactResolutionCount =
            Map.foldl' (\count resolution -> count + fromEnum (rootResolutionExactResolved resolution)) 0 resolutions,
          aggregateDigestRootSupport =
            populationDigest
              (\(root, supportKeys) -> root + IntSet.foldl' (+) 0 supportKeys)
              (Map.toAscList (rasRootSupport summary)),
          aggregateDigestInverseSupport =
            populationDigest
              (\(supportKey, roots) -> supportKey + Set.foldl' (+) 0 roots)
              (IntMap.toAscList inverseSupport),
          aggregateDigestInverseMembershipCount = sum (fmap Set.size inverseSupport)
        }

refreshLiveDigest :: LiveRefreshFixture -> Either LiveRefreshObstruction LiveRefreshDigest
refreshLiveDigest fixture =
  fmap
    liveRefreshDigest
    ( refreshLivePruningState
        (livePruningAdapter (liveRefreshUpdatedRequests fixture))
        (liveRefreshDelta fixture)
        ()
        (liveRefreshRequests fixture)
        (liveRefreshPriorState fixture)
    )

liveRefreshDigest :: ProbeLivePruningState -> LiveRefreshDigest
liveRefreshDigest state =
  let requestDigests = fmap aggregateDigest (lpsRequests state)
   in LiveRefreshDigest
        { liveRefreshDigestRequests =
            populationDigest
              (\(requestKey, digest) -> preparedRequestKeyWeight requestKey + aggregateDigestChecksum digest)
              (Map.toAscList requestDigests),
          liveRefreshDigestReusableRequests =
            populationDigest preparedRequestKeyWeight (lpsReusableRequestKeys state),
          liveRefreshDigestRootCount = sum (fmap (populationCount . aggregateDigestResolutions) requestDigests),
          liveRefreshDigestSupportCount = sum (fmap (populationCount . aggregateDigestRootSupport) requestDigests),
          liveRefreshDigestInverseMembershipCount = sum (fmap aggregateDigestInverseMembershipCount requestDigests),
          liveRefreshDigestExactScopeCount = length (lpsExactScopeCover state),
          liveRefreshDigestHasObstruction = maybe False (const True) (lpsRefreshObstruction state)
        }

aggregateDigestChecksum :: AggregateDigest -> Int
aggregateDigestChecksum digest =
  populationChecksum (aggregateDigestResolutions digest)
    + aggregateDigestExactResolutionCount digest
    + populationChecksum (aggregateDigestRootSupport digest)
    + populationChecksum (aggregateDigestInverseSupport digest)
    + aggregateDigestInverseMembershipCount digest

preparedRequestKeyWeight :: PreparedRequestCacheKey Int -> Int
preparedRequestKeyWeight key =
  prckQueryFingerprint key
    + prckPurpose key
    + maybe 0 id (prckEnvironmentFingerprint key)

searchDigest :: SearchInput -> Maybe SearchDigest
searchDigest input =
  fmap feasibleFamilyDigest
    (chooseMinimumFeasibleFamily (feasibleFamilySearch input))

feasibleFamilyDigest :: FeasibleFamily Int Int SearchReport Int -> SearchDigest
feasibleFamilyDigest family =
  let sections = ffsChosenSections family
      report = ffsReport family
   in SearchDigest
        { searchDigestContexts = populationDigest id (Map.keys sections),
          searchDigestSections = populationDigest id sections,
          searchDigestComplete = searchReportComplete report,
          searchDigestSatisfied = searchReportSatisfied report,
          searchDigestCost = ffsCost family
        }

foldRegionDigest :: [Int] -> PopulationDigest
foldRegionDigest input =
  let (finalCache, finalAggregate) =
        Foldl.fold
          ( regionFoldForRequest
              regionFoldAlgebra
              (RegionCache 0)
              AcceptEvenRegions
          )
          input
   in PopulationDigest (regionCacheAcceptedCount finalCache) (regionAggregateWeight finalAggregate)
