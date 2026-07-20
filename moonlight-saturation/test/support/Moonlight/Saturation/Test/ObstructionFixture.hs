{-# LANGUAGE RankNTypes #-}

module Moonlight.Saturation.Test.ObstructionFixture
  ( ProbeAggregateSummary,
    AggregateReplacementProfile (..),
    AggregateMergeFixture (..),
    aggregateMergeFixture,
    AggregateLookupProfile (..),
    AggregateLookupFixture (..),
    aggregateLookupFixture,
    RegionRequest (..),
    RegionCache (..),
    RegionSummary (..),
    RegionAggregate (..),
    regionFoldAlgebra,
    regionValues,
    SearchProfile (..),
    SearchReport (..),
    SearchInput (..),
    searchInput,
    feasibleFamilySearch,
    expectedSearchSections,
    LiveRequest (..),
    LiveRefreshObstruction (..),
    ProbeLivePruningState,
    LiveRefreshFixture (..),
    liveRefreshFixture,
    livePruningAdapter,
  )
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Functor.Const (Const)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Delta.Scope
import Moonlight.Saturation.Obstruction.Cohomological.Aggregate
import Moonlight.Saturation.Obstruction.Cohomological.LivePruning
import Moonlight.Saturation.Obstruction.Cohomological.Prepared
import Moonlight.Saturation.Obstruction.Cohomological.Region
import Moonlight.Saturation.Obstruction.Cohomological.Search

type ProbeAggregateSummary = RequestAggregateSummary Int () IntSet

aggregateSummaryFromSupport :: Map Int IntSet -> ProbeAggregateSummary
aggregateSummaryFromSupport rootSupport =
  RequestAggregateSummary
    (fmap rootResolvedExact rootSupport)
    rootSupport
    (requestAggregateSupportRoots rootSupport)

data AggregateReplacementProfile = SparseReplacement | HalfReplacement
  deriving stock (Eq, Ord, Show)

data AggregateMergeFixture = AggregateMergeFixture
  { aggregateMergeAffectedRoots :: !(Set Int),
    aggregateMergePrior :: !ProbeAggregateSummary,
    aggregateMergeUpdated :: !ProbeAggregateSummary,
    aggregateMergeExpected :: !ProbeAggregateSummary
  }
  deriving stock (Eq, Show)

aggregateMergeFixture :: AggregateReplacementProfile -> Int -> AggregateMergeFixture
aggregateMergeFixture profile size =
  let roots = nonNegativeRange size
      priorSupport = supportMap 0 roots
      affectedRoots = Set.fromDistinctAscList (filter (affectedBy profile) roots)
      retainedRoots = filter ((/= 0) . (`mod` 5)) (Set.toAscList affectedRoots)
      insertedRoots = [size .. size + max 1 (Set.size affectedRoots `div` 8) - 1]
      updatedSupport = supportMap (8 * max 1 size) (retainedRoots <> insertedRoots)
   in AggregateMergeFixture
        affectedRoots
        (aggregateSummaryFromSupport priorSupport)
        (aggregateSummaryFromSupport updatedSupport)
        (aggregateSummaryFromSupport (updatedSupport <> Map.withoutKeys priorSupport affectedRoots))

affectedBy :: AggregateReplacementProfile -> Int -> Bool
affectedBy profile root =
  case profile of
    SparseReplacement -> root `mod` 64 == 0
    HalfReplacement -> even root

supportMap :: Int -> [Int] -> Map Int IntSet
supportMap offset = Map.fromList . fmap (\root -> (root, supportKeysFor offset root))

supportKeysFor :: Int -> Int -> IntSet
supportKeysFor offset root =
  IntSet.fromDistinctAscList [offset + 4 * root .. offset + 4 * root + 3]

data AggregateLookupProfile = SparseImpactedKeys | DenseImpactedKeys
  deriving stock (Eq, Ord, Show)

data AggregateLookupFixture = AggregateLookupFixture
  { aggregateLookupSummary :: !ProbeAggregateSummary,
    aggregateLookupImpactedKeys :: !IntSet,
    aggregateLookupExpectedRoots :: !(Set Int)
  }
  deriving stock (Eq, Show)

aggregateLookupFixture :: AggregateLookupProfile -> Int -> AggregateLookupFixture
aggregateLookupFixture profile size =
  let summary = aggregateMergeExpected (aggregateMergeFixture HalfReplacement size)
      rootSupport = rasRootSupport summary
      impacted = IntSet.filter (lookupKeySelected profile) (foldMap id rootSupport)
      expected = Map.keysSet (Map.filter (not . IntSet.null . IntSet.intersection impacted) rootSupport)
   in AggregateLookupFixture summary impacted expected

lookupKeySelected :: AggregateLookupProfile -> Int -> Bool
lookupKeySelected profile supportKey =
  case profile of
    SparseImpactedKeys -> supportKey `mod` 64 == 0
    DenseImpactedKeys -> even supportKey

data RegionRequest = AcceptEvenRegions
  deriving stock (Eq, Ord, Show)

newtype RegionCache = RegionCache {regionCacheAcceptedCount :: Int}
  deriving stock (Eq, Ord, Show)

newtype RegionSummary = RegionSummary {regionSummaryWeight :: Int}
  deriving stock (Eq, Ord, Show)

newtype RegionAggregate = RegionAggregate {regionAggregateWeight :: Int}
  deriving stock (Eq, Ord, Show)

regionFoldAlgebra :: RegionFoldAlgebra RegionCache RegionRequest Int RegionSummary RegionAggregate
regionFoldAlgebra =
  RegionFoldAlgebra
    { rfaAcceptRegion = \AcceptEvenRegions -> even,
      rfaAnalyzeRegion = \cache _ region -> (RegionCache (regionCacheAcceptedCount cache + 1), RegionSummary region),
      rfaInsertSummary = \_ summary aggregate -> RegionAggregate (regionAggregateWeight aggregate + regionSummaryWeight summary),
      rfaInitialAggregate = const (RegionAggregate 0)
    }

regionValues :: Int -> [Int]
regionValues size = [1 .. max 0 size]

data SearchProfile = ExhaustiveSearch | LowerBoundPrunedSearch
  deriving stock (Eq, Ord, Show)

data SearchReport = SearchReport
  { searchReportComplete :: !Bool,
    searchReportSatisfied :: !Bool
  }
  deriving stock (Eq, Ord, Show)

data SearchInput = SearchInput
  { searchInputProfile :: !SearchProfile,
    searchInputCandidates :: !(Map Int [Int])
  }
  deriving stock (Eq, Show)

searchInput :: SearchProfile -> Int -> SearchInput
searchInput profile contextCount =
  SearchInput profile (Map.fromDistinctAscList [(context, [0, 1, 2, 3]) | context <- nonNegativeRange contextCount])

feasibleFamilySearch :: SearchInput -> FeasibleFamilySearch Int Int SearchReport Int
feasibleFamilySearch input =
  FeasibleFamilySearch
    { ffSearchEvaluateFamily = evaluateSearchFamily input,
      ffSearchReportSatisfied = searchReportSatisfied,
      ffSearchLowerBound = case searchInputProfile input of
        ExhaustiveSearch -> Nothing
        LowerBoundPrunedSearch -> Just sum,
      ffSearchFixedSections = Map.empty,
      ffSearchCandidateSections = searchInputCandidates input
    }

evaluateSearchFamily :: SearchInput -> Map Int Int -> (SearchReport, Int)
evaluateSearchFamily input sections =
  let complete = Map.size sections == Map.size (searchInputCandidates input)
      satisfied =
        complete
          && case searchInputProfile input of
            ExhaustiveSearch -> all (== 3) sections
            LowerBoundPrunedSearch -> True
   in (SearchReport complete satisfied, sum sections)

expectedSearchSections :: SearchInput -> Map Int Int
expectedSearchSections input =
  fmap (const expectedSection) (searchInputCandidates input)
  where
    expectedSection =
      case searchInputProfile input of
        ExhaustiveSearch -> 3
        LowerBoundPrunedSearch -> 0

data LiveRequest runtime = LiveRequest
  { liveRequestId :: !Int,
    liveRequestRoots :: !(Set Int)
  }
  deriving stock (Eq, Ord, Show)

data LiveRefreshObstruction = MissingPreparedLiveRefresh !Int
  deriving stock (Eq, Ord, Show)

type ProbeLivePruningState = LivePruningState () Int Int () IntSet LiveRefreshObstruction

data LiveRefreshFixture = LiveRefreshFixture
  { liveRefreshDelta :: !(ObstructionDelta Int),
    liveRefreshRequests :: ![LiveRequest ()],
    liveRefreshPriorState :: !ProbeLivePruningState,
    liveRefreshUpdatedRequests :: !(Map Int ProbeAggregateSummary),
    liveRefreshExpectedState :: !ProbeLivePruningState
  }
  deriving stock (Eq, Show)

liveRefreshFixture :: Int -> Int -> LiveRefreshFixture
liveRefreshFixture requestCount rootsPerRequest =
  let totalRoots = requestCount * rootsPerRequest
      requests = fmap (liveRequest rootsPerRequest) (nonNegativeRange requestCount)
      impacted = Set.fromDistinctAscList (filter ((== 0) . (`mod` 8)) (nonNegativeRange totalRoots))
      prior = Map.fromList [(liveRequestKey request, requestSummary Just request) | request <- requests]
      updated =
        Map.fromList
          [ (liveRequestId request, requestSummary (updatedKey impacted totalRoots) request)
          | request <- requests
          ]
      expectedRequests =
        Map.fromList
          [ (liveRequestKey request, requestSummary (Just . expectedKey impacted totalRoots) request)
          | request <- requests
          ]
      priorState = (emptyLivePruningState ()) {lpsRequests = prior, lpsReusableRequestKeys = Map.keysSet prior}
      expectedState =
        priorState
          { lpsRequests = expectedRequests,
            lpsReusableRequestKeys = Map.keysSet expectedRequests,
            lpsRefreshObstruction = Nothing
          }
   in LiveRefreshFixture
        (scopedDelta (dirtyScope (IntSet.fromDistinctAscList (Set.toAscList impacted))) Nothing)
        requests
        priorState
        updated
        expectedState

liveRequest :: Int -> Int -> LiveRequest ()
liveRequest rootsPerRequest requestId =
  LiveRequest requestId (Set.fromDistinctAscList [requestId * rootsPerRequest .. (requestId + 1) * rootsPerRequest - 1])

requestSummary :: (Int -> Maybe Int) -> LiveRequest runtime -> ProbeAggregateSummary
requestSummary supportKey request =
  aggregateSummaryFromSupport
    (Map.fromDistinctAscList (mapMaybe (\root -> fmap ((,) root . IntSet.singleton) (supportKey root)) (Set.toAscList (liveRequestRoots request))))

updatedKey :: Set Int -> Int -> Int -> Maybe Int
updatedKey impacted totalRoots root
  | Set.member root impacted = Just (totalRoots + root)
  | otherwise = Nothing

expectedKey :: Set Int -> Int -> Int -> Int
expectedKey impacted totalRoots root =
  if Set.member root impacted then totalRoots + root else root

livePruningAdapter :: Map Int ProbeAggregateSummary -> LivePruningAdapter () LiveRequest (Const ()) Int () LiveRefreshObstruction Int IntSet Int
livePruningAdapter updated =
  LivePruningAdapter
    { lpaRequestKey = liveRequestKey,
      lpaRequestRoots = \() -> liveRequestRoots,
      lpaRetainRequestState = const True,
      lpaRootKey = id,
      lpaCanonicalizeRoot = const id,
      lpaRefreshRequest = \_ () request _ _ ->
        maybe (Left (MissingPreparedLiveRefresh (liveRequestId request))) Right (Map.lookup (liveRequestId request) updated),
      lpaExactMatches = \() _ _ _ -> []
    }

liveRequestKey :: LiveRequest runtime -> PreparedRequestCacheKey Int
liveRequestKey request =
  mkPreparedRequestCacheKey (liveRequestId request) (liveRequestId request) Nothing

nonNegativeRange :: Int -> [Int]
nonNegativeRange size = [0 .. max 0 size - 1]
