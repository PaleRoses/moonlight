module Moonlight.Saturation.Obstruction.Cohomological.Aggregate
  ( RootFallbackDecision (..),
    RootObservation (..),
    RootEvidence,
    emptyRootEvidence,
    singletonRootEvidence,
    rootEvidenceFromObservations,
    rootEvidenceObservations,
    rootEvidenceContains,
    rootEvidenceHasExactFeasible,
    rootEvidenceHasExactSkipped,
    rootEvidenceHasExactTruncated,
    rootEvidenceHasExactInfeasible,
    rootEvidenceHasExactMatch,
    RootResolution,
    rootObstructed,
    rootResolvedExact,
    rootInfeasible,
    rootUnresolved,
    foldRootResolution,
    rootResolutionCoverage,
    rootResolutionEvidence,
    rootFallbackDecision,
    mergeRootResolution,
    rootResolutionExcludesFallback,
    rootResolutionExactResolved,
    RequestAggregateSummary (..),
    emptyRequestAggregateSummary,
    RequestAggregateProjection (..),
    indexRootSupport,
    insertRequestAggregateSummaryWith,
    rootResolutionFromRegionOutcome,
    rootResolutionFromRegionTraversal,
    requestAggregateSupportRoots,
    requestAggregateRootCount,
    mergeRequestAggregateSummaries,
    RootInvalidation (..),
    affectedRootsForDelta,
    rootsSupportedByKeys,
    rootsMatchingResolution,
  )
where

import Data.Bits ((.|.), bit, testBit)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word8)
import Moonlight.Delta.Scope
  ( Scope,
    Scoped,
    scopeKeys,
    scopedDeltaPayload,
    scopedDeltaSupport,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section.Region
  ( RegionAnalysisOutcome (..),
    RegionExactCoverage,
    RegionExactness (..),
    RegionTraversalSummary,
    rtsCoverage,
    rtsOutcome,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section
  ( SectionFeasibilityFailure (CoverageGap, EmptySupport),
  )

type RootFallbackDecision :: Type
data RootFallbackDecision
  = FallbackObstructed
  | FallbackResolvedExactMatch
  | FallbackInfeasible
  | FallbackUnresolved
  deriving stock (Eq, Ord, Show, Read)

type RootObservation :: Type -> Type
data RootObservation witness
  = ExactFeasibleObserved
  | ExactSkippedObserved
  | ExactTruncatedObserved !witness
  | ExactInfeasibleObserved
  | ExactMatchObserved
  deriving stock (Eq, Ord, Show, Read)

type RootObservationFlag :: Type
data RootObservationFlag
  = ExactFeasibleFlag
  | ExactSkippedFlag
  | ExactTruncatedFlag
  | ExactInfeasibleFlag
  | ExactMatchFlag
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

type RootEvidence :: Type -> Type
data RootEvidence witness = RootEvidence
  { rootEvidenceMask :: !Word8,
    rootEvidenceTruncatedWitnesses :: ![witness]
  }
  deriving stock (Eq, Ord, Show, Read)

instance Semigroup (RootEvidence witness) where
  RootEvidence leftMask leftWitnesses <> RootEvidence rightMask rightWitnesses =
    RootEvidence (leftMask .|. rightMask) (leftWitnesses <> rightWitnesses)

instance Monoid (RootEvidence witness) where
  mempty =
    emptyRootEvidence

emptyRootEvidence :: RootEvidence witness
emptyRootEvidence =
  RootEvidence 0 []

singletonRootEvidence :: RootObservation witness -> RootEvidence witness
singletonRootEvidence observation =
  RootEvidence
    (observationMask observation)
    (truncatedWitnesses observation)

rootEvidenceFromObservations ::
  Foldable f =>
  f (RootObservation witness) ->
  RootEvidence witness
rootEvidenceFromObservations =
  foldMap singletonRootEvidence

rootEvidenceObservations :: RootEvidence witness -> [RootObservation witness]
rootEvidenceObservations evidence =
  exactFlagObservations evidence <> fmap ExactTruncatedObserved (rootEvidenceTruncatedWitnesses evidence)

rootEvidenceContains :: Eq witness => RootObservation witness -> RootEvidence witness -> Bool
rootEvidenceContains observation evidence =
  case observation of
    ExactTruncatedObserved witness ->
      witness `elem` rootEvidenceTruncatedWitnesses evidence
    _ ->
      testBit (rootEvidenceMask evidence) (fromEnum (observationFlag observation))

rootEvidenceHasExactFeasible :: RootEvidence witness -> Bool
rootEvidenceHasExactFeasible evidence =
  testBit (rootEvidenceMask evidence) (fromEnum ExactFeasibleFlag)

rootEvidenceHasExactSkipped :: RootEvidence witness -> Bool
rootEvidenceHasExactSkipped evidence =
  testBit (rootEvidenceMask evidence) (fromEnum ExactSkippedFlag)

rootEvidenceHasExactTruncated :: RootEvidence witness -> Bool
rootEvidenceHasExactTruncated evidence =
  testBit (rootEvidenceMask evidence) (fromEnum ExactTruncatedFlag)

rootEvidenceHasExactInfeasible :: RootEvidence witness -> Bool
rootEvidenceHasExactInfeasible evidence =
  testBit (rootEvidenceMask evidence) (fromEnum ExactInfeasibleFlag)

rootEvidenceHasExactMatch :: RootEvidence witness -> Bool
rootEvidenceHasExactMatch evidence =
  testBit (rootEvidenceMask evidence) (fromEnum ExactMatchFlag)

observationMask :: RootObservation witness -> Word8
observationMask observation =
  bit (fromEnum (observationFlag observation))

observationFlag :: RootObservation witness -> RootObservationFlag
observationFlag observation =
  case observation of
    ExactFeasibleObserved -> ExactFeasibleFlag
    ExactSkippedObserved -> ExactSkippedFlag
    ExactTruncatedObserved _ -> ExactTruncatedFlag
    ExactInfeasibleObserved -> ExactInfeasibleFlag
    ExactMatchObserved -> ExactMatchFlag

truncatedWitnesses :: RootObservation witness -> [witness]
truncatedWitnesses observation =
  case observation of
    ExactTruncatedObserved witness -> [witness]
    _ -> []

exactFlagObservations :: RootEvidence witness -> [RootObservation witness]
exactFlagObservations evidence =
  [ observation
    | (observation, flag) <-
        [ (ExactFeasibleObserved, ExactFeasibleFlag),
          (ExactSkippedObserved, ExactSkippedFlag),
          (ExactInfeasibleObserved, ExactInfeasibleFlag),
          (ExactMatchObserved, ExactMatchFlag)
        ],
      testBit (rootEvidenceMask evidence) (fromEnum flag)
  ]

type RootResolution :: Type -> Type -> Type
data RootResolution witness coverage
  = RootObstructed !coverage
  | RootResolvedExact !coverage
  | RootInfeasible !coverage
  | RootUnresolved !(RootEvidence witness) !coverage
  deriving stock (Eq, Ord, Show, Read)

rootObstructed :: coverage -> RootResolution witness coverage
rootObstructed =
  RootObstructed

rootResolvedExact :: coverage -> RootResolution witness coverage
rootResolvedExact =
  RootResolvedExact

rootInfeasible :: coverage -> RootResolution witness coverage
rootInfeasible =
  RootInfeasible

rootUnresolved :: RootEvidence witness -> coverage -> RootResolution witness coverage
rootUnresolved evidence coverage
  | rootEvidenceOnlyInfeasible evidence =
      RootInfeasible coverage
  | otherwise =
      RootUnresolved evidence coverage

foldRootResolution ::
  (coverage -> result) ->
  (coverage -> result) ->
  (coverage -> result) ->
  (RootEvidence witness -> coverage -> result) ->
  RootResolution witness coverage ->
  result
foldRootResolution onObstructed onResolvedExact onInfeasible onUnresolved rootResolution =
  case rootResolution of
    RootObstructed coverage ->
      onObstructed coverage
    RootResolvedExact coverage ->
      onResolvedExact coverage
    RootInfeasible coverage ->
      onInfeasible coverage
    RootUnresolved evidence coverage ->
      onUnresolved evidence coverage

rootResolutionCoverage :: RootResolution witness coverage -> coverage
rootResolutionCoverage =
  foldRootResolution id id id (const id)

rootResolutionEvidence :: RootResolution witness coverage -> RootEvidence witness
rootResolutionEvidence rootResolution =
  case rootResolution of
    RootObstructed {} ->
      mempty
    RootResolvedExact {} ->
      rootEvidenceFromObservations
        [ ExactFeasibleObserved,
          ExactMatchObserved
        ]
    RootInfeasible {} ->
      singletonRootEvidence ExactInfeasibleObserved
    RootUnresolved evidence _ ->
      evidence

rootFallbackDecision :: RootResolution witness coverage -> RootFallbackDecision
rootFallbackDecision rootResolution =
  case rootResolution of
    RootObstructed {} ->
      FallbackObstructed
    RootResolvedExact {} ->
      FallbackResolvedExactMatch
    RootInfeasible {} ->
      FallbackInfeasible
    RootUnresolved {} ->
      FallbackUnresolved

mergeRootResolution ::
  Semigroup coverage =>
  RootResolution witness coverage ->
  RootResolution witness coverage ->
  RootResolution witness coverage
mergeRootResolution leftResolution rightResolution =
  let mergedCoverage =
        rootResolutionCoverage leftResolution <> rootResolutionCoverage rightResolution
      mergedEvidence =
        rootResolutionEvidence leftResolution <> rootResolutionEvidence rightResolution
   in case (leftResolution, rightResolution) of
        (RootObstructed {}, RootObstructed {}) ->
          RootObstructed mergedCoverage
        (RootResolvedExact {}, RootResolvedExact {}) ->
          RootResolvedExact mergedCoverage
        _
          | rootEvidenceOnlyInfeasible mergedEvidence ->
              RootInfeasible mergedCoverage
          | otherwise ->
              RootUnresolved mergedEvidence mergedCoverage

rootResolutionExcludesFallback :: RootResolution witness coverage -> Bool
rootResolutionExcludesFallback =
  (/= FallbackUnresolved) . rootFallbackDecision

rootResolutionExactResolved :: RootResolution witness coverage -> Bool
rootResolutionExactResolved =
  (== FallbackResolvedExactMatch) . rootFallbackDecision

rootEvidenceOnlyInfeasible :: RootEvidence witness -> Bool
rootEvidenceOnlyInfeasible evidence =
  rootEvidenceHasExactInfeasible evidence
    && not (rootEvidenceHasExactFeasible evidence)
    && not (rootEvidenceHasExactSkipped evidence)
    && not (rootEvidenceHasExactTruncated evidence)

type RequestAggregateSummary :: Type -> Type -> Type -> Type
data RequestAggregateSummary root witness coverage = RequestAggregateSummary
  { rasRootResolutions :: !(Map.Map root (RootResolution witness coverage)),
    rasRootSupport :: !(Map.Map root IntSet),
    rasSupportRoots :: !(IntMap (Set root))
  }
  deriving stock (Eq, Show)

emptyRequestAggregateSummary :: RequestAggregateSummary root witness coverage
emptyRequestAggregateSummary =
  RequestAggregateSummary
    { rasRootResolutions = Map.empty,
      rasRootSupport = Map.empty,
      rasSupportRoots = IntMap.empty
    }

type RequestAggregateProjection :: Type -> Type -> Type -> Type -> Type
data RequestAggregateProjection summary root witness coverage = RequestAggregateProjection
  { rapRoot :: !(summary -> root),
    rapSupportKeys :: !(summary -> IntSet),
    rapResolution :: !(summary -> RootResolution witness coverage)
  }

insertRequestAggregateSummaryWith ::
  (Ord root, Semigroup coverage) =>
  RequestAggregateProjection summary root witness coverage ->
  summary ->
  RequestAggregateSummary root witness coverage ->
  RequestAggregateSummary root witness coverage
insertRequestAggregateSummaryWith projection summary aggregateSummary =
  let rootValue =
        rapRoot projection summary
      supportKeys =
        rapSupportKeys projection summary
   in RequestAggregateSummary
        { rasRootResolutions =
            Map.insertWith
              mergeRootResolution
              rootValue
              (rapResolution projection summary)
              (rasRootResolutions aggregateSummary),
          rasRootSupport =
            Map.insertWith
              IntSet.union
              rootValue
              supportKeys
              (rasRootSupport aggregateSummary),
          rasSupportRoots =
            indexRootSupport
              rootValue
              supportKeys
              (rasSupportRoots aggregateSummary)
        }
{-# INLINE insertRequestAggregateSummaryWith #-}

rootResolutionFromRegionOutcome ::
  (coverage -> Bool) ->
  RegionAnalysisOutcome gap witness pruning ->
  coverage ->
  RootResolution witness coverage
rootResolutionFromRegionOutcome hasExactMatch outcome coverage
  | regionOutcomeIsObstructed outcome =
      rootObstructed coverage
  | regionOutcomeIsResolved outcome && hasExactMatch coverage =
      rootResolvedExact coverage
  | otherwise =
      rootUnresolved (rootEvidenceFromRegionOutcome hasExactMatch outcome coverage) coverage
{-# INLINE rootResolutionFromRegionOutcome #-}

rootResolutionFromRegionTraversal ::
  (coverage -> Bool) ->
  (RegionExactCoverage match gap -> coverage) ->
  RegionTraversalSummary region match gap witness pruning ->
  RootResolution witness coverage
rootResolutionFromRegionTraversal hasExactMatch projectCoverage traversalSummary =
  rootResolutionFromRegionOutcome
    hasExactMatch
    (rtsOutcome traversalSummary)
    (projectCoverage (rtsCoverage traversalSummary))
{-# INLINE rootResolutionFromRegionTraversal #-}

rootEvidenceFromRegionOutcome ::
  (coverage -> Bool) ->
  RegionAnalysisOutcome gap witness pruning ->
  coverage ->
  RootEvidence witness
rootEvidenceFromRegionOutcome hasExactMatch outcome coverage =
  rootEvidenceFromObservations
    ( exactFeasibleObservation outcome
        <> exactSkippedObservation outcome
        <> exactTruncatedObservation outcome
        <> exactInfeasibleObservation outcome
        <> exactMatchObservation coverage
    )
  where
    exactMatchObservation coverageValue =
      [ExactMatchObserved | hasExactMatch coverageValue]

regionOutcomeIsObstructed :: RegionAnalysisOutcome gap witness pruning -> Bool
regionOutcomeIsObstructed outcome =
  case outcome of
    RegionAnalysisPruned {} -> True
    RegionAnalysisObstructed {} -> True
    RegionAnalysisTruncated {} -> False
    RegionAnalysisExact {} -> False

regionOutcomeIsResolved :: RegionAnalysisOutcome gap witness pruning -> Bool
regionOutcomeIsResolved outcome =
  case outcome of
    RegionAnalysisPruned _ -> False
    RegionAnalysisObstructed _ -> False
    RegionAnalysisTruncated _ -> False
    RegionAnalysisExact exactness ->
      regionExactnessIsResolved exactness

regionExactnessIsResolved :: RegionExactness gap -> Bool
regionExactnessIsResolved exactness =
  case exactness of
    ExactCoverageFeasible -> True
    ExactCoverageInfeasible EmptySupport -> True
    ExactCoverageInfeasible (CoverageGap _) -> False
    ExactCoverageSkipped -> False

exactFeasibleObservation :: RegionAnalysisOutcome gap witness pruning -> [RootObservation witness]
exactFeasibleObservation outcome =
  case outcome of
    RegionAnalysisExact ExactCoverageFeasible -> [ExactFeasibleObserved]
    _ -> []

exactSkippedObservation :: RegionAnalysisOutcome gap witness pruning -> [RootObservation witness]
exactSkippedObservation outcome =
  case outcome of
    RegionAnalysisExact ExactCoverageSkipped -> [ExactSkippedObserved]
    _ -> []

exactTruncatedObservation :: RegionAnalysisOutcome gap witness pruning -> [RootObservation witness]
exactTruncatedObservation outcome =
  case outcome of
    RegionAnalysisTruncated witness -> [ExactTruncatedObserved witness]
    _ -> []

exactInfeasibleObservation :: RegionAnalysisOutcome gap witness pruning -> [RootObservation witness]
exactInfeasibleObservation outcome =
  case outcome of
    RegionAnalysisExact (ExactCoverageInfeasible _) -> [ExactInfeasibleObserved]
    _ -> []

type RootInvalidation :: Type -> Type
data RootInvalidation root
  = RootInvalidationAll
  | RootInvalidationSome !(Set root)
  deriving stock (Eq, Ord, Show, Read)

requestAggregateRootCount :: RequestAggregateSummary root witness coverage -> Int
requestAggregateRootCount =
  Map.size . rasRootResolutions

indexRootSupport ::
  Ord root =>
  root ->
  IntSet ->
  IntMap (Set root) ->
  IntMap (Set root)
indexRootSupport rootValue supportKeys supportRoots =
  IntSet.foldr
    (\supportKey -> IntMap.insertWith Set.union supportKey (Set.singleton rootValue))
    supportRoots
    supportKeys

requestAggregateSupportRoots ::
  Ord root =>
  Map.Map root IntSet ->
  IntMap (Set root)
requestAggregateSupportRoots =
  Map.foldrWithKey indexRootSupport IntMap.empty

mergeRequestAggregateSummaries ::
  Ord root =>
  Set root ->
  RequestAggregateSummary root witness coverage ->
  RequestAggregateSummary root witness coverage ->
  RequestAggregateSummary root witness coverage
mergeRequestAggregateSummaries affectedRoots priorSummary updatedSummary =
  let mergedRootSupport =
        rasRootSupport updatedSummary
          <> Map.withoutKeys (rasRootSupport priorSummary) affectedRoots
      replacedRoots =
        affectedRoots <> Map.keysSet (rasRootSupport updatedSummary)
      removedRootSupport =
        Map.restrictKeys (rasRootSupport priorSummary) replacedRoots
   in RequestAggregateSummary
        { rasRootResolutions =
            rasRootResolutions updatedSummary
              <> Map.withoutKeys (rasRootResolutions priorSummary) affectedRoots,
          rasRootSupport =
            mergedRootSupport,
          rasSupportRoots =
            replaceRootSupports
              removedRootSupport
              (rasRootSupport updatedSummary)
              (rasSupportRoots priorSummary)
        }

unindexRootSupport ::
  Ord root =>
  root ->
  IntSet ->
  IntMap (Set root) ->
  IntMap (Set root)
unindexRootSupport rootValue supportKeys supportRoots =
  IntSet.foldr
    (IntMap.update removeRoot)
    supportRoots
    supportKeys
  where
    removeRoot roots =
      let remainingRoots = Set.delete rootValue roots
       in if Set.null remainingRoots
            then Nothing
            else Just remainingRoots

replaceRootSupports ::
  Ord root =>
  Map.Map root IntSet ->
  Map.Map root IntSet ->
  IntMap (Set root) ->
  IntMap (Set root)
replaceRootSupports removedRootSupport insertedRootSupport supportRoots =
  Map.foldrWithKey
    indexRootSupport
    (Map.foldrWithKey unindexRootSupport supportRoots removedRootSupport)
    insertedRootSupport

affectedRootsForDelta ::
  Ord root =>
  (payload -> RequestAggregateSummary root witness coverage -> RootInvalidation root) ->
  Scoped IntSet payload ->
  RequestAggregateSummary root witness coverage ->
  Set root
affectedRootsForDelta payloadInvalidation matchingDelta aggregateSummary =
  case scopedDeltaPayload matchingDelta of
    Nothing ->
      affectedRootsForScope
        (scopedDeltaSupport matchingDelta)
        aggregateSummary
    Just payload ->
      case payloadInvalidation payload aggregateSummary of
        RootInvalidationAll ->
          aggregateRootSet aggregateSummary
        RootInvalidationSome payloadRoots ->
          Set.union
            payloadRoots
            ( affectedRootsForScope
                (scopedDeltaSupport matchingDelta)
                aggregateSummary
            )
{-# INLINE affectedRootsForDelta #-}

affectedRootsForScope ::
  Ord root =>
  Scope IntSet ->
  RequestAggregateSummary root witness coverage ->
  Set root
affectedRootsForScope frontier aggregateSummary =
  case scopeKeys frontier of
    Nothing ->
      aggregateRootSet aggregateSummary
    Just impactedKeys ->
      rootsSupportedByKeys impactedKeys aggregateSummary

aggregateRootSet ::
  RequestAggregateSummary root witness coverage ->
  Set root
aggregateRootSet =
  Map.keysSet . rasRootResolutions

rootsSupportedByKeys ::
  Ord root =>
  IntSet ->
  RequestAggregateSummary root witness coverage ->
  Set root
rootsSupportedByKeys impactedKeys aggregateSummary
  | IntSet.null impactedKeys =
      Set.empty
  | otherwise =
      IntSet.foldr
        (Set.union . rootsForSupportKey)
        Set.empty
        impactedKeys
  where
    rootsForSupportKey supportKey =
      IntMap.findWithDefault Set.empty supportKey (rasSupportRoots aggregateSummary)

rootsMatchingResolution ::
  (RootResolution witness coverage -> Bool) ->
  Map.Map root (RootResolution witness coverage) ->
  Set root
rootsMatchingResolution predicate =
  Map.keysSet . Map.filter predicate
