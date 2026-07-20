-- | Aggregated reusable law predicates over the differential substrate;
-- fixture carriers and generator oracles stay on the per-family modules.
module Moonlight.Differential.Effect.Harness
  ( -- * Algebra and delta actions
    finiteMapAbelianGroupLaws,
    zSetRoundTripAndCancellation,
    zSetSizeTracksSupport,
    indexedZSetCellCountTracksSupport,
    indexedZSetUnionsDenoteAddition,
    indexedGroupingDistributesOverAddition,
    cursorTimedZSetRoundTrip,
    cursorMergeDenotesTimedZSetAddition,
    deltaIdentityNeutral,
    deltaCompositionAssociative,
    deltaIdentityAction,
    deltaCompositionActsHomomorphically,
    deltaNullHonestyExtensional,

    -- * Batches, traces, descriptions, compaction
    batchRowCountTracksLiveCells,
    batchDescriptionProjectsInterval,
    batchMergerFuelDenotesBinaryMerge,
    traceAppendDenotesReplay,
    traceSemigroupAppendDenotesReplay,
    traceAccumulationDenotesPrefixReplay,
    tracePrefixFoldRebuildsPrefix,
    traceKeyFoldsDenoteFilteredReplay,
    traceKeyRowFoldsRebuildConsolidatedBatches,
    traceNullMatchesCollapsedDenotation,
    traceDescriptionProjectsFrontiers,
    traceDescriptionAdvanceMatchesTrace,
    traceDescriptionReadAvailabilityMatchesFrontierOracle,
    timeIndexSlicingConsumesReadObligations,
    partitionedPrefixCompactionPreservesDenotation,
    partitionedPrefixCompactionObeysDescriptionSince,
    partitionedPrefixKeyMismatchTyped,
    partitionedPrefixSummaryFailureTyped,
    partitionedPrefixOutsideRunSummaryTyped,

    -- * Arrangements
    arrangeByKeyDenotesReplayedTrace,
    arrangementAppendDenotesAppendThenArrange,
    arrangementKeyFoldFiltersReplayOracle,
    arrangementSliceFoldsFilterReplayOracleByTime,

    -- * Operators
    linearOperatorsDeltaTransparent,
    indexByPartitionsReflatten,
    indexedDeltaJoinIntegratesBilinearDeltas,
    starDeltaDecompositionEqualsRecomputation,
    foldDeltaJoinConsolidatesThroughBatch,
    countByKeyLinear,

    -- * Projections
    projectionWorkObeysSharedActionAlgebra,
    projectionDeltaObeysSharedActionAlgebra,
    projectionMaintenanceMatchesRecomputation,
    projectionCommitMatchesSupportRecomputation,

    -- * Stream calculus
    integralSamplerAgreesWithGenericFold,
    productIntegralSamplerAgreesWithGenericFold,
    memoTimeIsExtensionallyIdentity,
    streamDifferentiateIntegrateInverse,
    streamMobiusInversionLawful,
    locallyFiniteMobiusInvertsClosedIntervals,
    productMobiusCoefficientsFactor,
    productMobiusSupportFactors,
    naturalPrefixExecutionAgreesWithDenotation,
    naturalScalarLinearIncrementalizationBypassesReplay,
    naturalProductPrefixExecutionAgreesWithDenotation,
    naturalProductScansFactorAsNestedScans,

    -- * Row and index plane
    rowSubstrateSetAlgebra,
    rowSubstratesCanonicalizeNegativeRawIds,
    tupleWordConversionRejectsNegativeRepresentatives,
    rowSetDenseInsertDenotesIntSetInsert,
    indexedRowsValueBucketsDenoteBindings,
    indexedRegistryUpsertDeleteLaws,
    indexedRegistryValidationReportsObstructions,
    batchProjectsIntoIndexedRows,
    traceProjectionAccumulatesDuplicatePhysicalRows,
    projectedRowsDeltaMaintainsSnapshot,
    relationAdvanceMaintainsViews,

    -- * Worst-case-optimal joins
    genericWCOJDenotation,
    adaptiveWCOJDenotation,
    indexedAdaptiveWCOJDenotation,
    fusedIndexedWCOJDenotation,
    foldAdaptiveWCOJDenotation,
    bruteForceWCOJDenotation,
    bruteForceTriangleCount,

    -- * Runtime time and frontiers
    runtimeTimeScopeLaws,
    runtimeFrontierStoresProductAntichains,
    localFactConstructionAndAntichainLaws,
    capabilityDowngradeMonotoneAccepted,
    capabilityAdvanceRegressionTyped,

    -- * Runtime settle, restriction, and rows cache
    settleQuiescentInputIsFixpoint,
    settleBudgetExhaustionHonest,
    contextRestrictionUnknownEndpointRefused,
    rowsCachePinnedDropRefused,
    rowsCacheOverBudgetObservable,
    rowsCacheOverBudgetRequiresPins,
  )
where

import Moonlight.Differential.Effect.Harness.Algebra
import Moonlight.Differential.Effect.Harness.Arrangement
  ( arrangeByKeyDenotesReplayedTrace,
    arrangementAppendDenotesAppendThenArrange,
    arrangementKeyFoldFiltersReplayOracle,
    arrangementSliceFoldsFilterReplayOracleByTime,
  )
import Moonlight.Differential.Effect.Harness.Index
import Moonlight.Differential.Effect.Harness.Operator
  ( countByKeyLinear,
    foldDeltaJoinConsolidatesThroughBatch,
    indexByPartitionsReflatten,
    indexedDeltaJoinIntegratesBilinearDeltas,
    linearOperatorsDeltaTransparent,
    starDeltaDecompositionEqualsRecomputation,
  )
import Moonlight.Differential.Effect.Harness.Projection
  ( projectionCommitMatchesSupportRecomputation,
    projectionDeltaObeysSharedActionAlgebra,
    projectionMaintenanceMatchesRecomputation,
    projectionWorkObeysSharedActionAlgebra,
  )
import Moonlight.Differential.Effect.Harness.Runtime
import Moonlight.Differential.Effect.Harness.Stream
import Moonlight.Differential.Effect.Harness.TimeFrontier
import Moonlight.Differential.Effect.Harness.Trace
  ( batchDescriptionProjectsInterval,
    batchMergerFuelDenotesBinaryMerge,
    batchRowCountTracksLiveCells,
    partitionedPrefixCompactionObeysDescriptionSince,
    partitionedPrefixCompactionPreservesDenotation,
    partitionedPrefixKeyMismatchTyped,
    partitionedPrefixOutsideRunSummaryTyped,
    partitionedPrefixSummaryFailureTyped,
    timeIndexSlicingConsumesReadObligations,
    traceAccumulationDenotesPrefixReplay,
    traceAppendDenotesReplay,
    traceDescriptionAdvanceMatchesTrace,
    traceDescriptionProjectsFrontiers,
    traceDescriptionReadAvailabilityMatchesFrontierOracle,
    traceKeyFoldsDenoteFilteredReplay,
    traceKeyRowFoldsRebuildConsolidatedBatches,
    traceNullMatchesCollapsedDenotation,
    tracePrefixFoldRebuildsPrefix,
    traceSemigroupAppendDenotesReplay,
  )
import Moonlight.Differential.Effect.Harness.WCOJ
  ( adaptiveWCOJDenotation,
    bruteForceTriangleCount,
    bruteForceWCOJDenotation,
    foldAdaptiveWCOJDenotation,
    fusedIndexedWCOJDenotation,
    genericWCOJDenotation,
    indexedAdaptiveWCOJDenotation,
  )
