{-# LANGUAGE DerivingStrategies #-}

-- | Law-name vocabulary for the differential substrate's equational surface.
module Moonlight.Differential.Effect.LawNames
  ( LawName (..),
    lawName,
  )
where

import Data.Kind (Type)
import Moonlight.Core (IsLawName (..), constructorLawNameWithOverrides)

type LawName :: Type
data LawName
  = FiniteMapAbelianGroup
  | ZSetGroupCancellation
  | ZSetCanonicalSupportSize
  | IndexedZSetCanonicalSupportCellCount
  | IndexedZSetUnionsDenoteIndexedAddition
  | IndexedGroupingDistributesOverAddition
  | CursorPreservesTimedCanonicalOrder
  | CursorMergeDenotesTimedAddition
  | DeltaIdentityNeutral
  | DeltaCompositionAssociative
  | DeltaIdentityAction
  | DeltaCompositionActsHomomorphically
  | DeltaNullHonestyExtensional
  | BatchRowCountTracksLiveCells
  | BatchDescriptionProjectsInterval
  | BatchMergerFuelDenotesBinaryMerge
  | TraceAppendDenotesReplay
  | TraceSemigroupAppendDenotesReplay
  | TraceAccumulationDenotesPrefixReplay
  | TracePrefixFoldRebuildsPrefix
  | TraceKeyFoldsDenoteFilteredReplay
  | TraceKeyRowFoldsRebuildConsolidatedBatches
  | TraceNullMatchesCollapsedDenotation
  | TraceDescriptionProjectsFrontiers
  | TraceDescriptionAdvanceMatchesTrace
  | TraceDescriptionReadAvailabilityMatchesFrontierOracle
  | TimeIndexSlicingConsumesReadObligations
  | PartitionedPrefixCompactionPreservesDenotation
  | PartitionedPrefixCompactionObeysDescriptionSince
  | PartitionedPrefixKeyMismatchTyped
  | PartitionedPrefixSummaryFailureTyped
  | PartitionedPrefixOutsideRunSummaryTyped
  | ArrangeByKeyDenotesReplayedTrace
  | ArrangementAppendDenotesAppendThenArrange
  | ArrangementKeyFoldFiltersReplayOracle
  | ArrangementSliceFoldsFilterReplayOracleByTime
  | LinearOperatorsDeltaTransparent
  | IndexByPartitionsReflatten
  | IndexedDeltaJoinIntegratesBilinearDeltas
  | StarDeltaDecompositionEqualsRecomputation
  | ArrangedJoinsAgreeWithUnarranged
  | FoldDeltaJoinConsolidatesThroughBatch
  | DistinctFollowsSupportOracle
  | GroupViewAdvanceRebuildsIntegratedView
  | CountByKeyLinear
  | SemiNaiveFixpointMatchesReachabilityOracle
  | SemiNaiveArrangedFixpointMatchesReachabilityOracle
  | ProjectionWorkObeysSharedActionAlgebra
  | ProjectionDeltaObeysSharedActionAlgebra
  | ProjectionMaintenanceMatchesRecomputation
  | ProjectionCommitMatchesSupportRecomputation
  | StreamDifferentiateIntegrateInverse
  | StreamMobiusInversionLawful
  | LocallyFiniteMobiusInvertsClosedIntervals
  | ProductMobiusCoefficientsFactor
  | ProductMobiusSupportFactors
  | NaturalPrefixExecutionAgreesWithDenotation
  | NaturalScalarLinearIncrementalizationBypassesReplay
  | NaturalProductPrefixExecutionAgreesWithDenotation
  | NaturalProductScansFactorAsNestedScans
  | IntegralSamplerAgreesWithGenericFold
  | ProductIntegralSamplerAgreesWithGenericFold
  | MemoTimeIsExtensionallyIdentity
  | RowSetsAgreeWithIntSet
  | RowSubstratesCanonicalizeNonnegative
  | TupleWordConversionRejectsNegatives
  | RowSetDenseInsertDenotesIntSetInsert
  | IndexedRowsValueBucketsDenoteLiveBindings
  | IndexedRegistryReverseAxesGlued
  | IndexedRegistryTypedCorruptionObstructions
  | BatchProjectsIntoIndexedRows
  | TraceProjectionAccumulatesDuplicateRowsAlgebraically
  | ProjectedRowDeltasMatchSnapshotProjection
  | RelationAdvanceMaintainsViewsAtomically
  | GenericJoinAgreesWithBruteForceOracle
  | GenericJoinCountIsProposedDomainSize
  | IndexedExtendersAgreeWithSetBaseline
  | FusedIndexedFoldAgreesWithGenericAdaptive
  | AdaptiveJoinAgreesWithGeneric
  | AdaptiveFoldAgreesWithMaterialized
  | JoinExistenceAgreesWithGenericDenotation
  | DenseTriangleAgreesWithBruteForceOracle
  | DenseTriangleStatsExposeNormalizedEdgeCount
  | RuntimeTimePartialOrderLaws
  | RuntimeFrontierNormalizedAntichains
  | LocalFactAntichainDominanceNormalized
  | CapabilityDowngradeMonotoneAccepted
  | CapabilityAdvanceRegressionTyped
  | SettleQuiescentInputIsFixpoint
  | SettleBudgetExhaustionHonest
  | ContextRestrictionUnknownEndpointRefused
  | RowsCachePinnedDropRefused
  | RowsCacheOverBudgetObservable
  | RowsCacheOverBudgetRequiresPins
  | CircuitLinearAdvanceAgreesWithIncrementalize
  | CircuitJoinAdvanceAgreesWithIncrementalize
  | CircuitSharedArrangementAdvanceAgreesWithIncrementalize
  | CircuitAggregateAdvanceAgreesWithIncrementalize
  | CircuitDistinctAdvanceAgreesWithIncrementalize
  | CircuitConcatDifferenceAdvanceAgreesWithIncrementalize
  | CircuitFixpointAdvanceAgreesWithIncrementalize
  | CircuitLawfulForeignKernelAdvanceAgreesWithIncrementalize
  | CircuitEagerDenotationAgreesWithReferenceAlgebra
  | CircuitLinearAdvanceIsIncrementalizeOfDenotation
  deriving stock (Eq, Ord, Show)

instance IsLawName LawName where
  lawNameText = lawName

lawName :: LawName -> String
lawName =
  constructorLawNameWithOverrides
    [ ("ZSetGroupCancellation", "zset_group_cancellation"),
      ("ZSetCanonicalSupportSize", "zset_canonical_support_size"),
      ("IndexedZSetCanonicalSupportCellCount", "indexed_zset_canonical_support_cell_count"),
      ("IndexedZSetUnionsDenoteIndexedAddition", "indexed_zset_unions_denote_indexed_addition")
    ]
    . show
