{-# LANGUAGE DerivingStrategies #-}

module Groups where

import ArrangementJoin
import Common
import Operator
import Projection
import Relation
import RowIndex
import RowProjection
import RuntimeSettle
import Storage
import Stream
import TraceCompact
import TraceRead
import WCOJ
import Moonlight.Differential.Batch (BatchMergeFuel (..))
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, nf)

batchTraceBenchmarks :: Benchmark
batchTraceBenchmarks =
  bgroup
    "batch-trace"
    (batchSizes >>= batchTraceBenchmarksForSize)

storageKernelBenchmarks :: Benchmark
storageKernelBenchmarks =
  bgroup
    "storage-kernel"
    (storageKernelSizes >>= storageKernelBenchmarksForSize)

storageKernelBenchmarksForSize :: Int -> [Benchmark]
storageKernelBenchmarksForSize size =
  [ env (pure (indexedZSetStorageEntries size)) $ \entries ->
      bench (caseLabel "IndexedZSet.fromList storage" size) (nf indexedZSetConstructCellCount entries),
    env (pure (indexedZSetStorageMergeCase size)) $ \mergeInput ->
      bench (caseLabel "IndexedZSet.merge cancel" size) (nf indexedZSetMergeCellCount mergeInput),
    env (pure (indexedZSetStorageDisjointMergeCase size)) $ \mergeInput ->
      bench (caseLabel "IndexedZSet.merge disjoint" size) (nf indexedZSetMergeCellCount mergeInput),
    env (pure (indexedZSetStoragePartialCancelMergeCase size)) $ \mergeInput ->
      bench (caseLabel "IndexedZSet.merge partial-cancel" size) (nf indexedZSetMergeCellCount mergeInput),
    env (pure (indexedZSetSingletonSections size)) $ \sections ->
      bench (caseLabel "IndexedZSet.unions singleton sections" size) (nf indexedZSetUnionCellCount sections)
  ]

decomposedDbspDdBenchmarks :: Benchmark
decomposedDbspDdBenchmarks =
  bgroup
    "decomposed-storage"
    [ bgroup
        "batch-merger"
        ( batchMergerDecompositionBenchmarks
            <> (batchCoverFanIns >>= batchCoverMergeBenchmarksForFanIn)
        ),
      bgroup
        "steady-trace"
        (decomposedDbspDdSizes >>= steadyTraceBenchmarksForSize),
      bgroup
        "end-to-end-pipeline"
        (decomposedDbspDdSizes >>= decomposedPipelineBenchmarksForSize),
      bgroup
        "relation-state"
        (decomposedDbspDdSizes >>= relationStateBenchmarksForSize),
      bgroup
        "retraction-heavy"
        (decomposedDbspDdSizes >>= retractionPipelineBenchmarksForSize),
      bgroup
        "local-calibration"
        (decomposedDbspDdSizes >>= localCalibrationBenchmarksForSize)
    ]

batchMergerDecompositionBenchmarks :: [Benchmark]
batchMergerDecompositionBenchmarks =
  [ env (pure (binaryBatchMergerCase 2048 (BatchMergeFuel 64))) $ \mergeInput ->
      bench "BatchMerger.work fuel=64 n=2048" (nf binaryBatchMergerFuelWeight mergeInput),
    env (pure (binaryBatchMergerCase 2048 (BatchMergeFuel 2048))) $ \mergeInput ->
      bench "BatchMerger.work fuel=2048 n=2048" (nf binaryBatchMergerFuelWeight mergeInput),
    env (pure (binaryBatchMergerCase 2048 (BatchMergeFuel 64))) $ \mergeInput ->
      bench "BatchMerger.finish n=2048" (nf binaryBatchMergerFinishWeight mergeInput)
  ]

batchCoverMergeBenchmarksForFanIn :: Int -> [Benchmark]
batchCoverMergeBenchmarksForFanIn fanIn =
  [ env (checkedBatchCoverCase (disjointBatchCoverCase 2048 fanIn)) $ \cover ->
      bench
        (fanInCaseLabel "Batch.merge-cover/disjoint" 2048 fanIn)
        (nf batchCoverMergeWeight cover),
    env (checkedBatchCoverCase (boundaryOverlapBatchCoverCase 2048 fanIn)) $ \cover ->
      bench
        (fanInCaseLabel "Batch.merge-cover/boundary-overlap" 2048 fanIn)
        (nf batchCoverMergeWeight cover),
    env (checkedBatchCoverCase (overlappingBatchCoverCase 2048 fanIn)) $ \cover ->
      bench
        (fanInCaseLabel "Batch.merge-cover/overlap" 2048 fanIn)
        (nf batchCoverMergeWeight cover)
  ]

steadyTraceBenchmarksForSize :: Int -> [Benchmark]
steadyTraceBenchmarksForSize size =
  [ env (pure (periodicTraceBatchCase size)) $ \batches ->
      bench (caseLabel "Trace.append batches" size) (nf traceAppendOnlyStats batches),
    env (checkedBenchCase "Trace.advance-since" traceAdvanceSinceStats (Right (periodicTraceBatchCase size))) $ \batches ->
      bench (caseLabel "Trace.advance-since" size) (nf traceAdvanceSinceStats batches),
    env (pure (periodicTraceBatchCase size)) $ \batches ->
      bench (caseLabel "Trace.compact-physical" size) (nf traceCompactPhysicalStats batches),
    env (pure (periodicTraceBatchCase size)) $ \batches ->
      bench (caseLabel "Trace.compact-physical/fueled" size) (nf traceCompactPhysicalStepWeight batches),
    env (pure (periodicTraceBatchCase size)) $ \batches ->
      bench (caseLabel "Trace.snapshot-to-batch" size) (nf traceSnapshotBatchRowCount batches),
    env (pure (periodicTraceBatchCase size)) $ \batches ->
      bench (caseLabel "Trace.steady physical profile" size) (nf traceAppendPeriodicPhysicalProfileWeight batches),
    env (pure (retainedPrefixTraceReadCase size)) $ \traceRead ->
      bench (caseLabel "Trace.steady retained-prefix read" size) (nf traceRetainedPrefixReadWeight traceRead),
    env (pure (retainedPrefixTraceReadCase size)) $ \traceRead ->
      bench (caseLabel "Trace.steady key-prefix read" size) (nf traceKeyPrefixReadWeight traceRead),
    env (pure (traceCompactNoOpCase size)) $ \traceValue ->
      bench (caseLabel "Trace.steady compact no-op" size) (nf traceCompactNoOpProfileWeight traceValue)
  ]

decomposedPipelineBenchmarksForSize :: Int -> [Benchmark]
decomposedPipelineBenchmarksForSize size =
  [ env (pure (decomposedPipelineCase size)) $ \pipeline ->
      bench (caseLabel "Pipeline.batch singleton-build" size) (nf decomposedPipelineBatchBuildWeight pipeline),
    env (pure (decomposedPipelineCase size)) $ \pipeline ->
      bench (caseLabel "Pipeline.trace append+periodic-maintenance+snapshot" size) (nf decomposedPipelineTraceIngestWeight pipeline),
    env (pure (decomposedPipelineCase size)) $ \pipeline ->
      bench (caseLabel "Pipeline.arrange" size) (nf decomposedPipelineArrangementWeight pipeline),
    env (pure (decomposedPipelineCase size)) $ \pipeline ->
      bench (caseLabel "Pipeline.join fold" size) (nf decomposedPipelineJoinWeight pipeline),
    env (checkedBenchCase "Pipeline.project rows" decomposedPipelineProjectionWeight (Right (decomposedPipelineCase size))) $ \pipeline ->
      bench (caseLabel "Pipeline.project rows" size) (nf decomposedPipelineProjectionWeight pipeline),
    env (checkedBenchCase "Pipeline.settle" decomposedPipelineSettleWeight (Right (decomposedPipelineCase size))) $ \pipeline ->
      bench (caseLabel "Pipeline.settle" size) (nf decomposedPipelineSettleWeight pipeline)
  ]

relationStateBenchmarksForSize :: Int -> [Benchmark]
relationStateBenchmarksForSize size =
  relationBootstrapBenchmark size
    : (relationAdvanceScenarios >>= relationAdvanceBenchmarkPair size)

relationAdvanceBenchmarkPair :: Int -> RelationAdvanceScenario -> [Benchmark]
relationAdvanceBenchmarkPair size scenario =
  [ relationAdvanceBenchmark size scenario,
    relationAdvanceDenseBenchmark size scenario
  ]

data RelationAdvanceScenario
  = RelationAdvanceUniform
  | RelationAdvanceHotKey
  | RelationAdvanceRetraction
  deriving stock (Eq, Ord, Show)

relationAdvanceScenarios :: [RelationAdvanceScenario]
relationAdvanceScenarios =
  [ RelationAdvanceUniform,
    RelationAdvanceHotKey,
    RelationAdvanceRetraction
  ]

relationBootstrapBenchmark :: Int -> Benchmark
relationBootstrapBenchmark size =
  env checkedBootstrapCase $ \relationCase ->
    bench (caseLabel label size) (nf relationBootstrapWeight relationCase)
  where
    label =
      "Relation.bootstrap"

    bootstrapCase =
      relationBootstrapCase size

    checkedBootstrapCase =
      checkedCase label (relationBootstrapWeight bootstrapCase) *> pure bootstrapCase

relationAdvanceBenchmark :: Int -> RelationAdvanceScenario -> Benchmark
relationAdvanceBenchmark size scenario =
  env (checkedBenchCase label relationAdvanceWeight (relationAdvanceCaseFor scenario size)) $ \relationCase ->
    bench (caseLabel label size) (nf relationAdvanceWeight relationCase)
  where
    label =
      relationAdvanceScenarioLabel scenario

relationAdvanceDenseBenchmark :: Int -> RelationAdvanceScenario -> Benchmark
relationAdvanceDenseBenchmark size scenario =
  env (checkedBenchCase (label <> " dense") relationAdvanceWeight (relationAdvanceCaseDenseFor scenario size)) $ \relationCase ->
    bench (caseLabel label size <> " dense") (nf relationAdvanceWeight relationCase)
  where
    label =
      relationAdvanceScenarioLabel scenario

relationAdvanceScenarioLabel :: RelationAdvanceScenario -> String
relationAdvanceScenarioLabel scenario =
  case scenario of
    RelationAdvanceUniform ->
      "Relation.advance/uniform"
    RelationAdvanceHotKey ->
      "Relation.advance/hot-key"
    RelationAdvanceRetraction ->
      "Relation.advance/retraction"

relationAdvanceCaseFor :: RelationAdvanceScenario -> Int -> Either String PreparedRelationAdvance
relationAdvanceCaseFor scenario =
  case scenario of
    RelationAdvanceUniform ->
      relationUniformCase
    RelationAdvanceHotKey ->
      relationHotKeyCase
    RelationAdvanceRetraction ->
      relationRetractionCase

relationAdvanceCaseDenseFor :: RelationAdvanceScenario -> Int -> Either String PreparedRelationAdvance
relationAdvanceCaseDenseFor scenario =
  case scenario of
    RelationAdvanceUniform ->
      relationUniformCaseDense
    RelationAdvanceHotKey ->
      relationHotKeyCaseDense
    RelationAdvanceRetraction ->
      relationRetractionCaseDense

retractionPipelineBenchmarksForSize :: Int -> [Benchmark]
retractionPipelineBenchmarksForSize size =
  [ env (pure (retractionPipelineCase size)) $ \pipeline ->
      bench (caseLabel "Retraction.trace apply" size) (nf retractionTraceApplyWeight pipeline),
    env (pure (retractionPipelineCase size)) $ \pipeline ->
      bench (caseLabel "Retraction.trace materialize-empty" size) (nf retractionMaterializedWeight pipeline),
    env (pure (retractionPipelineCase size)) $ \pipeline ->
      bench (caseLabel "Retraction.arrange after-delete" size) (nf retractionArrangementWeight pipeline),
    env (pure (retractionPipelineCase size)) $ \pipeline ->
      bench (caseLabel "Retraction.join delete-delta" size) (nf retractionJoinWeight pipeline),
    env (checkedBenchCase "Retraction.project after-delete" retractionProjectionWeight (Right (retractionPipelineCase size))) $ \pipeline ->
      bench (caseLabel "Retraction.project after-delete" size) (nf retractionProjectionWeight pipeline)
  ]

localCalibrationBenchmarksForSize :: Int -> [Benchmark]
localCalibrationBenchmarksForSize size =
  [ env (pure (spinesLikeCase size)) $ \spinesCaseValue ->
      bench (caseLabel "Trace.cold-load+arrange" size) (nf ddSpinesLikeLoadArrangeWeight spinesCaseValue),
    env (pure (spinesLikeCase size)) $ \spinesCaseValue ->
      bench (caseLabel "Arrangement.key-query sweep" size) (nf ddSpinesLikeKeyQueryWeight spinesCaseValue),
    env (checkedBenchCases "local row projection" [rowProjectionBatchRowsWeight, rowArrangementDirtyRestrictWeight] (Right (rowProjectionCase size))) $ \projectionCaseValue ->
      bench (caseLabel "RowProjection.snapshot-build rows" size) (nf rowProjectionBatchRowsWeight projectionCaseValue),
    env (checkedBenchCase "local row projection dense" rowProjectionBatchRowsWeight (Right (rowProjectionCaseDense size))) $ \projectionCaseValue ->
      bench (caseLabel "RowProjection.snapshot-build rows" size <> " dense") (nf rowProjectionBatchRowsWeight projectionCaseValue),
    env (checkedBenchCases "local row projection" [rowProjectionBatchRowsWeight, rowArrangementDirtyRestrictWeight] (Right (rowProjectionCase size))) $ \projectionCaseValue ->
      bench (caseLabel "RowArrangement.dirty-restrict" size) (nf rowArrangementDirtyRestrictWeight projectionCaseValue),
    env (checkedBenchCase "local row projection dense" rowArrangementDirtyRestrictWeight (Right (rowProjectionCaseDense size))) $ \projectionCaseValue ->
      bench (caseLabel "RowArrangement.dirty-restrict" size <> " dense") (nf rowArrangementDirtyRestrictWeight projectionCaseValue),
    env (checkedBatchCoverCase (disjointBatchCoverCase size 64)) $ \cover ->
      bench (caseLabel "Batch.merge-cover/disjoint fanin=64" size) (nf batchCoverMergeWeight cover)
  ]

batchTraceBenchmarksForSize :: Int -> [Benchmark]
batchTraceBenchmarksForSize size =
  [ env (pure (updateCase size)) $ \updates ->
      bench (caseLabel "Batch.fromUpdates/toUpdates" size) (nf batchBuildCellCount updates),
    env (pure (updateCase size)) $ \updates ->
      bench (caseLabel "Batch.singletonBatch sweep" size) (nf batchSingletonBuildCellCount updates),
    env (pure (duplicateHeavyUpdateCase size)) $ \updates ->
      bench (caseLabel "Batch.fromUpdates duplicate-heavy" size) (nf batchBuildCellCount updates),
    env (pure (cancellationHeavyUpdateCase size)) $ \updates ->
      bench (caseLabel "Batch.fromUpdates cancellation-heavy" size) (nf batchBuildCellCount updates),
    env (pure (skewedKeyUpdateCase size)) $ \updates ->
      bench (caseLabel "Batch.fromUpdates skewed-key" size) (nf batchBuildCellCount updates),
    env (pure (mergeCase size)) $ \mergeInputs ->
      bench (caseLabel "Batch.mergeBatch two-large" size) (nf mergeTwoLargeCount mergeInputs),
    env (pure (manySmallBatchCase size)) $ \batches ->
      bench (caseLabel "Batch.mergeBatches many-small" size) (nf mergeManySmallCount batches),
    env (pure (updateCase size)) $ \updates ->
      bench (caseLabel "Trace.traceAccumUpTo" size) (nf traceAccumWeight updates),
    env (pure (manySmallBatchCase size)) $ \batches ->
      bench (caseLabel "Trace.appendBatch+snapshot" size) (nf traceAppendToBatchCount batches),
    env (pure (periodicTraceBatchCase size)) $ \batches ->
      bench (caseLabel "Trace.append+periodic-maintenance+snapshot" size) (nf traceAppendPeriodicMaintenanceSnapshotCount batches),
    env (pure (periodicTraceBatchCase size)) $ \batches ->
      bench (caseLabel "Trace.spine physical profile periodic" size) (nf traceAppendPeriodicPhysicalProfileWeight batches),
    env (pure (retainedPrefixTraceReadCase size)) $ \traceRead ->
      bench (caseLabel "Trace.retained-prefix read" size) (nf traceRetainedPrefixReadWeight traceRead),
    env (pure (retainedPrefixTraceReadCase size)) $ \traceRead ->
      bench (caseLabel "Trace.retained-prefix materialized" size) (nf traceRetainedPrefixMaterializedWeight traceRead),
    env (pure (retainedPrefixTraceReadCase size)) $ \traceRead ->
      bench (caseLabel "Trace.key-prefix read" size) (nf traceKeyPrefixReadWeight traceRead),
    env (pure (retainedPrefixTraceReadCase size)) $ \traceRead ->
      bench (caseLabel "Trace.key-prefix viaArrangement" size) (nf traceKeyPrefixViaArrangementWeight traceRead),
    env (pure (physicallyEmptyTraceNullCase size)) $ \traceValue ->
      bench (caseLabel "Trace.null physically-empty" size) (nf traceNullWeight traceValue),
    env (pure (physicallyEmptyTraceNullCase size)) $ \traceValue ->
      bench (caseLabel "Trace.null physically-empty viaBatch" size) (nf traceNullViaBatchWeight traceValue),
    env (pure (traceCompactNoOpCase size)) $ \traceValue ->
      bench (caseLabel "Trace.compactPhysical no-op" size) (nf traceCompactNoOpProfileWeight traceValue)
  ]

streamCalculusBenchmarks :: Benchmark
streamCalculusBenchmarks =
  bgroup
    "stream-calculus"
    ( (streamNaturalSizes >>= naturalStreamBenchmarksForSize)
        <> (streamProductSizes >>= productStreamBenchmarksForSize)
    )

naturalStreamBenchmarksForSize :: Int -> [Benchmark]
naturalStreamBenchmarksForSize size =
  [ env (pure (naturalStreamCase size)) $ \streamCaseValue ->
      bench (caseLabel "Stream.prefix differentiate" size) (nf streamDifferentiateWeight streamCaseValue),
    env (pure (naturalStreamCase size)) $ \streamCaseValue ->
      bench (caseLabel "Stream.prefix integrate" size) (nf streamIntegrateWeight streamCaseValue),
    env (pure (naturalStreamCase size)) $ \streamCaseValue ->
      bench (caseLabel "Stream.prefix incrementalize scalar-linear" size) (nf streamIncrementalizeMapWeight streamCaseValue),
    env (pure (naturalStreamCase size)) $ \streamCaseValue ->
      bench (caseLabel "Stream.prefix incrementalize map" size) (nf streamIncrementalizeGenericMapWeight streamCaseValue),
    env (pure (naturalStreamCase size)) $ \streamCaseValue ->
      bench (caseLabel "Stream.calculus integrate" size) (nf streamCalculusIntegrateWeight streamCaseValue),
    env (pure (naturalStreamCase size)) $ \streamCaseValue ->
      bench (caseLabel "Stream.calculus integrate generic-fallback" size) (nf streamCalculusIntegrateFallbackWeight streamCaseValue),
    env (pure (naturalStreamCase size)) $ \streamCaseValue ->
      bench (caseLabel "Stream.calculus incrementalize map" size) (nf streamCalculusIncrementalizeMapWeight streamCaseValue),
    env (pure (naturalStreamCase size)) $ \streamCaseValue ->
      bench (caseLabel "Stream.calculus incrementalize map generic-fallback" size) (nf streamCalculusIncrementalizeMapFallbackWeight streamCaseValue)
  ]

productStreamBenchmarksForSize :: Int -> [Benchmark]
productStreamBenchmarksForSize size =
  [ env (pure (productStreamCase size)) $ \streamCaseValue ->
      bench (caseLabel "Stream.product-time prefix integrate.differentiate" size) (nf productStreamDifferentiateIntegrateWeight streamCaseValue),
    env (pure (productStreamCase size)) $ \streamCaseValue ->
      bench (caseLabel "Stream.product-time calculus integrate.differentiate" size) (nf productStreamCalculusWeight streamCaseValue),
    env (pure (productStreamCase size)) $ \streamCaseValue ->
      bench (caseLabel "Stream.product-time calculus integrate.differentiate generic-fallback" size) (nf productStreamCalculusFallbackWeight streamCaseValue)
  ]

operatorBenchmarks :: Benchmark
operatorBenchmarks =
  bgroup
    "operator-linear-aggregate-fixpoint"
    (operatorSizes >>= operatorBenchmarksForSize)

operatorBenchmarksForSize :: Int -> [Benchmark]
operatorBenchmarksForSize size =
  [ env (pure (operatorZSetCase size)) $ \zsetCaseValue ->
      bench (caseLabel "Linear.map.filter" size) (nf operatorLinearPipelineWeight zsetCaseValue),
    env (pure (operatorZSetCase size)) $ \zsetCaseValue ->
      bench (caseLabel "Linear.indexBy.Aggregate.countByKey" size) (nf operatorIndexCountWeight zsetCaseValue),
    env (pure (operatorDistinctCase size)) $ \distinctCaseValue ->
      bench (caseLabel "Aggregate.distinctDelta" size) (nf operatorDistinctDeltaWeight distinctCaseValue),
    env (pure (operatorGroupViewCase size)) $ \groupViewCaseValue ->
      bench (caseLabel "Aggregate.groupViewAdvance" size) (nf operatorGroupViewAdvanceWeight groupViewCaseValue),
    env (checkedBenchCase "Fixpoint.semiNaive path" semiNaivePathWeight (Right (fixpointPathCase size))) $ \fixpointCaseValue ->
      bench (caseLabel "Fixpoint.semiNaive path" size) (nf semiNaivePathWeight fixpointCaseValue),
    env (checkedBenchCase "Fixpoint.arranged path" arrangedSemiNaivePathWeight (Right (arrangedFixpointPathCase size))) $ \fixpointCaseValue ->
      bench (caseLabel "Fixpoint.arranged path" size) (nf arrangedSemiNaivePathWeight fixpointCaseValue)
  ]

collectionEdslComparisonBenchmarks :: Benchmark
collectionEdslComparisonBenchmarks =
  bgroup
    "collection-edsl-vs-substrate"
    (operatorSizes >>= collectionEdslComparisonBenchmarksForSize)

collectionEdslComparisonBenchmarksForSize :: Int -> [Benchmark]
collectionEdslComparisonBenchmarksForSize size =
  [ env (pure (collectionEdslComparisonCase size)) $ \comparison ->
      bench (caseLabel "raw map.filter" size) (nf rawMapFilterWeight comparison),
    env (pure (collectionEdslComparisonCase size)) $ \comparison ->
      bench (caseLabel "edsl map.filter" size) (nf edslMapFilterWeight comparison),
    env (pure (collectionEdslComparisonCase size)) $ \comparison ->
      bench (caseLabel "raw flatMap" size) (nf rawFlatMapWeight comparison),
    env (pure (collectionEdslComparisonCase size)) $ \comparison ->
      bench (caseLabel "edsl flatMap" size) (nf edslFlatMapWeight comparison),
    env (pure (collectionEdslComparisonCase size)) $ \comparison ->
      bench (caseLabel "raw concat.difference.negate" size) (nf rawGroupAlgebraWeight comparison),
    env (pure (collectionEdslComparisonCase size)) $ \comparison ->
      bench (caseLabel "edsl concat.difference.negate" size) (nf edslGroupAlgebraWeight comparison),
    env (pure (collectionEdslComparisonCase size)) $ \comparison ->
      bench (caseLabel "raw index.count" size) (nf rawIndexCountWeight comparison),
    env (pure (collectionEdslComparisonCase size)) $ \comparison ->
      bench (caseLabel "edsl index.count" size) (nf edslIndexCountWeight comparison),
    env (pure (collectionEdslComparisonCase size)) $ \comparison ->
      bench (caseLabel "raw index.deindex" size) (nf rawIndexDeindexWeight comparison),
    env (pure (collectionEdslComparisonCase size)) $ \comparison ->
      bench (caseLabel "edsl index.deindex" size) (nf edslIndexDeindexWeight comparison),
    env (pure (collectionEdslComparisonCase size)) $ \comparison ->
      bench (caseLabel "raw index.join" size) (nf rawJoinWeight comparison),
    env (pure (collectionEdslComparisonCase size)) $ \comparison ->
      bench (caseLabel "edsl index.join" size) (nf edslJoinWeight comparison)
  ]

arrangementJoinBenchmarks :: Benchmark
arrangementJoinBenchmarks =
  bgroup
    "arrangement-join"
    (batchSizes >>= arrangementJoinBenchmarksForSize)

arrangementJoinBenchmarksForSize :: Int -> [Benchmark]
arrangementJoinBenchmarksForSize size =
  [ env (pure (cursorMergeCase size)) $ \cursorMergeInput ->
      bench (caseLabel "Cursor.merge" size) (nf cursorMergeWeight cursorMergeInput),
    env (pure (preparedArrangementCase size)) $ \arrangement ->
      bench (caseLabel "Arrangement.foldSliceThrough" size) (nf arrangementSliceWeight arrangement),
    env (pure (preparedArrangementCase size)) $ \arrangement ->
      bench (caseLabel "Arrangement.foldSliceAfter" size) (nf arrangementSliceAfterWeight arrangement),
    env (pure (arrangementAppendCase size)) $ \appendInputs ->
      bench (caseLabel "Arrangement.appendArrangementBatch" size) (nf arrangementAppendWeight appendInputs),
    env (pure (arrangementManyBatchCase size)) $ \traceInput ->
      bench (caseLabel "Arrangement.arrangeByKey many-batch" size) (nf arrangementBuildTraceWeight traceInput),
    env (pure (arrangementManyBatchCase size)) $ \traceInput ->
      bench (caseLabel "Arrangement.arrangeByKey many-batch viaBatch" size) (nf arrangementBuildTraceViaBatchWeight traceInput),
    env (pure (preparedArrangementHotKeyCase size)) $ \arrangement ->
      bench (caseLabel "Arrangement.foldSliceThrough hot-key" size) (nf arrangementSliceWeight arrangement),
    env (pure (preparedArrangementHotKeyCase size)) $ \arrangement ->
      bench (caseLabel "Arrangement.foldSliceAfter hot-key" size) (nf arrangementSliceAfterWeight arrangement),
    env (pure (arrangementHotKeyAppendCase size)) $ \appendInputs ->
      bench (caseLabel "Arrangement.appendArrangementBatch hot-key" size) (nf arrangementAppendWeight appendInputs),
    env (pure (joinCase size)) $ \joinInputs ->
      bench (caseLabel "Operator.foldDeltaJoin materialized-view" size) (nf deltaJoinMaterializedCount joinInputs),
    env (pure (joinCase size)) $ \joinInputs ->
      bench (caseLabel "Operator.foldDeltaJoin" size) (nf foldDeltaJoinCount joinInputs),
    env (pure (joinSkewFanoutCase size)) $ \joinInputs ->
      bench (caseLabel "Operator.foldDeltaJoin materialized-view skew-fanout" size) (nf deltaJoinMaterializedCount joinInputs),
    env (pure (joinSkewFanoutCase size)) $ \joinInputs ->
      bench (caseLabel "Operator.foldDeltaJoin skew-fanout" size) (nf foldDeltaJoinCount joinInputs),
    env (pure (joinEmptyPrefixCase size)) $ \joinInputs ->
      bench (caseLabel "Operator.foldDeltaJoin materialized-view empty-prefix" size) (nf deltaJoinMaterializedCount joinInputs),
    env (pure (joinEmptyPrefixCase size)) $ \joinInputs ->
      bench (caseLabel "Operator.foldDeltaJoin empty-prefix" size) (nf foldDeltaJoinCount joinInputs),
    env (pure (joinCase size)) $ \joinInputs ->
      bench (caseLabel "Operator.foldDeltaJoin materialized-view collapsed-output" size) (nf deltaJoinCollapsedOutputCount joinInputs),
    env (pure (joinCase size)) $ \joinInputs ->
      bench (caseLabel "Operator.foldDeltaJoin collapsed-output" size) (nf foldDeltaJoinCollapsedOutputCount joinInputs)
  ]

wcojBenchmarks :: Benchmark
wcojBenchmarks =
  bgroup
    "wcoj"
    ( (wcojSizes >>= wcojBenchmarksForSize)
        <> (denseTriangleSizes >>= denseTriangleBenchmarksForSize)
    )

wcojBenchmarksForSize :: Int -> [Benchmark]
wcojBenchmarksForSize size =
  [ env (pure (wcojProblem size)) $ \problem ->
      bench (caseLabel "foldGenericJoin materialized-view" size) (nf foldGenericJoinMaterializedCount problem),
    env (pure (wcojProblem size)) $ \problem ->
      bench (caseLabel "foldGenericJoin" size) (nf foldGenericJoinCount problem),
    env (pure (wcojProblem size)) $ \problem ->
      bench (caseLabel "adaptiveJoin materialized" size) (nf adaptiveJoinCount problem),
    env (pure (wcojProblem size)) $ \problem ->
      bench (caseLabel "foldAdaptiveJoin" size) (nf foldAdaptiveJoinCount problem),
    env (pure (wcojProblem size)) $ \problem ->
      bench (caseLabel "foldIntIndexedAdaptiveJoin" size) (nf foldIntIndexedAdaptiveJoinCount problem),
    env (pure (wcojProblem size)) $ \problem ->
      bench (caseLabel "chooseSmallestSlot materialized-count" size) (nf wcojChooseSmallestSlotMaterializedWeight problem),
    env (pure (wcojProblem size)) $ \problem ->
      bench (caseLabel "chooseSmallestSlot direct-count" size) (nf wcojChooseSmallestSlotDirectWeight problem)
  ]

denseTriangleBenchmarksForSize :: Int -> [Benchmark]
denseTriangleBenchmarksForSize size =
  [ env (pure (denseTrianglePathCase size)) $ \triangleCase ->
      bench (caseLabel "DenseTriangle.count path" size) (nf denseTriangleCountWeight triangleCase),
    env (pure (denseTriangleStarCase size)) $ \triangleCase ->
      bench (caseLabel "DenseTriangle.count star" size) (nf denseTriangleCountWeight triangleCase),
    env (pure (denseTriangleCliqueCase size)) $ \triangleCase ->
      bench (caseLabel "DenseTriangle.count clique" size) (nf denseTriangleCountWeight triangleCase),
    env (pure (denseTriangleExactCliqueCase size)) $ \triangleCase ->
      bench (caseLabel "DenseTriangle.exact-count clique" size) (nf exactTriangleCountWeight triangleCase),
    env (pure (denseTriangleSkewedCase size)) $ \triangleCase ->
      bench (caseLabel "DenseTriangle.count skewed" size) (nf denseTriangleCountWeight triangleCase)
  ]

traceCompactionBenchmarks :: Benchmark
traceCompactionBenchmarks =
  bgroup
    "trace-compaction"
    (compactionSizes >>= traceCompactionBenchmarksForSize)

traceCompactionBenchmarksForSize :: Int -> [Benchmark]
traceCompactionBenchmarksForSize size =
  [ env (checkedBenchCase "partitioned-prefix compactable" compactPrefixWeight (Right (compactEntries size))) $ \entries ->
      bench (caseLabel "partitioned-prefix compactable" size) (nf compactPrefixWeight entries),
    env (checkedBenchCase "partitioned-prefix pending-blocked" compactPendingBlockedWeight (Right (compactEntries size))) $ \entries ->
      bench (caseLabel "partitioned-prefix pending-blocked" size) (nf compactPendingBlockedWeight entries),
    env (checkedBenchCase "partitioned-prefix retained-ids" compactRetainedIdsWeight (Right (compactEntries size))) $ \entries ->
      bench (caseLabel "partitioned-prefix retained-ids" size) (nf compactRetainedIdsWeight entries),
    env (checkedBenchCase "partitioned-prefix summary-outside-run" compactSummaryOutsideRunWeight (Right (compactEntries size))) $ \entries ->
      bench (caseLabel "partitioned-prefix summary-outside-run" size) (nf compactSummaryOutsideRunWeight entries)
  ]

rowIndexBenchmarks :: Benchmark
rowIndexBenchmarks =
  bgroup
    "row-index"
    [ bgroup
        "RowIdSet"
        (rowIndexSizes >>= rowIdSetBenchmarksForSize),
      bgroup
        "RowSet"
        (rowIndexSizes >>= rowSetBenchmarksForSize),
      bgroup
        "RowSet thresholds"
        rowSetThresholdBenchmarks,
      bgroup
        "IndexedRows"
        (rowIndexSizes >>= indexedRowsBenchmarksForSize)
    ]

rowIdSetBenchmarksForSize :: Int -> [Benchmark]
rowIdSetBenchmarksForSize size =
  [ env (pure (rowIdsForSize size)) $ \rowIds ->
      bench (caseLabel "build/insert/fold" size) (nf rowIdSetInsertFoldWeight rowIds),
    env (pure (rowIdsForSize size)) $ \rowIds ->
      bench (caseLabel "member sweep" size) (nf rowIdSetMemberSweepWeight rowIds),
    env (pure (rowIdSetUnionCase size)) $ \sets ->
      bench (caseLabel "union" size) (nf rowIdSetUnionWeight sets)
  ]

rowSetBenchmarksForSize :: Int -> [Benchmark]
rowSetBenchmarksForSize size =
  [ env (pure (rowIdsForSize size)) $ \rowIds ->
      bench (caseLabel "build/insert/fold" size) (nf rowSetInsertFoldWeight rowIds),
    env (pure (rowSetUnionCase size)) $ \sets ->
      bench (caseLabel "union/intersection" size) (nf rowSetUnionIntersectionWeight sets),
    env (pure (rowSetDenseCase size)) $ \rowSetValue ->
      bench (caseLabel "dense member/fold" size) (nf rowSetMemberFoldWeight rowSetValue),
    env (pure (rowSetIntersectsWitnessCase size)) $ \sets ->
      bench (caseLabel "intersects/witness" size) (nf rowSetIntersectsWeight sets),
    env (pure (rowSetIntersectsNoWitnessCase size)) $ \sets ->
      bench (caseLabel "intersects/no-witness" size) (nf rowSetIntersectsWeight sets),
    env (pure (rowSetIntersectsLateWitnessCase size)) $ \sets ->
      bench (caseLabel "intersects/late-witness" size) (nf rowSetIntersectsWeight sets),
    env (pure (rowIdSetIntersectsWitnessCase size)) $ \sets ->
      bench (caseLabel "intersects-row-id-set/witness" size) (nf rowIdSetIntersectsWeight sets),
    env (pure (rowIdSetIntersectsNoWitnessCase size)) $ \sets ->
      bench (caseLabel "intersects-row-id-set/no-witness" size) (nf rowIdSetIntersectsWeight sets),
    env (pure (rowIdSetIntersectsLateWitnessCase size)) $ \sets ->
      bench (caseLabel "intersects-row-id-set/late-witness" size) (nf rowIdSetIntersectsWeight sets)
  ]

indexedRowsBenchmarksForSize :: Int -> [Benchmark]
indexedRowsBenchmarksForSize size =
  [ env (checkedBenchCases "IndexedRows payload construction" [indexedRowsBuildWeight, indexedRowsInsertFreshWeight] (Right (indexedRowsPayloads size))) $ \payloads ->
      bench (caseLabel "fromPayloadMap" size) (nf indexedRowsBuildWeight payloads),
    env (checkedBenchCases "IndexedRows payload construction" [indexedRowsBuildWeight, indexedRowsInsertFreshWeight] (Right (indexedRowsPayloads size))) $ \payloads ->
      bench (caseLabel "insertFresh" size) (nf indexedRowsInsertFreshWeight payloads),
    env (checkedBenchCase "IndexedRows delete skewed" indexedRowsDeleteSkewWeight (preparedIndexedRowsDeleteSkew size)) $ \rows ->
      bench (caseLabel "delete skewed bucket" size) (nf indexedRowsDeleteSkewWeight rows),
    env (checkedCase "IndexedRows restrict setup" (preparedIndexedRows size)) $ \rows ->
      bench (caseLabel "restrictLiveRowsByPins" size) (nf indexedRowsRestrictWeight rows),
    env (checkedBenchCase "IndexedRows rebuild" indexedRowsRebuildWeight (preparedIndexedRows size)) $ \rows ->
      bench (caseLabel "rebuildValueIndex" size) (nf indexedRowsRebuildWeight rows)
  ]

rowsCacheBenchmarks :: Benchmark
rowsCacheBenchmarks =
  bgroup
    "context-rows-cache"
    [ env (pure (rowsCacheHitCase 512)) $ \cache ->
        bench "hit/touch n=512" (nf rowsCacheHitWeight cache),
      env (pure (rowsCacheMissCase 512)) $ \cache ->
        bench "miss/derive n=512" (nf rowsCacheMissWeight cache),
      env (pure (rowsCacheEvictionCase 512)) $ \cache ->
        bench "insert/evict n=512" (nf rowsCacheEvictionWeight cache),
      env (pure (rowsCachePinnedOverBudgetCase 512)) $ \cache ->
        bench "pinned over-budget n=512" (nf rowsCachePinnedOverBudgetWeight cache),
      env (pure (rowsCacheHitCase 128)) $ \cache ->
        bench "bulk-resize n=128" (nf rowsCacheBulkResizeWeight cache),
      env (pure (rowsCacheHitCase 256)) $ \cache ->
        bench "bulk-resize n=256" (nf rowsCacheBulkResizeWeight cache),
      env (pure (rowsCacheHitCase 512)) $ \cache ->
        bench "bulk-resize n=512" (nf rowsCacheBulkResizeWeight cache)
    ]

projectionBenchmarks :: Benchmark
projectionBenchmarks =
  bgroup
    "projection"
    (projectionSizes >>= projectionBenchmarksForSize)

projectionBenchmarksForSize :: Int -> [Benchmark]
projectionBenchmarksForSize size =
  [ env (pure (projectionWorkCase size)) $ \projectionWorkCaseValue ->
      bench (caseLabel "ProjectionWork union/dirty" size) (nf projectionWorkWeight projectionWorkCaseValue),
    env (pure (projectionDeltaCase size)) $ \projectionDeltaCaseValue ->
      bench (caseLabel "ProjectionDelta compose" size) (nf projectionDeltaComposeWeight projectionDeltaCaseValue),
    env (checkedBenchCase "runProjectionPhases" projectionMaintenanceWeight (Right (projectionMaintenanceCase size))) $ \projectionMaintenanceCaseValue ->
      bench (caseLabel "runProjectionPhases" size) (nf projectionMaintenanceWeight projectionMaintenanceCaseValue),
    env (pure (projectionPropagationCase size)) $ \projectionPropagationCaseValue ->
      bench (caseLabel "ProjectionPropagation affectedContexts" size) (nf projectionAffectedContextsWeight projectionPropagationCaseValue),
    env (pure (projectionPropagationCase size)) $ \projectionPropagationCaseValue ->
      bench (caseLabel "ProjectionPropagation commit" size) (nf projectionCommitWeight projectionPropagationCaseValue)
  ]

traceReadDescriptionBenchmarks :: Benchmark
traceReadDescriptionBenchmarks =
  bgroup
    "trace-read-description"
    (traceReadSizes >>= traceReadDescriptionBenchmarksForSize)

traceReadDescriptionBenchmarksForSize :: Int -> [Benchmark]
traceReadDescriptionBenchmarksForSize size =
  [ env (pure (traceDescriptionAdvanceCase size)) $ \descriptionCaseValue ->
      bench (caseLabel "TraceDescription advance/read" size) (nf traceDescriptionAdvanceWeight descriptionCaseValue),
    env (pure (timeIndexReadCase size)) $ \timeIndexCaseValue ->
      bench (caseLabel "TimeIndex.sliceTimeIndexAfter" size) (nf timeIndexRawReadWeight timeIndexCaseValue),
    env (checkedBenchCase "TimeIndex.sliceTimeIndexAfterDescription" timeIndexReadWeight (Right (timeIndexReadCase size))) $ \timeIndexCaseValue ->
      bench (caseLabel "TimeIndex.sliceTimeIndexAfterDescription" size) (nf timeIndexReadWeight timeIndexCaseValue)
  ]

reverseRowProjectionIndexBenchmarks :: Benchmark
reverseRowProjectionIndexBenchmarks =
  bgroup
    "reverse-row-projection-index"
    (rowIndexSizes >>= reverseRowProjectionIndexBenchmarksForSize)

reverseRowProjectionIndexBenchmarksForSize :: Int -> [Benchmark]
reverseRowProjectionIndexBenchmarksForSize size =
  [ env (pure (reverseIndexCase size)) $ \reverseCaseValue ->
      bench (caseLabel "ReverseBatch.add.lookupMany" size) (nf reverseIndexAddLookupWeight reverseCaseValue),
    env (pure (reverseIndexCase size)) $ \reverseCaseValue ->
      bench (caseLabel "ReverseBatch.rebuildIntAxis" size) (nf reverseIndexRebuildWeight reverseCaseValue),
    env
      ( checkedBenchCases
          "row projection snapshot"
          [ rowProjectionBatchRowsWeight,
            rowProjectionTraceArrangementWeight,
            rowProjectionTraceArrangementViaBatchWeight,
            rowProjectionProjectBatchDeltaWeight,
            rowArrangementDirtyRestrictWeight
          ]
          (Right (rowProjectionCase size))
      ) $ \projectionCaseValue ->
      env
        ( checkedBenchCases
            "row projection snapshot dense"
            [ rowProjectionBatchRowsWeight,
              rowProjectionTraceArrangementWeight,
              rowProjectionTraceArrangementViaBatchWeight,
              rowProjectionProjectBatchDeltaWeight,
              rowArrangementDirtyRestrictWeight
            ]
            (Right (rowProjectionCaseDense size))
        ) $ \projectionCaseDenseValue ->
        bgroup
          (caseLabel "row-projection/snapshot" size)
          [ bench (caseLabel "RowProjection.batchToIndexedRows" size) (nf rowProjectionBatchRowsWeight projectionCaseValue),
            bench (caseLabel "RowProjection.snapshotTraceToIndexedRowArrangement" size) (nf rowProjectionTraceArrangementWeight projectionCaseValue),
            bench (caseLabel "RowProjection.snapshotTraceToIndexedRowArrangement viaBatch" size) (nf rowProjectionTraceArrangementViaBatchWeight projectionCaseValue),
            bench (caseLabel "RowProjection.project-batch-delta" size) (nf rowProjectionProjectBatchDeltaWeight projectionCaseValue),
            bench (caseLabel "RowArrangement.dirty.restrict" size) (nf rowArrangementDirtyRestrictWeight projectionCaseValue),
            bench (caseLabel "RowProjection.batchToIndexedRows" size <> " dense") (nf rowProjectionBatchRowsWeight projectionCaseDenseValue),
            bench (caseLabel "RowProjection.snapshotTraceToIndexedRowArrangement" size <> " dense") (nf rowProjectionTraceArrangementWeight projectionCaseDenseValue),
            bench (caseLabel "RowProjection.snapshotTraceToIndexedRowArrangement viaBatch" size <> " dense") (nf rowProjectionTraceArrangementViaBatchWeight projectionCaseDenseValue),
            bench (caseLabel "RowProjection.project-batch-delta" size <> " dense") (nf rowProjectionProjectBatchDeltaWeight projectionCaseDenseValue),
            bench (caseLabel "RowArrangement.dirty.restrict" size <> " dense") (nf rowArrangementDirtyRestrictWeight projectionCaseDenseValue)
          ],
    env
      ( checkedBenchCases
          "row projection delta"
          [rowProjectionApplyBatchDeltaWeight, rowProjectionRebuildValueIndexWeight]
          (rowProjectionDeltaCase size)
      ) $ \projectionCaseValue ->
      env
        ( checkedBenchCase
            "row projection delta dense"
            rowProjectionApplyBatchDeltaWeight
            (rowProjectionDeltaCaseDense size)
        ) $ \projectionCaseDenseValue ->
        bgroup
          (caseLabel "row-projection/row-delta" size)
          [ bench (caseLabel "RowProjection.apply-batch-delta" size) (nf rowProjectionApplyBatchDeltaWeight projectionCaseValue),
            bench (caseLabel "RowProjection.rebuild-value-index" size) (nf rowProjectionRebuildValueIndexWeight projectionCaseValue),
            bench (caseLabel "RowProjection.apply-batch-delta" size <> " dense") (nf rowProjectionApplyBatchDeltaWeight projectionCaseDenseValue)
          ]
  ]

runtimeSettleBenchmarks :: Benchmark
runtimeSettleBenchmarks =
  bgroup
    "runtime-settle"
    [ env (checkedBenchCases "runtime settle" [runtimeSettleWeight, runtimeScopedSettleWeight] (Right (runtimeSettleCase 512))) $ \settleCaseValue ->
        bench "runRuntimeSettleLoop n=512" (nf runtimeSettleWeight settleCaseValue),
      env (checkedBenchCases "runtime settle" [runtimeSettleWeight, runtimeScopedSettleWeight] (Right (runtimeSettleCase 512))) $ \settleCaseValue ->
        bench "runRuntimeSettleLoopScoped n=512" (nf runtimeScopedSettleWeight settleCaseValue)
    ]
