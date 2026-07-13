module Allocation where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Void (Void)
import GHC.Stats qualified as RTS
import Moonlight.Differential.Algebra.ZSet qualified as ZSet
import ArrangementJoin
import CircuitFixpoint
import Common
import Operator
import Projection
import RowProjection
import Storage
import Stream
import TraceRead
import WCOJ
import Moonlight.Differential.Batch (BatchMergeFuel (..))
import Moonlight.Differential.Circuit
  ( Circuit,
    CircuitBatch,
    CircuitBuilder,
    CircuitOutputs,
    IndexedNode,
    InputPort,
    Node,
    SealedCircuit,
    advanceCircuit,
    buildCircuit,
    emptyCircuitBatch,
    feedInput,
    indexByNode,
    inputNode,
    joinNodes,
    outputDelta,
    withSealedCircuit,
  )
import System.Environment (getArgs)
import System.Mem (performGC)

data AllocationProbe = AllocationProbe
  { allocationProbeLabel :: !String,
    allocationProbeSetup :: !(IO ()),
    allocationProbeMeasure :: !(IO Int)
  }

mkAllocationProbe ::
  NFData input =>
  String ->
  input ->
  (input -> Int) ->
  AllocationProbe
mkAllocationProbe label input measure =
  AllocationProbe
    { allocationProbeLabel = label,
      allocationProbeSetup = evaluate (rnf input),
      allocationProbeMeasure = evaluate (measure input)
    }

mkCheckedAllocationProbe ::
  NFData input =>
  String ->
  Either String input ->
  (input -> Either String Int) ->
  AllocationProbe
mkCheckedAllocationProbe label input measure =
  AllocationProbe
    { allocationProbeLabel = label,
      allocationProbeSetup =
        checkedCase label input >>= evaluate . rnf,
      allocationProbeMeasure =
        checkedCase label input >>= checkedCase label . measure >>= evaluate
    }

allocationReportMain :: IO ()
allocationReportMain = do
  args <- getArgs
  statsEnabled <- RTS.getRTSStatsEnabled
  if statsEnabled
    then Foldable.traverse_ runAllocationProbe (allocationReportProbes (allocationReportFilters args))
    else putStrLn "RTS statistics are disabled; rerun with +RTS -T -RTS."

allocationReportFilters :: [String] -> [String]
allocationReportFilters args =
  case args of
    "--allocation-report" : filters ->
      filters
    filters ->
      filters

allocationReportProbes :: [String] -> [AllocationProbe]
allocationReportProbes filters =
  filter (allocationProbeMatches filters) allocationProbes

allocationProbeMatches :: [String] -> AllocationProbe -> Bool
allocationProbeMatches filters probe =
  null filters
    || Foldable.any (`List.isInfixOf` allocationProbeLabel probe) filters

runAllocationProbe :: AllocationProbe -> IO ()
runAllocationProbe probe = do
  allocationProbeSetup probe
  performGC
  before <- RTS.getRTSStats
  result <- allocationProbeMeasure probe
  performGC
  after <- RTS.getRTSStats
  putStrLn
    ( allocationProbeLabel probe
        <> ": allocated_bytes="
        <> show (RTS.allocated_bytes after - RTS.allocated_bytes before)
        <> " result="
        <> show result
    )

allocationProbes :: [AllocationProbe]
allocationProbes =
  [ mkAllocationProbe "Batch.fromUpdates duplicate-heavy n=2048" (duplicateHeavyUpdateCase 2048) batchBuildCellCount,
    mkAllocationProbe "Batch.fromUpdates skewed-key n=2048" (skewedKeyUpdateCase 2048) batchBuildCellCount,
    mkAllocationProbe "Batch.singletonBatch sweep n=2048" (updateCase 2048) batchSingletonBuildCellCount,
    mkAllocationProbe "Batch.mergeBatches many-small n=2048" (manySmallBatchCase 2048) mergeManySmallCount,
    mkAllocationProbe "Batch.merge-cover/disjoint fanin=64 n=2048" (disjointBatchCoverCase 2048 64) batchCoverMergeWeight,
    mkAllocationProbe "BatchMerger.work fuel=64 n=2048" (binaryBatchMergerCase 2048 (BatchMergeFuel 64)) binaryBatchMergerFuelWeight,
    mkAllocationProbe "IndexedZSet.fromList storage n=10000" (indexedZSetStorageEntries 10000) indexedZSetConstructCellCount,
    mkAllocationProbe "IndexedZSet.fromList storage n=100000" (indexedZSetStorageEntries 100000) indexedZSetConstructCellCount,
    mkAllocationProbe "IndexedZSet.merge cancel n=10000" (indexedZSetStorageMergeCase 10000) indexedZSetMergeCellCount,
    mkAllocationProbe "IndexedZSet.merge cancel n=100000" (indexedZSetStorageMergeCase 100000) indexedZSetMergeCellCount,
    mkAllocationProbe "IndexedZSet.merge disjoint n=10000" (indexedZSetStorageDisjointMergeCase 10000) indexedZSetMergeCellCount,
    mkAllocationProbe "IndexedZSet.merge disjoint n=100000" (indexedZSetStorageDisjointMergeCase 100000) indexedZSetMergeCellCount,
    mkAllocationProbe "IndexedZSet.merge partial-cancel n=10000" (indexedZSetStoragePartialCancelMergeCase 10000) indexedZSetMergeCellCount,
    mkAllocationProbe "IndexedZSet.merge partial-cancel n=100000" (indexedZSetStoragePartialCancelMergeCase 100000) indexedZSetMergeCellCount,
    mkAllocationProbe "IndexedZSet.unions singleton sections n=10000" (indexedZSetSingletonSections 10000) indexedZSetUnionCellCount,
    mkAllocationProbe "IndexedZSet.unions singleton sections n=100000" (indexedZSetSingletonSections 100000) indexedZSetUnionCellCount,
    mkAllocationProbe "Trace.append+periodic-maintenance+snapshot n=2048" (periodicTraceBatchCase 2048) traceAppendPeriodicMaintenanceSnapshotCount,
    mkAllocationProbe "Trace.compact-physical/fueled n=2048" (periodicTraceBatchCase 2048) traceCompactPhysicalStepWeight,
    mkAllocationProbe "Trace.spine physical profile periodic n=2048" (periodicTraceBatchCase 2048) traceAppendPeriodicPhysicalProfileWeight,
    mkAllocationProbe "Trace.retained-prefix read n=2048" (retainedPrefixTraceReadCase 2048) traceRetainedPrefixReadWeight,
    mkAllocationProbe "Trace.retained-prefix materialized n=2048" (retainedPrefixTraceReadCase 2048) traceRetainedPrefixMaterializedWeight,
    mkAllocationProbe "Trace.key-prefix read n=2048" (retainedPrefixTraceReadCase 2048) traceKeyPrefixReadWeight,
    mkAllocationProbe "Trace.key-prefix viaArrangement n=2048" (retainedPrefixTraceReadCase 2048) traceKeyPrefixViaArrangementWeight,
    mkAllocationProbe "Trace.null physically-empty n=2048" (physicallyEmptyTraceNullCase 2048) traceNullWeight,
    mkAllocationProbe "Trace.null physically-empty viaBatch n=2048" (physicallyEmptyTraceNullCase 2048) traceNullViaBatchWeight,
    mkAllocationProbe "Trace.compactPhysical no-op n=2048" (traceCompactNoOpCase 2048) traceCompactNoOpProfileWeight,
    mkCheckedAllocationProbe "Circuit.advance shared fan-out K=1" (sharedFanOutCase 1) sharedFanOutAdvanceWeight,
    mkCheckedAllocationProbe "Circuit.advance shared fan-out K=4" (sharedFanOutCase 4) sharedFanOutAdvanceWeight,
    mkCheckedAllocationProbe "Circuit.advance shared fan-out K=16" (sharedFanOutCase 16) sharedFanOutAdvanceWeight,
    mkCheckedAllocationProbe "Circuit.advance private fan-out K=1" (privateFanOutCase 1) privateFanOutAdvanceWeight,
    mkCheckedAllocationProbe "Circuit.advance private fan-out K=4" (privateFanOutCase 4) privateFanOutAdvanceWeight,
    mkCheckedAllocationProbe "Circuit.advance private fan-out K=16" (privateFanOutCase 16) privateFanOutAdvanceWeight,
    mkCheckedAllocationProbe "Circuit.fixpoint incremental path-growth n=64" (pathGrowthCase 64) incrementalWeight,
    mkCheckedAllocationProbe "Circuit.fixpoint re-saturate path-growth n=64" (pathGrowthCase 64) resaturateWeight,
    mkCheckedAllocationProbe "Circuit.fixpoint incremental path-growth n=128" (pathGrowthCase 128) incrementalWeight,
    mkCheckedAllocationProbe "Circuit.fixpoint re-saturate path-growth n=128" (pathGrowthCase 128) resaturateWeight,
    mkCheckedAllocationProbe "Circuit.fixpoint incremental churn n=96" (churnCase 96) incrementalWeight,
    mkCheckedAllocationProbe "Circuit.fixpoint re-saturate churn n=96" (churnCase 96) resaturateWeight,
    mkAllocationProbe "Arrangement.arrangeByKey many-batch n=2048" (arrangementManyBatchCase 2048) arrangementBuildTraceWeight,
    mkAllocationProbe "Arrangement.arrangeByKey many-batch viaBatch n=2048" (arrangementManyBatchCase 2048) arrangementBuildTraceViaBatchWeight,
    mkAllocationProbe "Arrangement.appendArrangementBatch n=2048" (arrangementAppendCase 2048) arrangementAppendWeight,
    mkAllocationProbe "Arrangement.appendArrangementBatch hot-key n=2048" (arrangementHotKeyAppendCase 2048) arrangementAppendWeight,
    mkAllocationProbe "Cursor.merge n=2048" (cursorMergeCase 2048) cursorMergeWeight,
    mkAllocationProbe "Operator.foldDeltaJoin materialized-view skew-fanout n=2048" (joinSkewFanoutCase 2048) deltaJoinMaterializedCount,
    mkAllocationProbe "Operator.foldDeltaJoin skew-fanout n=2048" (joinSkewFanoutCase 2048) foldDeltaJoinCount,
    mkAllocationProbe "Stream.differentiate n=2048" (naturalStreamCase 2048) streamDifferentiateWeight,
    mkAllocationProbe "Stream.integrate n=2048" (naturalStreamCase 2048) streamIntegrateWeight,
    mkAllocationProbe "Stream.incrementalize map n=2048" (naturalStreamCase 2048) streamIncrementalizeMapWeight,
    mkAllocationProbe "Stream.incrementalize generic map n=2048" (naturalStreamCase 2048) streamIncrementalizeGenericMapWeight,
    mkAllocationProbe "Stream.product-time integrate.differentiate n=8" (productStreamCase 8) productStreamDifferentiateIntegrateWeight,
    mkAllocationProbe "Stream.product-time integrate.differentiate n=16" (productStreamCase 16) productStreamDifferentiateIntegrateWeight,
    mkAllocationProbe "Stream.product-time integrate.differentiate n=32" (productStreamCase 32) productStreamDifferentiateIntegrateWeight,
    mkAllocationProbe "Operator.groupViewAdvance n=2048" (operatorGroupViewCase 2048) operatorGroupViewAdvanceWeight,
    mkCheckedAllocationProbe "Fixpoint.semiNaive path n=2048" (Right (fixpointPathCase 2048)) semiNaivePathWeight,
    mkCheckedAllocationProbe "Fixpoint.arranged path n=2048" (Right (arrangedFixpointPathCase 2048)) arrangedSemiNaivePathWeight,
    mkAllocationProbe "Projection.commit n=512" (projectionPropagationCase 512) projectionCommitWeight,
    mkCheckedAllocationProbe "Trace.ReadIndex slice n=2048" (Right (timeIndexReadCase 2048)) timeIndexReadWeight,
    mkCheckedAllocationProbe "RowProjection.snapshotTraceToIndexedRowArrangement n=2048" (Right (rowProjectionCase 2048)) rowProjectionTraceArrangementWeight,
    mkCheckedAllocationProbe "RowProjection.project-batch-delta n=2048" (Right (rowProjectionCase 2048)) rowProjectionProjectBatchDeltaWeight,
    mkAllocationProbe "foldGenericJoin materialized-view n=16" (wcojProblem 16) foldGenericJoinMaterializedCount,
    mkAllocationProbe "foldGenericJoin n=16" (wcojProblem 16) foldGenericJoinCount,
    mkAllocationProbe "adaptiveJoin materialized n=16" (wcojProblem 16) adaptiveJoinCount,
    mkAllocationProbe "foldAdaptiveJoin n=16" (wcojProblem 16) foldAdaptiveJoinCount,
    mkAllocationProbe "foldIntIndexedAdaptiveJoin n=16" (wcojProblem 16) foldIntIndexedAdaptiveJoinCount,
    mkAllocationProbe "WCOJ.chooseSmallestSlot materialized-count n=64" (wcojProblem 64) wcojChooseSmallestSlotMaterializedWeight,
    mkAllocationProbe "WCOJ.chooseSmallestSlot direct-count n=64" (wcojProblem 64) wcojChooseSmallestSlotDirectWeight,
    mkAllocationProbe "DenseTriangle.count clique n=512" (denseTriangleCliqueCase 512) denseTriangleCountWeight,
    mkAllocationProbe "DenseTriangle.exact-count clique n=128" (denseTriangleExactCliqueCase 128) exactTriangleCountWeight,
    mkAllocationProbe "DenseTriangle.count skewed n=512" (denseTriangleSkewedCase 512) denseTriangleCountWeight,
    mkAllocationProbe "Pipeline.trace append+periodic-maintenance+snapshot n=2048" (decomposedPipelineCase 2048) decomposedPipelineTraceIngestWeight,
    mkAllocationProbe "Pipeline.arrange n=2048" (decomposedPipelineCase 2048) decomposedPipelineArrangementWeight,
    mkAllocationProbe "Pipeline.join n=2048" (decomposedPipelineCase 2048) decomposedPipelineJoinWeight,
    mkCheckedAllocationProbe "Pipeline.project n=2048" (Right (decomposedPipelineCase 2048)) decomposedPipelineProjectionWeight,
    mkAllocationProbe "Retraction.trace apply n=2048" (retractionPipelineCase 2048) retractionTraceApplyWeight,
    mkAllocationProbe "Retraction.join n=2048" (retractionPipelineCase 2048) retractionJoinWeight
  ]

type CircuitFanOutRow = (Int, Int)

type CircuitFanOutWeightedRow = (CircuitFanOutRow, Int)

type CircuitFanOutJoinRow = (Int, CircuitFanOutRow, CircuitFanOutRow)

data CircuitFanOutBatch = CircuitFanOutBatch
  { circuitFanOutXRows :: ![CircuitFanOutWeightedRow],
    circuitFanOutYRows :: ![CircuitFanOutWeightedRow]
  }

instance NFData CircuitFanOutBatch where
  rnf (CircuitFanOutBatch xRows yRows) =
    rnf xRows `seq` rnf yRows

data SharedFanOutPorts s = SharedFanOutPorts
  { sharedFanOutXPort :: !(InputPort s CircuitFanOutRow),
    sharedFanOutYPorts :: ![InputPort s CircuitFanOutRow],
    sharedFanOutJoins :: ![Node s CircuitFanOutJoinRow]
  }

data SharedFanOutCase = SharedFanOutCase
  { sharedFanOutBatches :: ![CircuitFanOutBatch],
    sharedFanOutSealedCircuit :: !(SealedCircuit Void Int SharedFanOutPorts)
  }

instance NFData SharedFanOutCase where
  rnf (SharedFanOutCase batches sealed) =
    rnf batches `seq` sealed `seq` ()

data PrivateFanOutPorts s = PrivateFanOutPorts
  { privateFanOutXPorts :: ![InputPort s CircuitFanOutRow],
    privateFanOutYPorts :: ![InputPort s CircuitFanOutRow],
    privateFanOutJoins :: ![Node s CircuitFanOutJoinRow]
  }

data PrivateFanOutCase = PrivateFanOutCase
  { privateFanOutBatches :: ![CircuitFanOutBatch],
    privateFanOutSealedCircuit :: !(SealedCircuit Void Int PrivateFanOutPorts)
  }

instance NFData PrivateFanOutCase where
  rnf (PrivateFanOutCase batches sealed) =
    rnf batches `seq` sealed `seq` ()

circuitFanOutBatches :: [CircuitFanOutBatch]
circuitFanOutBatches =
  circuitFanOutBatchAt <$> [0 .. 63]

circuitFanOutBatchAt :: Int -> CircuitFanOutBatch
circuitFanOutBatchAt batch =
  CircuitFanOutBatch
    { circuitFanOutXRows = rows,
      circuitFanOutYRows = rows
    }
  where
    rows =
      circuitFanOutRowsAt 1 batch <> circuitFanOutRetractionsAt batch

circuitFanOutRetractionsAt :: Int -> [CircuitFanOutWeightedRow]
circuitFanOutRetractionsAt batch
  | batch >= 2 && (batch + 1) `mod` 4 == 0 =
      circuitFanOutRowsAt (-1) (batch - 2)
  | otherwise =
      []

circuitFanOutRowsAt :: Int -> Int -> [CircuitFanOutWeightedRow]
circuitFanOutRowsAt weight batch =
  rowAt <$> [0 .. 7]
  where
    rowAt offset =
      (((batch * 8 + offset) `mod` 256, batch), weight)

sharedFanOutCase :: Int -> Either String SharedFanOutCase
sharedFanOutCase fanOut =
  SharedFanOutCase circuitFanOutBatches
    <$> eitherShow (buildCircuit (sharedFanOutBuilder fanOut))

sharedFanOutBuilder ::
  Int ->
  CircuitBuilder s Void Int (SharedFanOutPorts s)
sharedFanOutBuilder fanOut = do
  (xPort, xRows) <- inputNode
  xIndex <- indexByNode fst xRows
  branches <- traverse (const (sharedFanOutBranch xIndex)) [1 .. fanOut]
  pure
    SharedFanOutPorts
      { sharedFanOutXPort = xPort,
        sharedFanOutYPorts = fst <$> branches,
        sharedFanOutJoins = snd <$> branches
      }

sharedFanOutBranch ::
  IndexedNode s Int CircuitFanOutRow ->
  CircuitBuilder s Void Int (InputPort s CircuitFanOutRow, Node s CircuitFanOutJoinRow)
sharedFanOutBranch xIndex = do
  (yPort, yRows) <- inputNode
  yIndex <- indexByNode fst yRows
  joined <- joinNodes xIndex yIndex
  pure (yPort, joined)

privateFanOutCase :: Int -> Either String PrivateFanOutCase
privateFanOutCase fanOut =
  PrivateFanOutCase circuitFanOutBatches
    <$> eitherShow (buildCircuit (privateFanOutBuilder fanOut))

privateFanOutBuilder ::
  Int ->
  CircuitBuilder s Void Int (PrivateFanOutPorts s)
privateFanOutBuilder fanOut = do
  branches <- traverse (const privateFanOutBranch) [1 .. fanOut]
  pure
    PrivateFanOutPorts
      { privateFanOutXPorts = (\(xPort, _, _) -> xPort) <$> branches,
        privateFanOutYPorts = (\(_, yPort, _) -> yPort) <$> branches,
        privateFanOutJoins = (\(_, _, joined) -> joined) <$> branches
      }

privateFanOutBranch ::
  CircuitBuilder
    s
    Void
    Int
    (InputPort s CircuitFanOutRow, InputPort s CircuitFanOutRow, Node s CircuitFanOutJoinRow)
privateFanOutBranch = do
  (xPort, xRows) <- inputNode
  (yPort, yRows) <- inputNode
  xIndex <- indexByNode fst xRows
  yIndex <- indexByNode fst yRows
  joined <- joinNodes xIndex yIndex
  pure (xPort, yPort, joined)

sharedFanOutAdvanceWeight :: SharedFanOutCase -> Either String Int
sharedFanOutAdvanceWeight (SharedFanOutCase batches sealed) =
  withSealedCircuit sealed $ \circuit ports ->
    replayFanOutBatches sharedFanOutCircuitBatch sharedFanOutOutputsSize ports circuit batches

privateFanOutAdvanceWeight :: PrivateFanOutCase -> Either String Int
privateFanOutAdvanceWeight (PrivateFanOutCase batches sealed) =
  withSealedCircuit sealed $ \circuit ports ->
    replayFanOutBatches privateFanOutCircuitBatch privateFanOutOutputsSize ports circuit batches

replayFanOutBatches ::
  (ports s -> CircuitFanOutBatch -> CircuitBatch s Int) ->
  (ports s -> CircuitOutputs s Int -> Either String Int) ->
  ports s ->
  Circuit s Void Int ->
  [CircuitFanOutBatch] ->
  Either String Int
replayFanOutBatches batchFor outputWeight ports circuit batches =
  snd
    <$> Foldable.foldl'
      (advanceFanOutBatch batchFor outputWeight ports)
      (Right (circuit, 0))
      batches

advanceFanOutBatch ::
  (ports s -> CircuitFanOutBatch -> CircuitBatch s Int) ->
  (ports s -> CircuitOutputs s Int -> Either String Int) ->
  ports s ->
  Either String (Circuit s Void Int, Int) ->
  CircuitFanOutBatch ->
  Either String (Circuit s Void Int, Int)
advanceFanOutBatch batchFor outputWeight ports state batch =
  state >>= \(circuit, total) ->
    case advanceCircuit (batchFor ports batch) circuit of
      Left obstruction ->
        Left (show obstruction)
      Right (outputs, nextCircuit) -> do
        batchWeight <- outputWeight ports outputs
        let nextTotal = total + batchWeight
        nextTotal `seq` Right (nextCircuit, nextTotal)

sharedFanOutCircuitBatch ::
  SharedFanOutPorts s ->
  CircuitFanOutBatch ->
  CircuitBatch s Int
sharedFanOutCircuitBatch ports batch =
  feedFanOutInputs (sharedFanOutYPorts ports) yDelta $
    feedInput (sharedFanOutXPort ports) xDelta emptyCircuitBatch
  where
    xDelta =
      ZSet.zsetFromList (circuitFanOutXRows batch)
    yDelta =
      ZSet.zsetFromList (circuitFanOutYRows batch)

privateFanOutCircuitBatch ::
  PrivateFanOutPorts s ->
  CircuitFanOutBatch ->
  CircuitBatch s Int
privateFanOutCircuitBatch ports batch =
  feedFanOutInputs (privateFanOutYPorts ports) yDelta $
    feedFanOutInputs (privateFanOutXPorts ports) xDelta emptyCircuitBatch
  where
    xDelta =
      ZSet.zsetFromList (circuitFanOutXRows batch)
    yDelta =
      ZSet.zsetFromList (circuitFanOutYRows batch)

feedFanOutInputs ::
  [InputPort s CircuitFanOutRow] ->
  ZSet.ZSet CircuitFanOutRow Int ->
  CircuitBatch s Int ->
  CircuitBatch s Int
feedFanOutInputs ports delta batch =
  Foldable.foldl' (\current port -> feedInput port delta current) batch ports

sharedFanOutOutputsSize :: SharedFanOutPorts s -> CircuitOutputs s Int -> Either String Int
sharedFanOutOutputsSize ports =
  fanOutOutputsSize (sharedFanOutJoins ports)

privateFanOutOutputsSize :: PrivateFanOutPorts s -> CircuitOutputs s Int -> Either String Int
privateFanOutOutputsSize ports =
  fanOutOutputsSize (privateFanOutJoins ports)

fanOutOutputsSize :: [Node s CircuitFanOutJoinRow] -> CircuitOutputs s Int -> Either String Int
fanOutOutputsSize joins outputs =
  fmap sum
    ( traverse
        (\joined -> eitherShow (fmap joinedDeltaSize (outputDelta joined outputs)))
        joins
    )

joinedDeltaSize :: ZSet.ZSet CircuitFanOutJoinRow Int -> Int
joinedDeltaSize delta =
  Foldable.foldl' countRow 0 (ZSet.zsetToAscList delta)
  where
    countRow :: Int -> (CircuitFanOutJoinRow, Int) -> Int
    countRow total ((key, (leftKey, leftBatch), (rightKey, rightBatch)), weight) =
      key
        `seq` leftKey
        `seq` leftBatch
        `seq` rightKey
        `seq` rightBatch
        `seq` weight
        `seq` total + 1
