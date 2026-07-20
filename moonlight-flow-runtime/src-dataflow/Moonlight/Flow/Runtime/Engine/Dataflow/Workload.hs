{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Moonlight.Flow.Runtime.Engine.Dataflow.Workload
  ( runtimeDataflowSnapshotForPatch,
    runtimeDataflowSnapshotForPatchWith,
    runtimeDataflowStepForPatch,
    runtimeDataflowStepForPatchWith,
    runtimeDataflowSignedSummary,
    runtimeDataflowVersionTrace,
    runtimeRepairStatsDelta,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text
  ( Text,
  )
import Data.Text qualified as Text
import Moonlight.Flow.Model.Delta
  ( QuotientPatch (..),
    atomPatchRows
  )
import Moonlight.Delta.Signed
  ( multiplicityChangeValue
  )
import Moonlight.Differential.Row.Patch
  ( EpochTransition (..),
    plainRowPatchChangeMap
  )
import Moonlight.Flow.Runtime.Core.Env
  ( RuntimeEnvelope (..),
  )
import Moonlight.Flow.Runtime.Core.Hydrate
  ( RuntimePatchPlan (..),
    RuntimePatchPlanError (..),
    planRuntimePatch,
  )
import Moonlight.Flow.Runtime.Core.Patch
  ( Patch,
  )
import Moonlight.Flow.Runtime.Core.RepairStats
  ( RuntimeRepairNodeStats (..),
    RuntimeRepairStats (..),
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Engine.Dataflow.Build
  ( runtimeDataflowSnapshotWith,
  )
import Moonlight.Flow.Runtime.Engine.Dataflow.Types
import Moonlight.Flow.Runtime.Engine.Patch.Schedule
  ( scheduleQuotientPatch,
  )
import Moonlight.Flow.Runtime.Engine.Step.Settle
  ( settleRuntime,
  )
import Moonlight.Flow.Runtime.Factor.State
  ( runtimeRepairStats,
  )
import Moonlight.Flow.Runtime.Types qualified as RuntimeTypes

runtimeDataflowSnapshotForPatch ::
  (Ord ctx, Ord prop, Show ctx, Show prop) =>
  Patch ->
  RuntimeTypes.Runtime ctx prop ->
  Either (RuntimeTypes.RuntimeApplyError ctx prop) RuntimeDataflowSnapshot
runtimeDataflowSnapshotForPatch =
  runtimeDataflowSnapshotForPatchWith defaultRuntimeDataflowRenderers
{-# INLINE runtimeDataflowSnapshotForPatch #-}

runtimeDataflowSnapshotForPatchWith ::
  (Ord ctx, Ord prop) =>
  RuntimeDataflowRenderers ctx prop ->
  Patch ->
  RuntimeTypes.Runtime ctx prop ->
  Either (RuntimeTypes.RuntimeApplyError ctx prop) RuntimeDataflowSnapshot
runtimeDataflowSnapshotForPatchWith renderers patch runtime =
  snd <$> runtimeDataflowStepForPatchWith renderers patch runtime
{-# INLINE runtimeDataflowSnapshotForPatchWith #-}

runtimeDataflowStepForPatch ::
  (Ord ctx, Ord prop, Show ctx, Show prop) =>
  Patch ->
  RuntimeTypes.Runtime ctx prop ->
  Either
    (RuntimeTypes.RuntimeApplyError ctx prop)
    (RuntimeTypes.Runtime ctx prop, RuntimeDataflowSnapshot)
runtimeDataflowStepForPatch =
  runtimeDataflowStepForPatchWith defaultRuntimeDataflowRenderers
{-# INLINE runtimeDataflowStepForPatch #-}

runtimeDataflowStepForPatchWith ::
  (Ord ctx, Ord prop) =>
  RuntimeDataflowRenderers ctx prop ->
  Patch ->
  RuntimeTypes.Runtime ctx prop ->
  Either
    (RuntimeTypes.RuntimeApplyError ctx prop)
    (RuntimeTypes.Runtime ctx prop, RuntimeDataflowSnapshot)
runtimeDataflowStepForPatchWith renderers patch runtimeValue@(RuntimeTypes.Runtime runtime) =
  case planRuntimePatch (Core.rsQuotientEpoch stateValue) (Core.rsSeedState stateValue) patch of
    Left err ->
      Left (runtimeDataflowApplyErrorFromPlanError err)
    Right RuntimePatchNoop ->
      Right (runtimeValue, runtimeDataflowSnapshotWith renderers runtime)
    Right (RuntimePatchSubmit quotientPatch) -> do
      scheduledRuntime <-
        first RuntimeTypes.RuntimeApplyRejected (scheduleQuotientPatch quotientPatch runtime)
      settledRuntime <-
        first RuntimeTypes.RuntimeApplyRejected (settleRuntime scheduledRuntime)
      let !snapshot =
            runtimeDataflowSnapshotWith renderers scheduledRuntime
          !repairDelta =
            runtimeRepairStatsDelta
              (runtimeRepairStats runtime)
              (runtimeRepairStats settledRuntime)
      Right
        ( RuntimeTypes.Runtime settledRuntime,
          snapshot
            { rdsWorkload =
                Just
                  ( runtimeDataflowWorkloadFromRepairDelta
                      (runtimeDataflowSignedSummary quotientPatch)
                      ( runtimeDataflowVersionTrace
                          (Core.rsLiveEpoch stateValue)
                          (Core.rsLiveEpoch (rdrState scheduledRuntime))
                          quotientPatch
                      )
                      repairDelta
                      snapshot
                  )
            }
        )
  where
    stateValue =
      rdrState runtime
{-# INLINE runtimeDataflowStepForPatchWith #-}

runtimeDataflowApplyErrorFromPlanError ::
  RuntimePatchPlanError ->
  RuntimeTypes.RuntimeApplyError ctx prop
runtimeDataflowApplyErrorFromPlanError err =
  case err of
    RuntimePatchInvalidSeedChunk chunk ->
      RuntimeTypes.RuntimeApplyInvalidSeedChunk chunk
    RuntimePatchSeedPending progress ->
      RuntimeTypes.RuntimeApplySeedPending progress
{-# INLINE runtimeDataflowApplyErrorFromPlanError #-}

runtimeDataflowWorkloadFromRepairDelta ::
  RuntimeDataflowSignedSummary ->
  RuntimeDataflowVersionTrace ->
  RuntimeDataflowRepairStats ->
  RuntimeDataflowSnapshot ->
  RuntimeDataflowWorkload
runtimeDataflowWorkloadFromRepairDelta deltaSummary versionTrace repairStats snapshot =
  RuntimeDataflowWorkload
    { rdwQueuedOperationCount = length ops,
      rdwTouchedCarrierCount = Set.size (readCarriers <> writeCarriers),
      rdwScheduledReadCarrierCount = Set.size readCarriers,
      rdwScheduledWriteCarrierCount = Set.size writeCarriers,
      rdwDeltaSummary = deltaSummary,
      rdwVersionTrace = versionTrace,
      rdwRepairStats = repairStats
    }
  where
    ops =
      rdsOps snapshot

    readCarriers =
      Set.fromList (foldMap rdoReads ops)

    writeCarriers =
      Set.fromList (foldMap rdoWrites ops)
{-# INLINE runtimeDataflowWorkloadFromRepairDelta #-}

runtimeDataflowSignedSummary :: QuotientPatch -> RuntimeDataflowSignedSummary
runtimeDataflowSignedSummary patch =
  RuntimeDataflowSignedSummary
    { rdsdsAtomPatchCount = IntMap.size (qpEvents patch),
      rdsdsTouchedRowCount = length multiplicities,
      rdsdsInsertedRowMultiplicity = sum positiveMultiplicities,
      rdsdsRemovedRowMultiplicity = sum negativeMultiplicities,
      rdsdsNetRowMultiplicity = sum signedMultiplicities
    }
  where
    multiplicities =
      foldMap
        (Map.elems . plainRowPatchChangeMap . atomPatchRows)
        (IntMap.elems (qpEvents patch))

    signedMultiplicities =
      fmap (fromIntegral . multiplicityChangeValue) multiplicities

    positiveMultiplicities =
      filter (> 0) signedMultiplicities

    negativeMultiplicities =
      fmap negate (filter (< 0) signedMultiplicities)
{-# INLINE runtimeDataflowSignedSummary #-}

runtimeDataflowVersionTrace ::
  Show liveEpoch =>
  liveEpoch ->
  liveEpoch ->
  QuotientPatch ->
  RuntimeDataflowVersionTrace
runtimeDataflowVersionTrace liveBefore liveScheduled patch =
  RuntimeDataflowVersionTrace
    { rdvtQuotientBefore = showText (etBefore (qpEpoch patch)),
      rdvtQuotientAfter = showText (etAfter (qpEpoch patch)),
      rdvtLiveBefore = showText liveBefore,
      rdvtLiveScheduled = showText liveScheduled,
      rdvtOrder =
        "partial by context/scope; ordered by quotient epoch, live epoch, phase, frontier within comparable scope"
    }
{-# INLINE runtimeDataflowVersionTrace #-}

runtimeRepairStatsDelta ::
  RuntimeRepairStats ->
  RuntimeRepairStats ->
  RuntimeDataflowRepairStats
runtimeRepairStatsDelta before after =
  RuntimeDataflowRepairStats
    { rdrsFactorRepairs = statDelta rprsFactorRepairs,
      rdrsCanonicalRepairs = statDelta rprsCanonicalRepairs,
      rdrsRepairSubscribers = statDelta rprsRepairSubscribers,
      rdrsNodesBuilt = statDelta rprsNodesBuilt,
      rdrsNodesReused = runtimeDataflowRepairNodeActionCount RuntimeRepairNodeReused nodeRows,
      rdrsNodesPatched = statDelta rprsNodesPatched,
      rdrsAffectedKeys = statDelta rprsAffectedKeys,
      rdrsSemanticAffectedKeys = statDelta rprsSemanticAffectedKeys,
      rdrsRecomputedCells = statDelta rprsRecomputedCells,
      rdrsEmittedCarrierDeltas = statDelta rprsEmittedCarrierDeltas,
      rdrsEmittedCarrierRows = statDelta rprsEmittedCarrierRows,
      rdrsProjectionRowsEmitted = statDelta rprsProjectionRowsEmitted,
      rdrsMaterializedSnapshots = statDelta rprsMaterializedSnapshots,
      rdrsInputDeltaRows = statDelta rprsInputDeltaRows,
      rdrsPreparedInputRebuilds = statDelta rprsPreparedInputRebuilds,
      rdrsPreparedInputPatchHits = statDelta rprsPreparedInputPatchHits,
      rdrsPreparedRelationRows = statDelta rprsPreparedRelationRows,
      rdrsStoreRebuilds = statDelta rprsStoreRebuilds,
      rdrsSupportEvaluations = statDelta rprsSupportEvaluations,
      rdrsSupportMemoHits = statDelta rprsSupportMemoHits,
      rdrsNodeRepairs = nodeRows
    }
  where
    nodeRows =
      runtimeDataflowRepairNodeDeltaRows before after

    statDelta field =
      saturatingIntMinus (field after) (field before)
{-# INLINE runtimeRepairStatsDelta #-}

runtimeDataflowRepairNodeDeltaRows ::
  RuntimeRepairStats ->
  RuntimeRepairStats ->
  [RuntimeDataflowRepairNode]
runtimeDataflowRepairNodeDeltaRows before after =
  Map.elems $
    Map.mergeWithKey
      ( \key beforeStats afterStats ->
          runtimeDataflowRepairNodeDeltaRow key (Just beforeStats) afterStats
      )
      (const Map.empty)
      ( Map.mapMaybeWithKey
          (\key afterStats -> runtimeDataflowRepairNodeDeltaRow key Nothing afterStats)
      )
      (rprsNodeRepairs before)
      (rprsNodeRepairs after)
{-# INLINE runtimeDataflowRepairNodeDeltaRows #-}

runtimeDataflowRepairNodeDeltaRow ::
  (Show queryId, Show factorNode) =>
  (queryId, factorNode) ->
  Maybe RuntimeRepairNodeStats ->
  RuntimeRepairNodeStats ->
  Maybe RuntimeDataflowRepairNode
runtimeDataflowRepairNodeDeltaRow (queryId, factorNode) maybeBefore afterStats
  | affectedKeys > 0 || recomputedCells > 0 || actionChanged =
      Just
        RuntimeDataflowRepairNode
          { rdrnQueryId = showText queryId,
            rdrnFactorNode = showText factorNode,
            rdrnAction = rrnsAction afterStats,
            rdrnAffectedKeys = affectedKeys,
            rdrnRecomputedCells = recomputedCells
          }
  | otherwise =
      Nothing
  where
    affectedKeys =
      saturatingIntMinus
        (rrnsAffectedKeys afterStats)
        (maybe 0 rrnsAffectedKeys maybeBefore)

    recomputedCells =
      saturatingIntMinus
        (rrnsRecomputedCells afterStats)
        (maybe 0 rrnsRecomputedCells maybeBefore)

    actionChanged =
      maybe True ((/= rrnsAction afterStats) . rrnsAction) maybeBefore
{-# INLINE runtimeDataflowRepairNodeDeltaRow #-}

runtimeDataflowRepairNodeActionCount ::
  RuntimeRepairNodeAction ->
  [RuntimeDataflowRepairNode] ->
  Int
runtimeDataflowRepairNodeActionCount action =
  length . filter ((== action) . rdrnAction)
{-# INLINE runtimeDataflowRepairNodeActionCount #-}

saturatingIntMinus :: Int -> Int -> Int
saturatingIntMinus newer older
  | newer >= older =
      newer - older
  | otherwise =
      0
{-# INLINE saturatingIntMinus #-}

showText :: Show a => a -> Text
showText =
  Text.pack . show
{-# INLINE showText #-}
