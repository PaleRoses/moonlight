{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Moonlight.Flow.Bench.Runtime.RbacEntitlement.Print
  ( printSummary,
    printSummaryEnvelope,
    printRbacBatchReport,
    printRbacTargetedTimingReport,
    printRbacLocalityMatrixReport,
    printResourceScopeReproducerReport,
    showNsMs,
  )
where

import Data.Foldable qualified as Foldable
import Data.List
  ( sortOn,
  )
import Data.Map.Strict qualified as Map
import Data.Ord
  ( Down (..),
  )
import Moonlight.Flow.Runtime.Types qualified as R
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Stats
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Types
import System.IO
  ( hFlush,
    stdout,
  )

printSummary :: RbacRunSummary -> IO ()
printSummary summaryValue = do
  putStrLn ("config=" <> show (rrsConfig summaryValue))
  putStrLn ("initial=" <> maybe "-" show (rrsInitialDigest summaryValue))
  putStrLn ("last_observed=" <> maybe "-" show (rrsLastObservedDigest summaryValue))
  Foldable.traverse_ printRbacBatchReport (rrsReports summaryValue)
  putStrLn ("final=" <> maybe "-" show (rrsFinalDigest summaryValue))

printSummaryEnvelope :: RbacRunSummary -> IO ()
printSummaryEnvelope summaryValue = do
  putStrLn ("config=" <> show (rrsConfig summaryValue))
  putStrLn ("initial=" <> maybe "-" show (rrsInitialDigest summaryValue))
  putStrLn ("last_observed=" <> maybe "-" show (rrsLastObservedDigest summaryValue))
  putStrLn ("final=" <> maybe "-" show (rrsFinalDigest summaryValue))

printRbacLocalityMatrixReport :: RbacLocalityMatrixReport -> IO ()
printRbacLocalityMatrixReport report = do
  putStrLn ("config=" <> show (rlmrConfig report))
  putStrLn
    ( "warmup_apply_ms="
        <> showNsMs (rlmrWarmupApplyNs report)
        <> " warmup_patch="
        <> show (rlmrWarmupPatch report)
        <> " warmup_diag="
        <> show (rlmrWarmupDiagnostics report)
    )
  Foldable.traverse_ printRbacLocalityScenarioReport (rlmrScenarios report)

printRbacTargetedTimingReport :: RbacTargetedTimingReport -> IO ()
printRbacTargetedTimingReport report = do
  putStrLn ("config=" <> show (rttrConfig report))
  putStrLn
    ( "warmup_apply_ms="
        <> showNsMs (rttrWarmupApplyNs report)
        <> " warmup_patch="
        <> show (rttrWarmupPatch report)
        <> " "
        <> showRepairStatsCompact (rttrWarmupRepairStats report)
        <> " warmup_diag="
        <> show (rttrWarmupDiagnostics report)
    )
  Foldable.traverse_ printRbacTargetedScenarioReport (rttrScenarios report)

printRbacTargetedScenarioReport :: RbacTargetedScenarioReport -> IO ()
printRbacTargetedScenarioReport report =
  putStrLn
    ( "target="
        <> show (rtsrScenario report)
        <> " apply_ms="
        <> showNsMs (rtsrApplyNs report)
        <> " patch="
        <> show (rtsrPatch report)
        <> " "
        <> showRepairStatsCompact (rtsrRepairStats report)
        <> " stale_delta="
        <> show (rtsrStaleRejectedDelta report)
        <> " registered_new_delta="
        <> show (rtsrRegisteredNewDelta report)
        <> " reuse="
        <> show (rtsrReuseStats report)
        <> " "
        <> showRuntimeStatsCompact (rtsrRuntimeStats report)
    )

printRbacLocalityScenarioReport :: RbacLocalityScenarioReport -> IO ()
printRbacLocalityScenarioReport report =
  putStrLn
    ( "scenario="
        <> show (rlsrScenario report)
        <> " apply_ms="
        <> showNsMs (rlsrApplyNs report)
        <> " fresh_ms="
        <> showNsMs (rlsrFreshCheckNs report)
        <> " registered_shapes_before="
        <> show (rlsrRegisteredFactorShapesBefore report)
        <> " registered_shapes_after="
        <> show (rlsrRegisteredFactorShapesAfter report)
        <> " stale_delta="
        <> show (rlsrStaleRejectedDelta report)
        <> " registered_new_delta="
        <> show (rlsrRegisteredNewDelta report)
        <> " patch_shape="
        <> show (rlsrPatchShape report)
        <> " patch="
        <> show (rlsrPatch report)
        <> " diag_after="
        <> show (rlsrDiagnosticsAfter report)
    )

printResourceScopeReproducerReport :: RbacResourceScopeReproducerReport -> IO ()
printResourceScopeReproducerReport report = do
  putStrLn ("config=" <> show (rrsrConfig report))
  putStrLn ("resource_scope_delete=" <> show (rrsrDeletedResourceScopeRows report))
  putStrLn ("resource_scope_insert=" <> show (rrsrInsertedResourceScopeRows report))
  putStrLn ("patch=" <> show (rrsrPatch report))
  Foldable.traverse_ printResourceScopeReproducerCase (rrsrCases report)

printResourceScopeReproducerCase :: RbacResourceScopeReproducerCaseReport -> IO ()
printResourceScopeReproducerCase report =
  putStrLn
    ( "case="
        <> show (rrscrPlanSet report)
        <> " seed_atoms="
        <> show (rrscrSeedAtoms report)
        <> " outcome="
        <> show (rrscrOutcome report)
    )

printRbacBatchReport :: RbacBatchReport -> IO ()
printRbacBatchReport report = do
  putStrLn
    ( "batch="
        <> show (rbrBatch report)
        <> " apply_ms="
        <> showNsMs (rbrApplyNs report)
        <> " read_ms="
        <> maybe "-" showNsMs (rbrReadNs report)
        <> " fresh_ms="
        <> maybe "-" showNsMs (rbrFreshCheckNs report)
        <> " semantic_ms="
        <> maybe "-" showNsMs (rbrSemanticCheckNs report)
        <> " fresh="
        <> show (rbrFreshMatched report)
        <> " effective="
        <> maybe "-" (show . rsdEffectiveCount) (rbrDigest report)
        <> " grant="
        <> maybe "-" (show . rrdPositiveCount . rsdGrant) (rbrDigest report)
        <> " conditional="
        <> maybe "-" (show . rrdPositiveCount . rsdConditionalGrant) (rbrDigest report)
        <> " denied="
        <> maybe "-" (show . rrdPositiveCount . rsdDenied) (rbrDigest report)
        <> " patch="
        <> show (rbrPatch report)
        <> " adversarial="
        <> show (rbrAdversarial report)
        <> " reuse="
        <> show (rbrReuseStats report)
        <> " reuse_diag="
        <> show (rbrReuseDiagnostics report)
        <> " "
        <> showRepairStatsCompact (rbrRepairStats report)
        <> " "
        <> showRuntimeStatsCompact (rbrRuntimeStats report)
    )
  hFlush stdout

showRepairStatsCompact :: R.RuntimeRepairStats -> String
showRepairStatsCompact stats =
  "repaired_factors="
    <> show (R.rprsFactorRepairs stats)
    <> " canonical_repairs="
    <> show (R.rprsCanonicalRepairs stats)
    <> " repair_subscribers="
    <> show (R.rprsRepairSubscribers stats)
    <> " rebuilt_factors="
    <> show (R.rprsNodesBuilt stats)
    <> " patched_nodes="
    <> show (R.rprsNodesPatched stats)
    <> " affected_keys="
    <> show (R.rprsAffectedKeys stats)
    <> " semantic_affected_keys="
    <> show (R.rprsSemanticAffectedKeys stats)
    <> " recomputed_cells="
    <> show (R.rprsRecomputedCells stats)
    <> " semantic_recomputed_cells="
    <> show (R.rprsRecomputedCells stats)
    <> " work_keys="
    <> show (R.rprsWorkKeys stats)
    <> " join_runs="
    <> show (R.rprsJoinRuns stats)
    <> " join_leaves="
    <> show (R.rprsJoinLeaves stats)
    <> " emitted_carrier_rows="
    <> show (R.rprsEmittedCarrierRows stats)
    <> " projection_rows_emitted="
    <> show (R.rprsProjectionRowsEmitted stats)
    <> " internal_carrier_rows="
    <> show (internalCarrierRows stats)
    <> " emitted_carrier_deltas="
    <> show (R.rprsEmittedCarrierDeltas stats)
    <> " input_delta_rows="
    <> show (R.rprsInputDeltaRows stats)
    <> " prepared_input_rebuilds="
    <> show (R.rprsPreparedInputRebuilds stats)
    <> " prepared_input_patch_hits="
    <> show (R.rprsPreparedInputPatchHits stats)
    <> " prepared_relation_rows="
    <> show (R.rprsPreparedRelationRows stats)
    <> " store_rebuilds="
    <> show (R.rprsStoreRebuilds stats)
    <> " "
    <> showRepairTelemetryCompact (R.rprsRepairTelemetry stats)
    <> " hot_factors="
    <> showHotRepairNodeStats stats

showRepairTelemetryCompact :: R.RepairTelemetry -> String
showRepairTelemetryCompact telemetry =
  "repair_selected_keys="
    <> show (R.rtSelectedOutputKeys telemetry)
    <> " repair_cells="
    <> show (R.rtSelectedRepairCells telemetry)
    <> " repair_row_entries="
    <> show (R.rtRepairRowMapEntries telemetry)
    <> " repair_support_outputs="
    <> show (R.rtRepairSupportOutputKeys telemetry)
    <> " repair_support_refs="
    <> show (R.rtRepairSupportRowRefsEnumerated telemetry)
    <> " repair_support_unique_refs="
    <> show (R.rtRepairSupportRowRefsUnique telemetry)
    <> " support_patch_cells="
    <> show (R.rtSupportCellsVisited telemetry)
    <> " support_patch_preserved="
    <> show (R.rtSupportPatchEdgesPreserved telemetry)
    <> " support_patch_inserted="
    <> show (R.rtSupportPatchEdgesInserted telemetry)
    <> " support_patch_deleted="
    <> show (R.rtSupportPatchEdgesDeleted telemetry)
    <> " support_patch_outputs_deleted="
    <> show (R.rtSupportPatchOutputKeysDeleted telemetry)
    <> " pv_atom_calls="
    <> show (R.rtPvAtomCalls telemetry)
    <> " pv_plus_calls="
    <> show (R.rtPvPlusCalls telemetry)
    <> " pv_times_calls="
    <> show (R.rtPvTimesCalls telemetry)
    <> " prov_intern_lookups="
    <> show (R.rtProvInternLookups telemetry)
    <> " prov_intern_inserts="
    <> show (R.rtProvInternInserts telemetry)
    <> " factor_cell_inserts="
    <> show (R.rtFactorCellInserts telemetry)
    <> " factor_cell_deletes="
    <> show (R.rtFactorCellDeletes telemetry)
    <> " factor_payload_sets="
    <> show (R.rtFactorPayloadSets telemetry)
{-# INLINE showRepairTelemetryCompact #-}


internalCarrierRows :: R.RuntimeRepairStats -> Int
internalCarrierRows stats =
  max 0 (R.rprsEmittedCarrierRows stats - R.rprsProjectionRowsEmitted stats)
{-# INLINE internalCarrierRows #-}

showHotRepairNodeStats :: R.RuntimeRepairStats -> String
showHotRepairNodeStats stats =
  show
    ( take 5
        ( sortOn
            (Down . repairNodeLoad . snd)
            (Map.toList (R.rprsNodeRepairs stats))
        )
    )


showRuntimeStatsCompact :: Maybe RbacRuntimeStats -> String
showRuntimeStatsCompact maybeStats =
  case maybeStats of
    Nothing ->
      "allocated_bytes=- max_live_bytes=- gc_cpu_ns=-"
    Just stats ->
      "allocated_bytes="
        <> show (rrsAllocatedBytesDelta stats)
        <> " max_live_bytes="
        <> show (rrsMaxLiveBytes stats)
        <> " gc_cpu_ns="
        <> show (rrsGcCpuNsDelta stats)
