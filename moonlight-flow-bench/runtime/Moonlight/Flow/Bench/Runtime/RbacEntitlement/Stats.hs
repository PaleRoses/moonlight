{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Moonlight.Flow.Bench.Runtime.RbacEntitlement.Stats
  ( runtimeDiagnosticsReuseStats,
    runtimeDiagnosticsReuseDiagnostics,
    runtimeDiagnosticsRepairStats,
    timed,
    timedApplyPatch,
    readRuntimeStatsSample,
    runtimeStatsDelta,
    repairStatsDelta,
    staleRejectedDelta,
    registeredNewDelta,
    saturatingWord64Minus,
    saturatingIntMinus,
    repairNodeLoad,
    showNsMs,
  )
where

import Control.Exception
  ( evaluate,
  )
import Data.Map.Strict qualified as Map
import Data.Word
  ( Word64,
  )
import GHC.Clock
  ( getMonotonicTimeNSec,
  )
import GHC.Stats
  ( RTSStats (..),
    getRTSStats,
    getRTSStatsEnabled,
  )
import Moonlight.Flow.Patch qualified as R
import Moonlight.Flow.Runtime.Apply qualified as R
import Moonlight.Flow.Runtime.Inspect qualified as R
import Moonlight.Flow.Runtime.Types qualified as R
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Types

runtimeDiagnosticsReuseStats :: R.Runtime ctx prop -> R.RuntimeReuseStats
runtimeDiagnosticsReuseStats =
  R.rdReuseStats . R.runtimeDiagnostics
{-# INLINE runtimeDiagnosticsReuseStats #-}

runtimeDiagnosticsReuseDiagnostics :: R.Runtime ctx prop -> R.RuntimeReuseDiagnostics
runtimeDiagnosticsReuseDiagnostics =
  R.rdReuseDiagnostics . R.runtimeDiagnostics
{-# INLINE runtimeDiagnosticsReuseDiagnostics #-}

runtimeDiagnosticsRepairStats :: R.Runtime ctx prop -> R.RuntimeRepairStats
runtimeDiagnosticsRepairStats =
  R.rdRepairStats . R.runtimeDiagnostics
{-# INLINE runtimeDiagnosticsRepairStats #-}



timed :: IO value -> IO (Word64, value)
timed action = do
  start <- getMonotonicTimeNSec
  value <- action
  end <- getMonotonicTimeNSec
  pure (end - start, value)

timedApplyPatch ::
  R.Patch ->
  R.Runtime RbacContext RbacProp ->
  IO (Word64, Either RbacBenchError (R.Runtime RbacContext RbacProp))
timedApplyPatch patchValue runtime0 =
  timed $
    case R.applyPatch patchValue runtime0 of
      Left err ->
        pure (Left (RbacApplyError err))
      Right runtime1 -> do
        _ <- evaluate (runtimeDiagnosticsReuseStats runtime1)
        _ <- evaluate (runtimeDiagnosticsReuseDiagnostics runtime1)
        _ <- evaluate (runtimeDiagnosticsRepairStats runtime1)
        pure (Right runtime1)

readRuntimeStatsSample :: IO (Maybe RbacRuntimeStatsSample)
readRuntimeStatsSample = do
  enabled <- getRTSStatsEnabled
  if enabled
    then do
      stats <- getRTSStats
      pure
        ( Just
            RbacRuntimeStatsSample
              { rrssAllocatedBytes = allocated_bytes stats,
                rrssMaxLiveBytes = max_live_bytes stats,
                rrssGcCpuNs = fromIntegral (gc_cpu_ns stats)
              }
        )
    else pure Nothing

runtimeStatsDelta :: Maybe RbacRuntimeStatsSample -> Maybe RbacRuntimeStatsSample -> Maybe RbacRuntimeStats
runtimeStatsDelta maybeBefore maybeAfter =
  case (maybeBefore, maybeAfter) of
    (Just before, Just after) ->
      Just
        RbacRuntimeStats
          { rrsAllocatedBytesDelta = saturatingWord64Minus (rrssAllocatedBytes after) (rrssAllocatedBytes before),
            rrsMaxLiveBytes = rrssMaxLiveBytes after,
            rrsGcCpuNsDelta = saturatingWord64Minus (rrssGcCpuNs after) (rrssGcCpuNs before)
          }
    _ ->
      Nothing

repairStatsDelta :: R.RuntimeRepairStats -> R.RuntimeRepairStats -> R.RuntimeRepairStats
repairStatsDelta before after =
  R.RuntimeRepairStats
    { R.rprsFactorRepairs = statDelta R.rprsFactorRepairs,
      R.rprsCanonicalRepairs = statDelta R.rprsCanonicalRepairs,
      R.rprsRepairSubscribers = statDelta R.rprsRepairSubscribers,
      R.rprsNodesBuilt = statDelta R.rprsNodesBuilt,
      R.rprsNodesPatched = statDelta R.rprsNodesPatched,
      R.rprsAffectedKeys = statDelta R.rprsAffectedKeys,
      R.rprsSemanticAffectedKeys = statDelta R.rprsSemanticAffectedKeys,
      R.rprsRecomputedCells = statDelta R.rprsRecomputedCells,
      R.rprsWorkKeys = statDelta R.rprsWorkKeys,
      R.rprsJoinRuns = statDelta R.rprsJoinRuns,
      R.rprsJoinLeaves = statDelta R.rprsJoinLeaves,
      R.rprsEmittedCarrierDeltas = statDelta R.rprsEmittedCarrierDeltas,
      R.rprsEmittedCarrierRows = statDelta R.rprsEmittedCarrierRows,
      R.rprsProjectionRowsEmitted = statDelta R.rprsProjectionRowsEmitted,
      R.rprsMaterializedSnapshots = statDelta R.rprsMaterializedSnapshots,
      R.rprsInputDeltaRows = statDelta R.rprsInputDeltaRows,
      R.rprsPreparedInputRebuilds = statDelta R.rprsPreparedInputRebuilds,
      R.rprsPreparedInputPatchHits = statDelta R.rprsPreparedInputPatchHits,
      R.rprsPreparedRelationRows = statDelta R.rprsPreparedRelationRows,
      R.rprsStoreRebuilds = statDelta R.rprsStoreRebuilds,
      R.rprsSupportEvaluations = statDelta R.rprsSupportEvaluations,
      R.rprsSupportMemoHits = statDelta R.rprsSupportMemoHits,
      R.rprsRepairTelemetry =
        R.repairTelemetryDifference
          (R.rprsRepairTelemetry after)
          (R.rprsRepairTelemetry before),
      R.rprsNodeRepairs =
        Map.mapMaybeWithKey
          repairNodeStatsDelta
          (R.rprsNodeRepairs after)
    }
  where
    statDelta field =
      saturatingIntMinus (field after) (field before)

    repairNodeStatsDelta key afterNodeStats =
      nonZeroRepairNodeStats
        ( R.RuntimeRepairNodeStats
          { R.rrnsAction = R.rrnsAction afterNodeStats,
            R.rrnsAffectedKeys =
              saturatingIntMinus
                (R.rrnsAffectedKeys afterNodeStats)
                (maybe 0 R.rrnsAffectedKeys (Map.lookup key (R.rprsNodeRepairs before))),
            R.rrnsRecomputedCells =
              saturatingIntMinus
                (R.rrnsRecomputedCells afterNodeStats)
                (maybe 0 R.rrnsRecomputedCells (Map.lookup key (R.rprsNodeRepairs before))),
            R.rrnsWorkKeys =
              nodeStatDelta R.rrnsWorkKeys key afterNodeStats,
            R.rrnsJoinRuns =
              nodeStatDelta R.rrnsJoinRuns key afterNodeStats,
            R.rrnsJoinLeaves =
              nodeStatDelta R.rrnsJoinLeaves key afterNodeStats,
            R.rrnsRepairTelemetry =
              R.repairTelemetryDifference
                (R.rrnsRepairTelemetry afterNodeStats)
                (maybe R.emptyRepairTelemetry R.rrnsRepairTelemetry (Map.lookup key (R.rprsNodeRepairs before)))
          }
        )

    nodeStatDelta field key afterNodeStats =
      saturatingIntMinus
        (field afterNodeStats)
        (maybe 0 field (Map.lookup key (R.rprsNodeRepairs before)))
{-# INLINE repairStatsDelta #-}

staleRejectedDelta :: R.RuntimeReuseDiagnostics -> R.RuntimeReuseDiagnostics -> Int
staleRejectedDelta before after =
  R.rrsStaleRejected (R.rrdStats after) - R.rrsStaleRejected (R.rrdStats before)
{-# INLINE staleRejectedDelta #-}

registeredNewDelta :: R.RuntimeReuseDiagnostics -> R.RuntimeReuseDiagnostics -> Int
registeredNewDelta before after =
  R.rrsRegisteredNew (R.rrdStats after) - R.rrsRegisteredNew (R.rrdStats before)
{-# INLINE registeredNewDelta #-}

nonZeroRepairNodeStats :: R.RuntimeRepairNodeStats -> Maybe R.RuntimeRepairNodeStats
nonZeroRepairNodeStats stats
  | repairNodeLoad stats > 0 =
      Just stats
  | otherwise =
      Nothing
{-# INLINE nonZeroRepairNodeStats #-}

saturatingWord64Minus :: Word64 -> Word64 -> Word64
saturatingWord64Minus newer older
  | newer >= older =
      newer - older
  | otherwise =
      0
{-# INLINE saturatingWord64Minus #-}

saturatingIntMinus :: Int -> Int -> Int
saturatingIntMinus newer older
  | newer >= older =
      newer - older
  | otherwise =
      0
{-# INLINE saturatingIntMinus #-}

repairNodeLoad :: R.RuntimeRepairNodeStats -> Int
repairNodeLoad stats =
  R.rrnsAffectedKeys stats
    + R.rrnsRecomputedCells stats
    + R.rrnsJoinLeaves stats
    + R.repairTelemetryWeight (R.rrnsRepairTelemetry stats)
{-# INLINE repairNodeLoad #-}

showNsMs :: Word64 -> String
showNsMs ns =
  show (fromIntegral ns / (1000000.0 :: Double))
{-# INLINE showNsMs #-}
