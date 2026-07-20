{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}

module Moonlight.Flow.Runtime.Core.RepairStats
  ( RuntimeRepairStats (..),
    RuntimeRepairInputStats (..),
    RuntimeRepairNodeAction (..),
    RuntimeRepairNodeStats (..),
    RepairTelemetry (..),
    emptyRepairTelemetry,
    repairTelemetryDifference,
    repairTelemetryWeight,
    emptyRuntimeRepairStats,
    emptyRuntimeRepairInputStats,
    appendRuntimeRepairStats,
    runtimeRepairStatsFromMaintenance,
    carrierDeltaRowCount,
  )
where

import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( MaintenanceMetrics (..),
    NodeAction (..),
    NodeMaintenance (..),
    RepairTelemetry (..),
    emptyRepairTelemetry,
    maintenanceActionCount,
    maintenanceAffectedKeyCount,
    maintenanceRecomputedCellCount,
    repairTelemetryDifference,
    repairTelemetryWeight,
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchRows,
  )
import Moonlight.Flow.Plan.Query.Core
  ( FactorNode,
  )

data RuntimeRepairNodeAction
  = RuntimeRepairNodeBuilt
  | RuntimeRepairNodeReused
  | RuntimeRepairNodePatched
  deriving stock (Eq, Ord, Show, Read)

data RuntimeRepairNodeStats = RuntimeRepairNodeStats
  { rrnsAction :: !RuntimeRepairNodeAction,
    rrnsAffectedKeys :: {-# UNPACK #-} !Int,
    rrnsRecomputedCells :: {-# UNPACK #-} !Int,
    rrnsWorkKeys :: {-# UNPACK #-} !Int,
    rrnsJoinRuns :: {-# UNPACK #-} !Int,
    rrnsJoinLeaves :: {-# UNPACK #-} !Int,
    rrnsRepairTelemetry :: !RepairTelemetry
  }
  deriving stock (Eq, Ord, Show, Read)

data RuntimeRepairInputStats = RuntimeRepairInputStats
  { rrisPreparedInputRebuilds :: {-# UNPACK #-} !Int,
    rrisPreparedInputPatchHits :: {-# UNPACK #-} !Int,
    rrisPreparedRelationRows :: {-# UNPACK #-} !Int,
    rrisStoreRebuilds :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show, Read)

data RuntimeRepairStats = RuntimeRepairStats
  { rprsFactorRepairs :: {-# UNPACK #-} !Int,
    rprsCanonicalRepairs :: {-# UNPACK #-} !Int,
    rprsRepairSubscribers :: {-# UNPACK #-} !Int,
    rprsNodesBuilt :: {-# UNPACK #-} !Int,
    rprsNodesPatched :: {-# UNPACK #-} !Int,
    rprsAffectedKeys :: {-# UNPACK #-} !Int,
    rprsSemanticAffectedKeys :: {-# UNPACK #-} !Int,
    rprsRecomputedCells :: {-# UNPACK #-} !Int,
    rprsWorkKeys :: {-# UNPACK #-} !Int,
    rprsJoinRuns :: {-# UNPACK #-} !Int,
    rprsJoinLeaves :: {-# UNPACK #-} !Int,
    rprsEmittedCarrierDeltas :: {-# UNPACK #-} !Int,
    rprsEmittedCarrierRows :: {-# UNPACK #-} !Int,
    rprsProjectionRowsEmitted :: {-# UNPACK #-} !Int,
    rprsMaterializedSnapshots :: {-# UNPACK #-} !Int,
    rprsInputDeltaRows :: {-# UNPACK #-} !Int,
    rprsPreparedInputRebuilds :: {-# UNPACK #-} !Int,
    rprsPreparedInputPatchHits :: {-# UNPACK #-} !Int,
    rprsPreparedRelationRows :: {-# UNPACK #-} !Int,
    rprsStoreRebuilds :: {-# UNPACK #-} !Int,
    rprsSupportEvaluations :: {-# UNPACK #-} !Int,
    rprsSupportMemoHits :: {-# UNPACK #-} !Int,
    rprsRepairTelemetry :: !RepairTelemetry,
    rprsNodeRepairs :: !(Map (QueryId, FactorNode) RuntimeRepairNodeStats)
  }
  deriving stock (Eq, Ord, Show, Read)

emptyRuntimeRepairInputStats :: RuntimeRepairInputStats
emptyRuntimeRepairInputStats =
  RuntimeRepairInputStats
    { rrisPreparedInputRebuilds = 0,
      rrisPreparedInputPatchHits = 0,
      rrisPreparedRelationRows = 0,
      rrisStoreRebuilds = 0
    }
{-# INLINE emptyRuntimeRepairInputStats #-}

emptyRuntimeRepairStats :: RuntimeRepairStats
emptyRuntimeRepairStats =
  RuntimeRepairStats
    { rprsFactorRepairs = 0,
      rprsCanonicalRepairs = 0,
      rprsRepairSubscribers = 0,
      rprsNodesBuilt = 0,
      rprsNodesPatched = 0,
      rprsAffectedKeys = 0,
      rprsSemanticAffectedKeys = 0,
      rprsRecomputedCells = 0,
      rprsWorkKeys = 0,
      rprsJoinRuns = 0,
      rprsJoinLeaves = 0,
      rprsEmittedCarrierDeltas = 0,
      rprsEmittedCarrierRows = 0,
      rprsProjectionRowsEmitted = 0,
      rprsMaterializedSnapshots = 0,
      rprsInputDeltaRows = 0,
      rprsPreparedInputRebuilds = 0,
      rprsPreparedInputPatchHits = 0,
      rprsPreparedRelationRows = 0,
      rprsStoreRebuilds = 0,
      rprsSupportEvaluations = 0,
      rprsSupportMemoHits = 0,
      rprsRepairTelemetry = emptyRepairTelemetry,
      rprsNodeRepairs = Map.empty
    }
{-# INLINE emptyRuntimeRepairStats #-}

appendRuntimeRepairStats :: RuntimeRepairStats -> RuntimeRepairStats -> RuntimeRepairStats
appendRuntimeRepairStats newer older =
  RuntimeRepairStats
    { rprsFactorRepairs = rprsFactorRepairs newer + rprsFactorRepairs older,
      rprsCanonicalRepairs = rprsCanonicalRepairs newer + rprsCanonicalRepairs older,
      rprsRepairSubscribers = rprsRepairSubscribers newer + rprsRepairSubscribers older,
      rprsNodesBuilt = rprsNodesBuilt newer + rprsNodesBuilt older,
      rprsNodesPatched = rprsNodesPatched newer + rprsNodesPatched older,
      rprsAffectedKeys = rprsAffectedKeys newer + rprsAffectedKeys older,
      rprsSemanticAffectedKeys = rprsSemanticAffectedKeys newer + rprsSemanticAffectedKeys older,
      rprsRecomputedCells = rprsRecomputedCells newer + rprsRecomputedCells older,
      rprsWorkKeys = rprsWorkKeys newer + rprsWorkKeys older,
      rprsJoinRuns = rprsJoinRuns newer + rprsJoinRuns older,
      rprsJoinLeaves = rprsJoinLeaves newer + rprsJoinLeaves older,
      rprsEmittedCarrierDeltas = rprsEmittedCarrierDeltas newer + rprsEmittedCarrierDeltas older,
      rprsEmittedCarrierRows = rprsEmittedCarrierRows newer + rprsEmittedCarrierRows older,
      rprsProjectionRowsEmitted = rprsProjectionRowsEmitted newer + rprsProjectionRowsEmitted older,
      rprsMaterializedSnapshots = rprsMaterializedSnapshots newer + rprsMaterializedSnapshots older,
      rprsInputDeltaRows = rprsInputDeltaRows newer + rprsInputDeltaRows older,
      rprsPreparedInputRebuilds = rprsPreparedInputRebuilds newer + rprsPreparedInputRebuilds older,
      rprsPreparedInputPatchHits = rprsPreparedInputPatchHits newer + rprsPreparedInputPatchHits older,
      rprsPreparedRelationRows = rprsPreparedRelationRows newer + rprsPreparedRelationRows older,
      rprsStoreRebuilds = rprsStoreRebuilds newer + rprsStoreRebuilds older,
      rprsSupportEvaluations = rprsSupportEvaluations newer + rprsSupportEvaluations older,
      rprsSupportMemoHits = rprsSupportMemoHits newer + rprsSupportMemoHits older,
      rprsRepairTelemetry = rprsRepairTelemetry newer <> rprsRepairTelemetry older,
      rprsNodeRepairs =
        Map.unionWith
          appendRuntimeRepairNodeStats
          (rprsNodeRepairs newer)
          (rprsNodeRepairs older)
    }
{-# INLINE appendRuntimeRepairStats #-}

runtimeRepairStatsFromMaintenance ::
  QueryId ->
  Int ->
  RuntimeRepairInputStats ->
  [RelationalCarrierDelta ctx Carrier prop boundary evidence] ->
  Int ->
  [RelationalCarrierDelta ctx Carrier prop boundary evidence] ->
  MaintenanceMetrics ->
  RuntimeRepairStats
runtimeRepairStatsFromMaintenance queryId repairSubscribers inputStats emittedDeltas projectionRows materializedSnapshots metrics =
  let !affectedKeys =
        maintenanceAffectedKeyCount metrics
      !emittedRows =
        carrierDeltaRowCount emittedDeltas
   in RuntimeRepairStats
        { rprsFactorRepairs = 1,
          rprsCanonicalRepairs = 1,
          rprsRepairSubscribers = repairSubscribers,
          rprsNodesBuilt = maintenanceActionCount NodeBuilt metrics,
          rprsNodesPatched = maintenanceActionCount NodePatched metrics,
          rprsAffectedKeys = affectedKeys,
          rprsSemanticAffectedKeys = affectedKeys,
          rprsRecomputedCells = maintenanceRecomputedCellCount metrics,
          rprsWorkKeys = maintenanceWorkKeyCount metrics,
          rprsJoinRuns = maintenanceJoinRunCount metrics,
          rprsJoinLeaves = maintenanceJoinLeafCount metrics,
          rprsEmittedCarrierDeltas = length emittedDeltas,
          rprsEmittedCarrierRows = emittedRows,
          rprsProjectionRowsEmitted = projectionRows,
          rprsMaterializedSnapshots = length materializedSnapshots,
          rprsInputDeltaRows = mmInputDeltaRows metrics,
          rprsPreparedInputRebuilds = rrisPreparedInputRebuilds inputStats,
          rprsPreparedInputPatchHits = rrisPreparedInputPatchHits inputStats,
          rprsPreparedRelationRows = rrisPreparedRelationRows inputStats,
          rprsStoreRebuilds = rrisStoreRebuilds inputStats,
          rprsSupportEvaluations = mmSupportEvaluations metrics,
          rprsSupportMemoHits = mmSupportMemoHits metrics,
          rprsRepairTelemetry = maintenanceRepairTelemetry metrics,
          rprsNodeRepairs =
            Map.mapKeys
              (queryId,)
              (fmap runtimeRepairNodeStatsFromMaintenance (mmNodes metrics))
        }
{-# INLINE runtimeRepairStatsFromMaintenance #-}

runtimeRepairNodeStatsFromMaintenance :: NodeMaintenance -> RuntimeRepairNodeStats
runtimeRepairNodeStatsFromMaintenance maintenance =
  RuntimeRepairNodeStats
    { rrnsAction = runtimeRepairNodeActionFromMaintenance (nmAction maintenance),
      rrnsAffectedKeys = nmAffectedKeys maintenance,
      rrnsRecomputedCells = nmRecomputedCells maintenance,
      rrnsWorkKeys = nmWorkKeys maintenance,
      rrnsJoinRuns = nmJoinRuns maintenance,
      rrnsJoinLeaves = nmJoinLeaves maintenance,
      rrnsRepairTelemetry = nmRepairTelemetry maintenance
    }
{-# INLINE runtimeRepairNodeStatsFromMaintenance #-}

runtimeRepairNodeActionFromMaintenance :: NodeAction -> RuntimeRepairNodeAction
runtimeRepairNodeActionFromMaintenance action =
  case action of
    NodeBuilt ->
      RuntimeRepairNodeBuilt
    NodeReused ->
      RuntimeRepairNodeReused
    NodePatched ->
      RuntimeRepairNodePatched
{-# INLINE runtimeRepairNodeActionFromMaintenance #-}

appendRuntimeRepairNodeStats ::
  RuntimeRepairNodeStats ->
  RuntimeRepairNodeStats ->
  RuntimeRepairNodeStats
appendRuntimeRepairNodeStats newer older =
  RuntimeRepairNodeStats
    { rrnsAction = max (rrnsAction newer) (rrnsAction older),
      rrnsAffectedKeys = rrnsAffectedKeys newer + rrnsAffectedKeys older,
      rrnsRecomputedCells = rrnsRecomputedCells newer + rrnsRecomputedCells older,
      rrnsWorkKeys = rrnsWorkKeys newer + rrnsWorkKeys older,
      rrnsJoinRuns = rrnsJoinRuns newer + rrnsJoinRuns older,
      rrnsJoinLeaves = rrnsJoinLeaves newer + rrnsJoinLeaves older,
      rrnsRepairTelemetry = rrnsRepairTelemetry newer <> rrnsRepairTelemetry older
    }
{-# INLINE appendRuntimeRepairNodeStats #-}

maintenanceWorkKeyCount :: MaintenanceMetrics -> Int
maintenanceWorkKeyCount =
  sum . fmap nmWorkKeys . Map.elems . mmNodes
{-# INLINE maintenanceWorkKeyCount #-}

maintenanceJoinRunCount :: MaintenanceMetrics -> Int
maintenanceJoinRunCount =
  sum . fmap nmJoinRuns . Map.elems . mmNodes
{-# INLINE maintenanceJoinRunCount #-}

maintenanceJoinLeafCount :: MaintenanceMetrics -> Int
maintenanceJoinLeafCount =
  sum . fmap nmJoinLeaves . Map.elems . mmNodes
{-# INLINE maintenanceJoinLeafCount #-}

maintenanceRepairTelemetry :: MaintenanceMetrics -> RepairTelemetry
maintenanceRepairTelemetry =
  foldMap nmRepairTelemetry . Map.elems . mmNodes
{-# INLINE maintenanceRepairTelemetry #-}

carrierDeltaRowCount ::
  [RelationalCarrierDelta ctx carrier prop boundary evidence] ->
  Int
carrierDeltaRowCount =
  sum . fmap (Set.size . plainRowPatchRows . deRows)
{-# INLINE carrierDeltaRowCount #-}
