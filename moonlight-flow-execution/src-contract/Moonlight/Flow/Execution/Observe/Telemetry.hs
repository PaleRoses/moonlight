module Moonlight.Flow.Execution.Observe.Telemetry
  ( ProvTelemetry (..),
    RepairTelemetryLevel (..),
    RepairTelemetryConfig (..),
    summaryRepairTelemetryConfig,
    detailedRepairTelemetryConfig,
    defaultRepairTelemetryConfig,
    RepairTelemetry (..),
    emptyRepairTelemetry,
    repairTelemetryDifference,
    repairTelemetryWeight,
    IncrementalUpdateTrace (..),
    emptyIncrementalUpdateTrace,
    NodeAction (..),
    NodeMaintenance (..),
    MaintenanceMetrics (..),
    recordInputDeltaRows,
    recordSupportEvaluation,
    recordSupportEvaluations,
    recordSupportMemoHit,
    recordSupportMemoHits,
    recordNodeMaintenance,
    maintenanceActionCount,
    maintenanceAffectedKeyCount,
    maintenanceRecomputedCellCount,
    FactorCacheTelemetry (..),
    DispatchBranch (..),
    DispatchTelemetry (..),
    mkDispatchTelemetry,
    snapshotProvTelemetry,
    snapshotFactorCacheTelemetry,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvenanceObstruction (..),
    ProvVal (..),
    ProvGen (..),
    ProvEntry (..),
    ProvArena,
    paNodes,
    paCons,
    paNext
  )
import Moonlight.Flow.Execution.Observe.Provenance.GC
  ( ProvGCStats (..),
    liveRatio,
  )
import Moonlight.Flow.Execution.Observe.RepairTelemetry

data ProvTelemetry = ProvTelemetry
  { ptArenaNodes :: {-# UNPACK #-} !Int,
    ptNurseryNodes :: {-# UNPACK #-} !Int,
    ptCachedNodes :: {-# UNPACK #-} !Int,
    ptStableNodes :: {-# UNPACK #-} !Int,
    ptHashConsEntries :: {-# UNPACK #-} !Int,
    ptPaNext :: {-# UNPACK #-} !Int,
    ptLiveRatio :: !(Maybe Double),
    ptProvenanceObstruction :: !(Maybe ProvenanceObstruction),
    ptLastGC :: !(Maybe ProvGCStats)
  }
  deriving stock (Eq, Show)

data NodeAction
  = NodeBuilt
  | NodeReused
  | NodePatched
  deriving stock (Eq, Ord, Show)

data NodeMaintenance = NodeMaintenance
  { nmAction :: !NodeAction,
    nmAffectedKeys :: {-# UNPACK #-} !Int,
    nmRecomputedCells :: {-# UNPACK #-} !Int,
    nmWorkKeys :: {-# UNPACK #-} !Int,
    nmJoinRuns :: {-# UNPACK #-} !Int,
    nmJoinLeaves :: {-# UNPACK #-} !Int,
    nmRepairTelemetry :: !RepairTelemetry
  }
  deriving stock (Eq, Show)

data MaintenanceMetrics = MaintenanceMetrics
  { mmNodes :: !(Map FactorNode NodeMaintenance),
    mmInputDeltaRows :: {-# UNPACK #-} !Int,
    mmSupportEvaluations :: {-# UNPACK #-} !Int,
    mmSupportMemoHits :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

recordInputDeltaRows :: Int -> MaintenanceMetrics -> MaintenanceMetrics
recordInputDeltaRows rowCount metrics =
  metrics
    { mmInputDeltaRows = mmInputDeltaRows metrics + rowCount
    }
{-# INLINE recordInputDeltaRows #-}

recordSupportEvaluation :: MaintenanceMetrics -> MaintenanceMetrics
recordSupportEvaluation =
  recordSupportEvaluations 1
{-# INLINE recordSupportEvaluation #-}

recordSupportEvaluations :: Int -> MaintenanceMetrics -> MaintenanceMetrics
recordSupportEvaluations count metrics =
  metrics
    { mmSupportEvaluations =
        mmSupportEvaluations metrics + max 0 count
    }
{-# INLINE recordSupportEvaluations #-}

recordSupportMemoHit :: MaintenanceMetrics -> MaintenanceMetrics
recordSupportMemoHit =
  recordSupportMemoHits 1
{-# INLINE recordSupportMemoHit #-}

recordSupportMemoHits :: Int -> MaintenanceMetrics -> MaintenanceMetrics
recordSupportMemoHits count metrics =
  metrics
    { mmSupportMemoHits =
        mmSupportMemoHits metrics + max 0 count
    }
{-# INLINE recordSupportMemoHits #-}

recordNodeMaintenance ::
  FactorNode ->
  NodeMaintenance ->
  MaintenanceMetrics ->
  MaintenanceMetrics
recordNodeMaintenance node maintenance metrics =
  metrics
    { mmNodes =
        Map.insertWith
          mergeNodeMaintenance
          node
          maintenance
          (mmNodes metrics)
    }
{-# INLINE recordNodeMaintenance #-}

mergeNodeMaintenance :: NodeMaintenance -> NodeMaintenance -> NodeMaintenance
mergeNodeMaintenance newer older =
  NodeMaintenance
    { nmAction =
        strongerNodeAction (nmAction newer) (nmAction older),
      nmAffectedKeys =
        nmAffectedKeys newer + nmAffectedKeys older,
      nmRecomputedCells =
        nmRecomputedCells newer + nmRecomputedCells older,
      nmWorkKeys =
        nmWorkKeys newer + nmWorkKeys older,
      nmJoinRuns =
        nmJoinRuns newer + nmJoinRuns older,
      nmJoinLeaves =
        nmJoinLeaves newer + nmJoinLeaves older,
      nmRepairTelemetry =
        nmRepairTelemetry newer <> nmRepairTelemetry older
    }
{-# INLINE mergeNodeMaintenance #-}

strongerNodeAction :: NodeAction -> NodeAction -> NodeAction
strongerNodeAction left right =
  if nodeActionRank left >= nodeActionRank right
    then left
    else right
{-# INLINE strongerNodeAction #-}

nodeActionRank :: NodeAction -> Int
nodeActionRank action =
  case action of
    NodeReused ->
      0
    NodeBuilt ->
      1
    NodePatched ->
      2
{-# INLINE nodeActionRank #-}

instance Semigroup MaintenanceMetrics where
  left <> right =
    MaintenanceMetrics
      { mmNodes = Map.unionWith mergeNodeMaintenance (mmNodes left) (mmNodes right),
        mmInputDeltaRows = mmInputDeltaRows left + mmInputDeltaRows right,
        mmSupportEvaluations = mmSupportEvaluations left + mmSupportEvaluations right,
        mmSupportMemoHits = mmSupportMemoHits left + mmSupportMemoHits right
      }

instance Monoid MaintenanceMetrics where
  mempty =
    MaintenanceMetrics
      { mmNodes = Map.empty,
        mmInputDeltaRows = 0,
        mmSupportEvaluations = 0,
        mmSupportMemoHits = 0
      }

maintenanceActionCount :: NodeAction -> MaintenanceMetrics -> Int
maintenanceActionCount action =
  Map.size . Map.filter ((== action) . nmAction) . mmNodes
{-# INLINE maintenanceActionCount #-}

maintenanceAffectedKeyCount :: MaintenanceMetrics -> Int
maintenanceAffectedKeyCount =
  sum . fmap nmAffectedKeys . Map.elems . mmNodes
{-# INLINE maintenanceAffectedKeyCount #-}

maintenanceRecomputedCellCount :: MaintenanceMetrics -> Int
maintenanceRecomputedCellCount =
  sum . fmap nmRecomputedCells . Map.elems . mmNodes
{-# INLINE maintenanceRecomputedCellCount #-}

data FactorCacheTelemetry = FactorCacheTelemetry
  { fctLocalFactors :: {-# UNPACK #-} !Int,
    fctMessages :: {-# UNPACK #-} !Int,
    fctBagBeliefs :: {-# UNPACK #-} !Int,
    fctSupportMemoNodes :: {-# UNPACK #-} !Int,
    fctSupportMemoRows :: !(Maybe Int),
    fctMaintenance :: !MaintenanceMetrics,
    fctProv :: !ProvTelemetry
  }
  deriving stock (Eq, Show)

snapshotFactorCacheTelemetry ::
  Maybe ProvGCStats ->
  MaintenanceMetrics ->
  Map FactorNode entry ->
  Int ->
  Maybe Int ->
  Maybe [ProvVal] ->
  ProvArena ->
  FactorCacheTelemetry
snapshotFactorCacheTelemetry lastGc metrics factorNodes supportMemoNodes supportMemoRows roots arena =
  FactorCacheTelemetry
    { fctLocalFactors =
        countFactorNodes factorNodeIsBag factorNodes,
      fctMessages =
        countFactorNodes factorNodeIsSeparator factorNodes,
      fctBagBeliefs =
        countFactorNodes factorNodeIsBagBelief factorNodes,
      fctSupportMemoNodes =
        supportMemoNodes,
      fctSupportMemoRows =
        supportMemoRows,
      fctMaintenance =
        metrics,
      fctProv =
        snapshotProvTelemetry lastGc roots arena
    }
{-# INLINE snapshotFactorCacheTelemetry #-}

countFactorNodes ::
  (FactorNode -> Bool) ->
  Map FactorNode entry ->
  Int
countFactorNodes predicate =
  Map.size . Map.filterWithKey (\node _entry -> predicate node)
{-# INLINE countFactorNodes #-}

data DispatchBranch
  = DispatchAcyclic
  | DispatchAdaptive
  | DispatchFactorized
  deriving stock (Eq, Show)

data DispatchTelemetry = DispatchTelemetry
  { dtBranch :: !DispatchBranch,
    dtVisibleRowsBefore :: !(Maybe Int),
    dtVisibleRowsAfter :: !(Maybe Int),
    dtRowsPruned :: !(Maybe Int),
    dtRowsEmitted :: {-# UNPACK #-} !Int,
    dtSupportAtoms :: !(Maybe Int),
    dtSupportRows :: !(Maybe Int),
    dtWallTimeNs :: !(Maybe Word64),
    dtProvenanceObstruction :: !(Maybe ProvenanceObstruction),
    dtFactorCache :: !(Maybe FactorCacheTelemetry)
  }
  deriving stock (Eq, Show)

mkDispatchTelemetry ::
  DispatchBranch ->
  Maybe Int ->
  Maybe Int ->
  Int ->
  Maybe Int ->
  Maybe Int ->
  Maybe ProvenanceObstruction ->
  Maybe FactorCacheTelemetry ->
  DispatchTelemetry
mkDispatchTelemetry branch visibleBefore visibleAfter rowsEmitted supportAtoms supportRows obstruction factorCache =
  DispatchTelemetry
    { dtBranch = branch,
      dtVisibleRowsBefore = visibleBefore,
      dtVisibleRowsAfter = visibleAfter,
      dtRowsPruned = (-) <$> visibleBefore <*> visibleAfter,
      dtRowsEmitted = rowsEmitted,
      dtSupportAtoms = supportAtoms,
      dtSupportRows = supportRows,
      dtWallTimeNs = Nothing,
      dtProvenanceObstruction = obstruction,
      dtFactorCache = factorCache
    }
{-# INLINE mkDispatchTelemetry #-}

snapshotProvTelemetry :: Maybe ProvGCStats -> Maybe [ProvVal] -> ProvArena -> ProvTelemetry
snapshotProvTelemetry lastGc maybeRoots arena =
  let (nurseryNodes, cachedNodes, stableNodes) =
        IntMap.foldl' countGeneration (0, 0, 0) (paNodes arena)
      (maybeLiveRatio, maybeObstruction) =
        case maybeRoots of
          Nothing ->
            (Nothing, Nothing)
          Just roots ->
            case liveRatio roots arena of
              Left obstruction ->
                (Nothing, Just obstruction)
              Right ratio ->
                (Just ratio, Nothing)
   in ProvTelemetry
        { ptArenaNodes = IntMap.size (paNodes arena),
          ptNurseryNodes = nurseryNodes,
          ptCachedNodes = cachedNodes,
          ptStableNodes = stableNodes,
          ptHashConsEntries = Map.size (paCons arena),
          ptPaNext = paNext arena,
          ptLiveRatio = maybeLiveRatio,
          ptProvenanceObstruction = maybeObstruction,
          ptLastGC = lastGc
        }
  where
    countGeneration ::
      (Int, Int, Int) ->
      ProvEntry ->
      (Int, Int, Int)
    countGeneration counts entry =
      let (nurseryNodes, cachedNodes, stableNodes) = counts
       in case peGen entry of
            GenNursery -> (nurseryNodes + 1, cachedNodes, stableNodes)
            GenCached -> (nurseryNodes, cachedNodes + 1, stableNodes)
            GenStable -> (nurseryNodes, cachedNodes, stableNodes + 1)
