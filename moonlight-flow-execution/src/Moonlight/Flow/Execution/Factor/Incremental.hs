{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Execution.Factor.Incremental
  ( IncrementalUpdateTrace (..),
    emptyIncrementalUpdateTrace,
    buildFactorFromSourceBundles,
    updateFactorIncremental,
  )
where

import Control.Monad.ST
  ( ST,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Delta.Patch qualified as CorePatch
import Moonlight.Flow.Execution.Dense.Plan
  ( DenseArrangement,
    DenseArrangementId (..),
    DenseJoinPlanError,
    SourceBundle (..),
    denseArrangementDirtyRows,
    denseArrangementId,
    denseArrangementRestrictToDirtyRows,
    sourceBundleArrangement,
  )
import Moonlight.Flow.Execution.Dense.WCOJ
  ( DenseLeafWitness (..),
    foldProjectDenseWCOJDeltaWitnessesWithTelemetry,
    foldProjectDenseWCOJWitnesses,
    foldProjectDenseWCOJWitnessesWithTelemetry,
  )
import Moonlight.Flow.Execution.Factor.Core
  ( Factor,
    mkFactor,
  )
import Moonlight.Flow.Execution.Factor.Contribution
  ( FactorContribution (..),
    FactorContributionChange (..),
    FactorContributionIndex,
    FactorSourceCell (..),
    FactorSupportPatchStats (..),
    advanceFactorContributionIndex,
    coalesceFactorContributionRenderedValues,
    emptyFactorContributionIndex,
    factorContributionIndexSupportKeysForSourceCells,
    factorContributionIndexValueAt,
    factorContributionSupportPatchStats,
    insertFactorContribution,
  )
import Moonlight.Flow.Execution.Factor.Delta
  ( FactorDelta,
    factorDeltaFromCellPatches,
    patchFactorCellValue,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvVal (..),
    ProvArena
  )
import Moonlight.Flow.Execution.Observe.RepairTelemetry
  ( IncrementalUpdateTrace (..),
    RepairTelemetry (..),
    RepairTelemetryConfig,
    emptyIncrementalUpdateTrace,
    emptyRepairTelemetry,
    recordRepairRowGauges,
    recordRepairSupportRowRefs,
    recordSelectedRepairCell,
    recordSelectedRepairKeys,
    repairTelemetryDetailed,
    summaryRepairTelemetryConfig,
  )
import Moonlight.Differential.Row.Patch
  ( emptyShapedPatch
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Differential.Index.RowSet
  ( rowSetNull,
  )

data FactorBuildState = FactorBuildState
  { fbsRows :: !(Map.Map AssignmentTupleKey ProvVal),
    fbsContributions :: !FactorContributionIndex,
    fbsJoinLeaves :: {-# UNPACK #-} !Int
  }

emptyFactorBuildState :: FactorBuildState
emptyFactorBuildState =
  FactorBuildState
    { fbsRows = Map.empty,
      fbsContributions = emptyFactorContributionIndex,
      fbsJoinLeaves = 0
    }
{-# INLINE emptyFactorBuildState #-}

data FactorRepairState = FactorRepairState
  { frsFreshContributions :: !(Map.Map AssignmentTupleKey [FactorContribution]),
    frsJoinLeaves :: {-# UNPACK #-} !Int,
    frsRepairTelemetry :: !RepairTelemetry,
    frsRepairTelemetryConfig :: !RepairTelemetryConfig
  }

emptyFactorRepairState :: RepairTelemetryConfig -> FactorRepairState
emptyFactorRepairState config =
  FactorRepairState
    { frsFreshContributions = Map.empty,
      frsJoinLeaves = 0,
      frsRepairTelemetry = emptyRepairTelemetry,
      frsRepairTelemetryConfig = config
    }
{-# INLINE emptyFactorRepairState #-}

buildFactorFromSourceBundles ::
  [SlotId] ->
  [SourceBundle] ->
  ProvArena ->
  Either DenseJoinPlanError (ProvArena, Factor, FactorContributionIndex)
buildFactorFromSourceBundles outputSchema bundles arena0 =
  let !sources =
        fmap sourceBundleArrangement bundles
   in fmap
        ( \(!arena1, !buildState) ->
            let !factor =
                  mkFactor outputSchema (fbsRows buildState)
             in ( arena1,
                  factor,
                  coalesceFactorContributionRenderedValues factor (fbsContributions buildState)
                )
        )
        ( foldProjectDenseWCOJWitnesses
            outputSchema
            sources
            arena0
            emptyFactorBuildState
            insertBuiltWitness
        )
{-# INLINE buildFactorFromSourceBundles #-}

insertBuiltWitness ::
  AssignmentTupleKey ->
  DenseLeafWitness ->
  ProvArena ->
  FactorBuildState ->
  ST s (ProvArena, FactorBuildState)
insertBuiltWitness key witness arena state = do
  let !contribution =
        witnessContribution witness
      (!arena1, !contributions1, !contributionChange) =
        insertFactorContribution
          summaryRepairTelemetryConfig
          key
          contribution
          arena
          (fbsContributions state)
      !rows1 =
        if Set.member key (fccTouchedKeys contributionChange)
          then Map.insert key (factorContributionIndexValueAt key contributions1) (fbsRows state)
          else fbsRows state
  pure
    ( arena1,
      state
        { fbsRows = rows1,
          fbsContributions = contributions1,
          fbsJoinLeaves = fbsJoinLeaves state + 1
        }
      )
{-# INLINE insertBuiltWitness #-}

insertRepairWitness ::
  AssignmentTupleKey ->
  DenseLeafWitness ->
  ProvArena ->
  FactorRepairState ->
  ST s (ProvArena, FactorRepairState)
insertRepairWitness key witness arena state = do
  let !config =
        frsRepairTelemetryConfig state
      !contribution =
        witnessContribution witness
      !supportRowSet =
        dlwSupportCells witness
      !supportRowsEnumerated =
        dlwSupportRowsEnumerated witness
      !supportRowsUnique =
        Set.size supportRowSet
      !freshContributions1 =
        Map.insertWith (<>) key [contribution] (frsFreshContributions state)
      !telemetry1 =
        recordRepairSupportRowRefs config supportRowsEnumerated supportRowsUnique $
          recordSelectedRepairCell config $
            dlwTelemetry witness <> frsRepairTelemetry state
  pure
    ( arena,
      state
        { frsFreshContributions = freshContributions1,
          frsJoinLeaves = frsJoinLeaves state + 1,
          frsRepairTelemetry = telemetry1
        }
    )
{-# INLINE insertRepairWitness #-}

data DeltaTraversalMode
  = DeltaNoDirtySources
  | DeltaSingleDirtySource !Int
  | DeltaMultipleDirtySources
  deriving stock (Eq, Show)

data DirtySourceScan = DirtySourceScan
  { dssNextSourceIx :: {-# UNPACK #-} !Int,
    dssTraversalMode :: !DeltaTraversalMode
  }

emptyDirtySourceScan :: DirtySourceScan
emptyDirtySourceScan =
  DirtySourceScan
    { dssNextSourceIx = 0,
      dssTraversalMode = DeltaNoDirtySources
    }
{-# INLINE emptyDirtySourceScan #-}

deltaTraversalMode :: [DenseArrangement] -> DeltaTraversalMode
deltaTraversalMode sources =
  dssTraversalMode (foldl' scanDirtySource emptyDirtySourceScan sources)
{-# INLINE deltaTraversalMode #-}

scanDirtySource :: DirtySourceScan -> DenseArrangement -> DirtySourceScan
scanDirtySource scan src =
  let !sourceIx =
        dssNextSourceIx scan
      !nextSourceIx =
        sourceIx + 1
      !mode
        | rowSetNull (denseArrangementDirtyRows src) =
            dssTraversalMode scan
        | otherwise =
            case dssTraversalMode scan of
              DeltaNoDirtySources ->
                DeltaSingleDirtySource sourceIx
              DeltaSingleDirtySource _ ->
                DeltaMultipleDirtySources
              DeltaMultipleDirtySources ->
                DeltaMultipleDirtySources
   in DirtySourceScan
        { dssNextSourceIx = nextSourceIx,
          dssTraversalMode = mode
        }
{-# INLINE scanDirtySource #-}

witnessContribution :: DenseLeafWitness -> FactorContribution
witnessContribution witness =
  FactorContribution
    { fctValue = dlwValue witness,
      fctSupportCells = dlwSupportCells witness
    }
{-# INLINE witnessContribution #-}

data FactorPatchState = FactorPatchState
  { fpsFactor :: !Factor,
    fpsChanges :: !(Map.Map AssignmentTupleKey (CorePatch.CellPatch ProvVal)),
    fpsTouchedKeys :: !(Set AssignmentTupleKey),
    fpsSupportOutputKeys :: {-# UNPACK #-} !Int,
    fpsRecomputedCells :: {-# UNPACK #-} !Int,
    fpsRepairTelemetry :: !RepairTelemetry
  }

emptyFactorPatchState ::
  Factor ->
  FactorPatchState
emptyFactorPatchState factor =
  FactorPatchState
    { fpsFactor = factor,
      fpsChanges = Map.empty,
      fpsTouchedKeys = Set.empty,
      fpsSupportOutputKeys = 0,
      fpsRecomputedCells = 0,
      fpsRepairTelemetry = emptyRepairTelemetry
    }
{-# INLINE emptyFactorPatchState #-}

updateFactorIncremental ::
  RepairTelemetryConfig ->
  [SlotId] ->
  [SourceBundle] ->
  Factor ->
  FactorContributionIndex ->
  ProvArena ->
  Either DenseJoinPlanError (ProvArena, Factor, FactorContributionIndex, FactorDelta, IncrementalUpdateTrace)
updateFactorIncremental telemetryConfig outputSchema bundles oldFactor oldContributions arena0 = do
  (!arena1, !currentDelta) <-
    currentDeltaContributions
      telemetryConfig
      outputSchema
      currentSources
      arena0
  let !freshContributions =
        frsFreshContributions currentDelta
      !currentAffectedKeys =
        Map.keysSet freshContributions
      !workKeys =
        Set.union currentAffectedKeys oldAffectedKeys
  pure $
    if Set.null workKeys
        then
          ( arena1,
            oldFactor,
            oldContributions,
            emptyShapedPatch outputSchema,
            emptyIncrementalUpdateTrace
              { iutJoinRuns = currentAffectedJoinRuns,
                iutJoinLeaves = frsJoinLeaves currentDelta,
                iutRepairTelemetry = frsRepairTelemetry currentDelta
              }
          )
        else
          let (!arena2, !contributions1, !contributionChange) =
                advanceFactorContributionIndex
                  telemetryConfig
                  dirtyCells
                  freshContributions
                  arena1
                  oldContributions
              !patchState =
                patchFactorFromChange
                  telemetryConfig
                  contributions1
                  contributionChange
                  (emptyFactorPatchState oldFactor)
              !touchedCount =
                Set.size (fpsTouchedKeys patchState)
              !delta =
                factorDeltaFromCellPatches
                  outputSchema
                  (fpsChanges patchState)
              !repairTelemetry =
                frsRepairTelemetry currentDelta <> fpsRepairTelemetry patchState
              !traceValue =
                IncrementalUpdateTrace
                  { iutAffectedKeys = touchedCount,
                    iutRecomputedCells = fpsRecomputedCells patchState,
                    iutWorkKeys = Set.size workKeys,
                    iutJoinRuns = currentAffectedJoinRuns,
                    iutJoinLeaves = frsJoinLeaves currentDelta,
                    iutRepairTelemetry =
                      recordSelectedRepairKeys telemetryConfig (Set.size workKeys) $
                        recordRepairRowGauges
                          telemetryConfig
                          (Map.size freshContributions)
                          (supportOutputCount patchState)
                          repairTelemetry
                  }
           in
            let !factor =
                  fpsFactor patchState
             in ( arena2,
                  factor,
                  coalesceFactorContributionRenderedValues factor contributions1,
                  delta,
                  traceValue
                )
  where
    !currentSources =
      fmap sourceBundleArrangement bundles
    !dirtyCells =
      dirtySourceCells bundles
    !oldAffectedKeys =
      factorContributionIndexSupportKeysForSourceCells dirtyCells oldContributions
    !currentAffectedJoinRuns =
      currentDeltaJoinRuns currentSources
{-# INLINE updateFactorIncremental #-}

currentDeltaJoinRuns ::
  [DenseArrangement] ->
  Int
currentDeltaJoinRuns sources =
  case deltaTraversalMode sources of
    DeltaNoDirtySources ->
      0
    DeltaSingleDirtySource _ ->
      1
    DeltaMultipleDirtySources ->
      1
{-# INLINE currentDeltaJoinRuns #-}

currentDeltaContributions ::
  RepairTelemetryConfig ->
  [SlotId] ->
  [DenseArrangement] ->
  ProvArena ->
  Either DenseJoinPlanError (ProvArena, FactorRepairState)
currentDeltaContributions telemetryConfig outputSchema sources arena0 =
  case deltaTraversalMode sources of
    DeltaNoDirtySources ->
      Right (arena0, emptyFactorRepairState telemetryConfig)
    DeltaSingleDirtySource sourceIx ->
      foldProjectDenseWCOJWitnessesWithTelemetry
        telemetryConfig
        outputSchema
        (restrictOnlySourceToDirty sourceIx sources)
        arena0
        (emptyFactorRepairState telemetryConfig)
        insertRepairWitness
    DeltaMultipleDirtySources ->
      foldProjectDenseWCOJDeltaWitnessesWithTelemetry
        telemetryConfig
        outputSchema
        sources
        arena0
        (emptyFactorRepairState telemetryConfig)
        insertRepairWitness
{-# INLINE currentDeltaContributions #-}

restrictOnlySourceToDirty ::
  Int ->
  [DenseArrangement] ->
  [DenseArrangement]
restrictOnlySourceToDirty sourceIx sources =
  fmap restrictSource (zip [0 :: Int ..] sources)
  where
    restrictSource (!ix, !src)
      | ix == sourceIx =
          denseArrangementRestrictToDirtyRows src
      | otherwise =
          src
{-# INLINE restrictOnlySourceToDirty #-}

dirtySourceCells :: [SourceBundle] -> Set FactorSourceCell
dirtySourceCells bundles =
  Set.unions
    [ Set.map
        ( \key ->
            FactorSourceCell
              { fscSourceId = sourceBundleSourceId bundle,
                fscKey = key
              }
        )
        (sbDirtyKeys bundle)
      | bundle <- bundles,
        not (Set.null (sbDirtyKeys bundle))
    ]
{-# INLINE dirtySourceCells #-}

sourceBundleSourceId :: SourceBundle -> Int
sourceBundleSourceId =
  unDenseArrangementId . denseArrangementId . sbCurrent
{-# INLINE sourceBundleSourceId #-}

patchFactorFromChange ::
  RepairTelemetryConfig ->
  FactorContributionIndex ->
  FactorContributionChange ->
  FactorPatchState ->
  FactorPatchState
patchFactorFromChange telemetryConfig contributionIndex contributionChange state0 =
  let !supportStats =
        factorContributionSupportPatchStats contributionIndex contributionChange
      !state1 =
        state0
          { fpsSupportOutputKeys =
              supportEdgeOutputCount contributionChange
          }
      !state2 =
        patchContributionKeys contributionIndex (fccTouchedKeys contributionChange) state1
      !repairTelemetry =
        repairTelemetryFromFactorPatches
          telemetryConfig
          (fpsChanges state2) $
          repairTelemetryFromSupportPatchStats telemetryConfig supportStats $
            fpsRepairTelemetry state2 <> fccRepairTelemetry contributionChange
   in state2 {fpsRepairTelemetry = repairTelemetry}
{-# INLINE patchFactorFromChange #-}

supportEdgeOutputCount :: FactorContributionChange -> Int
supportEdgeOutputCount change =
  Set.size $
    Set.union
      (Map.keysSet (fccInsertedSupportEdges change))
      (Map.keysSet (fccDeletedSupportEdges change))
{-# INLINE supportEdgeOutputCount #-}

supportOutputCount :: FactorPatchState -> Int
supportOutputCount =
  fpsSupportOutputKeys
{-# INLINE supportOutputCount #-}

repairTelemetryFromSupportPatchStats ::
  RepairTelemetryConfig ->
  FactorSupportPatchStats ->
  RepairTelemetry ->
  RepairTelemetry
repairTelemetryFromSupportPatchStats config stats telemetry
  | repairTelemetryDetailed config =
      telemetry
        { rtSupportCellsVisited =
            rtSupportCellsVisited telemetry + fspsCellsVisited stats,
          rtSupportPatchEdgesPreserved =
            rtSupportPatchEdgesPreserved telemetry + fspsEdgesPreserved stats,
          rtSupportPatchEdgesInserted =
            rtSupportPatchEdgesInserted telemetry + fspsEdgesInserted stats,
          rtSupportPatchEdgesDeleted =
            rtSupportPatchEdgesDeleted telemetry + fspsEdgesDeleted stats,
          rtSupportPatchOutputKeysDeleted =
            rtSupportPatchOutputKeysDeleted telemetry + fspsOutputKeysDeleted stats
        }
  | otherwise =
      telemetry
{-# INLINE repairTelemetryFromSupportPatchStats #-}

repairTelemetryFromFactorPatches ::
  RepairTelemetryConfig ->
  Map.Map AssignmentTupleKey (CorePatch.CellPatch ProvVal) ->
  RepairTelemetry ->
  RepairTelemetry
repairTelemetryFromFactorPatches config patches telemetry
  | repairTelemetryDetailed config =
      Map.foldlWithKey' recordPatch telemetry patches
  | otherwise =
      telemetry
  where
    recordPatch ::
      RepairTelemetry ->
      AssignmentTupleKey ->
      CorePatch.CellPatch ProvVal ->
      RepairTelemetry
    recordPatch !patchTelemetry _key patch =
      CorePatch.matchCell
        patchTelemetry
        (\_newValue -> patchTelemetry {rtFactorCellInserts = rtFactorCellInserts patchTelemetry + 1})
        (\_oldValue -> patchTelemetry {rtFactorCellDeletes = rtFactorCellDeletes patchTelemetry + 1})
        (\_oldValue _newValue -> patchTelemetry {rtFactorPayloadSets = rtFactorPayloadSets patchTelemetry + 1})
        patch
{-# INLINE repairTelemetryFromFactorPatches #-}

patchContributionKeys ::
  FactorContributionIndex ->
  Set AssignmentTupleKey ->
  FactorPatchState ->
  FactorPatchState
patchContributionKeys contributionIndex keys state0 =
  Set.foldl'
    patchOne
    state0
    keys
  where
    patchOne !state key =
      let !newValue =
            factorContributionIndexValueAt key contributionIndex
       in markFactorKeyTouched key $
            patchFactorKeyValue key newValue state
{-# INLINE patchContributionKeys #-}

patchFactorKeyValue ::
  AssignmentTupleKey ->
  ProvVal ->
  FactorPatchState ->
  FactorPatchState
patchFactorKeyValue key newValue !state0 =
  let (!factor1, !maybeChange) =
        patchFactorCellValue key newValue (fpsFactor state0)
      !changes1 =
        case maybeChange of
          Nothing ->
            fpsChanges state0
          Just change ->
            recordProvenFactorCellPatch key change (fpsChanges state0)
   in state0
        { fpsFactor = factor1,
          fpsChanges = changes1
        }
{-# INLINE patchFactorKeyValue #-}

recordProvenFactorCellPatch ::
  AssignmentTupleKey ->
  CorePatch.CellPatch ProvVal ->
  Map.Map AssignmentTupleKey (CorePatch.CellPatch ProvVal) ->
  Map.Map AssignmentTupleKey (CorePatch.CellPatch ProvVal)
recordProvenFactorCellPatch key latest changes =
  case Map.lookup key changes of
    Nothing ->
      Map.insert key latest changes
    Just original ->
      Map.insert
        key
        (factorCellPatchFromEndpoints (CorePatch.cellBefore original) (CorePatch.cellAfter latest))
        changes
{-# INLINE recordProvenFactorCellPatch #-}

factorCellPatchFromEndpoints ::
  Maybe ProvVal ->
  Maybe ProvVal ->
  CorePatch.CellPatch ProvVal
factorCellPatchFromEndpoints before after =
  case (before, after) of
    (Nothing, Nothing) ->
      CorePatch.assertAbsent
    (Nothing, Just value) ->
      CorePatch.insert value
    (Just value, Nothing) ->
      CorePatch.delete value
    (Just beforeValue, Just afterValue) ->
      CorePatch.replace beforeValue afterValue
{-# INLINE factorCellPatchFromEndpoints #-}

markFactorKeyTouched ::
  AssignmentTupleKey ->
  FactorPatchState ->
  FactorPatchState
markFactorKeyTouched key state =
  state
    { fpsTouchedKeys = Set.insert key (fpsTouchedKeys state),
      fpsRecomputedCells = fpsRecomputedCells state + 1
    }
{-# INLINE markFactorKeyTouched #-}
