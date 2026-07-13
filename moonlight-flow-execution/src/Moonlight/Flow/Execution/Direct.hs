{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.Flow.Execution.Direct
  ( RuntimeRootSelection (..),
    RuntimeSection (..),
    RuntimeComposedRows (..),
    RuntimeQueryPlanObstruction (..),
    wholeRuntimeSection,
    composedRuntimeSection,
    evalPlanRows,
    evalPlanRowsWithRootSelection,
    evalPlanRowsWithCompiledStoragePlanAndRootSelection,
    evalPlanRowsWithCompiledStoragePlanAndRootSelectionWithRuntimeSections,
    evalPlanOutputsWithRootSelection,
    evalPlanOutputsWithCompiledStoragePlanAndRootSelection,
    evalPlanOutputsWithCompiledStoragePlanAndRootSelectionWithRuntimeSections,
    DenseArrangement,
    evalPlanPreparedDecomp,
    evalPlanPreparedStore,
    evalPlanStoreWithSectionOverride,
    evalPlanOutputsFromPreparedStore,
    evalPlanOutputsFromPreparedStoreCached,
    repairPlanFactorCacheFromPreparedStore,
    evalPlanOutputsWithProvenanceFromPreparedStoreCached,
    evalPlanPreparedSources,
    evalPlanPreparedSourceOverride,
    evalPlanPreparedSourcesWithDirtyRows,
    evalPlanAssignmentsFromPreparedSources,
    evalPlanDeltaAssignmentsFromPreparedSources,
    evalPlanOutputsFromPreparedSources,
    evalPinnedRow,
    evalPlanSupportRows,
    evalPlanOutputAt,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Core
  ( AtomId,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvArena,
    ProvVal,
    ProvenanceObstruction (..),
  )
import Moonlight.Flow.Execution.Prepared.Base qualified as RelBase
import Moonlight.Flow.Execution.Dense.Plan
  ( DenseArrangement,
    DenseArrangementPatchError,
  )
import Moonlight.Flow.Execution.Dense.Plan qualified as Dense
import Moonlight.Flow.Execution.Prepared.Run
  ( PreparedOp (..),
    PreparedProvenanceError,
    PreparedProvenanceRow (..),
    PreparedProvenanceRows (..),
    PreparedResult (..),
    PreparedRunMode (..),
    PreparedRunSpec (..),
    prValue,
    runPrepared,
    runPreparedMeasuredWithDecomp,
    runPreparedValueWithDecomp,
    runPreparedValueWithStructuralSourcesWithDecomp,
    structuralDecompFromPlan,
    supportRelations,
  )
import Moonlight.Flow.Execution.Factor.Types
  ( FactorCache,
  )
import Moonlight.Flow.Execution.Prepared.Request
  ( frontierRestriction,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    coerceTupleKey,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchFromList
  )
import Moonlight.Flow.Plan.Query.Core qualified as RelPlan
import Moonlight.Differential.Row.Block
import Moonlight.Flow.Model.RowIdentity
  ( rowBlockIdentityForQuery,
  )
import Moonlight.Flow.Storage.Restriction
  ( Restriction,
    emptyRestriction,
    restrictRootSlot,
  )
import Moonlight.Flow.Storage.Relation
  ( atomRowsFromTupleKeys,
    materializeAtomRow,
  )
import Moonlight.Flow.Storage.Plan
  ( CompiledStoragePlan,
    StoragePlanError,
    compileStoragePlan,
    storagePlanFromQueryPlan,
  )
import Moonlight.Flow.Storage.Store
  ( Store,
    StorageError,
    storeFromRelations,
    storeFromPlan,
    storeWithPlannedAtomRows,
  )
import Moonlight.Flow.Storage.View
  ( View,
    unrestrictedView,
  )

data RuntimeRootSelection
  = RuntimeAllRoots
  | RuntimeRootKeys !IntSet.IntSet
  deriving stock (Eq, Ord, Show)

data RuntimeSection
  = RuntimeWholeSection !(RowBlock 'Canonical)
  | RuntimeComposedSection !RuntimeComposedRows
  deriving stock (Eq, Show)

data RuntimeComposedRows = RuntimeComposedRows
  { rcrBaseRows :: !(RowBlock 'Canonical),
    rcrMaskedRows :: !(RowBlock 'Canonical),
    rcrExtraRows :: !(Maybe (RowBlock 'Canonical))
  }
  deriving stock (Eq, Show)

wholeRuntimeSection :: RowBlock 'Canonical -> RuntimeSection
wholeRuntimeSection =
  RuntimeWholeSection
{-# INLINE wholeRuntimeSection #-}

composedRuntimeSection ::
  RowBlock 'Canonical ->
  RowBlock 'Canonical ->
  Maybe (RowBlock 'Canonical) ->
  RuntimeSection
composedRuntimeSection baseRows maskedRows maybeExtraRows =
  case runtimeComposedRowsAsWhole composedRows of
    Just rows ->
      RuntimeWholeSection rows
    Nothing ->
      RuntimeComposedSection composedRows
  where
    composedRows =
      RuntimeComposedRows
        { rcrBaseRows = baseRows,
          rcrMaskedRows = maskedRows,
          rcrExtraRows = maybeExtraRows
        }
{-# INLINE composedRuntimeSection #-}

data RuntimeQueryPlanObstruction
  = RuntimeQueryPlanProvenanceObstruction !ProvenanceObstruction
  | RuntimeQueryPlanOutputProjectionObstruction !RelPlan.OutputProjectionObstruction
  | RuntimeQueryPlanRowBuildObstruction !RowBuildError
  | RuntimeQueryPlanBasePreparedObstruction !RelBase.BuildBasePreparedDBError
  | RuntimeQueryPlanBasePreparedPatchObstruction !RelBase.PatchBasePreparedDBError
  | RuntimeQueryPlanDenseArrangementPatchObstruction !DenseArrangementPatchError
  | RuntimeQueryPlanStoragePlanObstruction !StoragePlanError
  | RuntimeQueryPlanStorageObstruction !StorageError
  | RuntimeQueryPlanPreparedProvenanceObstruction !PreparedProvenanceError
  | RuntimeQueryPlanProvenanceRowMissing !AtomId !RowTupleKey
  deriving stock (Eq, Show)

evalPlanRows ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  IntMap.IntMap (RowBlock 'Canonical) ->
  Either RuntimeQueryPlanObstruction (RowBlock 'Canonical)
evalPlanRows plan =
  evalPlanRowsWithRootSelection plan RuntimeAllRoots

evalPlanRowsWithRootSelection ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  RuntimeRootSelection ->
  IntMap.IntMap (RowBlock 'Canonical) ->
  Either RuntimeQueryPlanObstruction (RowBlock 'Canonical)
evalPlanRowsWithRootSelection plan rootSelection sections =
  evalPlanStoreFromSections plan sections
    >>= evalPlanRowsFromStoreWithRootSelection plan rootSelection

evalPlanRowsWithCompiledStoragePlanAndRootSelection ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  CompiledStoragePlan ->
  RuntimeRootSelection ->
  IntMap.IntMap (RowBlock 'Canonical) ->
  Either RuntimeQueryPlanObstruction (RowBlock 'Canonical)
evalPlanRowsWithCompiledStoragePlanAndRootSelection plan compiledStoragePlan rootSelection sections =
  evalPlanStoreFromSectionsWithCompiledStoragePlan compiledStoragePlan sections
    >>= evalPlanRowsFromStoreWithRootSelection plan rootSelection

evalPlanRowsWithCompiledStoragePlanAndRootSelectionWithRuntimeSections ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  CompiledStoragePlan ->
  RuntimeRootSelection ->
  IntMap.IntMap RuntimeSection ->
  Either RuntimeQueryPlanObstruction (RowBlock 'Canonical)
evalPlanRowsWithCompiledStoragePlanAndRootSelectionWithRuntimeSections plan compiledStoragePlan rootSelection sections =
  case runtimeSectionsAsWhole sections of
    Just rowSections ->
      evalPlanRowsWithCompiledStoragePlanAndRootSelection plan compiledStoragePlan rootSelection rowSections
    Nothing ->
      evalPlanRowsFromRuntimeSections plan rootSelection sections
{-# INLINE evalPlanRowsWithCompiledStoragePlanAndRootSelectionWithRuntimeSections #-}

evalPlanOutputsWithRootSelection ::
  RelPlan.QueryOutput output key =>
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  RuntimeRootSelection ->
  IntMap.IntMap (RowBlock 'Canonical) ->
  Either RuntimeQueryPlanObstruction [output]
evalPlanOutputsWithRootSelection plan rootSelection sections = do
  rows <- evalPlanRowsWithRootSelection plan rootSelection sections
  first
    RuntimeQueryPlanOutputProjectionObstruction
    (RelPlan.projectQueryPlanOutputs plan (atomRowsToList rows))

evalPlanOutputsWithCompiledStoragePlanAndRootSelection ::
  RelPlan.QueryOutput output key =>
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  CompiledStoragePlan ->
  RuntimeRootSelection ->
  IntMap.IntMap (RowBlock 'Canonical) ->
  Either RuntimeQueryPlanObstruction [output]
evalPlanOutputsWithCompiledStoragePlanAndRootSelection plan compiledStoragePlan rootSelection sections = do
  rows <- evalPlanRowsWithCompiledStoragePlanAndRootSelection plan compiledStoragePlan rootSelection sections
  first
    RuntimeQueryPlanOutputProjectionObstruction
    (RelPlan.projectQueryPlanOutputs plan (atomRowsToList rows))

evalPlanOutputsWithCompiledStoragePlanAndRootSelectionWithRuntimeSections ::
  RelPlan.QueryOutput output key =>
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  CompiledStoragePlan ->
  RuntimeRootSelection ->
  IntMap.IntMap RuntimeSection ->
  Either RuntimeQueryPlanObstruction [output]
evalPlanOutputsWithCompiledStoragePlanAndRootSelectionWithRuntimeSections plan compiledStoragePlan rootSelection sections = do
  rows <- evalPlanRowsWithCompiledStoragePlanAndRootSelectionWithRuntimeSections plan compiledStoragePlan rootSelection sections
  first
    RuntimeQueryPlanOutputProjectionObstruction
    (RelPlan.projectQueryPlanOutputs plan (atomRowsToList rows))
{-# INLINE evalPlanOutputsWithCompiledStoragePlanAndRootSelectionWithRuntimeSections #-}

evalPinnedRow ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  AtomId ->
  RowBlock 'Canonical ->
  RowDesc ->
  IntMap.IntMap (RowBlock 'Canonical) ->
  Either RuntimeQueryPlanObstruction Bool
evalPinnedRow plan atomId rows desc sections =
  do
    store <- evalPlanStoreFromSections plan sections
    let view =
          unrestrictedView
    first
        RuntimeQueryPlanProvenanceObstruction
        ( runPreparedValue
            plan
            emptyRestriction
            store
            view
            (PreparedExistsPinned atomId (materializeAtomRow rows desc))
        )

evalPlanSupportRows ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  IntMap.IntMap (RowBlock 'Canonical) ->
  Either RuntimeQueryPlanObstruction (IntMap.IntMap (RowBlock 'Canonical))
evalPlanSupportRows plan sections =
  do
    store <- evalPlanStoreFromSections plan sections
    let view =
          unrestrictedView
    support <-
      first
        RuntimeQueryPlanProvenanceObstruction
        (runPreparedValue plan emptyRestriction store view PreparedSupport)
    first RuntimeQueryPlanRowBuildObstruction $
      supportRelations plan store support

evalPlanOutputAt ::
  RelPlan.QueryOutput output key =>
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  RowBlock 'Canonical ->
  RowDesc ->
  Either RuntimeQueryPlanObstruction (Maybe output)
evalPlanOutputAt plan rows desc =
  first
    RuntimeQueryPlanOutputProjectionObstruction
    (RelPlan.projectQueryPlanOutput plan (materializeAtomRow rows desc))

evalPlanRowsIdentity ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  Store ->
  RowBlockIdentity
evalPlanRowsIdentity plan _store =
  evalPlanRowsIdentityForPlan plan
{-# INLINE evalPlanRowsIdentity #-}

evalPlanRowsIdentityForPlan ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  RowBlockIdentity
evalPlanRowsIdentityForPlan plan =
  rowBlockIdentityForQuery
    0
    0
    (RelPlan.qpFingerprint plan)
    (RelPlan.qpId plan)
    0
{-# INLINE evalPlanRowsIdentityForPlan #-}

evalPlanRowsFromStoreWithRootSelection ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  RuntimeRootSelection ->
  Store ->
  Either RuntimeQueryPlanObstruction (RowBlock 'Canonical)
evalPlanRowsFromStoreWithRootSelection plan rootSelection store =
  let view =
        unrestrictedView
      restriction =
        runtimeRootSelectionRestriction plan rootSelection
   in do
        rows <-
          first
            RuntimeQueryPlanProvenanceObstruction
            (runPreparedValue plan restriction store view (PreparedRows Nothing))
        first RuntimeQueryPlanRowBuildObstruction $
          atomRowsFromTupleKeys
            (evalPlanRowsIdentity plan store)
            (RelPlan.qpFullSchema plan)
            rows

evalPlanStoreFromSections ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  IntMap.IntMap (RowBlock 'Canonical) ->
  Either RuntimeQueryPlanObstruction Store
evalPlanStoreFromSections plan sections = do
  compiledStoragePlan <-
    first
      RuntimeQueryPlanStoragePlanObstruction
      (compileStoragePlan (storagePlanFromQueryPlan plan))
  evalPlanStoreFromSectionsWithCompiledStoragePlan compiledStoragePlan sections

evalPlanStoreFromSectionsWithCompiledStoragePlan ::
  CompiledStoragePlan ->
  IntMap.IntMap (RowBlock 'Canonical) ->
  Either RuntimeQueryPlanObstruction Store
evalPlanStoreFromSectionsWithCompiledStoragePlan compiledStoragePlan sections =
  first
    RuntimeQueryPlanStorageObstruction
    (storeFromPlan compiledStoragePlan (IntMap.map atomRowsDelta sections))

evalPlanPreparedDecomp ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  RelPlan.DecompPlan
evalPlanPreparedDecomp =
  structuralDecompFromPlan
{-# INLINE evalPlanPreparedDecomp #-}

evalPlanPreparedStore ::
  CompiledStoragePlan ->
  IntMap.IntMap (RowBlock 'Canonical) ->
  Either RuntimeQueryPlanObstruction Store
evalPlanPreparedStore =
  evalPlanStoreFromSectionsWithCompiledStoragePlan
{-# INLINE evalPlanPreparedStore #-}

evalPlanStoreWithSectionOverride ::
  CompiledStoragePlan ->
  Int ->
  RowBlock 'Canonical ->
  Store ->
  Either RuntimeQueryPlanObstruction Store
evalPlanStoreWithSectionOverride compiledStoragePlan atomKey rows =
  first
    RuntimeQueryPlanStorageObstruction
    . storeWithPlannedAtomRows compiledStoragePlan atomKey (atomRowsDelta rows)
{-# INLINE evalPlanStoreWithSectionOverride #-}

evalPlanOutputsFromPreparedStore ::
  RelPlan.QueryOutput output key =>
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  RelPlan.DecompPlan ->
  RuntimeRootSelection ->
  Store ->
  Either RuntimeQueryPlanObstruction [output]
evalPlanOutputsFromPreparedStore plan decomp rootSelection store = do
  rows <-
    first
      RuntimeQueryPlanProvenanceObstruction
      ( runPreparedValueWithDecomp
          plan
          (runtimeRootSelectionRestriction plan rootSelection)
          store
          unrestrictedView
          decomp
          (PreparedRows Nothing)
      )
  first
    RuntimeQueryPlanOutputProjectionObstruction
    (RelPlan.projectQueryPlanOutputs plan rows)
{-# INLINE evalPlanOutputsFromPreparedStore #-}

evalPlanOutputsFromPreparedStoreCached ::
  RelPlan.QueryOutput output key =>
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  RelPlan.DecompPlan ->
  RuntimeRootSelection ->
  Store ->
  FactorCache ->
  IntMap.IntMap RowDelta ->
  PreparedOp [RowTupleKey] ->
  Either RuntimeQueryPlanObstruction ([output], Maybe FactorCache)
evalPlanOutputsFromPreparedStoreCached plan decomp rootSelection store cache atomDeltas op = do
  result <-
    first
      RuntimeQueryPlanProvenanceObstruction
      ( runPreparedMeasuredWithDecomp
          plan
          (runtimeRootSelectionRestriction plan rootSelection)
          store
          unrestrictedView
          atomDeltas
          decomp
          op
          cache
      )
  outputs <-
    first
      RuntimeQueryPlanOutputProjectionObstruction
      (RelPlan.projectQueryPlanOutputs plan (prValue result))
  pure (outputs, prFactorCache result)
{-# INLINE evalPlanOutputsFromPreparedStoreCached #-}

-- | Repair one retained factor cache without enumerating output assignments.
repairPlanFactorCacheFromPreparedStore ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  RelPlan.DecompPlan ->
  RuntimeRootSelection ->
  Store ->
  FactorCache ->
  IntMap.IntMap RowDelta ->
  Either RuntimeQueryPlanObstruction (Maybe FactorCache)
repairPlanFactorCacheFromPreparedStore plan decomp rootSelection store cache atomDeltas = do
  result <-
    first
      RuntimeQueryPlanProvenanceObstruction
      ( runPreparedMeasuredWithDecomp
          plan
          (runtimeRootSelectionRestriction plan rootSelection)
          store
          unrestrictedView
          atomDeltas
          decomp
          PreparedExists
          cache
      )
  pure (prFactorCache result)
{-# INLINE repairPlanFactorCacheFromPreparedStore #-}

-- | Evaluate one prepared join while retaining both its repaired factor cache
-- and output-row provenance in that cache's collected arena.
evalPlanOutputsWithProvenanceFromPreparedStoreCached ::
  RelPlan.QueryOutput output key =>
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  RelPlan.DecompPlan ->
  RuntimeRootSelection ->
  Store ->
  FactorCache ->
  IntMap.IntMap RowDelta ->
  Either
    RuntimeQueryPlanObstruction
    (ProvArena, [(output, RowTupleKey, [ProvVal])], Maybe FactorCache)
evalPlanOutputsWithProvenanceFromPreparedStoreCached plan decomp rootSelection store cache atomDeltas = do
  result <-
    first
      RuntimeQueryPlanProvenanceObstruction
      ( runPreparedMeasuredWithDecomp
          plan
          (runtimeRootSelectionRestriction plan rootSelection)
          store
          unrestrictedView
          atomDeltas
          decomp
          (PreparedRowsWithProvenance Nothing)
          cache
      )
  provenanceRows <-
    first
      RuntimeQueryPlanPreparedProvenanceObstruction
      (prValue result)
  outputs <-
    fmap catMaybes $
      traverse
      (\preparedRow -> do
          maybeOutput <-
            first
              RuntimeQueryPlanOutputProjectionObstruction
              (RelPlan.projectQueryPlanOutput plan (pprTuple preparedRow))
          pure
            ( fmap
                (\output -> (output, pprTuple preparedRow, pprFactors preparedRow))
                maybeOutput
            )
      )
      (pprsRows provenanceRows)
  pure (pprsArena provenanceRows, outputs, prFactorCache result)
{-# INLINE evalPlanOutputsWithProvenanceFromPreparedStoreCached #-}

evalPlanPreparedSources ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  IntMap.IntMap (RowBlock 'Canonical) ->
  Either RuntimeQueryPlanObstruction [DenseArrangement]
evalPlanPreparedSources plan sections =
  first
    RuntimeQueryPlanDenseArrangementPatchObstruction
    (runtimeSectionSources plan (IntMap.map RuntimeWholeSection sections))

evalPlanPreparedSourceOverride ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  Int ->
  RowBlock 'Canonical ->
  [DenseArrangement] ->
  [DenseArrangement]
evalPlanPreparedSourceOverride plan atomKey rows =
  zipWith3
    overrideSource
    [0 :: Int ..]
    (Vector.toList (RelPlan.qpAtoms plan))
  where
    overrideSource sourceId atomSpec source =
      let queryAtomId =
            RelPlan.asQueryAtomId atomSpec
       in if RelPlan.queryAtomKey queryAtomId == atomKey
            then
              Dense.denseAtomSourceFromRowBlock
                (Dense.DenseArrangementId sourceId)
                (RelPlan.queryAtomAsAtomId queryAtomId)
                rows
            else source

-- | Mark the current rows whose membership or attached interpretation changed
-- as the semi-naive input frontier for one prepared multi-source join.
evalPlanPreparedSourcesWithDirtyRows ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  IntMap.IntMap (Set.Set RowTupleKey) ->
  [DenseArrangement] ->
  [DenseArrangement]
evalPlanPreparedSourcesWithDirtyRows plan dirtyRowsByAtom =
  zipWith
    (\atomSpec source ->
        let atomKey =
              RelPlan.queryAtomKey (RelPlan.asQueryAtomId atomSpec)
            dirtyRows =
              IntMap.findWithDefault Set.empty atomKey dirtyRowsByAtom
         in Dense.denseArrangementWithDirtyKeys
              (Set.map coerceTupleKey dirtyRows)
              source
    )
    (Vector.toList (RelPlan.qpAtoms plan))
{-# INLINE evalPlanPreparedSourcesWithDirtyRows #-}

evalPlanOutputsFromPreparedSources ::
  RelPlan.QueryOutput output key =>
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  RelPlan.DecompPlan ->
  RuntimeRootSelection ->
  [DenseArrangement] ->
  Either RuntimeQueryPlanObstruction [output]
evalPlanOutputsFromPreparedSources plan decomp rootSelection sources = do
  rows <-
    evalPlanAssignmentsFromPreparedSources
      plan
      decomp
      rootSelection
      sources
  first
    RuntimeQueryPlanOutputProjectionObstruction
    (RelPlan.projectQueryPlanOutputs plan rows)

evalPlanAssignmentsFromPreparedSources ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  RelPlan.DecompPlan ->
  RuntimeRootSelection ->
  [DenseArrangement] ->
  Either RuntimeQueryPlanObstruction [RowTupleKey]
evalPlanAssignmentsFromPreparedSources plan decomp rootSelection sources =
  first
    RuntimeQueryPlanProvenanceObstruction
    ( runPreparedValueWithStructuralSourcesWithDecomp
        plan
        (runtimeRootSelectionRestriction plan rootSelection)
        decomp
        sources
        (PreparedRows Nothing)
    )
{-# INLINE evalPlanAssignmentsFromPreparedSources #-}

evalPlanDeltaAssignmentsFromPreparedSources ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  RelPlan.DecompPlan ->
  RuntimeRootSelection ->
  [DenseArrangement] ->
  Either RuntimeQueryPlanObstruction [RowTupleKey]
evalPlanDeltaAssignmentsFromPreparedSources plan decomp rootSelection sources =
  first
    RuntimeQueryPlanProvenanceObstruction
    ( runPreparedValueWithStructuralSourcesWithDecomp
        plan
        (runtimeRootSelectionRestriction plan rootSelection)
        decomp
        sources
        (PreparedDeltaRows Nothing)
    )
{-# INLINE evalPlanDeltaAssignmentsFromPreparedSources #-}

runtimeSectionsAsWhole :: IntMap.IntMap RuntimeSection -> Maybe (IntMap.IntMap (RowBlock 'Canonical))
runtimeSectionsAsWhole =
  traverse runtimeSectionAsWhole
{-# INLINE runtimeSectionsAsWhole #-}

runtimeSectionAsWhole :: RuntimeSection -> Maybe (RowBlock 'Canonical)
runtimeSectionAsWhole =
  \case
    RuntimeWholeSection rows ->
      Just rows
    RuntimeComposedSection rows ->
      runtimeComposedRowsAsWhole rows
{-# INLINE runtimeSectionAsWhole #-}

runtimeComposedRowsAsWhole :: RuntimeComposedRows -> Maybe (RowBlock 'Canonical)
runtimeComposedRowsAsWhole rows
  | rowBlockCount (rcrMaskedRows rows) == 0,
    maybe True ((== 0) . rowBlockCount) (rcrExtraRows rows) =
      Just (rcrBaseRows rows)
  | otherwise =
      Nothing
{-# INLINE runtimeComposedRowsAsWhole #-}

evalPlanRowsFromRuntimeSections ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  RuntimeRootSelection ->
  IntMap.IntMap RuntimeSection ->
  Either RuntimeQueryPlanObstruction (RowBlock 'Canonical)
evalPlanRowsFromRuntimeSections plan rootSelection sections = do
  sources <-
    first
      RuntimeQueryPlanDenseArrangementPatchObstruction
      (runtimeSectionSources plan sections)
  rows <-
    first
      RuntimeQueryPlanProvenanceObstruction
      ( runPreparedValueWithStructuralSources
          plan
          (runtimeRootSelectionRestriction plan rootSelection)
          sources
          (PreparedRows Nothing)
      )
  first RuntimeQueryPlanRowBuildObstruction $
    atomRowsFromTupleKeys
      (evalPlanRowsIdentityForPlan plan)
      (RelPlan.qpFullSchema plan)
      rows
{-# INLINE evalPlanRowsFromRuntimeSections #-}

runtimeSectionSources ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  IntMap.IntMap RuntimeSection ->
  Either DenseArrangementPatchError [DenseArrangement]
runtimeSectionSources plan sections =
  traverse
    (runtimeAtomSource sections)
    (zip [0 :: Int ..] (Vector.toList (RelPlan.qpAtoms plan)))
{-# INLINE runtimeSectionSources #-}

runtimeAtomSource ::
  IntMap.IntMap RuntimeSection ->
  (Int, RelPlan.AtomSpec tag tuple key) ->
  Either DenseArrangementPatchError DenseArrangement
runtimeAtomSource sections (sourceId, atomSpec) =
  case IntMap.lookup atomKey sections of
    Nothing ->
      Dense.denseAtomSourceFromRows arrangementId atomId (RelPlan.asColumns atomSpec) []
    Just section ->
      runtimeSectionSource arrangementId atomId section
  where
    arrangementId =
      Dense.DenseArrangementId sourceId

    queryAtomId =
      RelPlan.asQueryAtomId atomSpec

    atomKey =
      RelPlan.queryAtomKey queryAtomId

    atomId =
      RelPlan.queryAtomAsAtomId queryAtomId
{-# INLINE runtimeAtomSource #-}

runtimeSectionSource ::
  Dense.DenseArrangementId ->
  AtomId ->
  RuntimeSection ->
  Either DenseArrangementPatchError DenseArrangement
runtimeSectionSource arrangementId atomId =
  \case
    RuntimeWholeSection rows ->
      Right (Dense.denseAtomSourceFromRowBlock arrangementId atomId rows)
    RuntimeComposedSection rows ->
      Dense.denseAtomSourceFromComposedRowBlocks
        arrangementId
        atomId
        (rcrBaseRows rows)
        (rcrMaskedRows rows)
        (rcrExtraRows rows)
{-# INLINE runtimeSectionSource #-}

runPreparedValueWithStructuralSources ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  Restriction ->
  [DenseArrangement] ->
  PreparedOp a ->
  Either ProvenanceObstruction a
runPreparedValueWithStructuralSources plan restriction sources op =
  prValue
    <$> runPrepared
      PreparedRunSpec
        { prsPlan = plan,
          prsRestriction = restriction,
          prsStore = storeFromRelations IntMap.empty,
          prsView = unrestrictedView,
          prsAtomDeltas = IntMap.empty,
          prsStructuralSources = Just sources,
          prsOp = op,
          prsMode = PreparedValueOnly
        }
{-# INLINE runPreparedValueWithStructuralSources #-}

atomRowsDelta :: RowBlock 'Canonical -> RowDelta
atomRowsDelta rows =
  plainRowPatchFromList
    ( foldRowBlock
        ( \entries desc ->
            (materializeAtomRow rows desc, MultiplicityChange 1) : entries
        )
        []
        rows
    )

runtimeRootSelectionRestriction ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  RuntimeRootSelection ->
  Restriction
runtimeRootSelectionRestriction plan rootSelection =
  case rootSelection of
    RuntimeAllRoots ->
      emptyRestriction
    RuntimeRootKeys rootKeys ->
      case RelPlan.qpDomain plan of
        RelPlan.RootDomainQueryPlan ->
          restrictRootSlot (RelPlan.qpRootSlot plan) rootKeys
        RelPlan.StructuralQueryPlan ->
          frontierRestriction plan (Just rootKeys)

runPreparedValue ::
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  Restriction ->
  Store ->
  View ->
  PreparedOp a ->
  Either ProvenanceObstruction a
runPreparedValue plan restriction store view op =
  prValue
    <$> runPrepared
      PreparedRunSpec
        { prsPlan = plan,
          prsRestriction = restriction,
          prsStore = store,
          prsView = view,
          prsAtomDeltas = IntMap.empty,
          prsStructuralSources = Nothing,
          prsOp = op,
          prsMode = PreparedValueOnly
        }

atomRowsToList :: RowBlock state -> [RowTupleKey]
atomRowsToList rows =
  reverse
    ( foldRowBlock
        (\acc desc -> materializeAtomRow rows desc : acc)
        []
        rows
    )
