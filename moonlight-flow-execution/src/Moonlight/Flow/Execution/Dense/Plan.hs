{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Execution.Dense.Plan
  ( DenseArrangementId (..),
    DenseArrangement,
    DenseProjectedRows,
    SelectedOutputDomain (..),
    selectedOutputDomainFromKeys,
    DenseJoinPlan,
    DenseJoinPlanError (..),
    SourceBundle (..),
    mkDenseJoinPlan,
    mkDenseJoinPlanWithSupportSources,
    denseAtomSource,
    denseAtomSourceFromRowBlock,
    denseAtomSourceFromComposedRowBlocks,
    denseAtomSourceFromRows,
    denseAtomSourceFromRowsById,
    denseProjectedAtomSourceFromRows,
    denseProjectedRowsFromRows,
    denseProjectedRowsClearDirtyRows,
    denseDiagnosticValidRowsById,
    denseDiagnosticRowsValueIndex,
    denseDiagnosticProjectedRowsFromValidRowsAndValueIndex,
    denseFactorSource,
    denseSourcesFromStorageView,
    denseArrangementId,
    denseArrangementAtomId,
    denseArrangementSchema,
    denseArrangementSchemaKeys,
    denseArrangementColumnIndex,
    denseArrangementRows,
    denseArrangementDirtyRows,
    denseArrangementKeyAt,
    denseArrangementValueIndex,
    denseArrangementValueAt,
    denseArrangementPayloadAt,
    denseArrangementPayloadAtWithTelemetry,
    denseArrangementUnionSchema,
    denseArrangementDeltaJoinSource,
    denseJoinPlanProblem,
    denseJoinPlanFullSchema,
    denseJoinPlanOutputSchema,
    denseJoinPlanSources,
    denseJoinPlanSupportSources,
    denseJoinPlanSelectedKeys,
    denseApplyPinsToArrangement,
    denseRestrictArrangementBySlotValues,
    denseRestrictArrangementByPinnedRows,
    denseArrangementWithDirtyKeys,
    denseArrangementRestrictToDirtyRows,
    denseArrangementClearDirtyRows,
    DenseArrangementPatchError (..),
    patchDenseAtomArrangement,
    patchDenseProjectedRows,
    sourceBundleArrangement,
  )
where

import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.SmallArray (SmallArray)
import Data.Primitive.SmallArray qualified as SmallArray
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import Moonlight.Differential.Join.WCOJ.Delta
  ( DeltaJoinSource (..),
  )
import Moonlight.Differential.Join.WCOJ.Dense.Executor
  ( DenseDeltaProblem,
    DenseDeltaProblemError,
    denseDeltaProblemFullSchema,
    denseDeltaProblemOutputSchema,
    mkDenseDeltaProblem,
  )
import Moonlight.Flow.Execution.Factor.Core
  ( Factor,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvVal (..),
    ProvArena
  )
import Moonlight.Flow.Execution.Observe.Provenance.Value
  ( pvAtom,
    pvAtomWithTelemetry
  )
import Moonlight.Flow.Execution.Observe.RepairTelemetry
  ( RepairTelemetry,
    RepairTelemetryConfig,
    emptyRepairTelemetry,
  )
import Moonlight.Flow.Internal.PrimArray
  ( primArrayFromListStrict,
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Delta.Signed
  ( Multiplicity (..),
    MultiplicityChange (..),
    addMultiplicity,
    applyMultiplicityChange,
    positiveMultiplicityChange,
    zeroMultiplicity,
    zeroMultiplicityChange
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchChangeMap,
    plainRowPatchNull
  )
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Differential.Row.Block
  ( RowLayout,
    RowBlock,
    RowState (Canonical),
  )
import Moonlight.Differential.Row.Block qualified as RowBlock
import Moonlight.Differential.Index.IndexedRows
  ( IndexedRows,
    IndexedRowsBuildError (..),
    IndexedRowsDeleteError,
    IndexedRowsInsertError (..),
    IndexedRowsPayloadError,
    emptyIndexedRows,
    indexedRowsDelete,
    indexedRowsFromPayloadMap,
    indexedRowsFromPayloadMapWithValueIndex,
    indexedRowsInsertFresh,
    indexedRowsInsertWithId,
    indexedRowsLayout,
    indexedRowsLiveRows,
    indexedRowsLiveRowSet,
    indexedRowsLookupId,
    indexedRowsLookupPayload,
    indexedRowsSetPayload,
    indexedRowsValueIndex,
  )
import Moonlight.Differential.Index.RowArrangement
  ( IndexedRowArrangement (indexedRowArrangementRows),
    indexedRowArrangementColumnIndex,
    indexedRowArrangementDirtyRows,
    indexedRowArrangementFromRows,
    indexedRowArrangementFromRowsWithSections,
    indexedRowArrangementKeyAt,
    indexedRowArrangementLayout,
    indexedRowArrangementPayloadAt,
    indexedRowArrangementRestrictRowsByPins,
    indexedRowArrangementRestrictToDirtyRows,
    indexedRowArrangementValueIndex,
    indexedRowArrangementVisibleRows,
    indexedRowArrangementWithDirtyKeys,
    indexedRowArrangementWithRows,
  )
import Moonlight.Flow.Storage.Index.TupleFormat
  ( indexedTupleArrangementValueAt,
    repKeyPins,
    rowLayoutColumnIndex,
    tupleKeyIndexedFormat,
  )
import Moonlight.Flow.Storage.Relation
  ( Relation (relRows),
    emptyRelation,
  )
import Moonlight.Flow.Storage.Store
import Moonlight.Flow.Storage.View
import Moonlight.Differential.Index.RowIdSet
  ( RowIdSet,
    rowIdSetFromIntSetCanonical,
    rowIdSetUnion,
    singletonRowIdSet,
  )
import Moonlight.Differential.Index.RowSet
  ( RowSet,
    emptyRowSet,
    rowSetFullRange,
    rowSetFromIntSetCanonical,
    rowSetIntersection,
    rowSetIntersectionWithRowIdSet,
    rowSetFoldl',
    rowSetDifference,
    rowSetToIntSet,
    rowSetUnion,
  )
import Moonlight.Differential.Index.RowId
  ( RowId,
    mkRowId,
    rowIdInt,
  )

type DenseArrangementPatchError :: Type
data DenseArrangementPatchError
  = DenseArrangementPatchNonAtomSource !DenseArrangementId
  | DenseArrangementPatchProjectedSource !DenseArrangementId
  | DenseArrangementPatchRowBlockSource !DenseArrangementId
  | DenseArrangementPatchRowWidthMismatch !RowTupleKey !Int !Int
  | DenseArrangementPatchMissingRowDelete !RowTupleKey !MultiplicityChange
  | DenseArrangementPatchMultiplicityUnderflow !RowTupleKey !Multiplicity !MultiplicityChange
  | DenseArrangementPatchRowBlockLayoutMismatch !RowLayout !RowLayout
  | DenseArrangementPatchRowsBuildFailed !(NonEmpty (IndexedRowsBuildError RowLayout RowTupleKey))
  | DenseArrangementPatchInsertFailed !RowTupleKey !(IndexedRowsInsertError RowLayout RowTupleKey)
  | DenseArrangementPatchDeleteFailed !RowTupleKey !(IndexedRowsDeleteError RowLayout RowTupleKey)
  | DenseArrangementPatchPayloadUpdateFailed !RowTupleKey !(IndexedRowsPayloadError RowTupleKey)
  deriving stock (Eq, Show)

type DenseArrangementId :: Type
newtype DenseArrangementId = DenseArrangementId
  { unDenseArrangementId :: Int
  }
  deriving stock (Eq, Ord, Show)

type DenseArrangement :: Type
data DenseArrangement
  = AtomArrangement
      !DenseArrangementId
      {-# UNPACK #-} !AtomId
      !AtomRowsSource
  | FactorArrangement
      !DenseArrangementId
      !(IndexedRowArrangement RowLayout AssignmentTupleKey ProvVal)
  deriving stock (Show)

type AtomRowsSource :: Type
data AtomRowsSource
  = MaterializedAtomRows !(IndexedRowArrangement RowLayout RowTupleKey Multiplicity)
  | ProjectedAtomRows !ProjectedAtomRowsView
  | RowBlockAtomRows !RowBlockAtomRowsView
  deriving stock (Show)

type RowBlockAtomRowsView :: Type
data RowBlockAtomRowsView = RowBlockAtomRowsView
  { rbarBaseRows :: !(RowBlock 'Canonical),
    rbarExtraRows :: !(Maybe (RowBlock 'Canonical)),
    rbarVisibleRows :: !RowSet,
    rbarDirtyRows :: !RowSet,
    rbarValueIndex :: !(IntMap (IntMap RowIdSet))
  }
  deriving stock (Show)

type DenseProjectedRows :: Type
type DenseProjectedRows = IndexedRowArrangement RowLayout RowTupleKey Multiplicity

type ProjectedAtomRowsView :: Type
data ProjectedAtomRowsView = ProjectedAtomRowsView
  { pavSchema :: !RowLayout,
    pavSlotSources :: !(IntMap [Int]),
    pavPhysicalRows :: !DenseProjectedRows,
    pavVisibleRows :: !RowSet,
    pavDirtyRows :: !RowSet,
    pavValueIndex :: !(IntMap (IntMap RowIdSet))
  }
  deriving stock (Show)

type SelectedOutputDomain :: Type
data SelectedOutputDomain = SelectedOutputDomain
  { sodKeySet :: !(Set.Set AssignmentTupleKey),
    sodRows :: !RowSet,
    sodRowsBySlotValue :: !(IntMap (IntMap RowIdSet))
  }
  deriving stock (Show)

selectedOutputDomainFromKeys ::
  [SlotId] ->
  Set.Set AssignmentTupleKey ->
  Maybe SelectedOutputDomain
selectedOutputDomainFromKeys outputSchema selectedKeys
  | null validKeys =
      Nothing
  | otherwise =
      Just
        SelectedOutputDomain
          { sodKeySet = Set.fromAscList validKeys,
            sodRows = rowSetFullRange (length validKeys),
            sodRowsBySlotValue =
              selectedRowsBySlotValue schemaKeys validKeys
          }
  where
    !outputWidth =
      length outputSchema

    !schemaKeys =
      fmap slotIdKey outputSchema

    validKeys =
      filter
        (\key -> tupleKeyWidth key == outputWidth)
        (Set.toAscList selectedKeys)
{-# INLINE selectedOutputDomainFromKeys #-}

selectedRowsBySlotValue ::
  [Int] ->
  [AssignmentTupleKey] ->
  IntMap (IntMap RowIdSet)
selectedRowsBySlotValue schemaKeys keys =
  foldl' insertKey IntMap.empty (zip [0 :: Int ..] keys)
  where
    insertKey ::
      IntMap (IntMap RowIdSet) ->
      (Int, AssignmentTupleKey) ->
      IntMap (IntMap RowIdSet)
    insertKey !index (!rowId, !key) =
      maybe index (\checkedRowId -> insertSlots index checkedRowId key 0 schemaKeys) (rowIdFromInt rowId)

    insertSlots ::
      IntMap (IntMap RowIdSet) ->
      RowId ->
      AssignmentTupleKey ->
      Int ->
      [Int] ->
      IntMap (IntMap RowIdSet)
    insertSlots !index !_rowId !_key !_slotIx [] =
      index
    insertSlots !index !rowId !key !slotIx (slotKey : rest) =
      case tupleKeyIndexInt key slotIx of
        Nothing ->
          insertSlots index rowId key (slotIx + 1) rest
        Just repKey ->
          insertSlots
            (insertSelectedBucket rowId slotKey repKey index)
            rowId
            key
            (slotIx + 1)
            rest
{-# INLINE selectedRowsBySlotValue #-}

insertSelectedBucket ::
  RowId ->
  Int ->
  Int ->
  IntMap (IntMap RowIdSet) ->
  IntMap (IntMap RowIdSet)
insertSelectedBucket rowId slotKey repKey =
  IntMap.insertWith
    (IntMap.unionWith rowIdSetUnion)
    slotKey
    (IntMap.singleton repKey (singletonRowIdSet rowId))
{-# INLINE insertSelectedBucket #-}

rowIdFromInt :: Int -> Maybe RowId
rowIdFromInt =
  either (const Nothing) Just . mkRowId
{-# INLINE rowIdFromInt #-}

type DenseJoinPlan :: Type
data DenseJoinPlan = DenseJoinPlan
  { denseJoinPlanProblem :: !DenseDeltaProblem,
    denseJoinPlanSources :: !(SmallArray DenseArrangement),
    denseJoinPlanSupportSources :: !IntSet,
    denseJoinPlanSelectedKeys :: !(Maybe (Set.Set AssignmentTupleKey))
  }

data DenseJoinPlanError
  = DenseJoinPlanProblemError !DenseDeltaProblemError
  | DenseJoinPlanSupportSourceOutOfRange !Int !Int
  deriving stock (Eq, Ord, Show)

type SourceBundle :: Type
data SourceBundle = SourceBundle
  { sbCurrent :: !DenseArrangement,
    sbDirtyKeys :: !(Set.Set AssignmentTupleKey)
  }
  deriving stock (Show)

mkDenseJoinPlan ::
  [SlotId] ->
  [SlotId] ->
  [DenseArrangement] ->
  Either DenseJoinPlanError DenseJoinPlan
mkDenseJoinPlan fullSchema outputSchema sources =
  mkDenseJoinPlanChecked fullSchema outputSchema Nothing Nothing sources
{-# INLINE mkDenseJoinPlan #-}

mkDenseJoinPlanWithSupportSources ::
  [SlotId] ->
  [SlotId] ->
  IntSet ->
  Maybe SelectedOutputDomain ->
  [DenseArrangement] ->
  Either DenseJoinPlanError DenseJoinPlan
mkDenseJoinPlanWithSupportSources fullSchema outputSchema supportSources selectedOutput sources =
  mkDenseJoinPlanChecked fullSchema outputSchema (Just supportSources) selectedOutput sources
{-# INLINE mkDenseJoinPlanWithSupportSources #-}

mkDenseJoinPlanChecked ::
  [SlotId] ->
  [SlotId] ->
  Maybe IntSet ->
  Maybe SelectedOutputDomain ->
  [DenseArrangement] ->
  Either DenseJoinPlanError DenseJoinPlan
mkDenseJoinPlanChecked fullSchema outputSchema requestedSupportSources selectedOutput sources = do
  problem <-
    either
      (Left . DenseJoinPlanProblemError)
      Right
      ( mkDenseDeltaProblem
          (fmap denseArrangementDeltaJoinSource sources)
          (fmap slotIdKey fullSchema)
          (fmap slotIdKey outputSchema)
          ((\selected -> (sodRows selected, sodRowsBySlotValue selected)) <$> selectedOutput)
      )
  let sourceArray =
        SmallArray.smallArrayFromList sources
      sourceCount =
        SmallArray.sizeofSmallArray sourceArray
      supportSources =
        maybe (sourceIndexSet sourceCount) id requestedSupportSources
  Foldable.traverse_ (validateSupportSource sourceCount) (IntSet.toAscList supportSources)
  pure
    DenseJoinPlan
      { denseJoinPlanProblem = problem,
        denseJoinPlanSources = sourceArray,
        denseJoinPlanSupportSources = supportSources,
        denseJoinPlanSelectedKeys = sodKeySet <$> selectedOutput
      }

validateSupportSource :: Int -> Int -> Either DenseJoinPlanError ()
validateSupportSource sourceCount sourceId
  | sourceId >= 0 && sourceId < sourceCount =
      Right ()
  | otherwise =
      Left (DenseJoinPlanSupportSourceOutOfRange sourceCount sourceId)

denseJoinPlanFullSchema :: DenseJoinPlan -> PrimArray Int
denseJoinPlanFullSchema =
  denseDeltaProblemFullSchema . denseJoinPlanProblem

denseJoinPlanOutputSchema :: DenseJoinPlan -> PrimArray Int
denseJoinPlanOutputSchema =
  denseDeltaProblemOutputSchema . denseJoinPlanProblem

sourceIndexSet :: Int -> IntSet
sourceIndexSet count
  | count <= 0 =
      IntSet.empty
  | otherwise =
      IntSet.fromDistinctAscList [0 .. count - 1]
{-# INLINE sourceIndexSet #-}

denseSourcesFromStorageView ::
  JoinMeta ->
  Store ->
  View ->
  [DenseArrangement]
denseSourcesFromStorageView meta store view =
  [ denseAtomSource (DenseArrangementId sourceId) store view (mkAtomId atomKey)
    | (sourceId, atomKey) <- zip [0 ..] (IntMap.keys (jmAtomSchemas meta))
  ]
{-# INLINE denseSourcesFromStorageView #-}

denseAtomSource :: DenseArrangementId -> Store -> View -> AtomId -> DenseArrangement
denseAtomSource arrangementId store view atomId =
  let atomKey = atomIdKey atomId
   in case IntMap.lookup atomKey (storeRelations store) of
        Nothing ->
          AtomArrangement
            arrangementId
            atomId
            ( MaterializedAtomRows
                ( indexedRowArrangementWithRows
                    emptyRowSet
                    emptyRowSet
                    (indexedRowArrangementFromRows (relRows (emptyRelationForSlots [])))
                )
            )
        Just pr ->
          AtomArrangement
            arrangementId
            atomId
            ( MaterializedAtomRows
                ( indexedRowArrangementWithRows
                    (viewRows store view atomKey)
                    emptyRowSet
                    (indexedRowArrangementFromRows (relRows pr))
                )
            )
{-# INLINE denseAtomSource #-}

denseAtomSourceFromRows ::
  DenseArrangementId ->
  AtomId ->
  RowLayout ->
  [RowTupleKey] ->
  Either DenseArrangementPatchError DenseArrangement
denseAtomSourceFromRows arrangementId atomId schema rows = do
  rowCounts <- rowCountsFromRows schema rows
  indexedRows <- indexedRowsFromAtomRowCounts schema rowCounts
  pure
    ( AtomArrangement
        arrangementId
        atomId
        (MaterializedAtomRows (indexedRowArrangementFromRows indexedRows))
    )
{-# INLINE denseAtomSourceFromRows #-}

denseAtomSourceFromRowsById ::
  DenseArrangementId ->
  AtomId ->
  RowLayout ->
  IntMap RowTupleKey ->
  Either DenseArrangementPatchError DenseArrangement
denseAtomSourceFromRowsById arrangementId atomId schema rowsById = do
  indexedRows <- indexedRowsFromKeyedAtomRows schema rowsById
  pure
    ( AtomArrangement
        arrangementId
        atomId
        (MaterializedAtomRows (indexedRowArrangementFromRows indexedRows))
    )
{-# INLINE denseAtomSourceFromRowsById #-}

denseAtomSourceFromRowBlock ::
  DenseArrangementId ->
  AtomId ->
  RowBlock 'Canonical ->
  DenseArrangement
denseAtomSourceFromRowBlock arrangementId atomId rows =
  AtomArrangement
    arrangementId
    atomId
    (RowBlockAtomRows (wholeRowBlockAtomRows rows))
{-# INLINE denseAtomSourceFromRowBlock #-}

denseAtomSourceFromComposedRowBlocks ::
  DenseArrangementId ->
  AtomId ->
  RowBlock 'Canonical ->
  RowBlock 'Canonical ->
  Maybe (RowBlock 'Canonical) ->
  Either DenseArrangementPatchError DenseArrangement
denseAtomSourceFromComposedRowBlocks arrangementId atomId baseRows maskedRows maybeExtraRows = do
  validateSameRowBlockLayout baseRows maskedRows
  Foldable.traverse_ (validateSameRowBlockLayout baseRows) maybeExtraRows
  pure
    ( AtomArrangement
        arrangementId
        atomId
        (RowBlockAtomRows (composedRowBlockAtomRows baseRows maskedRows maybeExtraRows))
    )
{-# INLINE denseAtomSourceFromComposedRowBlocks #-}

validateSameRowBlockLayout ::
  RowBlock 'Canonical ->
  RowBlock 'Canonical ->
  Either DenseArrangementPatchError ()
validateSameRowBlockLayout expected actual =
  if RowBlock.rowBlockLayout expected == RowBlock.rowBlockLayout actual
    then Right ()
    else Left (DenseArrangementPatchRowBlockLayoutMismatch (RowBlock.rowBlockLayout expected) (RowBlock.rowBlockLayout actual))
{-# INLINE validateSameRowBlockLayout #-}

wholeRowBlockAtomRows :: RowBlock 'Canonical -> RowBlockAtomRowsView
wholeRowBlockAtomRows rows =
  rowBlockAtomRows rows Nothing (rowSetFullRange (RowBlock.rowBlockCount rows)) emptyRowSet
{-# INLINE wholeRowBlockAtomRows #-}

composedRowBlockAtomRows ::
  RowBlock 'Canonical ->
  RowBlock 'Canonical ->
  Maybe (RowBlock 'Canonical) ->
  RowBlockAtomRowsView
composedRowBlockAtomRows baseRows maskedRows maybeExtraRows =
  rowBlockAtomRows baseRows maybeExtraRows visibleRows emptyRowSet
  where
    baseVisibleRows =
      rowSetDifference
        (rowSetFullRange (RowBlock.rowBlockCount baseRows))
        (rowSetFromIntSetCanonical (RowBlock.rowBlockRowIndices baseRows maskedRows))

    visibleRows =
      maybe
        baseVisibleRows
        (rowSetUnion baseVisibleRows . extraRowRange baseRows)
        maybeExtraRows
{-# INLINE composedRowBlockAtomRows #-}

rowBlockAtomRows ::
  RowBlock 'Canonical ->
  Maybe (RowBlock 'Canonical) ->
  RowSet ->
  RowSet ->
  RowBlockAtomRowsView
rowBlockAtomRows baseRows maybeExtraRows visibleRows dirtyRows =
  RowBlockAtomRowsView
    { rbarBaseRows = baseRows,
      rbarExtraRows = maybeExtraRows,
      rbarVisibleRows = visibleRows,
      rbarDirtyRows = dirtyRows,
      rbarValueIndex = rowBlockAtomRowsValueIndex baseRows maybeExtraRows (rowBlockAtomRowsUniverse baseRows maybeExtraRows)
    }
{-# INLINE rowBlockAtomRows #-}

rowBlockAtomRowsUniverse :: RowBlock 'Canonical -> Maybe (RowBlock 'Canonical) -> RowSet
rowBlockAtomRowsUniverse baseRows =
  maybe
    (rowSetFullRange (RowBlock.rowBlockCount baseRows))
    (rowSetUnion (rowSetFullRange (RowBlock.rowBlockCount baseRows)) . extraRowRange baseRows)
{-# INLINE rowBlockAtomRowsUniverse #-}

extraRowRange :: RowBlock 'Canonical -> RowBlock 'Canonical -> RowSet
extraRowRange baseRows extraRows =
  rowSetFromIntSetCanonical $
    IntSet.fromDistinctAscList
      [baseCount .. baseCount + RowBlock.rowBlockCount extraRows - 1]
  where
    baseCount =
      RowBlock.rowBlockCount baseRows
{-# INLINE extraRowRange #-}

rowBlockAtomRowsValueIndex ::
  RowBlock 'Canonical ->
  Maybe (RowBlock 'Canonical) ->
  RowSet ->
  IntMap (IntMap RowIdSet)
rowBlockAtomRowsValueIndex baseRows maybeExtraRows visibleRows =
  rowSetFoldl'
    ( \index rowId ->
        case rowBlockAtomRowsRowSlots baseRows maybeExtraRows (rowIdInt rowId) of
          Nothing ->
            index
          Just slotValues ->
            rowSlotsValueIndexInsert (rowIdInt rowId) (RowBlock.rowBlockLayout baseRows) slotValues index
    )
    IntMap.empty
    visibleRows
{-# INLINE rowBlockAtomRowsValueIndex #-}

rowSlotsValueIndexInsert ::
  Int ->
  RowLayout ->
  VU.Vector Word64 ->
  IntMap (IntMap RowIdSet) ->
  IntMap (IntMap RowIdSet)
rowSlotsValueIndexInsert rowKey schema slotValues index0 =
  Vector.ifoldl'
    ( \index columnIx slot ->
        case slotValueAt columnIx of
          Nothing ->
            index
          Just repKey ->
            IntMap.insertWith
              (IntMap.unionWith rowIdSetUnion)
              (slotIdKey slot)
              (IntMap.singleton repKey (rowIdSetFromIntSetCanonical (IntSet.singleton rowKey)))
              index
    )
    index0
    schema
  where
    slotValueAt columnIx =
      if columnIx < 0 || columnIx >= VU.length slotValues
        then Nothing
        else Just (fromIntegral (slotValues VU.! columnIx))
{-# INLINE rowSlotsValueIndexInsert #-}

rowBlockAtomRowsRowSlots ::
  RowBlock 'Canonical ->
  Maybe (RowBlock 'Canonical) ->
  Int ->
  Maybe (VU.Vector Word64)
rowBlockAtomRowsRowSlots baseRows maybeExtraRows rowKey =
  if rowKey < baseCount
    then rowSlotsAt baseRows rowKey
    else maybeExtraRows >>= \extraRows -> rowSlotsAt extraRows (rowKey - baseCount)
  where
    baseCount =
      RowBlock.rowBlockCount baseRows
{-# INLINE rowBlockAtomRowsRowSlots #-}

rowSlotsAt :: RowBlock 'Canonical -> Int -> Maybe (VU.Vector Word64)
rowSlotsAt rows rowKey =
  RowBlock.rowSlots rows <$> RowBlock.rowBlockDescAt rows rowKey
{-# INLINE rowSlotsAt #-}

rowBlockAtomRowsKeyAt :: RowBlockAtomRowsView -> Int -> Maybe RowTupleKey
rowBlockAtomRowsKeyAt source rowKey =
  tupleKeyFromInts . fmap fromIntegral . VU.toList
    <$> rowBlockAtomRowsRowSlots (rbarBaseRows source) (rbarExtraRows source) rowKey
{-# INLINE rowBlockAtomRowsKeyAt #-}

rowBlockAtomRowsValueAt :: RowBlockAtomRowsView -> SlotId -> Int -> Maybe RepKey
rowBlockAtomRowsValueAt source slot rowKey = do
  columnIx <- IntMap.lookup (slotIdKey slot) (rowLayoutColumnIndex (RowBlock.rowBlockLayout (rbarBaseRows source)))
  slotValues <- rowBlockAtomRowsRowSlots (rbarBaseRows source) (rbarExtraRows source) rowKey
  if columnIx < 0 || columnIx >= VU.length slotValues
    then Nothing
    else Just (RepKey (fromIntegral (slotValues VU.! columnIx)))
{-# INLINE rowBlockAtomRowsValueAt #-}

rowBlockAtomRowsWithRows :: RowSet -> RowSet -> RowBlockAtomRowsView -> RowBlockAtomRowsView
rowBlockAtomRowsWithRows visibleRows dirtyRows source =
  source
    { rbarVisibleRows = visibleRows,
      rbarDirtyRows = dirtyRows
    }
{-# INLINE rowBlockAtomRowsWithRows #-}

rowBlockAtomRowsRestrictRowsByPins :: IntMap Int -> RowBlockAtomRowsView -> RowBlockAtomRowsView
rowBlockAtomRowsRestrictRowsByPins pins source =
  rowBlockAtomRowsWithRows
    (restrictRowsByPins columnIndex (rbarValueIndex source) pins (rbarVisibleRows source))
    (restrictRowsByPins columnIndex (rbarValueIndex source) pins (rbarDirtyRows source))
    source
  where
    columnIndex =
      rowLayoutColumnIndex (RowBlock.rowBlockLayout (rbarBaseRows source))
{-# INLINE rowBlockAtomRowsRestrictRowsByPins #-}

rowBlockAtomRowsRestrictRowsByAllowedValues ::
  IntMap (HashSet RepKey) ->
  RowBlockAtomRowsView ->
  RowBlockAtomRowsView
rowBlockAtomRowsRestrictRowsByAllowedValues allowed source =
  rowBlockAtomRowsWithRows
    (restrictRowsByAllowedValues columnIndex (rbarValueIndex source) allowed (rbarVisibleRows source))
    (restrictRowsByAllowedValues columnIndex (rbarValueIndex source) allowed (rbarDirtyRows source))
    source
  where
    columnIndex =
      rowLayoutColumnIndex (RowBlock.rowBlockLayout (rbarBaseRows source))
{-# INLINE rowBlockAtomRowsRestrictRowsByAllowedValues #-}

rowBlockAtomRowsRestrictToDirtyRows :: RowBlockAtomRowsView -> RowBlockAtomRowsView
rowBlockAtomRowsRestrictToDirtyRows source =
  rowBlockAtomRowsWithRows (rbarDirtyRows source) (rbarDirtyRows source) source
{-# INLINE rowBlockAtomRowsRestrictToDirtyRows #-}

rowBlockAtomRowsWithDirtyKeys ::
  Set.Set AssignmentTupleKey ->
  RowBlockAtomRowsView ->
  RowBlockAtomRowsView
rowBlockAtomRowsWithDirtyKeys dirtyKeys source =
  source
    { rbarDirtyRows =
        rowSetIntersection
          (rbarVisibleRows source)
          ( rowSetFromIntSetCanonical
              ( IntSet.fromList
                  [ rowKey
                    | dirtyKey <- Set.toAscList dirtyKeys,
                      rowKey <- rowBlockAtomRowsRowKeysForTuple (coerceTupleKey dirtyKey) source
                  ]
              )
          )
    }
{-# INLINE rowBlockAtomRowsWithDirtyKeys #-}

rowBlockAtomRowsRowKeysForTuple :: RowTupleKey -> RowBlockAtomRowsView -> [Int]
rowBlockAtomRowsRowKeysForTuple row source =
  [ rowKey
    | rowKey <- IntSet.toAscList (rowSetToIntSet (rbarVisibleRows source)),
      rowBlockAtomRowsKeyAt source rowKey == Just row
  ]
{-# INLINE rowBlockAtomRowsRowKeysForTuple #-}

denseProjectedAtomSourceFromRows ::
  DenseArrangementId ->
  AtomId ->
  RowLayout ->
  StalkRecipe ->
  DenseProjectedRows ->
  DenseArrangement
denseProjectedAtomSourceFromRows arrangementId atomId schema recipe physicalRows =
  AtomArrangement
    arrangementId
    atomId
    (ProjectedAtomRows (projectedAtomRows schema recipe physicalRows))
{-# INLINE denseProjectedAtomSourceFromRows #-}

denseProjectedRowsFromRows ::
  RowLayout ->
  [RowTupleKey] ->
  Either DenseArrangementPatchError DenseProjectedRows
denseProjectedRowsFromRows physicalSchema rows = do
  rowCounts <- rowCountsFromRows physicalSchema rows
  indexedRows <- indexedRowsFromAtomRowCounts physicalSchema rowCounts
  pure (indexedRowArrangementFromRows indexedRows)
{-# INLINE denseProjectedRowsFromRows #-}

denseProjectedRowsClearDirtyRows :: DenseProjectedRows -> DenseProjectedRows
denseProjectedRowsClearDirtyRows rows =
  indexedRowArrangementFromRowsWithSections
    (indexedRowArrangementRows rows)
    (indexedRowArrangementVisibleRows rows)
    emptyRowSet
{-# INLINE denseProjectedRowsClearDirtyRows #-}

denseDiagnosticValidRowsById :: RowLayout -> [RowTupleKey] -> Either DenseArrangementPatchError (IntMap RowTupleKey)
denseDiagnosticValidRowsById =
  validRowsById
{-# INLINE denseDiagnosticValidRowsById #-}

denseDiagnosticRowsValueIndex :: RowLayout -> IntMap RowTupleKey -> Either DenseArrangementPatchError (IntMap (IntMap RowIdSet))
denseDiagnosticRowsValueIndex =
  rowsValueIndex
{-# INLINE denseDiagnosticRowsValueIndex #-}

denseDiagnosticProjectedRowsFromValidRowsAndValueIndex ::
  RowLayout ->
  IntMap RowTupleKey ->
  IntMap (IntMap RowIdSet) ->
  Either DenseArrangementPatchError DenseProjectedRows
denseDiagnosticProjectedRowsFromValidRowsAndValueIndex schema rowsById valueIndex = do
  indexedRows <- indexedRowsFromKeyedAtomRowsWithValueIndex schema rowsById valueIndex
  pure (indexedRowArrangementFromRows indexedRows)
{-# INLINE denseDiagnosticProjectedRowsFromValidRowsAndValueIndex #-}

projectedAtomRows ::
  RowLayout ->
  StalkRecipe ->
  DenseProjectedRows ->
  ProjectedAtomRowsView
projectedAtomRows schema recipe physicalRows =
  let !slotSources =
        projectedSlotSources schema recipe
      !visibleRows =
        projectedCompatibleRows schema slotSources physicalRows
   in ProjectedAtomRowsView
        { pavSchema = schema,
          pavSlotSources = slotSources,
          pavPhysicalRows = physicalRows,
          pavVisibleRows = visibleRows,
          pavDirtyRows = emptyRowSet,
          pavValueIndex = projectedValueIndex schema slotSources physicalRows
        }
{-# INLINE projectedAtomRows #-}

projectedCompatibleRows ::
  RowLayout ->
  IntMap [Int] ->
  DenseProjectedRows ->
  RowSet
projectedCompatibleRows schema slotSources physicalRows =
  case traverse projectedSlotSourcesForSchema (Vector.toList schema) of
    Nothing ->
      emptyRowSet
    Just schemaSources
      | any null schemaSources ->
          emptyRowSet
      | all singleSource schemaSources ->
          indexedRowArrangementVisibleRows physicalRows
      | otherwise ->
          rowSetFromIntSetCanonical
            ( rowSetFoldl'
                insertCompatibleRow
                IntSet.empty
                (indexedRowArrangementVisibleRows physicalRows)
            )
      where
        insertCompatibleRow rows rowId
          | let !rowKey = rowIdInt rowId,
            all (projectedSourcesCompatible physicalRows rowKey) schemaSources =
              IntSet.insert rowKey rows
          | otherwise =
              rows
  where
    projectedSlotSourcesForSchema :: SlotId -> Maybe [Int]
    projectedSlotSourcesForSchema slot =
      IntMap.lookup (slotIdKey slot) slotSources

    singleSource :: [source] -> Bool
    singleSource sources =
      case sources of
        [_] ->
          True
        _ ->
          False
{-# INLINE projectedCompatibleRows #-}

projectedSourcesCompatible ::
  DenseProjectedRows ->
  Int ->
  [Int] ->
  Bool
projectedSourcesCompatible physicalRows rowId sources =
  case projectedColumnValueInRows physicalRows rowId sources of
    Nothing ->
      False
    Just _ ->
      True
{-# INLINE projectedSourcesCompatible #-}

projectedValueIndex ::
  RowLayout ->
  IntMap [Int] ->
  DenseProjectedRows ->
  IntMap (IntMap RowIdSet)
projectedValueIndex schema slotSources physicalRows =
  IntMap.fromList
    [ (slotIdKey slot, byRep)
      | slot <- Vector.toList schema,
        sourcePosition <- projectedSlotPrimarySource slot,
        byRep <- physicalSourceValueIndex sourcePosition
    ]
  where
    physicalValueIndex =
      indexedRowArrangementValueIndex physicalRows

    projectedSlotPrimarySource slot =
      case IntMap.lookup (slotIdKey slot) slotSources of
        Just (sourcePosition : _) ->
          [sourcePosition]
        _ ->
          []

    physicalSourceValueIndex sourcePosition =
      case IntMap.lookup sourcePosition physicalValueIndex of
        Nothing ->
          []
        Just byRep ->
          [byRep]
{-# INLINE projectedValueIndex #-}

projectedSlotSources :: RowLayout -> StalkRecipe -> IntMap [Int]
projectedSlotSources schema recipe =
  IntMap.fromList
    [ (slotIdKey slot, fmap physicalSourcePosition sources)
      | (slot, sources) <-
          zip
            (Vector.toList schema)
            (Vector.toList (stalkRecipeColumns recipe))
    ]
{-# INLINE projectedSlotSources #-}

denseArrangementWithDirtyKeys ::
  Set.Set AssignmentTupleKey ->
  DenseArrangement ->
  DenseArrangement
denseArrangementWithDirtyKeys dirtyKeys = \case
  AtomArrangement arrangementId atomId source ->
    AtomArrangement
      arrangementId
      atomId
      (atomRowsSourceWithDirtyKeys dirtyKeys source)
  FactorArrangement arrangementId arrangement ->
    FactorArrangement
      arrangementId
      (indexedRowArrangementWithDirtyKeys dirtyKeys arrangement)
{-# INLINE denseArrangementWithDirtyKeys #-}

atomRowsSourceWithDirtyKeys ::
  Set.Set AssignmentTupleKey ->
  AtomRowsSource ->
  AtomRowsSource
atomRowsSourceWithDirtyKeys dirtyKeys = \case
  MaterializedAtomRows arrangement ->
    MaterializedAtomRows (markRowsDirtyByKey dirtyKeys arrangement)
  ProjectedAtomRows projected ->
    ProjectedAtomRows
      projected
        { pavDirtyRows =
            rowSetIntersection
              (pavVisibleRows projected)
              ( rowSetFromIntSetCanonical
                  ( IntSet.fromList
                      [ rowKey
                        | dirtyKey <- Set.toAscList dirtyKeys,
                          rowKey <- projectedRowKeysForAssignment dirtyKey projected
                      ]
                  )
              )
        }
  RowBlockAtomRows source ->
    RowBlockAtomRows (rowBlockAtomRowsWithDirtyKeys dirtyKeys source)
{-# INLINE atomRowsSourceWithDirtyKeys #-}

markRowsDirtyByKey ::
  Set.Set AssignmentTupleKey ->
  IndexedRowArrangement RowLayout RowTupleKey Multiplicity ->
  IndexedRowArrangement RowLayout RowTupleKey Multiplicity
markRowsDirtyByKey dirtyKeys =
  indexedRowArrangementWithDirtyKeys
    (Set.fromList (fmap coerceTupleKey (Set.toAscList dirtyKeys)))
{-# INLINE markRowsDirtyByKey #-}

projectedRowKeysForAssignment :: AssignmentTupleKey -> ProjectedAtomRowsView -> [Int]
projectedRowKeysForAssignment key projected =
  case tupleKeyWidth key == Vector.length (pavSchema projected) of
    False ->
      []
    True ->
      rowSetFoldl'
        ( \rows checkedRowId ->
            let !rowKey = rowIdInt checkedRowId
             in case projectedAtomRowsKeyAt projected rowKey of
                  Just projectedKey
                    | projectedKey == coerceTupleKey key ->
                        rowKey : rows
                  _ ->
                    rows
        )
        []
        (pavVisibleRows projected)
{-# INLINE projectedRowKeysForAssignment #-}

denseFactorSource :: DenseArrangementId -> Factor -> DenseArrangement
denseFactorSource arrangementId factor =
  FactorArrangement
    arrangementId
    (indexedRowArrangementFromRows factor)
{-# INLINE denseFactorSource #-}

denseArrangementId :: DenseArrangement -> DenseArrangementId
denseArrangementId = \case
  AtomArrangement arrangementId _ _ ->
    arrangementId
  FactorArrangement arrangementId _ ->
    arrangementId
{-# INLINE denseArrangementId #-}

denseArrangementAtomId :: DenseArrangement -> Maybe AtomId
denseArrangementAtomId = \case
  AtomArrangement _ atomId _ ->
    Just atomId
  FactorArrangement {} ->
    Nothing
{-# INLINE denseArrangementAtomId #-}

denseArrangementSchema :: DenseArrangement -> [SlotId]
denseArrangementSchema = \case
  AtomArrangement _ _ source ->
    atomRowsSourceSchema source
  FactorArrangement _ arrangement ->
    Vector.toList (indexedRowArrangementLayout arrangement)
{-# INLINE denseArrangementSchema #-}

denseArrangementSchemaKeys :: DenseArrangement -> PrimArray Int
denseArrangementSchemaKeys =
  primArrayFromListStrict . fmap slotIdKey . denseArrangementSchema
{-# INLINE denseArrangementSchemaKeys #-}

denseArrangementColumnIndex :: DenseArrangement -> IntMap Int
denseArrangementColumnIndex = \case
  AtomArrangement _ _ source ->
    atomRowsSourceColumnIndex source
  FactorArrangement _ arrangement ->
    indexedRowArrangementColumnIndex arrangement
{-# INLINE denseArrangementColumnIndex #-}

denseArrangementRows :: DenseArrangement -> RowSet
denseArrangementRows = \case
  AtomArrangement _ _ source ->
    atomRowsSourceVisibleRows source
  FactorArrangement _ arrangement ->
    indexedRowArrangementVisibleRows arrangement
{-# INLINE denseArrangementRows #-}

denseArrangementDirtyRows :: DenseArrangement -> RowSet
denseArrangementDirtyRows = \case
  AtomArrangement _ _ source ->
    atomRowsSourceDirtyRows source
  FactorArrangement _ arrangement ->
    indexedRowArrangementDirtyRows arrangement
{-# INLINE denseArrangementDirtyRows #-}

denseArrangementKeyAt :: DenseArrangement -> Int -> Maybe AssignmentTupleKey
denseArrangementKeyAt source rowKey =
  case source of
    AtomArrangement _ _ atomSource ->
      coerceTupleKey <$> atomRowsSourceKeyAt atomSource rowKey
    FactorArrangement _ rowArrangement ->
      rowIdFromInt rowKey >>= \rowId ->
        indexedRowArrangementKeyAt rowArrangement rowId
{-# INLINE denseArrangementKeyAt #-}

denseArrangementValueIndex :: DenseArrangement -> IntMap (IntMap RowIdSet)
denseArrangementValueIndex = \case
  AtomArrangement _ _ source ->
    atomRowsSourceValueIndex source
  FactorArrangement _ arrangement ->
    indexedRowArrangementValueIndex arrangement
{-# INLINE denseArrangementValueIndex #-}

denseArrangementValueAt :: DenseArrangement -> SlotId -> Int -> Maybe RepKey
denseArrangementValueAt source sid rowKey =
  case source of
    AtomArrangement _ _ atomSource ->
      atomRowsSourceValueAt atomSource sid rowKey
    FactorArrangement _ rowArrangement ->
      rowIdFromInt rowKey >>= \rowId ->
        indexedTupleArrangementValueAt rowArrangement sid rowId
{-# INLINE denseArrangementValueAt #-}

denseArrangementPayloadAt :: DenseArrangement -> Int -> ProvArena -> (ProvArena, ProvVal)
denseArrangementPayloadAt source rowKey arena =
  case source of
    AtomArrangement _ atomId atomSource ->
      case atomRowsSourceKeyAt atomSource rowKey of
        Nothing -> (arena, PVZero)
        Just row -> pvAtom atomId row arena
    FactorArrangement _ rowArrangement ->
      case rowIdFromInt rowKey >>= \rowId -> indexedRowArrangementPayloadAt rowArrangement rowId of
        Nothing -> (arena, PVZero)
        Just val -> (arena, val)
{-# INLINE denseArrangementPayloadAt #-}

denseArrangementPayloadAtWithTelemetry ::
  RepairTelemetryConfig ->
  DenseArrangement ->
  Int ->
  ProvArena ->
  (ProvArena, ProvVal, RepairTelemetry)
denseArrangementPayloadAtWithTelemetry config source rowKey arena =
  case source of
    AtomArrangement _ atomId atomSource ->
      case atomRowsSourceKeyAt atomSource rowKey of
        Nothing ->
          (arena, PVZero, emptyRepairTelemetry)
        Just row ->
          pvAtomWithTelemetry config atomId row arena
    FactorArrangement _ rowArrangement ->
      case rowIdFromInt rowKey >>= \rowId -> indexedRowArrangementPayloadAt rowArrangement rowId of
        Nothing ->
          (arena, PVZero, emptyRepairTelemetry)
        Just val ->
          (arena, val, emptyRepairTelemetry)
{-# INLINE denseArrangementPayloadAtWithTelemetry #-}

denseArrangementUnionSchema :: [DenseArrangement] -> [SlotId]
denseArrangementUnionSchema arrangements =
  fmap mkSlotId
    . IntSet.toAscList
    . IntSet.unions
    $ [ IntSet.fromList (fmap slotIdKey (denseArrangementSchema arrangement))
        | arrangement <- arrangements
      ]
{-# INLINE denseArrangementUnionSchema #-}

denseArrangementDeltaJoinSource :: DenseArrangement -> DeltaJoinSource
denseArrangementDeltaJoinSource arrangement =
  DeltaJoinSource
    { deltaSourceRows = denseArrangementRows arrangement,
      deltaSourceDirtyRows = denseArrangementDirtyRows arrangement,
      deltaSourceValueIndex = denseArrangementValueIndex arrangement,
      deltaSourceValueAt = \slot rowId -> unRepKey <$> denseArrangementValueAt arrangement (mkSlotId slot) rowId
    }
{-# INLINE denseArrangementDeltaJoinSource #-}

denseApplyPinsToArrangement :: IntMap RepKey -> DenseArrangement -> DenseArrangement
denseApplyPinsToArrangement pins = \case
  AtomArrangement arrangementId atomId source ->
    AtomArrangement
      arrangementId
      atomId
      (atomRowsSourceRestrictRowsByPins (repKeyPins pins) source)
  FactorArrangement arrangementId arrangement ->
    FactorArrangement
      arrangementId
      (indexedRowArrangementRestrictRowsByPins (repKeyPins pins) arrangement)
{-# INLINE denseApplyPinsToArrangement #-}

denseRestrictArrangementBySlotValues ::
  IntMap (HashSet RepKey) ->
  DenseArrangement ->
  DenseArrangement
denseRestrictArrangementBySlotValues allowed = \case
  AtomArrangement arrangementId atomId source ->
    AtomArrangement
      arrangementId
      atomId
      (atomRowsSourceRestrictRowsByAllowedValues allowed source)
  FactorArrangement arrangementId arrangement ->
    FactorArrangement
      arrangementId
      (arrangementRestrictRowsByAllowedValues allowed arrangement)
{-# INLINE denseRestrictArrangementBySlotValues #-}

denseRestrictArrangementByPinnedRows ::
  IntMap (Maybe RowTupleKey) ->
  DenseArrangement ->
  DenseArrangement
denseRestrictArrangementByPinnedRows pinnedRows arrangement =
  case denseArrangementAtomId arrangement of
    Nothing ->
      arrangement
    Just atomId ->
      case IntMap.lookup (atomIdKey atomId) pinnedRows of
        Nothing ->
          arrangement
        Just Nothing ->
          denseArrangementWithRows emptyRowSet emptyRowSet arrangement
        Just (Just row) ->
          denseRestrictArrangementToAtomRow row arrangement
{-# INLINE denseRestrictArrangementByPinnedRows #-}

denseArrangementRestrictToDirtyRows :: DenseArrangement -> DenseArrangement
denseArrangementRestrictToDirtyRows = \case
  AtomArrangement arrangementId atomId source ->
    AtomArrangement
      arrangementId
      atomId
      (atomRowsSourceRestrictToDirtyRows source)
  FactorArrangement arrangementId arrangement ->
    FactorArrangement
      arrangementId
      (indexedRowArrangementRestrictToDirtyRows arrangement)
{-# INLINE denseArrangementRestrictToDirtyRows #-}

sourceBundleArrangement :: SourceBundle -> DenseArrangement
sourceBundleArrangement bundle =
  denseArrangementWithDirtyKeys
    (sbDirtyKeys bundle)
    (sbCurrent bundle)
{-# INLINE sourceBundleArrangement #-}

patchDenseAtomArrangement ::
  RowDelta ->
  DenseArrangement ->
  Either DenseArrangementPatchError DenseArrangement
patchDenseAtomArrangement rowDelta arrangement
  | plainRowPatchNull rowDelta =
      Right arrangement
  | otherwise =
      case arrangement of
        FactorArrangement arrangementId _ ->
          Left (DenseArrangementPatchNonAtomSource arrangementId)
        AtomArrangement arrangementId atomId source ->
          AtomArrangement arrangementId atomId
            <$> patchAtomRowsSource arrangementId rowDelta source
{-# INLINE patchDenseAtomArrangement #-}

patchDenseProjectedRows ::
  RowDelta ->
  DenseProjectedRows ->
  Either DenseArrangementPatchError DenseProjectedRows
patchDenseProjectedRows =
  patchIndexedAtomRowsArrangement
{-# INLINE patchDenseProjectedRows #-}

patchAtomRowsSource ::
  DenseArrangementId ->
  RowDelta ->
  AtomRowsSource ->
  Either DenseArrangementPatchError AtomRowsSource
patchAtomRowsSource arrangementId rowDelta =
  \case
    ProjectedAtomRows _ ->
      Left (DenseArrangementPatchProjectedSource arrangementId)
    RowBlockAtomRows _ ->
      Left (DenseArrangementPatchRowBlockSource arrangementId)
    MaterializedAtomRows arrangement ->
      MaterializedAtomRows <$> patchIndexedAtomRowsArrangement rowDelta arrangement
{-# INLINE patchAtomRowsSource #-}

type IndexedRowsPatchState :: Type
data IndexedRowsPatchState = IndexedRowsPatchState
  { irpsRows :: !(IndexedRows RowLayout RowTupleKey Multiplicity),
    irpsTouchedRows :: !IntSet
  }

patchIndexedAtomRowsArrangement ::
  RowDelta ->
  IndexedRowArrangement RowLayout RowTupleKey Multiplicity ->
  Either DenseArrangementPatchError (IndexedRowArrangement RowLayout RowTupleKey Multiplicity)
patchIndexedAtomRowsArrangement rowDelta arrangement = do
  patchState <-
    Map.foldlWithKey'
      patchIndexedRowsStep
      ( Right
          IndexedRowsPatchState
            { irpsRows = indexedRowArrangementRows arrangement,
              irpsTouchedRows = IntSet.empty
            }
      )
      (plainRowPatchChangeMap rowDelta)
  let patchedRows =
        irpsRows patchState
      liveRows =
        indexedRowsLiveRowSet patchedRows
      dirtyRows =
        rowSetFromIntSetCanonical
          (IntSet.intersection (irpsTouchedRows patchState) (indexedRowsLiveRows patchedRows))
  pure $
    indexedRowArrangementFromRowsWithSections
      patchedRows
      liveRows
      dirtyRows
{-# INLINE patchIndexedAtomRowsArrangement #-}

patchIndexedRowsStep ::
  Either DenseArrangementPatchError IndexedRowsPatchState ->
  RowTupleKey ->
  MultiplicityChange ->
  Either DenseArrangementPatchError IndexedRowsPatchState
patchIndexedRowsStep eitherState _row deltaMultiplicity
  | deltaMultiplicity == zeroMultiplicityChange =
      eitherState
patchIndexedRowsStep eitherState row deltaMultiplicity = do
  state <- eitherState
  let rows =
        irpsRows state
      expectedWidth =
        Vector.length (indexedRowsLayout rows)
  if tupleKeyWidth row /= expectedWidth
    then Left (DenseArrangementPatchRowWidthMismatch row expectedWidth (tupleKeyWidth row))
    else patchIndexedRowsByMultiplicity row deltaMultiplicity state
{-# INLINE patchIndexedRowsStep #-}

patchIndexedRowsByMultiplicity ::
  RowTupleKey ->
  MultiplicityChange ->
  IndexedRowsPatchState ->
  Either DenseArrangementPatchError IndexedRowsPatchState
patchIndexedRowsByMultiplicity row deltaMultiplicity state =
  case indexedRowsLookupId row (irpsRows state) of
    Nothing ->
      insertPatchedRow row deltaMultiplicity state
    Just rowId ->
      patchExistingIndexedRow row rowId deltaMultiplicity state
{-# INLINE patchIndexedRowsByMultiplicity #-}

insertPatchedRow ::
  RowTupleKey ->
  MultiplicityChange ->
  IndexedRowsPatchState ->
  Either DenseArrangementPatchError IndexedRowsPatchState
insertPatchedRow row deltaMultiplicity state =
  case positiveMultiplicityChange deltaMultiplicity of
    Nothing ->
      Left (DenseArrangementPatchMissingRowDelete row deltaMultiplicity)
    Just multiplicity ->
      case indexedRowsInsertFresh tupleKeyIndexedFormat row multiplicity (irpsRows state) of
        Left insertError ->
          Left (DenseArrangementPatchInsertFailed row insertError)
        Right (rowId, rows) ->
          let !rowKey =
                rowIdInt rowId
           in Right
                state
                  { irpsRows = rows,
                    irpsTouchedRows = IntSet.insert rowKey (irpsTouchedRows state)
                  }
{-# INLINE insertPatchedRow #-}

patchExistingIndexedRow ::
  RowTupleKey ->
  RowId ->
  MultiplicityChange ->
  IndexedRowsPatchState ->
  Either DenseArrangementPatchError IndexedRowsPatchState
patchExistingIndexedRow row rowId deltaMultiplicity state =
  case indexedRowsLookupPayload row (irpsRows state) of
    Nothing ->
      Left (DenseArrangementPatchMissingRowDelete row deltaMultiplicity)
    Just oldMultiplicity ->
      case applyMultiplicityChange oldMultiplicity deltaMultiplicity of
        Nothing ->
          Left (DenseArrangementPatchMultiplicityUnderflow row oldMultiplicity deltaMultiplicity)
        Just newMultiplicity ->
          if newMultiplicity == zeroMultiplicity
            then deletePatchedRow row rowId state
            else
              case indexedRowsSetPayload row newMultiplicity (irpsRows state) of
                Left payloadError ->
                  Left (DenseArrangementPatchPayloadUpdateFailed row payloadError)
                Right rows ->
                  let !rowKey =
                        rowIdInt rowId
                   in Right
                        state
                          { irpsRows = rows,
                            irpsTouchedRows = IntSet.insert rowKey (irpsTouchedRows state)
                          }
{-# INLINE patchExistingIndexedRow #-}

deletePatchedRow ::
  RowTupleKey ->
  RowId ->
  IndexedRowsPatchState ->
  Either DenseArrangementPatchError IndexedRowsPatchState
deletePatchedRow row rowId state =
  case indexedRowsDelete tupleKeyIndexedFormat row (irpsRows state) of
    Left deleteError ->
      Left (DenseArrangementPatchDeleteFailed row deleteError)
    Right (_deletedRowId, _oldPayload, rows) ->
      let !rowKey =
            rowIdInt rowId
       in Right
            state
              { irpsRows = rows,
                irpsTouchedRows = IntSet.insert rowKey (irpsTouchedRows state)
              }
{-# INLINE deletePatchedRow #-}

emptyRelationForSlots :: [SlotId] -> Relation
emptyRelationForSlots =
  emptyRelation . Vector.fromList

validRowsById :: RowLayout -> [RowTupleKey] -> Either DenseArrangementPatchError (IntMap RowTupleKey)
validRowsById schema rows =
  atomRowsByIdFromCounts <$> rowCountsFromRows schema rows
{-# INLINE validRowsById #-}

indexedRowsFromKeyedAtomRows ::
  RowLayout ->
  IntMap RowTupleKey ->
  Either DenseArrangementPatchError (IndexedRows RowLayout RowTupleKey Multiplicity)
indexedRowsFromKeyedAtomRows schema rowsById = do
  validateRowsById schema rowsById
  IntMap.foldlWithKey' insertKeyedRow (Right (emptyIndexedRows rowLayoutColumnIndex schema)) rowsById
  where
    insertKeyedRow eitherRows rowKey row = do
      rows <- eitherRows
      case mkRowId rowKey of
        Left rowIdError ->
          Left (DenseArrangementPatchInsertFailed row (IndexedRowsInsertInvalidRowId rowIdError))
        Right rowId ->
          case indexedRowsInsertWithId tupleKeyIndexedFormat rowId row (Multiplicity 1) rows of
            Right rows' ->
              Right rows'
            Left insertError ->
              Left (DenseArrangementPatchInsertFailed row insertError)
{-# INLINE indexedRowsFromKeyedAtomRows #-}

indexedRowsFromKeyedAtomRowsWithValueIndex ::
  RowLayout ->
  IntMap RowTupleKey ->
  IntMap (IntMap RowIdSet) ->
  Either DenseArrangementPatchError (IndexedRows RowLayout RowTupleKey Multiplicity)
indexedRowsFromKeyedAtomRowsWithValueIndex schema rowsById valueIndex = do
  rows <- indexedRowsFromKeyedAtomRows schema rowsById
  if indexedRowsValueIndex rows == valueIndex
    then Right rows
    else Left (DenseArrangementPatchRowsBuildFailed (IndexedRowsBuildValueIndexMismatch :| []))
{-# INLINE indexedRowsFromKeyedAtomRowsWithValueIndex #-}

indexedRowsFromAtomRowCounts ::
  RowLayout ->
  Map.Map RowTupleKey Multiplicity ->
  Either DenseArrangementPatchError (IndexedRows RowLayout RowTupleKey Multiplicity)
indexedRowsFromAtomRowCounts schema rowCounts =
  case indexedRowsFromPayloadMap tupleKeyIndexedFormat rowLayoutColumnIndex schema rowCounts of
    Right rows ->
      Right rows
    Left errors ->
      Left (DenseArrangementPatchRowsBuildFailed errors)
{-# INLINE indexedRowsFromAtomRowCounts #-}

indexedRowsFromAtomRowCountsWithValueIndex ::
  RowLayout ->
  Map.Map RowTupleKey Multiplicity ->
  IntMap (IntMap RowIdSet) ->
  Either DenseArrangementPatchError (IndexedRows RowLayout RowTupleKey Multiplicity)
indexedRowsFromAtomRowCountsWithValueIndex schema rowCounts valueIndex =
  case indexedRowsFromPayloadMapWithValueIndex tupleKeyIndexedFormat rowLayoutColumnIndex schema rowCounts valueIndex of
    Right rows ->
      Right rows
    Left errors ->
      Left (DenseArrangementPatchRowsBuildFailed errors)
{-# INLINE indexedRowsFromAtomRowCountsWithValueIndex #-}

rowCountsFromRows :: RowLayout -> [RowTupleKey] -> Either DenseArrangementPatchError (Map.Map RowTupleKey Multiplicity)
rowCountsFromRows schema =
  foldl' insertValidRow (Right Map.empty)
  where
    !expectedWidth =
      Vector.length schema

    insertValidRow eitherCounts row = do
      counts <- eitherCounts
      if tupleKeyWidth row == expectedWidth
        then Right (Map.insertWith addMultiplicity row (Multiplicity 1) counts)
        else Left (DenseArrangementPatchRowWidthMismatch row expectedWidth (tupleKeyWidth row))
{-# INLINE rowCountsFromRows #-}

validateRowsById :: RowLayout -> IntMap RowTupleKey -> Either DenseArrangementPatchError ()
validateRowsById schema =
  IntMap.foldlWithKey' validateRow (Right ())
  where
    !expectedWidth =
      Vector.length schema

    validateRow eitherValid rowKey row = do
      () <- eitherValid
      case mkRowId rowKey of
        Left rowIdError ->
          Left (DenseArrangementPatchInsertFailed row (IndexedRowsInsertInvalidRowId rowIdError))
        Right _ ->
          if tupleKeyWidth row == expectedWidth
            then Right ()
            else Left (DenseArrangementPatchRowWidthMismatch row expectedWidth (tupleKeyWidth row))
{-# INLINE validateRowsById #-}

atomRowsByIdFromCounts :: Map.Map RowTupleKey Multiplicity -> IntMap RowTupleKey
atomRowsByIdFromCounts rowCounts =
  IntMap.fromDistinctAscList
    [ (rowId, row)
      | (rowId, (row, _multiplicity)) <- zip [0 ..] (Map.toAscList rowCounts)
    ]
{-# INLINE atomRowsByIdFromCounts #-}

rowsValueIndex :: RowLayout -> IntMap RowTupleKey -> Either DenseArrangementPatchError (IntMap (IntMap RowIdSet))
rowsValueIndex schema rowsById = do
  validateRowsById schema rowsById
  pure
    ( fmap (fmap rowIdSetFromIntSetCanonical)
        (IntMap.foldlWithKey' insertRow IntMap.empty rowsById)
    )
  where
    insertRow index rowId row =
      Vector.ifoldl'
        ( \index' columnIx slot ->
            case tupleKeyIndexInt row columnIx of
              Nothing ->
                index'
              Just repKey ->
                insertSelectedIntSetBucket rowId (slotIdKey slot) repKey index'
        )
        index
        schema
{-# INLINE rowsValueIndex #-}

insertSelectedIntSetBucket ::
  Int ->
  Int ->
  Int ->
  IntMap (IntMap IntSet) ->
  IntMap (IntMap IntSet)
insertSelectedIntSetBucket rowId slotKey repKey =
  IntMap.alter (Just . insertRepBucket . maybe IntMap.empty id) slotKey
  where
    insertRepBucket =
      IntMap.alter (Just . IntSet.insert rowId . maybe IntSet.empty id) repKey
{-# INLINE insertSelectedIntSetBucket #-}

atomRowsSourceSchema :: AtomRowsSource -> [SlotId]
atomRowsSourceSchema = \case
  MaterializedAtomRows arrangement ->
    Vector.toList (indexedRowArrangementLayout arrangement)
  ProjectedAtomRows projected ->
    Vector.toList (pavSchema projected)
  RowBlockAtomRows source ->
    Vector.toList (RowBlock.rowBlockLayout (rbarBaseRows source))
{-# INLINE atomRowsSourceSchema #-}

atomRowsSourceColumnIndex :: AtomRowsSource -> IntMap Int
atomRowsSourceColumnIndex = \case
  MaterializedAtomRows arrangement ->
    indexedRowArrangementColumnIndex arrangement
  ProjectedAtomRows projected ->
    rowLayoutColumnIndex (pavSchema projected)
  RowBlockAtomRows source ->
    rowLayoutColumnIndex (RowBlock.rowBlockLayout (rbarBaseRows source))
{-# INLINE atomRowsSourceColumnIndex #-}

atomRowsSourceVisibleRows :: AtomRowsSource -> RowSet
atomRowsSourceVisibleRows = \case
  MaterializedAtomRows arrangement ->
    indexedRowArrangementVisibleRows arrangement
  ProjectedAtomRows projected ->
    pavVisibleRows projected
  RowBlockAtomRows source ->
    rbarVisibleRows source
{-# INLINE atomRowsSourceVisibleRows #-}

atomRowsSourceDirtyRows :: AtomRowsSource -> RowSet
atomRowsSourceDirtyRows = \case
  MaterializedAtomRows arrangement ->
    indexedRowArrangementDirtyRows arrangement
  ProjectedAtomRows projected ->
    pavDirtyRows projected
  RowBlockAtomRows source ->
    rbarDirtyRows source
{-# INLINE atomRowsSourceDirtyRows #-}

atomRowsSourceValueIndex :: AtomRowsSource -> IntMap (IntMap RowIdSet)
atomRowsSourceValueIndex = \case
  MaterializedAtomRows arrangement ->
    indexedRowArrangementValueIndex arrangement
  ProjectedAtomRows projected ->
    pavValueIndex projected
  RowBlockAtomRows source ->
    rbarValueIndex source
{-# INLINE atomRowsSourceValueIndex #-}

atomRowsSourceKeyAt :: AtomRowsSource -> Int -> Maybe RowTupleKey
atomRowsSourceKeyAt source rowKey =
  case source of
    MaterializedAtomRows arrangement ->
      rowIdFromInt rowKey >>= indexedRowArrangementKeyAt arrangement
    ProjectedAtomRows projected ->
      projectedAtomRowsKeyAt projected rowKey
    RowBlockAtomRows rowBlockRows ->
      rowBlockAtomRowsKeyAt rowBlockRows rowKey
{-# INLINE atomRowsSourceKeyAt #-}

atomRowsSourceValueAt :: AtomRowsSource -> SlotId -> Int -> Maybe RepKey
atomRowsSourceValueAt source sid rowKey =
  case source of
    MaterializedAtomRows arrangement ->
      rowIdFromInt rowKey >>= indexedTupleArrangementValueAt arrangement sid
    ProjectedAtomRows projected ->
      projectedAtomRowsValueAt projected sid rowKey
    RowBlockAtomRows rowBlockRows ->
      rowBlockAtomRowsValueAt rowBlockRows sid rowKey
{-# INLINE atomRowsSourceValueAt #-}

atomRowsSourceRestrictRowsByPins :: IntMap Int -> AtomRowsSource -> AtomRowsSource
atomRowsSourceRestrictRowsByPins pins = \case
  MaterializedAtomRows arrangement ->
    MaterializedAtomRows (indexedRowArrangementRestrictRowsByPins pins arrangement)
  ProjectedAtomRows projected ->
    ProjectedAtomRows (projectedAtomRowsRestrictRowsByPins pins projected)
  RowBlockAtomRows rowBlockRows ->
    RowBlockAtomRows (rowBlockAtomRowsRestrictRowsByPins pins rowBlockRows)
{-# INLINE atomRowsSourceRestrictRowsByPins #-}

atomRowsSourceRestrictRowsByAllowedValues ::
  IntMap (HashSet RepKey) ->
  AtomRowsSource ->
  AtomRowsSource
atomRowsSourceRestrictRowsByAllowedValues allowed = \case
  MaterializedAtomRows arrangement ->
    MaterializedAtomRows (arrangementRestrictRowsByAllowedValues allowed arrangement)
  ProjectedAtomRows projected ->
    ProjectedAtomRows (projectedAtomRowsRestrictRowsByAllowedValues allowed projected)
  RowBlockAtomRows rowBlockRows ->
    RowBlockAtomRows (rowBlockAtomRowsRestrictRowsByAllowedValues allowed rowBlockRows)
{-# INLINE atomRowsSourceRestrictRowsByAllowedValues #-}

atomRowsSourceRestrictToDirtyRows :: AtomRowsSource -> AtomRowsSource
atomRowsSourceRestrictToDirtyRows = \case
  MaterializedAtomRows arrangement ->
    MaterializedAtomRows (indexedRowArrangementRestrictToDirtyRows arrangement)
  ProjectedAtomRows projected ->
    ProjectedAtomRows
      projected
        { pavVisibleRows = pavDirtyRows projected,
          pavDirtyRows = pavDirtyRows projected
        }
  RowBlockAtomRows rowBlockRows ->
    RowBlockAtomRows (rowBlockAtomRowsRestrictToDirtyRows rowBlockRows)
{-# INLINE atomRowsSourceRestrictToDirtyRows #-}

arrangementRestrictRowsByAllowedValues ::
  IntMap (HashSet RepKey) ->
  IndexedRowArrangement RowLayout key payload ->
  IndexedRowArrangement RowLayout key payload
arrangementRestrictRowsByAllowedValues allowed arrangement =
  indexedRowArrangementWithRows
    ( restrictRowsByAllowedValues
        (indexedRowArrangementColumnIndex arrangement)
        (indexedRowArrangementValueIndex arrangement)
        allowed
        (indexedRowArrangementVisibleRows arrangement)
    )
    ( restrictRowsByAllowedValues
        (indexedRowArrangementColumnIndex arrangement)
        (indexedRowArrangementValueIndex arrangement)
        allowed
        (indexedRowArrangementDirtyRows arrangement)
    )
    arrangement
{-# INLINE arrangementRestrictRowsByAllowedValues #-}

restrictRowsByAllowedValues ::
  IntMap Int ->
  IntMap (IntMap RowIdSet) ->
  IntMap (HashSet RepKey) ->
  RowSet ->
  RowSet
restrictRowsByAllowedValues columnIndex valueIndex allowed rows0 =
  IntMap.foldlWithKey'
    restrictSlot
    rows0
    allowed
  where
    restrictSlot rows slotKey allowedReps
      | IntMap.notMember slotKey columnIndex =
          rows
      | HashSet.null allowedReps =
          emptyRowSet
      | otherwise =
          case allowedRepBucket valueIndex slotKey allowedReps of
            Nothing ->
              emptyRowSet
            Just bucket ->
              rowSetIntersectionWithRowIdSet bucket rows
{-# INLINE restrictRowsByAllowedValues #-}

allowedRepBucket ::
  IntMap (IntMap RowIdSet) ->
  Int ->
  HashSet RepKey ->
  Maybe RowIdSet
allowedRepBucket valueIndex slotKey =
  HashSet.foldl' insertAllowed Nothing
  where
    byRep =
      IntMap.findWithDefault IntMap.empty slotKey valueIndex

    insertAllowed maybeBucket (RepKey repKey) =
      case IntMap.lookup repKey byRep of
        Nothing ->
          maybeBucket
        Just bucket ->
          Just $
            maybe
              bucket
              (`rowIdSetUnion` bucket)
              maybeBucket
{-# INLINE allowedRepBucket #-}

denseRestrictArrangementToAtomRow :: RowTupleKey -> DenseArrangement -> DenseArrangement
denseRestrictArrangementToAtomRow row arrangement =
  case tupleKeyWidth row == length (denseArrangementSchema arrangement) of
    False ->
      denseArrangementWithRows emptyRowSet emptyRowSet arrangement
    True ->
      denseRestrictArrangementBySlotValues
        ( IntMap.fromList
            [ (slotIdKey slot, HashSet.singleton repKey)
              | (slot, repKey) <- zip (denseArrangementSchema arrangement) (tupleKeyToRepKeys row)
            ]
        )
        arrangement
{-# INLINE denseRestrictArrangementToAtomRow #-}

denseArrangementWithRows :: RowSet -> RowSet -> DenseArrangement -> DenseArrangement
denseArrangementWithRows visibleRows dirtyRows = \case
  AtomArrangement arrangementId atomId source ->
    AtomArrangement arrangementId atomId (atomRowsSourceWithRows visibleRows dirtyRows source)
  FactorArrangement arrangementId arrangement ->
    FactorArrangement arrangementId (indexedRowArrangementWithRows visibleRows dirtyRows arrangement)
{-# INLINE denseArrangementWithRows #-}

denseArrangementClearDirtyRows :: DenseArrangement -> DenseArrangement
denseArrangementClearDirtyRows arrangement =
  denseArrangementWithRows (denseArrangementRows arrangement) emptyRowSet arrangement
{-# INLINE denseArrangementClearDirtyRows #-}

atomRowsSourceWithRows :: RowSet -> RowSet -> AtomRowsSource -> AtomRowsSource
atomRowsSourceWithRows visibleRows dirtyRows = \case
  MaterializedAtomRows arrangement ->
    MaterializedAtomRows (indexedRowArrangementWithRows visibleRows dirtyRows arrangement)
  ProjectedAtomRows projected ->
    ProjectedAtomRows
      projected
        { pavVisibleRows = visibleRows,
          pavDirtyRows = dirtyRows
        }
  RowBlockAtomRows rowBlockRows ->
    RowBlockAtomRows (rowBlockAtomRowsWithRows visibleRows dirtyRows rowBlockRows)
{-# INLINE atomRowsSourceWithRows #-}

projectedAtomRowsKeyAt :: ProjectedAtomRowsView -> Int -> Maybe RowTupleKey
projectedAtomRowsKeyAt projected rowId =
  tupleKeyFromRepKeys
    <$> traverse
      (\slot -> projectedAtomRowsValueAt projected slot rowId)
      (Vector.toList (pavSchema projected))
{-# INLINE projectedAtomRowsKeyAt #-}

projectedAtomRowsValueAt :: ProjectedAtomRowsView -> SlotId -> Int -> Maybe RepKey
projectedAtomRowsValueAt projected slot rowId = do
  sources <- IntMap.lookup (slotIdKey slot) (pavSlotSources projected)
  projectedColumnValueInRows (pavPhysicalRows projected) rowId sources
{-# INLINE projectedAtomRowsValueAt #-}

projectedColumnValueInRows ::
  DenseProjectedRows ->
  Int ->
  [Int] ->
  Maybe RepKey
projectedColumnValueInRows physicalRows rowKey sources =
  case sources of
    [] ->
      Nothing
    sourcePosition : rest -> do
      rowId <- rowIdFromInt rowKey
      value <- indexedTupleArrangementValueAt physicalRows (mkSlotId sourcePosition) rowId
      let matches =
            all
              ( \restPosition ->
                  indexedTupleArrangementValueAt physicalRows (mkSlotId restPosition) rowId == Just value
              )
              rest
      if matches
        then Just value
        else Nothing
{-# INLINE projectedColumnValueInRows #-}

projectedAtomRowsRestrictRowsByPins ::
  IntMap Int ->
  ProjectedAtomRowsView ->
  ProjectedAtomRowsView
projectedAtomRowsRestrictRowsByPins pins projected =
  projected
    { pavVisibleRows =
        restrictRowsByPins
          (rowLayoutColumnIndex (pavSchema projected))
          (pavValueIndex projected)
          pins
          (pavVisibleRows projected),
      pavDirtyRows =
        restrictRowsByPins
          (rowLayoutColumnIndex (pavSchema projected))
          (pavValueIndex projected)
          pins
          (pavDirtyRows projected)
    }
{-# INLINE projectedAtomRowsRestrictRowsByPins #-}

projectedAtomRowsRestrictRowsByAllowedValues ::
  IntMap (HashSet RepKey) ->
  ProjectedAtomRowsView ->
  ProjectedAtomRowsView
projectedAtomRowsRestrictRowsByAllowedValues allowed projected =
  projected
    { pavVisibleRows =
        restrictRowsByAllowedValues
          (rowLayoutColumnIndex (pavSchema projected))
          (pavValueIndex projected)
          allowed
          (pavVisibleRows projected),
      pavDirtyRows =
        restrictRowsByAllowedValues
          (rowLayoutColumnIndex (pavSchema projected))
          (pavValueIndex projected)
          allowed
          (pavDirtyRows projected)
    }
{-# INLINE projectedAtomRowsRestrictRowsByAllowedValues #-}

restrictRowsByPins ::
  IntMap Int ->
  IntMap (IntMap RowIdSet) ->
  IntMap Int ->
  RowSet ->
  RowSet
restrictRowsByPins columnIndex valueIndex pins rows0 =
  IntMap.foldlWithKey'
    restrictSlot
    rows0
    pins
  where
    restrictSlot rows slotKey repKey
      | IntMap.notMember slotKey columnIndex =
          rows
      | otherwise =
          case IntMap.lookup slotKey valueIndex >>= IntMap.lookup repKey of
            Nothing ->
              emptyRowSet
            Just bucket ->
              rowSetIntersectionWithRowIdSet bucket rows
{-# INLINE restrictRowsByPins #-}

physicalSourcePosition :: SlotSource -> Int
physicalSourcePosition = \case
  SourceResult ->
    0
  SourceChild childIndex ->
    childIndex + 1
{-# INLINE physicalSourcePosition #-}
