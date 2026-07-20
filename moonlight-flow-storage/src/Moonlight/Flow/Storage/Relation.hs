{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Storage.Relation
  ( JoinEnv,
    Relation (..),
    RowIdDelta (..),
    emptyRowIdDelta,
    normalizeRowIdDelta,
    rowIdDeltaNull,
    RelationPatchError (..),
    RelationEpoch (..),
    relationEpochDigestWords,
    emptyRelation,
    relationFromRows,
    relationFromTupleRows,
    atomRowsFromTupleKeys,
    atomRowsToTupleKeys,
    materializeAtomRow,
    relationFromAtomRows,
    relationFromKeyedRows,
    rowPatchFromAtomRows,
    applyRelationPatch,
    applyRelationPatchTracked,
    relationRows,
    relationLayout,
    relationSupportRows,
    rowForId,
    rowIdForRow,
    rowContentHash,
    rowMultiplicityHash,
    rowMatchesRelation,
    filterRowsByEnv,
    slotValuesFromFeasible,
    candidateRowIds,
    candidateValues,
    relationEpoch,
  )
where

import Data.Bits (xor)
import Data.Foldable qualified as Foldable
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import Moonlight.Core
  ( SlotId,
    slotIdKey,
  )
import Moonlight.Flow.Internal.Digest
  ( mix64,
    wordOfInt,
  )
import Moonlight.Delta.Signed
  ( Multiplicity (..),
    MultiplicityChange (..),
    addMultiplicity,
    applyMultiplicityChange,
    multiplicityValue,
    positiveMultiplicityChange,
    zeroMultiplicity,
    zeroMultiplicityChange
  )
import Moonlight.Differential.Row.Patch
  ( PlainRowPatch,
    plainRowPatchChangeMap,
    plainRowPatchFromList,
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Differential.Index.IndexedRows
  ( IndexedRows,
    IndexedRowsBuildError,
    IndexedRowsDeleteError,
    IndexedRowsInsertError (..),
    IndexedRowsPayloadError,
    indexedRowsColumnIndex,
    indexedRowsDelete,
    indexedRowsFromPayloadMap,
    indexedRowsInsertFresh,
    indexedRowsInsertWithId,
    indexedRowsKeyAt,
    indexedRowsLiveRowSet,
    indexedRowsLookupId,
    indexedRowsLookupPayload,
    indexedRowsNextRowId,
    indexedRowsPayloadMap,
    indexedRowsLayout,
    indexedRowsSetPayload,
    indexedRowsValueIndex,
  )
import Moonlight.Differential.Index.RowId
  ( RowId,
    RowIdCursor,
    mkRowId,
    rowIdCursorExclusiveUniverse,
    rowIdInt,
  )
import Moonlight.Flow.Storage.Index.TupleFormat
  ( emptyIndexedRows,
    rowLayoutColumnIndex,
    tupleKeyIndexedFormat,
  )
import Moonlight.Differential.Index.RowSet
  ( RowSet,
    emptyRowSet,
    rowSetIntersectionWithRowIdSet,
    rowSetIntersectsRowIdSet,
    rowSetSize,
  )
import Moonlight.Differential.Row.Block
  ( RowDesc,
    RowBlock,
    RowBlockIdentity,
    RowBuildError (..),
    RowLayout,
    RowState (Canonical),
    containsRow,
    foldRowBlock,
    fromSlotRows,
    rowSlots,
    rowBlockLayout,
  )

type JoinEnv :: Type
type JoinEnv = IntMap RepKey

type Relation :: Type
data Relation = Relation
  { relRows :: !(IndexedRows RowLayout RowTupleKey Multiplicity),
    relSupportHash :: {-# UNPACK #-} !Word64,
    relMultiplicityHash :: {-# UNPACK #-} !Word64
  }
  deriving stock (Eq, Show)

type RowIdDelta :: Type -> Type
data RowIdDelta row = RowIdDelta
  { ridInserted :: !(IntMap row),
    ridDeleted :: !(IntMap row)
  }
  deriving stock (Eq, Show)

emptyRowIdDelta :: RowIdDelta row
emptyRowIdDelta =
  RowIdDelta
    { ridInserted = IntMap.empty,
      ridDeleted = IntMap.empty
    }
{-# INLINE emptyRowIdDelta #-}

normalizeRowIdDelta :: Eq row => RowIdDelta row -> RowIdDelta row
normalizeRowIdDelta delta =
  RowIdDelta
    { ridInserted =
        IntMap.differenceWith
          keepInserted
          (ridInserted delta)
          (ridDeleted delta),
      ridDeleted =
        IntMap.differenceWith
          keepDeleted
          (ridDeleted delta)
          (ridInserted delta)
    }
  where
    keepInserted :: Eq candidate => candidate -> candidate -> Maybe candidate
    keepInserted inserted deleted =
      if inserted == deleted
        then Nothing
        else Just inserted

    keepDeleted :: Eq candidate => candidate -> candidate -> Maybe candidate
    keepDeleted deleted inserted =
      if inserted == deleted
        then Nothing
        else Just deleted
{-# INLINE normalizeRowIdDelta #-}

rowIdDeltaNull :: Eq row => RowIdDelta row -> Bool
rowIdDeltaNull delta =
  IntMap.null (ridInserted normalizedDelta)
    && IntMap.null (ridDeleted normalizedDelta)
  where
    normalizedDelta =
      normalizeRowIdDelta delta
{-# INLINE rowIdDeltaNull #-}

type RelationPatchError :: Type
data RelationPatchError
  = RelationPatchRowWidthMismatch !RowTupleKey !Int !Int
  | RelationPatchInitialNegativeMultiplicity !RowTupleKey !Multiplicity
  | RelationPatchMissingRowDelete !RowTupleKey !MultiplicityChange
  | RelationPatchMultiplicityUnderflow !RowTupleKey !Multiplicity !MultiplicityChange
  | RelationPatchIndexedRowsBuildFailed !(NonEmpty (IndexedRowsBuildError RowLayout RowTupleKey))
  | RelationPatchInsertFailed !RowTupleKey !(IndexedRowsInsertError RowLayout RowTupleKey)
  | RelationPatchDeleteFailed !RowTupleKey !(IndexedRowsDeleteError RowLayout RowTupleKey)
  | RelationPatchPayloadUpdateFailed !RowTupleKey !(IndexedRowsPayloadError RowTupleKey)
  deriving stock (Eq, Show)

type RelationEpoch :: Type
data RelationEpoch = RelationEpoch
  { reRowIdCursor :: !RowIdCursor,
    reLiveRows :: {-# UNPACK #-} !Int,
    reSupportHash :: {-# UNPACK #-} !Word64,
    reMultiplicityHash :: {-# UNPACK #-} !Word64
  }
  deriving stock (Eq, Ord, Show)

relationEpochDigestWords :: RelationEpoch -> [Word64]
relationEpochDigestWords epoch =
  [ wordOfInt (rowIdCursorExclusiveUniverse (reRowIdCursor epoch)),
    wordOfInt (reLiveRows epoch),
    reSupportHash epoch,
    reMultiplicityHash epoch
  ]
{-# INLINE relationEpochDigestWords #-}

emptyRelation :: RowLayout -> Relation
emptyRelation schema =
  Relation
    { relRows = emptyIndexedRows schema,
      relSupportHash = 0,
      relMultiplicityHash = 0
    }
{-# INLINE emptyRelation #-}

relationLayout :: Relation -> RowLayout
relationLayout =
  indexedRowsLayout . relRows
{-# INLINE relationLayout #-}

relationRows :: Relation -> Map RowTupleKey Multiplicity
relationRows =
  indexedRowsPayloadMap . relRows
{-# INLINE relationRows #-}

rowContentHash :: RowId -> RowTupleKey -> Word64
rowContentHash rowId =
  rowContentHashInt (rowIdInt rowId)
{-# INLINE rowContentHash #-}

rowContentHashInt :: Int -> RowTupleKey -> Word64
rowContentHashInt rid row =
  tupleKeyFoldlInts'
    (\acc value -> mix64 acc (wordOfInt value))
    (mix64 0x9e3779b97f4a7c15 (wordOfInt rid))
    row
{-# INLINE rowContentHashInt #-}

rowMultiplicityHash :: RowId -> RowTupleKey -> Multiplicity -> Word64
rowMultiplicityHash rowId row multiplicity =
  rowMultiplicityHashInt (rowIdInt rowId) row multiplicity
{-# INLINE rowMultiplicityHash #-}

rowMultiplicityHashInt :: Int -> RowTupleKey -> Multiplicity -> Word64
rowMultiplicityHashInt rowId row multiplicity =
  mix64
    (rowContentHashInt rowId row)
    (fromIntegral (multiplicityValue multiplicity))
{-# INLINE rowMultiplicityHashInt #-}

rowForId :: Relation -> RowId -> Maybe RowTupleKey
rowForId relation rid =
  indexedRowsKeyAt rid (relRows relation)
{-# INLINE rowForId #-}

rowIdForRow :: Relation -> RowTupleKey -> Maybe RowId
rowIdForRow relation row =
  indexedRowsLookupId row (relRows relation)
{-# INLINE rowIdForRow #-}

rowMatchesRelation :: Relation -> RowTupleKey -> Bool
rowMatchesRelation relation row =
  tupleKeyWidth row == Vector.length (relationLayout relation)
{-# INLINE rowMatchesRelation #-}

relationEpoch :: Relation -> RelationEpoch
relationEpoch relation =
  RelationEpoch
    { reRowIdCursor = indexedRowsNextRowId (relRows relation),
      reLiveRows = rowSetSize (indexedRowsLiveRowSet (relRows relation)),
      reSupportHash = relSupportHash relation,
      reMultiplicityHash = relMultiplicityHash relation
    }
{-# INLINE relationEpoch #-}

relationFromRows ::
  RowLayout ->
  Map RowTupleKey Multiplicity ->
  Either RelationPatchError Relation
relationFromRows schema rowCounts = do
  liveRows <- traverseValidRelationRows rowCounts
  relationFromLiveRowCounts schema liveRows
  where
    !expectedWidth =
      Vector.length schema

    traverseValidRelationRows =
      Map.foldlWithKey' insertLiveRow (Right Map.empty)

    insertLiveRow eitherRows row multiplicity = do
      rows <- eitherRows
      if multiplicity == zeroMultiplicity
        then Right rows
        else
          if tupleKeyWidth row /= expectedWidth
            then Left (RelationPatchRowWidthMismatch row expectedWidth (tupleKeyWidth row))
            else Right (Map.insert row multiplicity rows)
{-# INLINE relationFromRows #-}

relationFromTupleRows ::
  RowLayout ->
  [RowTupleKey] ->
  Either RelationPatchError Relation
relationFromTupleRows schema rows =
  relationFromRows
    schema
    (Map.fromListWith addMultiplicity (fmap tupleRowMultiplicity rows))
  where
    tupleRowMultiplicity :: RowTupleKey -> (RowTupleKey, Multiplicity)
    tupleRowMultiplicity row =
      (row, Multiplicity 1)
{-# INLINE relationFromTupleRows #-}

atomRowsFromTupleKeys :: Foldable f => RowBlockIdentity -> RowLayout -> f RowTupleKey -> Either RowBuildError (RowBlock 'Canonical)
atomRowsFromTupleKeys identityValue schemaValue rows =
  fromSlotRows identityValue schemaValue =<< traverse tupleRowToSlots (Foldable.toList rows)
  where
    tupleRowToSlots :: RowTupleKey -> Either RowBuildError (VU.Vector Word64)
    tupleRowToSlots rowValue =
      case tupleKeyToWord64Vector rowValue of
        Left (NegativeRepKey rawKey) ->
          Left (RowNegativeSlotValue rawKey)
        Right slotValues ->
          Right slotValues
{-# INLINE atomRowsFromTupleKeys #-}

atomRowsToTupleKeys :: RowBlock state -> [RowTupleKey]
atomRowsToTupleKeys rows =
  foldRowBlock
    (\acc desc -> materializeAtomRow rows desc : acc)
    []
    rows
{-# INLINE atomRowsToTupleKeys #-}

materializeAtomRow :: RowBlock state -> RowDesc -> RowTupleKey
materializeAtomRow rows =
  tupleKeyFromInts . fmap fromIntegral . VU.toList . rowSlots rows
{-# INLINE materializeAtomRow #-}

relationFromAtomRows :: RowBlock 'Canonical -> Either RelationPatchError Relation
relationFromAtomRows relation =
  let rowCounts =
        foldRowBlock
          ( \counts desc ->
              Map.insertWith
                addMultiplicity
                (materializeAtomRow relation desc)
                (Multiplicity 1)
                counts
          )
          Map.empty
          relation
   in relationFromRows (rowBlockLayout relation) rowCounts
{-# INLINE relationFromAtomRows #-}

relationFromLiveRowCounts :: RowLayout -> Map RowTupleKey Multiplicity -> Either RelationPatchError Relation
relationFromLiveRowCounts schema liveRows =
  case indexedRowsFromPayloadMap tupleKeyIndexedFormat rowLayoutColumnIndex schema liveRows of
    Left errors ->
      Left (RelationPatchIndexedRowsBuildFailed errors)
    Right indexedRows ->
      Right
        Relation
        { relRows = indexedRows,
          relSupportHash = supportHash,
          relMultiplicityHash = multiplicityHash
        }
  where
    !(supportHash, multiplicityHash) =
      relationHashesForRows liveRows
{-# INLINE relationFromLiveRowCounts #-}

relationHashesForRows :: Map RowTupleKey Multiplicity -> (Word64, Word64)
relationHashesForRows =
  foldl'
    ( \(supportHash, multiplicityHash) (rowKey, (row, multiplicity)) ->
        ( supportHash `xor` rowContentHashInt rowKey row,
          multiplicityHash `xor` rowMultiplicityHashInt rowKey row multiplicity
        )
    )
    (0, 0)
    . zip [0 ..]
    . Map.toAscList
{-# INLINE relationHashesForRows #-}

relationFromKeyedRows :: RowLayout -> [(Int, RowTupleKey)] -> Either RelationPatchError Relation
relationFromKeyedRows columns keyedRows =
  foldl'
    insertKeyedRow
    (Right (emptyRelation columns))
    keyedRows
  where
    !expectedWidth =
      Vector.length columns

    insertKeyedRow ::
      Either RelationPatchError Relation ->
      (Int, RowTupleKey) ->
      Either RelationPatchError Relation
    insertKeyedRow eitherRelation (!rowKey, !row) = do
      relation <- eitherRelation
      if tupleKeyWidth row /= expectedWidth
        then Left (RelationPatchRowWidthMismatch row expectedWidth (tupleKeyWidth row))
        else
          case mkRowId rowKey of
            Left rowIdError ->
              Left (RelationPatchInsertFailed row (IndexedRowsInsertInvalidRowId rowIdError))
            Right rowId ->
              insertWithRowId relation rowId row (Multiplicity 1)
{-# INLINE relationFromKeyedRows #-}

insertWithRowId ::
  Relation ->
  RowId ->
  RowTupleKey ->
  Multiplicity ->
  Either RelationPatchError Relation
insertWithRowId !relation !rowId !row !multiplicity =
  case indexedRowsInsertWithId tupleKeyIndexedFormat rowId row multiplicity (relRows relation) of
    Left insertError ->
      Left (RelationPatchInsertFailed row insertError)
    Right rows' ->
      Right
        relation
          { relRows = rows',
            relSupportHash =
              relSupportHash relation `xor` rowContentHash rowId row,
            relMultiplicityHash =
              relMultiplicityHash relation
                `xor` rowMultiplicityHash rowId row multiplicity
          }
{-# INLINE insertWithRowId #-}

rowPatchFromAtomRows :: RowBlock 'Canonical -> RowBlock 'Canonical -> PlainRowPatch RowTupleKey
rowPatchFromAtomRows oldRows newRows =
  plainRowPatchFromList $
    let removed =
          foldRowBlock
            ( \delta desc ->
                if containsRow newRows oldRows desc
                  then delta
                else (materializeAtomRow oldRows desc, MultiplicityChange (-1)) : delta
            )
            []
            oldRows
     in foldRowBlock
          ( \delta desc ->
              if containsRow oldRows newRows desc
                then delta
                else (materializeAtomRow newRows desc, MultiplicityChange 1) : delta
          )
          removed
          newRows
{-# INLINE rowPatchFromAtomRows #-}


rowRefDeltaCounts :: PlainRowPatch RowTupleKey -> Map RowTupleKey MultiplicityChange
rowRefDeltaCounts =
  plainRowPatchChangeMap
{-# INLINE rowRefDeltaCounts #-}

applyRelationPatch ::
  PlainRowPatch RowTupleKey ->
  Relation ->
  Either RelationPatchError Relation
applyRelationPatch delta relation =
  fst <$> applyRelationPatchTracked delta relation
{-# INLINE applyRelationPatch #-}

applyRelationPatchTracked ::
  PlainRowPatch RowTupleKey ->
  Relation ->
  Either RelationPatchError (Relation, RowIdDelta RowTupleKey)
applyRelationPatchTracked delta relation =
  Map.foldlWithKey' step (Right (relation, emptyRowIdDelta)) (rowRefDeltaCounts delta)
  where
    step ::
      Either RelationPatchError (Relation, RowIdDelta RowTupleKey) ->
      RowTupleKey ->
      MultiplicityChange ->
      Either RelationPatchError (Relation, RowIdDelta RowTupleKey)
    step eitherState _row deltaMultiplicity
      | deltaMultiplicity == zeroMultiplicityChange =
          eitherState
    step eitherState !row !d = do
      (!rel, !rowDelta) <- eitherState
      if not (rowMatchesRelation rel row)
        then
          Left
            ( RelationPatchRowWidthMismatch
                row
                (Vector.length (indexedRowsLayout (relRows rel)))
                (tupleKeyWidth row)
            )
        else
          case indexedRowsLookupId row (relRows rel) of
            Nothing ->
              insertNewRow rel rowDelta row d
            Just rowKey ->
              patchExistingRow rel rowDelta row rowKey d

    insertNewRow ::
      Relation ->
      RowIdDelta RowTupleKey ->
      RowTupleKey ->
      MultiplicityChange ->
      Either RelationPatchError (Relation, RowIdDelta RowTupleKey)
    insertNewRow !rel !rowDelta !row !d =
      case positiveMultiplicityChange d of
        Nothing ->
          Left (RelationPatchMissingRowDelete row d)
        Just multiplicity ->
          insertFreshPositiveRow rel rowDelta row multiplicity

    insertFreshPositiveRow ::
      Relation ->
      RowIdDelta RowTupleKey ->
      RowTupleKey ->
      Multiplicity ->
      Either RelationPatchError (Relation, RowIdDelta RowTupleKey)
    insertFreshPositiveRow !rel !rowDelta !row !multiplicity =
      case indexedRowsInsertFresh tupleKeyIndexedFormat row multiplicity (relRows rel) of
        Left insertError ->
          Left (RelationPatchInsertFailed row insertError)
        Right (rowId, rows') ->
          let !rowKey = rowIdInt rowId
              !rel' =
                rel
                  { relRows = rows',
                    relSupportHash = relSupportHash rel `xor` rowContentHash rowId row,
                    relMultiplicityHash =
                      relMultiplicityHash rel `xor` rowMultiplicityHash rowId row multiplicity
                  }
              !rowDelta' =
                rowDelta
                  { ridInserted =
                      IntMap.insert rowKey row (ridInserted rowDelta)
                  }
           in Right (rel', rowDelta')

    patchExistingRow ::
      Relation ->
      RowIdDelta RowTupleKey ->
      RowTupleKey ->
      RowId ->
      MultiplicityChange ->
      Either RelationPatchError (Relation, RowIdDelta RowTupleKey)
    patchExistingRow !rel !rowDelta !row !rowId !d =
      case indexedRowsLookupPayload row (relRows rel) of
        Nothing ->
          Left (RelationPatchMissingRowDelete row d)
        Just oldCount ->
          case applyMultiplicityChange oldCount d of
            Nothing ->
              Left
                ( RelationPatchMultiplicityUnderflow
                    row
                    oldCount
                    d
                )
            Just newCount ->
              if newCount == zeroMultiplicity
                then deleteExistingRow rel rowDelta row rowId oldCount
                else
                  case indexedRowsSetPayload row newCount (relRows rel) of
                    Left payloadError ->
                      Left (RelationPatchPayloadUpdateFailed row payloadError)
                    Right rows' ->
                      let !rowKey = rowIdInt rowId
                       in
                      Right
                        ( rel
                            { relRows = rows',
                              relMultiplicityHash =
                                relMultiplicityHash rel
                                  `xor` rowMultiplicityHashInt rowKey row oldCount
                                  `xor` rowMultiplicityHashInt rowKey row newCount
                            },
                          rowDelta
                        )

    deleteExistingRow ::
      Relation ->
      RowIdDelta RowTupleKey ->
      RowTupleKey ->
      RowId ->
      Multiplicity ->
      Either RelationPatchError (Relation, RowIdDelta RowTupleKey)
    deleteExistingRow !rel !rowDelta !row !rowId !oldCount =
      case indexedRowsDelete tupleKeyIndexedFormat row (relRows rel) of
        Left deleteError ->
          Left (RelationPatchDeleteFailed row deleteError)
        Right (_deletedRowId, _oldPayload, rows') ->
          let !rowKey = rowIdInt rowId
              !rel' =
                rel
                  { relRows = rows',
                    relSupportHash = relSupportHash rel `xor` rowContentHashInt rowKey row,
                    relMultiplicityHash =
                      relMultiplicityHash rel
                        `xor` rowMultiplicityHashInt rowKey row oldCount
                  }
              !rowDelta' =
                rowDelta
                  { ridDeleted =
                      IntMap.insert rowKey row (ridDeleted rowDelta)
                  }
           in Right (rel', rowDelta')
{-# INLINE applyRelationPatchTracked #-}

relationSupportRows ::
  RowBlockIdentity ->
  Relation ->
  Either RowBuildError (RowBlock 'Canonical)
relationSupportRows identityValue relation =
  atomRowsFromTupleKeys
    identityValue
    (relationLayout relation)
    (Map.keys (relationRows relation))
{-# INLINE relationSupportRows #-}

filterRowsByEnv ::
  Relation ->
  RowSet ->
  JoinEnv ->
  RowSet
filterRowsByEnv relation =
  IntMap.foldlWithKey' step
  where
    step feasible slotKey (RepKey repKey)
      | IntMap.member slotKey (indexedRowsColumnIndex (relRows relation)) =
          case IntMap.lookup slotKey (indexedRowsValueIndex (relRows relation)) >>= IntMap.lookup repKey of
            Nothing ->
              emptyRowSet
            Just bucket ->
              rowSetIntersectionWithRowIdSet bucket feasible
      | otherwise =
          feasible
{-# INLINE filterRowsByEnv #-}

slotValuesFromFeasible ::
  Relation ->
  RowSet ->
  SlotId ->
  Maybe (HashSet RepKey)
slotValuesFromFeasible relation feasible slot
  | IntMap.notMember slotKey (indexedRowsColumnIndex (relRows relation)) =
      Nothing
  | otherwise =
      Just
        ( HashSet.fromList
            [ RepKey repKey
            | (repKey, bucket) <- IntMap.toList byValue,
              rowSetIntersectsRowIdSet bucket feasible
            ]
        )
  where
    !slotKey =
      slotIdKey slot

    byValue =
      IntMap.findWithDefault IntMap.empty slotKey (indexedRowsValueIndex (relRows relation))
{-# INLINE slotValuesFromFeasible #-}

candidateRowIds :: JoinEnv -> Relation -> RowSet
candidateRowIds env relation =
  filterRowsByEnv relation (indexedRowsLiveRowSet (relRows relation)) env
{-# INLINE candidateRowIds #-}

candidateValues ::
  JoinEnv ->
  SlotId ->
  Relation ->
  Maybe (HashSet RepKey)
candidateValues env slot relation =
  slotValuesFromFeasible
    relation
    (candidateRowIds env relation)
    slot
{-# INLINE candidateValues #-}
