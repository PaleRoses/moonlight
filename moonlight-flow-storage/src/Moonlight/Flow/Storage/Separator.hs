{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Storage.Separator
  ( SeparatorTupleKey,
    SeparatorSpec (..),
    SeparatorIndex (..),
    emptySeparatorIndex,
    separatorKeyFromRow,
    separatorKeyFromRowId,
    buildSeparatorIndex,
    applySeparatorIndexDelta,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( AtomId,
    SlotId,
    slotIdKey,
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Differential.Row.Block
  ( RowLayout,
  )
import Moonlight.Differential.Index.IndexedRows
  ( indexedRowsColumnIndex,
    indexedRowsLiveRowSet,
  )
import Moonlight.Differential.Index.RowId
  ( RowId,
    mkRowId,
    rowIdInt,
  )
import Moonlight.Flow.Storage.Relation
  ( Relation (..),
    RowIdDelta (..),
    rowForId,
  )
import Moonlight.Differential.Index.RowIdSet
  ( RowIdSet,
    rowIdSetDelete,
    rowIdSetNull,
    rowIdSetUnion,
    singletonRowIdSet,
  )
import Moonlight.Differential.Index.RowSet
  ( rowSetFoldl',
  )

type SeparatorSpec :: Type
data SeparatorSpec = SeparatorSpec
  { ssAtom :: !AtomId,
    ssSlots :: !RowLayout
  }
  deriving stock (Eq, Ord, Show)

type SeparatorIndex :: Type
data SeparatorIndex = SeparatorIndex
  { siSlots :: !RowLayout,
    siByKey :: !(Map SeparatorTupleKey RowIdSet),
    siRowToKey :: !(IntMap SeparatorTupleKey)
  }
  deriving stock (Eq, Show)

emptySeparatorIndex :: RowLayout -> SeparatorIndex
emptySeparatorIndex sep =
  SeparatorIndex
    { siSlots = sep,
      siByKey = Map.empty,
      siRowToKey = IntMap.empty
    }
{-# INLINE emptySeparatorIndex #-}

separatorKeyFromRow :: Relation -> RowLayout -> RowTupleKey -> Maybe SeparatorTupleKey
separatorKeyFromRow relation sepSlots row =
  coerceTupleKey . tupleKeyFromRepKeys <$> traverse readSlot sepSlots
  where
    readSlot :: SlotId -> Maybe RepKey
    readSlot slot = do
      column <- IntMap.lookup (slotIdKey slot) (indexedRowsColumnIndex (relRows relation))
      tupleKeyIndex row column
{-# INLINE separatorKeyFromRow #-}

separatorKeyFromRowId :: Relation -> RowLayout -> RowId -> Maybe SeparatorTupleKey
separatorKeyFromRowId relation sepSlots rowId = do
  row <- rowForId relation rowId
  separatorKeyFromRow relation sepSlots row
{-# INLINE separatorKeyFromRowId #-}

buildSeparatorIndex :: Relation -> RowLayout -> SeparatorIndex
buildSeparatorIndex relation sepSlots =
  rowSetFoldl' step (emptySeparatorIndex sepSlots) (indexedRowsLiveRowSet (relRows relation))
  where
    step sep rowId =
      case separatorKeyFromRowId relation sepSlots rowId of
        Nothing ->
          sep
        Just key ->
          let !rowKey = rowIdInt rowId
           in
          sep
            { siByKey =
                Map.insertWith
                  rowIdSetUnion
                  key
                  (singletonRowIdSet rowId)
                  (siByKey sep),
              siRowToKey =
                IntMap.insert rowKey key (siRowToKey sep)
            }
{-# INLINE buildSeparatorIndex #-}

applySeparatorIndexDelta ::
  Relation ->
  RowIdDelta RowTupleKey ->
  SeparatorIndex ->
  SeparatorIndex
applySeparatorIndexDelta relation delta sep0 =
  let sep1 =
        IntMap.foldlWithKey' removeRow sep0 (ridDeleted delta)
   in IntMap.foldlWithKey' addRow sep1 (ridInserted delta)
  where
    removeRow :: SeparatorIndex -> Int -> RowTupleKey -> SeparatorIndex
    removeRow sep rowKey _row =
      case (mkRowId rowKey, IntMap.lookup rowKey (siRowToKey sep)) of
        (_, Nothing) ->
          sep
        (Left _, Just _) ->
          sep
        (Right rowId, Just key) ->
          sep
            { siByKey =
                Map.update
                  ( \bucket ->
                      let bucket' = rowIdSetDelete rowId bucket
                       in if rowIdSetNull bucket'
                            then Nothing
                            else Just bucket'
                  )
                  key
                  (siByKey sep),
              siRowToKey =
                IntMap.delete rowKey (siRowToKey sep)
            }

    addRow :: SeparatorIndex -> Int -> RowTupleKey -> SeparatorIndex
    addRow sep rowKey row =
      case (mkRowId rowKey, separatorKeyFromRow relation (siSlots sep) row) of
        (_, Nothing) ->
          sep
        (Left _, Just _) ->
          sep
        (Right rowId, Just key) ->
          sep
            { siByKey =
                Map.insertWith
                  rowIdSetUnion
                  key
                  (singletonRowIdSet rowId)
                  (siByKey sep),
              siRowToKey =
                IntMap.insert rowKey key (siRowToKey sep)
            }
{-# INLINE applySeparatorIndexDelta #-}
