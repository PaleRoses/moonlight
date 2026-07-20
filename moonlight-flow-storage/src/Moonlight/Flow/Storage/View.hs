{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Storage.View
  ( View (..),
    unrestrictedView,
    SupportIds,
    normalizeSupportIds,
    supportAnyWitnessExists,
    supportAllRelationsFeasible,
    viewRows,
    setViewRows,
    intersectViewRows,
    intersectViewRowsBySlotValue,
    materializeViewRows,
    viewFeasibleRowIds,
    viewSlotValues,
    ViewSignature (..),
    viewSignature,
  )
where

import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Moonlight.Core
  ( SlotId,
    slotIdKey,
  )
import Moonlight.Flow.Internal.Digest
  ( mix64,
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Differential.Index.IndexedRows
  ( indexedRowsLiveRowSet,
    indexedRowsValueIndex,
  )
import Moonlight.Flow.Storage.Relation
  ( JoinEnv,
    Relation (..),
    RelationEpoch,
    filterRowsByEnv,
    relationEpoch,
    slotValuesFromFeasible,
  )
import Moonlight.Flow.Storage.Store
  ( Store,
    storeRelations,
  )
import Moonlight.Differential.Index.RowSet
  ( RowSet,
    emptyRowSet,
    rowSetDigest,
    rowSetIntersection,
    rowSetIntersectionWithRowIdSet,
    rowSetNull,
  )

type View :: Type
newtype View = View
  { viewActiveRows :: IntMap RowSet
  }
  deriving stock (Eq, Show)

unrestrictedView :: View
unrestrictedView =
  View IntMap.empty
{-# INLINE unrestrictedView #-}

type SupportIds :: Type
type SupportIds = IntMap RowSet

normalizeSupportIds :: SupportIds -> SupportIds
normalizeSupportIds =
  IntMap.filter (not . rowSetNull)
{-# INLINE normalizeSupportIds #-}

supportAnyWitnessExists :: SupportIds -> Bool
supportAnyWitnessExists =
  Foldable.any (not . rowSetNull)
{-# INLINE supportAnyWitnessExists #-}

supportAllRelationsFeasible :: SupportIds -> Bool
supportAllRelationsFeasible support =
  not (IntMap.null support)
    && Foldable.all (not . rowSetNull) support
{-# INLINE supportAllRelationsFeasible #-}

baseRowsOf :: Store -> Int -> RowSet
baseRowsOf store atomKey =
  case IntMap.lookup atomKey (storeRelations store) of
    Nothing ->
      emptyRowSet
    Just relation ->
      indexedRowsLiveRowSet (relRows relation)
{-# INLINE baseRowsOf #-}

viewRows :: Store -> View -> Int -> RowSet
viewRows store (View activeRows) atomKey =
  case IntMap.lookup atomKey activeRows of
    Just rows ->
      rows
    Nothing ->
      baseRowsOf store atomKey
{-# INLINE viewRows #-}

setViewRows :: Store -> Int -> RowSet -> View -> View
setViewRows store atomKey rows (View activeRows) =
  let baseRows =
        baseRowsOf store atomKey
      activeRows'
        | rows == baseRows =
            IntMap.delete atomKey activeRows
        | otherwise =
            IntMap.insert atomKey rows activeRows
   in View activeRows'
{-# INLINE setViewRows #-}

intersectViewRows :: Store -> Int -> RowSet -> View -> View
intersectViewRows store atomKey rows view =
  setViewRows store atomKey (rowSetIntersection rows (viewRows store view atomKey)) view
{-# INLINE intersectViewRows #-}

intersectViewRowsBySlotValue :: Store -> Int -> SlotId -> Int -> View -> View
intersectViewRowsBySlotValue store atomKey slot repKey view =
  case IntMap.lookup atomKey (storeRelations store) of
    Nothing ->
      setViewRows store atomKey emptyRowSet view
    Just relation ->
      case IntMap.lookup (slotIdKey slot) (indexedRowsValueIndex (relRows relation)) >>= IntMap.lookup repKey of
        Nothing ->
          setViewRows store atomKey emptyRowSet view
        Just bucket ->
          setViewRows
            store
            atomKey
            (rowSetIntersectionWithRowIdSet bucket (viewRows store view atomKey))
            view
{-# INLINE intersectViewRowsBySlotValue #-}

materializeViewRows :: Store -> View -> SupportIds
materializeViewRows store view =
  IntMap.mapWithKey
    (\atomKey _relation -> viewRows store view atomKey)
    (storeRelations store)
{-# INLINE materializeViewRows #-}

viewFeasibleRowIds :: Store -> View -> Int -> JoinEnv -> RowSet
viewFeasibleRowIds store view atomKey env =
  maybe
    emptyRowSet
    (\relation -> filterRowsByEnv relation (viewRows store view atomKey) env)
    (IntMap.lookup atomKey (storeRelations store))
{-# INLINE viewFeasibleRowIds #-}

viewSlotValues :: Store -> View -> Int -> SlotId -> JoinEnv -> HashSet RepKey
viewSlotValues store view atomKey slot env =
  fromMaybe HashSet.empty $ do
    relation <- IntMap.lookup atomKey (storeRelations store)
    slotValuesFromFeasible
      relation
      (filterRowsByEnv relation (viewRows store view atomKey) env)
      slot
{-# INLINE viewSlotValues #-}

type ViewSignature :: Type
data ViewSignature = ViewSignature
  { vsAtomEpochs :: !(IntMap RelationEpoch),
    vsOverrideHash :: {-# UNPACK #-} !Word64
  }
  deriving stock (Eq, Show)

viewSignature :: Store -> View -> ViewSignature
viewSignature store (View activeRows) =
  ViewSignature
    { vsAtomEpochs =
        IntMap.map relationEpoch (storeRelations store),
      vsOverrideHash =
        IntMap.foldlWithKey'
          ( \acc atomKey rows ->
              mix64 acc (mix64 (fromIntegral atomKey) (rowSetDigest rows))
          )
          0xcbf29ce484222325
          activeRows
    }
{-# INLINE viewSignature #-}
