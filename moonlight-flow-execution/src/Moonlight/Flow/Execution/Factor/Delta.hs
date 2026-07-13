{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Execution.Factor.Delta
  ( FactorDelta,
    factorDeltaFromCellPatches,
    factorDeltaFromPatch,
    patchFactorCellValue,
    patchFactorCells,
    remapFactorDeltaProvIds,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Moonlight.Delta.Patch qualified as CorePatch
import Moonlight.Differential.Row.Patch
  ( ShapedPatch (..),
    emptyShapedPatch,
  )
import Moonlight.Flow.Execution.Factor.Core
  ( Factor,
    deleteFactorCell,
    insertFactorCell,
    setFactorCellPayload,
  )
import Moonlight.Differential.Index.IndexedRows
  ( indexedRowsPayloadMap,
    indexedRowsLayout,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvenanceObstruction (..),
    ProvVal (..)
  )
import Moonlight.Flow.Execution.Observe.Provenance.GC
  ( ProvIdRemap,
    remapProvVal
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Plan.Query.Core

type FactorDelta =
  ShapedPatch [SlotId] AssignmentTupleKey ProvVal

factorDeltaFromCellPatches ::
  [SlotId] ->
  Map AssignmentTupleKey (CorePatch.CellPatch ProvVal) ->
  FactorDelta
factorDeltaFromCellPatches schema changes =
  factorDeltaFromPatch schema (CorePatch.fromAscList (Map.toAscList changes))
{-# INLINE factorDeltaFromCellPatches #-}

factorDeltaFromPatch ::
  [SlotId] ->
  CorePatch.Patch AssignmentTupleKey ProvVal ->
  FactorDelta
factorDeltaFromPatch schema delta =
  (emptyShapedPatch schema)
    { spdDelta = delta
    }
{-# INLINE factorDeltaFromPatch #-}

patchFactorCells ::
  Map AssignmentTupleKey ProvVal ->
  Factor ->
  (Factor, FactorDelta)
patchFactorCells recomputed factor0 =
  let (factor1, changes) =
        Map.mapAccumWithKey
          (\factor key value -> patchFactorCellValue key value factor)
          factor0
          recomputed
   in ( factor1,
        factorDeltaFromCellPatches
          (Vector.toList (indexedRowsLayout factor0))
          (Map.mapMaybe id changes)
      )

patchFactorCellValue ::
  AssignmentTupleKey ->
  ProvVal ->
  Factor ->
  (Factor, Maybe (CorePatch.CellPatch ProvVal))
patchFactorCellValue key newValueRaw factor =
  let oldValue =
        Map.lookup key (indexedRowsPayloadMap factor)
      newValue =
        normalizeCell newValueRaw
   in if oldValue == newValue
        then (factor, Nothing)
        else
          ( patchFactorCell key oldValue newValue factor,
            Just (factorCellPatch oldValue newValue)
          )
{-# INLINE patchFactorCellValue #-}

factorCellPatch :: Maybe ProvVal -> Maybe ProvVal -> CorePatch.CellPatch ProvVal
factorCellPatch oldValue newValue =
  case (oldValue, newValue) of
    (Nothing, Nothing) ->
      CorePatch.assertAbsent
    (Nothing, Just value) ->
      CorePatch.insert value
    (Just value, Nothing) ->
      CorePatch.delete value
    (Just oldPayload, Just newPayload) ->
      CorePatch.replace oldPayload newPayload
{-# INLINE factorCellPatch #-}

patchFactorCell ::
  AssignmentTupleKey ->
  Maybe ProvVal ->
  Maybe ProvVal ->
  Factor ->
  Factor
patchFactorCell key oldValue newValue factor =
  case (oldValue, newValue) of
    (Nothing, Nothing) ->
      factor
    (Nothing, Just value) ->
      insertFactorCell key value factor
    (Just _oldValue, Nothing) ->
      deleteFactorCell key factor
    (Just _oldValue, Just value) ->
      setFactorCellPayload key value factor
{-# INLINE patchFactorCell #-}

normalizeCell :: ProvVal -> Maybe ProvVal
normalizeCell = \case
  PVZero ->
    Nothing
  value ->
    Just value
{-# INLINE normalizeCell #-}

remapFactorDeltaProvIds ::
  ProvIdRemap ->
  FactorDelta ->
  Either ProvenanceObstruction FactorDelta
remapFactorDeltaProvIds remap delta =
  let remapCell change = do
        CorePatch.traverseCell (remapProvVal remap) change
   in do
        changes <- CorePatch.traverseWithKey (const remapCell) (spdDelta delta)
        Right delta {spdDelta = changes}
{-# INLINE remapFactorDeltaProvIds #-}
