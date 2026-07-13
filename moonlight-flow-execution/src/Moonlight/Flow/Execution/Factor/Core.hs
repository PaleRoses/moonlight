{-# LANGUAGE DataKinds #-}

module Moonlight.Flow.Execution.Factor.Core
  ( AssignmentTupleKey,
    Factor,
    emptyFactor,
    singletonFactor,
    mkFactor,
    keyFitsFactor,
    insertFactorCell,
    deleteFactorCell,
    setFactorCellPayload,
    factorMembershipRows,
    remapFactorProvIds,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Moonlight.Core
  ( SlotId,
  )
import Moonlight.Differential.Index.IndexedRows
  ( IndexedRows,
    indexedRowsDelete,
    indexedRowsInsertFresh,
    indexedRowsMapPayloadEither,
    indexedRowsPayloadMap,
    indexedRowsLayout,
    indexedRowsSetPayload,
  )
import Moonlight.Flow.Storage.Index.TupleFormat
  ( emptyIndexedRows,
    tupleKeyIndexedFormat,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchFromList,
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
import Moonlight.Differential.Row.Block
  ( RowLayout,
  )

type Factor =
  IndexedRows RowLayout AssignmentTupleKey ProvVal

emptyFactor :: [SlotId] -> Factor
emptyFactor =
  emptyIndexedRows . Vector.fromList
{-# INLINE emptyFactor #-}

singletonFactor :: Factor
singletonFactor =
  case indexedRowsInsertFresh
    tupleKeyIndexedFormat
    emptyTupleKey
    PVOne
    (emptyFactor [])
    of
      Left _insertError ->
        emptyFactor []
      Right (_rowId, rows) ->
        rows
{-# INLINE singletonFactor #-}

mkFactor :: [SlotId] -> Map AssignmentTupleKey ProvVal -> Factor
mkFactor schema =
  Map.foldlWithKey'
    (\factor key value -> insertFactorCell key value factor)
    (emptyFactor schema)
{-# INLINE mkFactor #-}

keyFitsFactor :: AssignmentTupleKey -> Factor -> Bool
keyFitsFactor key factor =
  tupleKeyWidth key == Vector.length (indexedRowsLayout factor)
{-# INLINE keyFitsFactor #-}

insertFactorCell :: AssignmentTupleKey -> ProvVal -> Factor -> Factor
insertFactorCell key value factor =
  case value of
    PVZero ->
      factor
    _ ->
      if keyFitsFactor key factor
        then
          case indexedRowsInsertFresh tupleKeyIndexedFormat key value factor of
            Left _insertError ->
              factor
            Right (_rowId, rows') ->
              rows'
        else factor
{-# INLINE insertFactorCell #-}

deleteFactorCell :: AssignmentTupleKey -> Factor -> Factor
deleteFactorCell key factor =
  case indexedRowsDelete tupleKeyIndexedFormat key factor of
    Left _deleteError ->
      factor
    Right (_rowId, _payload, rows') ->
      rows'
{-# INLINE deleteFactorCell #-}

setFactorCellPayload :: AssignmentTupleKey -> ProvVal -> Factor -> Factor
setFactorCellPayload key value factor =
  either (const factor) id (indexedRowsSetPayload key value factor)
{-# INLINE setFactorCellPayload #-}

factorMembershipRows :: Factor -> RowDelta
factorMembershipRows factorValue =
  plainRowPatchFromList
    [ (coerceTupleKey assignmentKey, MultiplicityChange 1)
      | assignmentKey <- Map.keys (indexedRowsPayloadMap factorValue)
    ]
{-# INLINE factorMembershipRows #-}

remapFactorProvIds ::
  ProvIdRemap ->
  Factor ->
  Either ProvenanceObstruction Factor
remapFactorProvIds remap factor =
  indexedRowsMapPayloadEither
    (remapProvVal remap)
    factor
{-# INLINE remapFactorProvIds #-}
