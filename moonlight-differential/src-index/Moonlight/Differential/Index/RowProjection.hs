{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Index.RowProjection
  ( IndexedRowsProjectionError (..),
    ProjectedRowsAdvanceError (..),
    ProjectedRowsDelta (..),
    RowChange (..),
    RowChanges (..),
    batchToIndexedRows,
    projectBatchDelta,
    applyProjectedRowsDelta,
    snapshotTraceToIndexedRows,
  )
where

import Control.Monad
  ( (<=<),
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.Kind
  ( Type,
  )
import Data.List.NonEmpty
  ( NonEmpty,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map

import Moonlight.Core (AdditiveGroup (..), AdditiveMonoid (..))
import Moonlight.Differential.Batch
  ( Batch,
    foldBatch,
  )
import Moonlight.Differential.Index.IndexedRows
  ( IndexedRowFormat,
    IndexedRowsBuildError,
    IndexedRowsDeleteError,
    IndexedRowsInsertError (..),
    IndexedRowsPayloadError,
    IndexedRows,
    indexedRowsDelete,
    indexedRowsFromPayloadMap,
    indexedRowsInsertFresh,
    indexedRowsLookupId,
    indexedRowsLookupPayload,
    indexedRowsSetPayload,
  )
import Moonlight.Differential.Index.RowId
  ( RowId,
  )
import Moonlight.Differential.Trace
  ( Trace,
    foldTraceBatchRows,
  )

-- | A typed obstruction for materializing a logical cell into the physical row
-- substrate. Duplicate physical row keys are rejected because 'IndexedRows' is a
-- snapshot-like arrangement with one payload per row key. Trace projection has
-- an explicit payload group law, so repeated projected trace rows are
-- accumulated there instead of being guessed here.
type IndexedRowsProjectionError :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data IndexedRowsProjectionError time key val weight rowKey layout
  = IndexedRowsProjectionRejected !time !key !val !weight
  | IndexedRowsProjectionInsertFailed !rowKey !(IndexedRowsInsertError layout rowKey)
  | IndexedRowsProjectionBuildFailed !(NonEmpty (IndexedRowsBuildError layout rowKey))
  deriving stock (Eq, Ord, Show)

type ProjectedRowsDelta :: Type -> Type -> Type
newtype ProjectedRowsDelta rowKey payload = ProjectedRowsDelta
  { unProjectedRowsDelta :: Map rowKey payload
  }
  deriving stock (Eq, Ord, Show)

type RowChange :: Type -> Type -> Type
data RowChange rowKey payload
  = RowInserted !RowId !rowKey !payload
  | RowPayloadChanged !RowId !rowKey !payload !payload
  | RowDeleted !RowId !rowKey !payload
  deriving stock (Eq, Ord, Show)

type RowChanges :: Type -> Type -> Type
newtype RowChanges rowKey payload = RowChanges
  { unRowChanges :: [RowChange rowKey payload]
  }
  deriving stock (Eq, Ord, Show)

type ProjectedRowsAdvanceError :: Type -> Type -> Type
data ProjectedRowsAdvanceError layout rowKey
  = ProjectedRowsAdvanceMissingRowId !rowKey
  | ProjectedRowsAdvanceInsertFailed !rowKey !(IndexedRowsInsertError layout rowKey)
  | ProjectedRowsAdvanceDeleteFailed !rowKey !(IndexedRowsDeleteError layout rowKey)
  | ProjectedRowsAdvancePayloadFailed !rowKey !(IndexedRowsPayloadError rowKey)
  deriving stock (Eq, Ord, Show)

type ProjectedRowsAdvanceState :: Type -> Type -> Type -> Type
data ProjectedRowsAdvanceState layout rowKey payload = ProjectedRowsAdvanceState
  { projectedRowsAdvanceChangesRev :: ![RowChange rowKey payload],
    projectedRowsAdvanceRows :: !(IndexedRows layout rowKey payload)
  }

batchToIndexedRows ::
  Ord rowKey =>
  IndexedRowFormat layout rowKey ->
  (layout -> IntMap Int) ->
  layout ->
  (time -> key -> val -> weight -> Maybe (rowKey, payload)) ->
  Batch time key val weight ->
  Either
    (IndexedRowsProjectionError time key val weight rowKey layout)
    (IndexedRows layout rowKey payload)
batchToIndexedRows format layoutColumnIndex layout projectCell =
  buildRows <=< foldBatch collectProjectedCell (Right Map.empty)
  where
    collectProjectedCell eitherPayloads time key val weight = do
      payloads <- eitherPayloads
      case projectCell time key val weight of
        Nothing ->
          Left (IndexedRowsProjectionRejected time key val weight)
        Just (rowKey, payload) ->
          insertProjectedPayload rowKey payload payloads

    buildRows payloads =
      case indexedRowsFromPayloadMap format layoutColumnIndex layout payloads of
        Left buildErrors ->
          Left (IndexedRowsProjectionBuildFailed buildErrors)
        Right rows ->
          Right rows

insertProjectedPayload ::
  Ord rowKey =>
  rowKey ->
  payload ->
  Map rowKey payload ->
  Either
    (IndexedRowsProjectionError time key val weight rowKey layout)
    (Map rowKey payload)
insertProjectedPayload rowKey payload payloads
  = Map.alterF insertPayload rowKey payloads
  where
    insertPayload Nothing =
      Right (Just payload)
    insertPayload (Just _existingPayload) =
      Left (IndexedRowsProjectionInsertFailed rowKey (IndexedRowsInsertDuplicateKey rowKey))

snapshotTraceToIndexedRows ::
  (Ord rowKey, Eq payload, AdditiveGroup payload) =>
  IndexedRowFormat layout rowKey ->
  (layout -> IntMap Int) ->
  layout ->
  (time -> key -> val -> weight -> Maybe (rowKey, payload)) ->
  Trace time key val weight ->
  Either
    (IndexedRowsProjectionError time key val weight rowKey layout)
    (IndexedRows layout rowKey payload)
snapshotTraceToIndexedRows format layoutColumnIndex layout projectCell =
  buildRows <=< foldTraceBatchRows collectProjectedCell (Right Map.empty)
  where
    collectProjectedCell eitherPayloads time key val weight = do
      payloads <- eitherPayloads
      case projectCell time key val weight of
        Nothing ->
          Left (IndexedRowsProjectionRejected time key val weight)
        Just (rowKey, payload) ->
          Right (accumulateProjectedPayload rowKey payload payloads)

    buildRows payloads =
      case indexedRowsFromPayloadMap format layoutColumnIndex layout payloads of
        Left buildErrors ->
          Left (IndexedRowsProjectionBuildFailed buildErrors)
        Right rows ->
          Right rows

projectBatchDelta ::
  (Ord rowKey, Eq payload, AdditiveGroup payload) =>
  (time -> key -> val -> weight -> Maybe (rowKey, payload)) ->
  Batch time key val weight ->
  Either
    (IndexedRowsProjectionError time key val weight rowKey layout)
    (ProjectedRowsDelta rowKey payload)
projectBatchDelta projectCell =
  fmap ProjectedRowsDelta . foldBatch collectProjectedCell (Right Map.empty)
  where
    collectProjectedCell eitherPayloads time key val weight = do
      payloads <- eitherPayloads
      case projectCell time key val weight of
        Nothing ->
          Left (IndexedRowsProjectionRejected time key val weight)
        Just (rowKey, payload) ->
          Right (accumulateProjectedPayload rowKey payload payloads)

applyProjectedRowsDelta ::
  (Ord rowKey, Eq payload, AdditiveGroup payload) =>
  IndexedRowFormat layout rowKey ->
  ProjectedRowsDelta rowKey payload ->
  IndexedRows layout rowKey payload ->
  Either
    (ProjectedRowsAdvanceError layout rowKey)
    (RowChanges rowKey payload, IndexedRows layout rowKey payload)
applyProjectedRowsDelta format (ProjectedRowsDelta deltas) rows =
  finishProjectedRowsAdvance
    <$> Map.foldlWithKey'
      (advanceProjectedRowsAtKey format)
      (Right (emptyProjectedRowsAdvanceState rows))
      deltas

emptyProjectedRowsAdvanceState ::
  IndexedRows layout rowKey payload ->
  ProjectedRowsAdvanceState layout rowKey payload
emptyProjectedRowsAdvanceState rows =
  ProjectedRowsAdvanceState
    { projectedRowsAdvanceChangesRev = [],
      projectedRowsAdvanceRows = rows
    }
{-# INLINE emptyProjectedRowsAdvanceState #-}

advanceProjectedRowsAtKey ::
  (Ord rowKey, Eq payload, AdditiveGroup payload) =>
  IndexedRowFormat layout rowKey ->
  Either (ProjectedRowsAdvanceError layout rowKey) (ProjectedRowsAdvanceState layout rowKey payload) ->
  rowKey ->
  payload ->
  Either (ProjectedRowsAdvanceError layout rowKey) (ProjectedRowsAdvanceState layout rowKey payload)
advanceProjectedRowsAtKey format eitherState rowKey deltaPayload = do
  state <- eitherState
  advanceProjectedRowsLiveKey format rowKey deltaPayload state
{-# INLINE advanceProjectedRowsAtKey #-}

advanceProjectedRowsLiveKey ::
  (Ord rowKey, Eq payload, AdditiveGroup payload) =>
  IndexedRowFormat layout rowKey ->
  rowKey ->
  payload ->
  ProjectedRowsAdvanceState layout rowKey payload ->
  Either (ProjectedRowsAdvanceError layout rowKey) (ProjectedRowsAdvanceState layout rowKey payload)
advanceProjectedRowsLiveKey format rowKey deltaPayload state =
  case keepProjectedPayload deltaPayload of
    Nothing ->
      Right state
    Just nonzeroDelta ->
      case indexedRowsLookupPayload rowKey rows of
        Nothing ->
          insertProjectedRowsDelta format rowKey nonzeroDelta state
        Just oldPayload ->
          updateProjectedRowsDelta format rowKey oldPayload nonzeroDelta state
  where
    rows =
      projectedRowsAdvanceRows state
{-# INLINE advanceProjectedRowsLiveKey #-}

insertProjectedRowsDelta ::
  Ord rowKey =>
  IndexedRowFormat layout rowKey ->
  rowKey ->
  payload ->
  ProjectedRowsAdvanceState layout rowKey payload ->
  Either (ProjectedRowsAdvanceError layout rowKey) (ProjectedRowsAdvanceState layout rowKey payload)
insertProjectedRowsDelta format rowKey payload state =
  case indexedRowsInsertFresh format rowKey payload (projectedRowsAdvanceRows state) of
    Left obstruction ->
      Left (ProjectedRowsAdvanceInsertFailed rowKey obstruction)
    Right (rowId, rows) ->
      Right
        state
          { projectedRowsAdvanceChangesRev =
              RowInserted rowId rowKey payload : projectedRowsAdvanceChangesRev state,
            projectedRowsAdvanceRows = rows
          }
{-# INLINE insertProjectedRowsDelta #-}

updateProjectedRowsDelta ::
  (Ord rowKey, Eq payload, AdditiveGroup payload) =>
  IndexedRowFormat layout rowKey ->
  rowKey ->
  payload ->
  payload ->
  ProjectedRowsAdvanceState layout rowKey payload ->
  Either (ProjectedRowsAdvanceError layout rowKey) (ProjectedRowsAdvanceState layout rowKey payload)
updateProjectedRowsDelta format rowKey oldPayload deltaPayload state =
  case indexedRowsLookupId rowKey (projectedRowsAdvanceRows state) of
    Nothing ->
      Left (ProjectedRowsAdvanceMissingRowId rowKey)
    Just rowId ->
      case keepProjectedPayload newPayload of
        Nothing ->
          deleteProjectedRowsDelta format rowKey state
        Just livePayload ->
          setProjectedRowsPayload rowKey rowId oldPayload livePayload state
  where
    newPayload =
      add oldPayload deltaPayload
{-# INLINE updateProjectedRowsDelta #-}

deleteProjectedRowsDelta ::
  Ord rowKey =>
  IndexedRowFormat layout rowKey ->
  rowKey ->
  ProjectedRowsAdvanceState layout rowKey payload ->
  Either (ProjectedRowsAdvanceError layout rowKey) (ProjectedRowsAdvanceState layout rowKey payload)
deleteProjectedRowsDelta format rowKey state =
  case indexedRowsDelete format rowKey (projectedRowsAdvanceRows state) of
    Left obstruction ->
      Left (ProjectedRowsAdvanceDeleteFailed rowKey obstruction)
    Right (rowId, oldPayload, rows) ->
      Right
        state
          { projectedRowsAdvanceChangesRev =
              RowDeleted rowId rowKey oldPayload : projectedRowsAdvanceChangesRev state,
            projectedRowsAdvanceRows = rows
          }
{-# INLINE deleteProjectedRowsDelta #-}

setProjectedRowsPayload ::
  Ord rowKey =>
  rowKey ->
  RowId ->
  payload ->
  payload ->
  ProjectedRowsAdvanceState layout rowKey payload ->
  Either (ProjectedRowsAdvanceError layout rowKey) (ProjectedRowsAdvanceState layout rowKey payload)
setProjectedRowsPayload rowKey rowId oldPayload newPayload state =
  case indexedRowsSetPayload rowKey newPayload (projectedRowsAdvanceRows state) of
    Left obstruction ->
      Left (ProjectedRowsAdvancePayloadFailed rowKey obstruction)
    Right rows ->
      Right
        state
          { projectedRowsAdvanceChangesRev =
              RowPayloadChanged rowId rowKey oldPayload newPayload : projectedRowsAdvanceChangesRev state,
            projectedRowsAdvanceRows = rows
          }
{-# INLINE setProjectedRowsPayload #-}

finishProjectedRowsAdvance ::
  ProjectedRowsAdvanceState layout rowKey payload ->
  (RowChanges rowKey payload, IndexedRows layout rowKey payload)
finishProjectedRowsAdvance state =
  ( RowChanges (reverse (projectedRowsAdvanceChangesRev state)),
    projectedRowsAdvanceRows state
  )
{-# INLINE finishProjectedRowsAdvance #-}

accumulateProjectedPayload ::
  (Ord rowKey, Eq payload, AdditiveGroup payload) =>
  rowKey ->
  payload ->
  Map rowKey payload ->
  Map rowKey payload
accumulateProjectedPayload rowKey payload payloads
  = Map.alter mergePayload rowKey payloads
  where
    mergePayload Nothing =
      keepProjectedPayload payload
    mergePayload (Just oldPayload) =
      keepProjectedPayload (add oldPayload payload)

keepProjectedPayload :: (Eq payload, AdditiveGroup payload) => payload -> Maybe payload
keepProjectedPayload payload
  | payload == zero =
      Nothing
  | otherwise =
      Just payload
