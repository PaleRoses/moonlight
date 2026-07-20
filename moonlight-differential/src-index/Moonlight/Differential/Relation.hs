{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Trace-backed relation carriers; 'RelationState' and 'CoreRelationViews'
-- are sealed behind the bootstrap/advance lifecycle, with 'validateRelation'
-- as the coherence law hook.
module Moonlight.Differential.Relation
  ( RelationRevision (..),
    RelationPlan (..),
    RelationState,
    relationTrace,
    relationViews,
    relationRevision,
    CoreRelationViews,
    relationByKey,
    relationRows,
    RelationChanges (..),
    RelationAdvance (..),
    RelationBootstrapError (..),
    RelationAdvanceError (..),
    RelationValidationError (..),
    bootstrapRelation,
    advanceRelation,
    validateRelation,
  )
where

import Data.Kind
  ( Type,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import Numeric.Natural
  ( Natural,
  )

import Moonlight.Core (AdditiveGroup)
import Moonlight.Core
  ( PartialOrder,
  )
import Moonlight.Differential.Arrangement
  ( Arrangement,
    appendArrangementBatch,
    arrangeByKey,
  )
import Moonlight.Differential.Batch
  ( Batch,
  )
import Moonlight.Differential.Index.IndexedRows
  ( IndexedRowFormat,
    IndexedRows,
    indexedRowsPayloadMap,
  )
import Moonlight.Differential.Index.RowProjection
  ( IndexedRowsProjectionError,
    ProjectedRowsAdvanceError,
    RowChanges,
    applyProjectedRowsDelta,
    projectBatchDelta,
    snapshotTraceToIndexedRows,
  )
import Moonlight.Differential.Trace
  ( Trace,
    traceAppendBatch,
  )

type RelationRevision :: Type
newtype RelationRevision = RelationRevision
  { unRelationRevision :: Natural
  }
  deriving stock (Eq, Ord, Show)

type RelationPlan :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data RelationPlan time key val weight layout rowKey payload = RelationPlan
  { relationIndexedFormat :: !(IndexedRowFormat layout rowKey),
    relationLayoutColumnIndex :: !(layout -> IntMap Int),
    relationLayout :: !layout,
    relationProjectCell :: !(time -> key -> val -> weight -> Maybe (rowKey, payload))
  }

type CoreRelationViews :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data CoreRelationViews time key val weight layout rowKey payload = CoreRelationViews
  { relationByKey :: !(Arrangement time key val weight),
    relationRows :: !(IndexedRows layout rowKey payload)
  }
  deriving stock (Eq, Show)

type RelationState :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data RelationState time key val weight layout rowKey payload = RelationState
  { relationTrace :: !(Trace time key val weight),
    relationViews :: !(CoreRelationViews time key val weight layout rowKey payload),
    relationRevision :: !RelationRevision
  }
  deriving stock (Eq, Show)

type RelationChanges :: Type -> Type -> Type
newtype RelationChanges rowKey payload = RelationChanges
  { relationRowChanges :: RowChanges rowKey payload
  }
  deriving stock (Eq, Ord, Show)

type RelationAdvance :: Type -> Type -> Type -> Type
data RelationAdvance batch changes state = RelationAdvance
  { relationInputBatch :: !batch,
    relationChanges :: !changes,
    relationNextState :: !state
  }
  deriving stock (Eq, Show)

type RelationBootstrapError :: Type -> Type -> Type -> Type -> Type -> Type -> Type
newtype RelationBootstrapError time key val weight rowKey layout
  = RelationBootstrapProjectionFailed (IndexedRowsProjectionError time key val weight rowKey layout)
  deriving stock (Eq, Ord, Show)

type RelationAdvanceError :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data RelationAdvanceError time key val weight rowKey layout
  = RelationAdvanceProjectionFailed !(IndexedRowsProjectionError time key val weight rowKey layout)
  | RelationAdvanceRowsFailed !(ProjectedRowsAdvanceError layout rowKey)
  deriving stock (Eq, Ord, Show)

type RelationValidationError :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data RelationValidationError time key val weight rowKey layout
  = RelationValidationArrangementDiverged
  | RelationValidationRowsDiverged
  | RelationValidationProjectionFailed !(IndexedRowsProjectionError time key val weight rowKey layout)
  deriving stock (Eq, Ord, Show)

bootstrapRelation ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight, Ord rowKey, Eq payload, AdditiveGroup payload) =>
  RelationPlan time key val weight layout rowKey payload ->
  Trace time key val weight ->
  Either
    (RelationBootstrapError time key val weight rowKey layout)
    (RelationState time key val weight layout rowKey payload)
bootstrapRelation plan traceValue =
  case snapshotRelationRows plan traceValue of
    Left obstruction ->
      Left (RelationBootstrapProjectionFailed obstruction)
    Right rows ->
      Right
        RelationState
          { relationTrace = traceValue,
            relationViews =
              CoreRelationViews
                { relationByKey = arrangeByKey traceValue,
                  relationRows = rows
                },
            relationRevision = RelationRevision 0
          }

advanceRelation ::
  (PartialOrder time, Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight, Ord rowKey, Eq payload, AdditiveGroup payload) =>
  RelationPlan time key val weight layout rowKey payload ->
  Batch time key val weight ->
  RelationState time key val weight layout rowKey payload ->
  Either
    (RelationAdvanceError time key val weight rowKey layout)
    (RelationAdvance (Batch time key val weight) (RelationChanges rowKey payload) (RelationState time key val weight layout rowKey payload))
advanceRelation plan batch state =
  case projectBatchDelta (relationProjectCell plan) batch of
    Left obstruction ->
      Left (RelationAdvanceProjectionFailed obstruction)
    Right projectedDelta ->
      case applyProjectedRowsDelta (relationIndexedFormat plan) projectedDelta oldRows of
        Left obstruction ->
          Left (RelationAdvanceRowsFailed obstruction)
        Right (rowChanges, nextRows) ->
          Right
            RelationAdvance
              { relationInputBatch = batch,
                relationChanges = RelationChanges rowChanges,
                relationNextState =
                  state
                    { relationTrace = nextTrace,
                      relationViews =
                        oldViews
                          { relationByKey = appendArrangementBatch batch (relationByKey oldViews),
                            relationRows = nextRows
                          },
                      relationRevision = nextRelationRevision (relationRevision state)
                    }
              }
  where
    oldViews =
      relationViews state

    oldRows =
      relationRows oldViews

    nextTrace =
      traceAppendBatch batch (relationTrace state)

validateRelation ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight, Ord rowKey, Eq payload, AdditiveGroup payload) =>
  RelationPlan time key val weight layout rowKey payload ->
  RelationState time key val weight layout rowKey payload ->
  Either (NonEmpty (RelationValidationError time key val weight rowKey layout)) ()
validateRelation plan state =
  validationFromErrors
    ( arrangementValidationErrors state
        <> rowValidationErrors plan state
    )

snapshotRelationRows ::
  (Ord rowKey, Eq payload, AdditiveGroup payload) =>
  RelationPlan time key val weight layout rowKey payload ->
  Trace time key val weight ->
  Either
    (IndexedRowsProjectionError time key val weight rowKey layout)
    (IndexedRows layout rowKey payload)
snapshotRelationRows plan =
  snapshotTraceToIndexedRows
    (relationIndexedFormat plan)
    (relationLayoutColumnIndex plan)
    (relationLayout plan)
    (relationProjectCell plan)

arrangementValidationErrors ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  RelationState time key val weight layout rowKey payload ->
  [RelationValidationError time key val weight rowKey layout]
arrangementValidationErrors state =
  if arrangeByKey (relationTrace state) == relationByKey (relationViews state)
    then []
    else [RelationValidationArrangementDiverged]

rowValidationErrors ::
  (Ord rowKey, Eq payload, AdditiveGroup payload) =>
  RelationPlan time key val weight layout rowKey payload ->
  RelationState time key val weight layout rowKey payload ->
  [RelationValidationError time key val weight rowKey layout]
rowValidationErrors plan state =
  case snapshotRelationRows plan (relationTrace state) of
    Left obstruction ->
      [RelationValidationProjectionFailed obstruction]
    Right snapshotRows ->
      if indexedRowsPayloadMap snapshotRows == indexedRowsPayloadMap (relationRows (relationViews state))
        then []
        else [RelationValidationRowsDiverged]

nextRelationRevision :: RelationRevision -> RelationRevision
nextRelationRevision (RelationRevision revision) =
  RelationRevision (revision + 1)

validationFromErrors :: [err] -> Either (NonEmpty err) ()
validationFromErrors errors =
  case errors of
    [] ->
      Right ()
    firstError : restErrors ->
      Left (firstError :| restErrors)
