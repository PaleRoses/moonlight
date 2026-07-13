{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Execution.Dense.WCOJ.Witness
  ( DenseLeafWitness (..),
    DenseSourceWitness (..),
    emptyDenseSourceWitness,
    emptyDenseLeafWitness,
    sourceRowsWitnessWithTelemetry,
    leafWitnessWithTelemetry,
    foldDeltaDirtyLeafWitnesses,
  )
where

import Control.Monad.ST (ST)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Primitive.SmallArray qualified as SmallArray
import Data.Set qualified as Set
import Moonlight.Differential.Row.Tuple
  ( AssignmentTupleKey,
  )
import Moonlight.Flow.Execution.Dense.Plan
  ( DenseArrangement,
    DenseArrangementId (..),
    DenseJoinPlan (..),
    denseArrangementId,
    denseArrangementKeyAt,
    denseArrangementPayloadAtWithTelemetry,
  )
import Moonlight.Differential.Join.WCOJ.Dense.Executor qualified as Dense
import Moonlight.Flow.Execution.Factor.Contribution
  ( FactorSourceCell (..),
  )
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvArena,
    ProvVal (..),
  )
import Moonlight.Flow.Execution.Observe.Provenance.Value
  ( pvPlusWithTelemetry,
    pvTimesWithTelemetry,
  )
import Moonlight.Flow.Execution.Observe.RepairTelemetry
  ( RepairTelemetry,
    RepairTelemetryConfig,
    emptyRepairTelemetry,
  )
import Moonlight.Differential.Index.RowSet
  ( RowSet,
    rowSetFoldl',
    rowSetNull,
  )
import Moonlight.Differential.Index.RowId
  ( rowIdInt,
  )

type DenseLeafWitness :: Type
data DenseLeafWitness = DenseLeafWitness
  { dlwValue :: !ProvVal,
    dlwSupportCells :: !(Set.Set FactorSourceCell),
    dlwSupportRowsEnumerated :: {-# UNPACK #-} !Int,
    dlwTelemetry :: !RepairTelemetry
  }
  deriving stock (Eq, Show)

type DenseSourceWitness :: Type
data DenseSourceWitness = DenseSourceWitness
  { dswValue :: !ProvVal,
    dswSupportCells :: !(Set.Set FactorSourceCell),
    dswSupportRowsEnumerated :: {-# UNPACK #-} !Int,
    dswTelemetry :: !RepairTelemetry
  }
  deriving stock (Eq, Show)

emptyDenseSourceWitness :: DenseSourceWitness
emptyDenseSourceWitness =
  DenseSourceWitness
    { dswValue = PVZero,
      dswSupportCells = Set.empty,
      dswSupportRowsEnumerated = 0,
      dswTelemetry = emptyRepairTelemetry
    }
{-# INLINE emptyDenseSourceWitness #-}

emptyDenseLeafWitness :: DenseLeafWitness
emptyDenseLeafWitness =
  DenseLeafWitness
    { dlwValue = PVOne,
      dlwSupportCells = Set.empty,
      dlwSupportRowsEnumerated = 0,
      dlwTelemetry = emptyRepairTelemetry
    }
{-# INLINE emptyDenseLeafWitness #-}

sourceRowsWitnessWithTelemetry ::
  RepairTelemetryConfig ->
  DenseJoinPlan ->
  Int ->
  DenseArrangement ->
  RowSet ->
  ProvArena ->
  (ProvArena, DenseSourceWitness)
sourceRowsWitnessWithTelemetry config plan sourceIx src rows arena0
  | rowSetNull rows =
      (arena0, emptyDenseSourceWitness)
  | otherwise =
      rowSetFoldl'
        collectRow
        (arena0, emptyDenseSourceWitness)
        rows
  where
    !isSupportSource =
      IntSet.member sourceIx (djSupportSources plan)

    !sourceId =
      unDenseArrangementId (denseArrangementId src)

    collectRow (!arena, !witness) rowId =
      let !rowKey =
            rowIdInt rowId
          (!arena1, !value, !payloadTelemetry) =
            denseArrangementPayloadAtWithTelemetry config src rowKey arena
          (!arena2, !sourceValue, !plusTelemetry) =
            pvPlusWithTelemetry config (dswValue witness) value arena1
          (!supportCells, !supportRowsEnumerated) =
            if isSupportSource
              then
                case denseArrangementKeyAt src rowKey of
                  Nothing ->
                    (dswSupportCells witness, dswSupportRowsEnumerated witness + 1)
                  Just sourceKey ->
                    ( Set.insert
                        FactorSourceCell
                          { fscSourceId = sourceId,
                            fscKey = sourceKey
                          }
                        (dswSupportCells witness),
                      dswSupportRowsEnumerated witness + 1
                    )
              else
                (dswSupportCells witness, dswSupportRowsEnumerated witness)
       in ( arena2,
            witness
              { dswValue = sourceValue,
                dswSupportCells = supportCells,
                dswSupportRowsEnumerated = supportRowsEnumerated,
                dswTelemetry = dswTelemetry witness <> payloadTelemetry <> plusTelemetry
              }
          )
{-# INLINE sourceRowsWitnessWithTelemetry #-}

leafWitnessWithTelemetry ::
  RepairTelemetryConfig ->
  Dense.DenseLeaf s ->
  DenseJoinPlan ->
  ProvArena ->
  ST s (ProvArena, DenseLeafWitness)
leafWitnessWithTelemetry config frame plan arena0 =
  foldPlanSourceIndexesM plan (arena0, emptyDenseLeafWitness) step
  where
    step (!arena, !witness) ix = do
      rows <- Dense.readDenseFeasible frame ix
      let !src =
            SmallArray.indexSmallArray (djSources plan) ix
          (!arena1, !sourceWitness) =
            sourceRowsWitnessWithTelemetry config plan ix src rows arena
          (!arena2, !value, !timesTelemetry) =
            pvTimesWithTelemetry config (dlwValue witness) (dswValue sourceWitness) arena1
       in pure
            ( arena2,
              witness
                { dlwValue = value,
                  dlwSupportCells = Set.union (dlwSupportCells witness) (dswSupportCells sourceWitness),
                  dlwSupportRowsEnumerated =
                    dlwSupportRowsEnumerated witness + dswSupportRowsEnumerated sourceWitness,
                  dlwTelemetry = dlwTelemetry witness <> dswTelemetry sourceWitness <> timesTelemetry
                }
            )
{-# INLINE leafWitnessWithTelemetry #-}

foldDeltaDirtyLeafWitnesses ::
  RepairTelemetryConfig ->
  AssignmentTupleKey ->
  Dense.DeltaDenseFrame s ->
  DenseJoinPlan ->
  ProvArena ->
  acc ->
  ( AssignmentTupleKey ->
    DenseLeafWitness ->
    ProvArena ->
    acc ->
    ST s (ProvArena, acc)
  ) ->
  ST s (ProvArena, acc)
foldDeltaDirtyLeafWitnesses config key frame plan arena0 acc0 step =
  do
    (!arena1, !witness) <-
      deltaCurrentLeafWitnessWithTelemetry config frame plan arena0
    case dlwValue witness of
      PVZero ->
        pure (arena1, acc0)
      _ ->
        step key witness arena1 acc0
{-# INLINE foldDeltaDirtyLeafWitnesses #-}

deltaCurrentLeafWitnessWithTelemetry ::
  RepairTelemetryConfig ->
  Dense.DeltaDenseFrame s ->
  DenseJoinPlan ->
  ProvArena ->
  ST s (ProvArena, DenseLeafWitness)
deltaCurrentLeafWitnessWithTelemetry config frame plan arena0 =
  foldPlanSourceIndexesM plan (arena0, emptyDenseLeafWitness) step
  where
    step (!arena, !witness) ix = do
      rows <- Dense.readDeltaFullFeasible frame ix
      let !src =
            SmallArray.indexSmallArray (djSources plan) ix
          (!arena1, !sourceWitness) =
            sourceRowsWitnessWithTelemetry config plan ix src rows arena
          (!arena2, !value, !timesTelemetry) =
            pvTimesWithTelemetry config (dlwValue witness) (dswValue sourceWitness) arena1
      pure
        ( arena2,
          witness
            { dlwValue = value,
              dlwSupportCells = Set.union (dlwSupportCells witness) (dswSupportCells sourceWitness),
              dlwSupportRowsEnumerated =
                dlwSupportRowsEnumerated witness + dswSupportRowsEnumerated sourceWitness,
              dlwTelemetry = dlwTelemetry witness <> dswTelemetry sourceWitness <> timesTelemetry
            }
        )
{-# INLINE deltaCurrentLeafWitnessWithTelemetry #-}

foldPlanSourceIndexesM ::
  DenseJoinPlan ->
  acc ->
  (acc -> Int -> ST s acc) ->
  ST s acc
foldPlanSourceIndexesM plan !initial step =
  go 0 initial
  where
    !sourceCount =
      SmallArray.sizeofSmallArray (djSources plan)

    go !ix !acc
      | ix >= sourceCount =
          pure acc
      | otherwise = do
          acc' <- step acc ix
          go (ix + 1) acc'
{-# INLINE foldPlanSourceIndexesM #-}
