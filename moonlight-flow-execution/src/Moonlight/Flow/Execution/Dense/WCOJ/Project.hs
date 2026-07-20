{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Flow.Execution.Dense.WCOJ.Project
  ( denseJoinRows,
    denseJoinDeltaRows,
    denseJoinSupportIds,
    joinProjectDenseWCOJ,
    foldProjectDenseWCOJ,
    foldProjectDenseWCOJKeys,
    foldProjectDenseWCOJWitnesses,
    foldProjectDenseWCOJWitnessesWithTelemetry,
    foldProjectDenseWCOJDeltaWitnessesWithTelemetry,
    foldProjectDenseWCOJWitnessesWithSupportSources,
    foldProjectDenseWCOJSelectedWitnesses,
    foldProjectDenseWCOJPlanWitnesses,
    foldProjectDenseWCOJDeltaPlanWitnesses,
    projectDenseWCOJRows,
  )
where

import Control.Monad.ST (ST, runST)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.PrimArray qualified as PrimArray
import Data.Primitive.SmallArray qualified as SmallArray
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Flow.Execution.Dense.Plan
  ( DenseArrangement,
    DenseJoinPlan,
    DenseJoinPlanError,
    denseArrangementAtomId,
    denseArrangementDirtyRows,
    denseArrangementUnionSchema,
    denseJoinPlanOutputSchema,
    denseJoinPlanProblem,
    denseJoinPlanSelectedKeys,
    denseJoinPlanSources,
    mkDenseJoinPlan,
    mkDenseJoinPlanWithSupportSources,
    selectedOutputDomainFromKeys,
  )
import Moonlight.Differential.Join.WCOJ.Dense.Executor qualified as Dense
import Moonlight.Flow.Execution.Dense.WCOJ.Witness
import Moonlight.Flow.Execution.Factor.Core
  ( Factor,
    mkFactor,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvArena,
    ProvVal (..),
  )
import Moonlight.Flow.Execution.Observe.Provenance.Value
  ( pvPlus,
  )
import Moonlight.Flow.Execution.Observe.RepairTelemetry
  ( RepairTelemetryConfig,
    summaryRepairTelemetryConfig,
  )
import Moonlight.Differential.Index.RowSet
  ( RowSet,
    rowSetNull,
    rowSetUnion,
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Plan.Query.Core

denseJoinRows :: DenseJoinPlan -> [RowTupleKey]
denseJoinRows plan =
  Dense.foldDenseWCOJ
    (denseJoinPlanProblem plan)
    ( \leaf rows -> do
        maybeRow <- Dense.denseLeafTupleKey (PrimArray.primArrayToList (denseJoinPlanOutputSchema plan)) leaf
        pure $
          case maybeRow of
            Nothing -> rows
            Just row -> row : rows
    )
    []

denseJoinDeltaRows :: DenseJoinPlan -> [RowTupleKey]
denseJoinDeltaRows plan =
  let !outputSchema =
        PrimArray.primArrayToList (denseJoinPlanOutputSchema plan)
   in Dense.foldDenseDeltaWCOJ
        (denseJoinPlanProblem plan)
        ( \leaf rows -> do
            maybeRow <- Dense.denseDeltaLeafTupleKey outputSchema leaf
            pure $
              case maybeRow of
                Nothing -> rows
                Just row -> row : rows
        )
        []

denseJoinSupportIds :: DenseJoinPlan -> IntMap RowSet
denseJoinSupportIds plan =
  let !problem =
        denseJoinPlanProblem plan
   in Dense.foldDenseWCOJ problem (step problem) IntMap.empty
  where
    step :: forall s. Dense.DenseDeltaProblem -> Dense.DenseLeaf s -> IntMap RowSet -> ST s (IntMap RowSet)
    step problem leaf support =
      Dense.foldDenseSourceIndexesM problem support (collectSupport leaf)

    collectSupport :: Dense.DenseLeaf s -> IntMap RowSet -> Int -> ST s (IntMap RowSet)
    collectSupport leaf support ix = do
      rows <- Dense.readDenseFeasible leaf ix
      let src = SmallArray.indexSmallArray (denseJoinPlanSources plan) ix
      pure $
        case denseArrangementAtomId src of
          Just atomIdValue ->
            IntMap.insertWith rowSetUnion (atomIdKey atomIdValue) rows support
          Nothing ->
            support

joinProjectDenseWCOJ ::
  [SlotId] ->
  [DenseArrangement] ->
  ProvArena ->
  Either DenseJoinPlanError (ProvArena, Factor)
joinProjectDenseWCOJ outputSchema sources arena0 =
  fmap
    (\(!arena1, !rows) -> (arena1, mkFactor outputSchema rows))
    (projectDenseWCOJRows outputSchema sources arena0)

foldProjectDenseWCOJ ::
  [SlotId] ->
  [DenseArrangement] ->
  ProvArena ->
  acc ->
  (AssignmentTupleKey -> ProvVal -> ProvArena -> acc -> (ProvArena, acc)) ->
  Either DenseJoinPlanError (ProvArena, acc)
foldProjectDenseWCOJ outputSchema sources arena0 initial step =
  foldProjectDenseWCOJWitnessesWithSupportSources
    summaryRepairTelemetryConfig
    outputSchema
    IntSet.empty
    sources
    arena0
    initial
    ( \key witness arena acc ->
        let (!arena1, !acc1) =
              step key (dlwValue witness) arena acc
         in pure (arena1, acc1)
    )

foldProjectDenseWCOJKeys ::
  [SlotId] ->
  [DenseArrangement] ->
  acc ->
  (AssignmentTupleKey -> acc -> acc) ->
  Either DenseJoinPlanError acc
foldProjectDenseWCOJKeys outputSchema sources initial step =
  case sources of
    [] ->
      Right
        ( if null outputSchema
            then step emptyTupleKey initial
            else initial
        )
    _ ->
      let !fullSchema =
            denseArrangementUnionSchema sources
          !outputSlotKeys =
            fmap slotIdKey outputSchema
       in fmap
            ( \plan ->
                Dense.foldDenseWCOJ
                  (denseJoinPlanProblem plan)
                  ( \leaf !acc -> do
                      maybeKey <- Dense.denseLeafTupleKey outputSlotKeys leaf
                      pure $
                        case maybeKey of
                          Nothing ->
                            acc
                          Just key ->
                            step key acc
                  )
                  initial
            )
            (mkDenseJoinPlan fullSchema outputSchema sources)

foldProjectDenseWCOJWitnesses ::
  [SlotId] ->
  [DenseArrangement] ->
  ProvArena ->
  acc ->
  ( forall s.
    AssignmentTupleKey ->
    DenseLeafWitness ->
    ProvArena ->
    acc ->
    ST s (ProvArena, acc)
  ) ->
  Either DenseJoinPlanError (ProvArena, acc)
foldProjectDenseWCOJWitnesses outputSchema sources =
  foldProjectDenseWCOJWitnessesWithSupportSources
    summaryRepairTelemetryConfig
    outputSchema
    (allSourceIndexes sources)
    sources

foldProjectDenseWCOJWitnessesWithTelemetry ::
  RepairTelemetryConfig ->
  [SlotId] ->
  [DenseArrangement] ->
  ProvArena ->
  acc ->
  ( forall s.
    AssignmentTupleKey ->
    DenseLeafWitness ->
    ProvArena ->
    acc ->
    ST s (ProvArena, acc)
  ) ->
  Either DenseJoinPlanError (ProvArena, acc)
foldProjectDenseWCOJWitnessesWithTelemetry config outputSchema sources =
  foldProjectDenseWCOJWitnessesWithSupportSources
    config
    outputSchema
    (allSourceIndexes sources)
    sources

foldProjectDenseWCOJDeltaWitnessesWithTelemetry ::
  RepairTelemetryConfig ->
  [SlotId] ->
  [DenseArrangement] ->
  ProvArena ->
  acc ->
  ( forall s.
    AssignmentTupleKey ->
    DenseLeafWitness ->
    ProvArena ->
    acc ->
    ST s (ProvArena, acc)
  ) ->
  Either DenseJoinPlanError (ProvArena, acc)
foldProjectDenseWCOJDeltaWitnessesWithTelemetry config outputSchema sources arena0 initial step
  | not (anySourceDirtyRows sources) =
      Right (arena0, initial)
  | otherwise =
      case sources of
        [] ->
          Right (arena0, initial)
        _ ->
          let !fullSchema =
                denseArrangementUnionSchema sources
           in fmap
                (\plan -> foldProjectDenseWCOJDeltaPlanWitnesses config plan arena0 initial step)
                ( mkDenseJoinPlanWithSupportSources
                    fullSchema
                    outputSchema
                    (allSourceIndexes sources)
                    Nothing
                    sources
                )

anySourceDirtyRows :: [DenseArrangement] -> Bool
anySourceDirtyRows =
  any (not . rowSetNull . denseArrangementDirtyRows)
{-# INLINE anySourceDirtyRows #-}

allSourceIndexes :: [DenseArrangement] -> IntSet
allSourceIndexes sources
  | null sources =
      IntSet.empty
  | otherwise =
      IntSet.fromRange (0, length sources - 1)
{-# INLINE allSourceIndexes #-}

foldProjectDenseWCOJWitnessesWithSupportSources ::
  RepairTelemetryConfig ->
  [SlotId] ->
  IntSet ->
  [DenseArrangement] ->
  ProvArena ->
  acc ->
  ( forall s.
    AssignmentTupleKey ->
    DenseLeafWitness ->
    ProvArena ->
    acc ->
    ST s (ProvArena, acc)
  ) ->
  Either DenseJoinPlanError (ProvArena, acc)
foldProjectDenseWCOJWitnessesWithSupportSources config outputSchema supportSources sources arena0 initial step =
  case sources of
    [] ->
      Right
        ( if null outputSchema
            then runST (step emptyTupleKey emptyDenseLeafWitness arena0 initial)
            else (arena0, initial)
        )
    _ ->
      let !fullSchema =
            denseArrangementUnionSchema sources
       in fmap
            (\plan -> foldProjectDenseWCOJPlanWitnesses config plan arena0 initial step)
            ( mkDenseJoinPlanWithSupportSources
                fullSchema
                outputSchema
                supportSources
                Nothing
                sources
            )

foldProjectDenseWCOJSelectedWitnesses ::
  RepairTelemetryConfig ->
  [SlotId] ->
  Set AssignmentTupleKey ->
  [DenseArrangement] ->
  ProvArena ->
  acc ->
  ( forall s.
    AssignmentTupleKey ->
    DenseLeafWitness ->
    ProvArena ->
    acc ->
    ST s (ProvArena, acc)
  ) ->
  Either DenseJoinPlanError (ProvArena, acc)
foldProjectDenseWCOJSelectedWitnesses config outputSchema selectedKeys sources arena0 initial step
  | Set.null selectedKeys =
      Right (arena0, initial)
  | not (outputSchemaContainedIn realFullSchema outputSchema) =
      Right (arena0, initial)
  | otherwise =
      case selectedOutputDomainFromKeys outputSchema selectedKeys of
        Nothing ->
          Right (arena0, initial)
        Just selectedOutput ->
          let !supportSources =
                allSourceIndexes sources
           in fmap
                (\plan -> foldProjectDenseWCOJPlanWitnesses config plan arena0 initial step)
                ( mkDenseJoinPlanWithSupportSources
                    realFullSchema
                    outputSchema
                    supportSources
                    (Just selectedOutput)
                    sources
                )
  where
    !realFullSchema =
      denseArrangementUnionSchema sources

outputSchemaContainedIn :: [SlotId] -> [SlotId] -> Bool
outputSchemaContainedIn fullSchema outputSchema =
  let !fullKeys =
        IntSet.fromList (fmap slotIdKey fullSchema)
   in all (\sid -> IntSet.member (slotIdKey sid) fullKeys) outputSchema
{-# INLINE outputSchemaContainedIn #-}

selectedOutputContains ::
  DenseJoinPlan ->
  AssignmentTupleKey ->
  Bool
selectedOutputContains plan key =
  case denseJoinPlanSelectedKeys plan of
    Nothing ->
      True
    Just selected ->
      Set.member key selected
{-# INLINE selectedOutputContains #-}

foldProjectDenseWCOJPlanWitnesses ::
  RepairTelemetryConfig ->
  DenseJoinPlan ->
  ProvArena ->
  acc ->
  ( forall s.
    AssignmentTupleKey ->
    DenseLeafWitness ->
    ProvArena ->
    acc ->
    ST s (ProvArena, acc)
  ) ->
  (ProvArena, acc)
foldProjectDenseWCOJPlanWitnesses config plan arena0 initial step =
  let !outputSchema =
        PrimArray.primArrayToList (denseJoinPlanOutputSchema plan)
      !problem =
        denseJoinPlanProblem plan
   in Dense.foldDenseWCOJ
        problem
        ( \leaf (!arena, !acc) -> do
            maybeKey <- Dense.denseLeafTupleKey outputSchema leaf
            case maybeKey of
              Nothing ->
                pure (arena, acc)
              Just key
                | not (selectedOutputContains plan key) ->
                    pure (arena, acc)
                | otherwise -> do
                    (!arena1, !witness) <-
                      leafWitnessWithTelemetry config leaf plan arena
                    case dlwValue witness of
                      PVZero ->
                        pure (arena1, acc)
                      _ ->
                        step key witness arena1 acc
        )
        (arena0, initial)

foldProjectDenseWCOJDeltaPlanWitnesses ::
  RepairTelemetryConfig ->
  DenseJoinPlan ->
  ProvArena ->
  acc ->
  ( forall s.
    AssignmentTupleKey ->
    DenseLeafWitness ->
    ProvArena ->
    acc ->
    ST s (ProvArena, acc)
  ) ->
  (ProvArena, acc)
foldProjectDenseWCOJDeltaPlanWitnesses config plan arena0 initial step =
  let !outputSchema =
        PrimArray.primArrayToList (denseJoinPlanOutputSchema plan)
      !problem =
        denseJoinPlanProblem plan
   in Dense.foldDenseDeltaWCOJ
        problem
        ( \leaf (!arena, !acc) -> do
            maybeKey <- Dense.denseDeltaLeafTupleKey outputSchema leaf
            case maybeKey of
              Nothing ->
                pure (arena, acc)
              Just key
                | not (selectedOutputContains plan key) ->
                    pure (arena, acc)
                | otherwise ->
                    foldDeltaDirtyLeafWitnesses config key leaf plan arena acc step
        )
        (arena0, initial)

projectDenseWCOJRows ::
  [SlotId] ->
  [DenseArrangement] ->
  ProvArena ->
  Either DenseJoinPlanError (ProvArena, Map AssignmentTupleKey ProvVal)
projectDenseWCOJRows outputSchema sources arena0 =
  foldProjectDenseWCOJ
    outputSchema
    sources
    arena0
    Map.empty
    insertOutput

insertOutput ::
  AssignmentTupleKey ->
  ProvVal ->
  ProvArena ->
  Map AssignmentTupleKey ProvVal ->
  (ProvArena, Map AssignmentTupleKey ProvVal)
insertOutput _ PVZero arena rows =
  (arena, rows)
insertOutput key value arena rows =
  case Map.lookup key rows of
    Nothing ->
      (arena, Map.insert key value rows)
    Just old ->
      let (arena1, merged) = pvPlus old value arena
       in (arena1, Map.insert key merged rows)
{-# INLINE insertOutput #-}
