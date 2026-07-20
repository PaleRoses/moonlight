{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Runtime.Engine.Step.Drain
  ( drainRuntimeDataflowQueue,
    stepScheduledRuntimeDataflowOp,
  )
where

import Data.Foldable qualified as Foldable
import Data.Bifunctor
  ( first,
  )
import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Delta.Time
  ( Timed (..),
  )
import Moonlight.Differential.Frontier
  ( RuntimeCapability,
    RuntimeFrontierError,
    downgradeRuntimeCapability,
    mintRootRuntimeCapability,
    runtimeCapabilityTime,
  )
import Moonlight.Differential.Runtime.Schedule
  ( ScheduleCell (..),
    ScheduledWork (..),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    RelationalRuntimeEpoch,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Runtime.Engine.Capability
  ( RelationalDrainEmission (..),
    relationalDrainEmissionForOp,
    validateRelationalCapabilityTransport,
  )
import Moonlight.Flow.Runtime.Engine.Dispatch.Core qualified as Dispatch
import Moonlight.Flow.Runtime.Engine.Queue.Frontier
  ( completeScheduledRuntimeDataflowOp,
  )
import Moonlight.Flow.Runtime.Engine.Queue.Types
  ( RuntimeDataflowQueue,
    completeRuntimeDataflowCapability,
    dequeueRuntimeDataflowQueue,
    emptyRuntimeDataflowQueue,
    enqueueRuntimeDataflowBatch,
    runtimeDataflowQueueCells,
    runtimeDataflowQueueFrontier,
    runtimeDataflowQueuePriorityPlan,
  )
import Moonlight.Flow.Runtime.Engine.State
  ( runtimeEngineQueue,
    setRuntimeEngineQueue,
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( RuntimeDataflowOp,
    ScheduledRuntimeDataflowOp,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
  )

drainRuntimeDataflowQueue ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop,
    Semigroup evidence
  ) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
drainRuntimeDataflowQueue !runtime0 = do
  (runtime1, queue1) <-
    drainRuntimeDataflowCells
      queue0
      runtimeWithoutQueueCells
  let state1 =
        rdrState runtime1
  Right
    runtime1
      { rdrState =
          setRuntimeEngineQueue
            queue1
            state1
      }
  where
    state0 =
      rdrState runtime0

    queue0 =
      runtimeEngineQueue state0

    runtimeWithoutQueueCells =
      runtime0
        { rdrState =
            setRuntimeEngineQueue
              ( emptyRuntimeDataflowQueue
                  (runtimeDataflowQueuePriorityPlan queue0)
                  (runtimeDataflowQueueFrontier queue0)
              )
              state0
        }
{-# INLINE drainRuntimeDataflowQueue #-}

data RuntimeDataflowEmission ctx prop boundary evidence = RuntimeDataflowEmission
  { rdfePhase :: !RelationalPhase,
    rdfeBatch :: !(NonEmpty (RuntimeDataflowOp ctx prop boundary evidence)),
    rdfeAuthorization :: !(RelationalDrainEmission ctx prop)
  }

drainRuntimeDataflowCells ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop,
    Semigroup evidence
  ) =>
  RuntimeDataflowQueue ctx prop boundary evidence ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr, RuntimeDataflowQueue ctx prop boundary evidence)
drainRuntimeDataflowCells queue0 runtime0 =
  case dequeueRuntimeDataflowQueue queue0 of
    Nothing ->
      Right (runtime0, queue0)
    Just (cell, queueWithoutCell) -> do
      let scheduledCell =
            scheduledWorkCell cell
          parentCapability =
            scheduleCellCapability scheduledCell
      completedQueue <-
        mapRuntimeFrontierCompletion
          (completeRuntimeDataflowCapability parentCapability queueWithoutCell)
      (runtime1, emissions) <-
        stepRuntimeDataflowBatch
          parentCapability
          (scheduleCellPayload scheduledCell)
          runtime0
      nextQueue <-
        Foldable.foldlM
          (authorizeRuntimeDataflowEmission parentCapability)
          completedQueue
          emissions
      drainRuntimeDataflowCells nextQueue runtime1
{-# INLINE drainRuntimeDataflowCells #-}

authorizeRelationalDrainEmission ::
  Ord ctx =>
  RuntimeCapability ctx RelationalRuntimeEpoch RelationalPhase ->
  RelationalDrainEmission ctx prop ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RuntimeCapability ctx RelationalRuntimeEpoch RelationalPhase)
authorizeRelationalDrainEmission parentCapability emission =
  case emission of
    EmitDowngrade targetTime ->
      case downgradeRuntimeCapability targetTime parentCapability of
        Left err ->
          Left (RuntimeCapabilityAdvanceInvalid err)
        Right childCapability ->
          Right childCapability
    EmitTransport transport targetTime ->
      case validateRelationalCapabilityTransport parentCapability transport targetTime of
        Left err ->
          Left (RuntimeCapabilityTransportIllegal err)
        Right () ->
          Right (mintRootRuntimeCapability targetTime)
{-# INLINE authorizeRelationalDrainEmission #-}

stepRuntimeDataflowBatch ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop,
    Semigroup evidence
  ) =>
  RuntimeCapability ctx RelationalRuntimeEpoch RelationalPhase ->
  NonEmpty (RuntimeDataflowOp ctx prop boundary evidence) ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      [RuntimeDataflowEmission ctx prop boundary evidence]
    )
stepRuntimeDataflowBatch capability batch runtime0 = do
  runtimeStepped <-
    Foldable.foldlM
      dispatchScheduledRuntimeDataflowOp
      runtime0
      (Timed (runtimeCapabilityTime capability) <$> NonEmpty.toList batch)
  let state1 =
        rdrState runtimeStepped
      queue1 =
        runtimeEngineQueue state1
  completedStepQueue <-
    mapRuntimeFrontierCompletion
      (completeRuntimeDataflowCapability capability queue1)
  followupEmissions <-
    fmap concat
      (traverse (runtimeEmissionsFromWork capability) (runtimeDataflowQueueCells queue1))
  Right
    ( runtimeStepped
        { rdrState =
            setRuntimeEngineQueue
              ( emptyRuntimeDataflowQueue
                  (runtimeDataflowQueuePriorityPlan completedStepQueue)
                  (runtimeDataflowQueueFrontier completedStepQueue)
              )
              state1
        },
      followupEmissions
    )
{-# INLINE stepRuntimeDataflowBatch #-}

runtimeEmissionsFromWork ::
  Ord ctx =>
  RuntimeCapability ctx RelationalRuntimeEpoch RelationalPhase ->
  ScheduledWork
    ctx
    RelationalRuntimeEpoch
    RelationalPhase
    RelationalPhase
    (NonEmpty (RuntimeDataflowOp ctx prop boundary evidence)) ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    [RuntimeDataflowEmission ctx prop boundary evidence]
runtimeEmissionsFromWork parentCapability work =
  traverse
    (runtimeEmissionFromOp parentCapability (scheduledWorkPriority work) (scheduledWorkTime work))
    (NonEmpty.toList (scheduleCellPayload (scheduledWorkCell work)))
{-# INLINE runtimeEmissionsFromWork #-}

runtimeEmissionFromOp ::
  Ord ctx =>
  RuntimeCapability ctx RelationalRuntimeEpoch RelationalPhase ->
  RelationalPhase ->
  RelationalCarrierTime ctx ->
  RuntimeDataflowOp ctx prop boundary evidence ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RuntimeDataflowEmission ctx prop boundary evidence)
runtimeEmissionFromOp parentCapability phaseValue targetTime op = do
  authorization <-
    case relationalDrainEmissionForOp parentCapability targetTime op of
      Left missing ->
        Left (RuntimeCapabilityTransportMissing missing)
      Right emission ->
        Right emission
  Right
    RuntimeDataflowEmission
      { rdfePhase = phaseValue,
        rdfeBatch = op :| [],
        rdfeAuthorization = authorization
      }
{-# INLINE runtimeEmissionFromOp #-}

authorizeRuntimeDataflowEmission ::
  Ord ctx =>
  RuntimeCapability ctx RelationalRuntimeEpoch RelationalPhase ->
  RuntimeDataflowQueue ctx prop boundary evidence ->
  RuntimeDataflowEmission ctx prop boundary evidence ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RuntimeDataflowQueue ctx prop boundary evidence)
authorizeRuntimeDataflowEmission parentCapability queue emission = do
  childCapability <-
    authorizeRelationalDrainEmission parentCapability (rdfeAuthorization emission)
  first RuntimeSchedulePriorityInvalid
    ( enqueueRuntimeDataflowBatch
        (rdfePhase emission)
        childCapability
        (rdfeBatch emission)
        queue
    )
{-# INLINE authorizeRuntimeDataflowEmission #-}

dispatchScheduledRuntimeDataflowOp ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop,
    Semigroup evidence
  ) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  ScheduledRuntimeDataflowOp ctx prop boundary evidence ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
dispatchScheduledRuntimeDataflowOp runtime scheduledOp =
  Dispatch.stepScheduledRuntimeDataflowOp scheduledOp runtime
{-# INLINE dispatchScheduledRuntimeDataflowOp #-}

stepScheduledRuntimeDataflowOp ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop,
    Semigroup evidence
  ) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  ScheduledRuntimeDataflowOp ctx prop boundary evidence ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
stepScheduledRuntimeDataflowOp runtime0 scheduledOp =
  do
    queueReady <-
      mapRuntimeFrontierCompletion
        (completeScheduledRuntimeDataflowOp scheduledOp (runtimeEngineQueue state0))
    Dispatch.stepScheduledRuntimeDataflowOp scheduledOp
      runtime0
        { rdrState =
            setRuntimeEngineQueue
              queueReady
              state0
        }
  where
    state0 =
      rdrState runtime0
{-# INLINE stepScheduledRuntimeDataflowOp #-}

mapRuntimeFrontierCompletion ::
  Either (RuntimeFrontierError ctx RelationalRuntimeEpoch RelationalPhase) value ->
  Either (RelationalRuntimeError ctx prop boundary evidence) value
mapRuntimeFrontierCompletion =
  either (Left . RuntimeFrontierPendingCompletionInvalid) Right
{-# INLINE mapRuntimeFrontierCompletion #-}
