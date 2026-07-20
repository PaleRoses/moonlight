module Moonlight.Flow.Runtime.Engine.Dispatch.Carrier
  ( scheduleCarrierActionFanout,
    stepAtomEventBatch,
    applyTouchesAndScheduleFanout,
    enqueueCarrierFanout,
    enqueueCarrierFanoutChecked,
    enqueueCarrierStoreOnly,
    enqueueCarrierStoreOnlyChecked,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.List.NonEmpty
  ( NonEmpty,
  )
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Delta.Time
  ( Timed (..),
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Time
import Moonlight.Flow.Carrier.Store
  ( CarrierStoreTouch,
  )
import Moonlight.Flow.Model.Delta
  ( AtomEvent
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (PhaseRestrict),
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
  )
import Moonlight.Flow.Runtime.Carrier.Core.Types
  ( CarrierCommitTrace,
  )
import Moonlight.Flow.Runtime.Carrier.Emit
  ( atomEventDeltaAt,
  )
import Moonlight.Flow.Runtime.Carrier.Store
  ( commitCarrierDeltas,
  )
import Moonlight.Flow.Runtime.Carrier.Store.Touch
  ( applyTouches,
  )
import Moonlight.Flow.Runtime.Engine.Schedule.Enqueue
  ( enqueueScheduledRuntimeDataflowOpRuntime,
  )
import Moonlight.Flow.Runtime.Engine.Schedule.Time
  ( allocateExecutionTimeForDataflowOp,
  )
import Moonlight.Flow.Runtime.Engine.Touch
  ( scheduleCarrierCommitTraceFanout,
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( runIndexDataflowOp,
    runRestrictDataflowOp,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnv (..),
    RuntimeEnvelope (..),
    rsRouting,
  )
import Moonlight.Flow.Runtime.Time
  ( RuntimeEventTime,
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( routeCarrierShard,
    routeIndexShard,
  )

scheduleCarrierActionFanout ::
  (Ord ctx, Ord prop) =>
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      CarrierCommitTrace ctx prop
    ) ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
scheduleCarrierActionFanout action = do
  (runtimeTouched, commitTrace) <- action
  scheduleCarrierCommitTraceFanout commitTrace runtimeTouched
{-# INLINE scheduleCarrierActionFanout #-}

stepAtomEventBatch ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  RelationalScope ->
  Timed (RuntimeEventTime ctx) (NonEmpty AtomEvent) ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
stepAtomEventBatch addr scope timedEvents runtime =
  fst
    <$> commitCarrierDeltas
      ( NonEmpty.toList
        ( fmap
            ( atomEventDeltaAt
                (reAtomCarrierEmitSpec (rdrEnv runtime))
                (timedAt timedEvents)
                addr
                scope
            )
            (timedValue timedEvents)
        )
    )
    runtime
{-# INLINE stepAtomEventBatch #-}

applyTouchesAndScheduleFanout ::
  (Ord ctx, Ord prop) =>
  [Timed (RuntimeEventTime ctx) (CarrierStoreTouch ctx Carrier prop)] ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
applyTouchesAndScheduleFanout touches runtime = do
  (runtimeTouched, commitTrace) <-
    applyTouches touches runtime
  scheduleCarrierCommitTraceFanout commitTrace runtimeTouched
{-# INLINE applyTouchesAndScheduleFanout #-}

enqueueCarrierFanout ::
  (Ord ctx, Ord prop) =>
  [Timed (RuntimeEventTime ctx) (RelationalCarrierDelta ctx Carrier prop boundary evidence)] ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
enqueueCarrierFanout outputs runtime0 =
  foldM
    (\runtime timedOutput -> enqueueCarrierFanoutChecked (timedAt timedOutput) [timedOutput] runtime)
    runtime0
    outputs
{-# INLINE enqueueCarrierFanout #-}

enqueueCarrierFanoutChecked ::
  (Ord ctx, Ord prop) =>
  RuntimeEventTime ctx ->
  [Timed (RuntimeEventTime ctx) (RelationalCarrierDelta ctx Carrier prop boundary evidence)] ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
enqueueCarrierFanoutChecked parentTime outputs runtime0 =
  foldM enqueueOne runtime0 outputs
  where
    enqueueOne runtime timedOutput = do
      ensureTimedCarrierDelta parentTime timedOutput
      let carrierDelta =
            timedValue timedOutput
          addr =
            deAddr carrierDelta
      indexShard <-
        case routeIndexShard addr (rsRouting (rdrState runtime)) of
          Nothing ->
            Left (RuntimeMissingIndexRoute addr)
          Just shard ->
            Right shard
      restrictShard <-
        case routeCarrierShard PhaseRestrict addr (rsRouting (rdrState runtime)) of
          Nothing ->
            Left (RuntimeMissingRestrictRoute addr)
          Just shard ->
            Right shard
      runtimeIndexed <-
        enqueueScheduledRuntimeDataflowOpRuntime
          (Timed (timedAt timedOutput) (runIndexDataflowOp indexShard carrierDelta))
          runtime
      let restrictOp0 =
            runRestrictDataflowOp restrictShard carrierDelta
      (runtimeTimed, restrictTime) <-
        allocateExecutionTimeForDataflowOp restrictOp0 runtimeIndexed
      let restrictDelta =
            carrierDelta {deTime = restrictTime}
      enqueueScheduledRuntimeDataflowOpRuntime
        (Timed restrictTime (runRestrictDataflowOp restrictShard restrictDelta))
        runtimeTimed
{-# INLINE enqueueCarrierFanoutChecked #-}

enqueueCarrierStoreOnly ::
  (Ord ctx, Ord prop) =>
  [Timed (RuntimeEventTime ctx) (RelationalCarrierDelta ctx Carrier prop boundary evidence)] ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
enqueueCarrierStoreOnly outputs runtime0 =
  foldM
    (\runtime timedOutput -> enqueueCarrierStoreOnlyChecked (timedAt timedOutput) [timedOutput] runtime)
    runtime0
    outputs
{-# INLINE enqueueCarrierStoreOnly #-}

enqueueCarrierStoreOnlyChecked ::
  (Ord ctx, Ord prop) =>
  RuntimeEventTime ctx ->
  [Timed (RuntimeEventTime ctx) (RelationalCarrierDelta ctx Carrier prop boundary evidence)] ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
enqueueCarrierStoreOnlyChecked parentTime outputs runtime0 =
  foldM enqueueOne runtime0 outputs
  where
    enqueueOne runtime timedOutput = do
      ensureTimedCarrierDelta parentTime timedOutput
      let carrierDelta =
            timedValue timedOutput
          addr =
            deAddr carrierDelta
      indexShard <-
        case routeIndexShard addr (rsRouting (rdrState runtime)) of
          Nothing ->
            Left (RuntimeMissingIndexRoute addr)
          Just shard ->
            Right shard
      enqueueScheduledRuntimeDataflowOpRuntime
        (Timed (timedAt timedOutput) (runIndexDataflowOp indexShard carrierDelta))
        runtime
{-# INLINE enqueueCarrierStoreOnlyChecked #-}

ensureTimedCarrierDelta ::
  Ord ctx =>
  RuntimeEventTime ctx ->
  Timed (RuntimeEventTime ctx) (RelationalCarrierDelta ctx Carrier prop boundary evidence) ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ()
ensureTimedCarrierDelta parentTime timedDelta = do
  validateOutputTime parentTime (timedAt timedDelta)
  if timedAt timedDelta == deTime (timedValue timedDelta)
    then Right ()
    else Left (RuntimeOperatorTimeEscape (deTime (timedValue timedDelta)) (timedAt timedDelta))
{-# INLINE ensureTimedCarrierDelta #-}

validateOutputTime ::
  RuntimeEventTime ctx ->
  RuntimeEventTime ctx ->
  Either (RelationalRuntimeError ctx prop boundary evidence) ()
validateOutputTime parentTime outputTime =
  if sameLogicalTime parentTime outputTime
    then Right ()
    else Left (RuntimeOperatorTimeEscape parentTime outputTime)
{-# INLINE validateOutputTime #-}

sameLogicalTime :: RuntimeEventTime ctx -> RuntimeEventTime ctx -> Bool
sameLogicalTime leftTime rightTime =
  relationalTimeQuotientEpoch leftTime == relationalTimeQuotientEpoch rightTime
    && relationalTimeLiveEpoch leftTime == relationalTimeLiveEpoch rightTime
{-# INLINE sameLogicalTime #-}
