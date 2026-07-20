{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Runtime.Engine.State
  ( RuntimeEngineState (..),
    emptyRuntimeEngineState,
    runtimeEngineQueue,
    setRuntimeEngineQueue,
    mapRuntimeEngineQueue,
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Differential.Runtime.Schedule
  ( SchedulePriorityPlan,
  )
import Moonlight.Flow.Carrier.Core.Frontier
  ( RelDiffFrontier,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase,
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Engine.Queue.Types
  ( RuntimeDataflowQueue,
    emptyRuntimeDataflowQueue,
  )

type RuntimeEngineState :: Type -> Type -> Type -> Type -> Type
data RuntimeEngineState ctx prop boundary evidence = RuntimeEngineState
  { resQueue :: !(RuntimeDataflowQueue ctx prop boundary evidence)
  }

emptyRuntimeEngineState ::
  SchedulePriorityPlan RelationalPhase ->
  RelDiffFrontier ctx RelationalPhase ->
  RuntimeEngineState ctx prop boundary evidence
emptyRuntimeEngineState priorityPlan frontier =
  RuntimeEngineState
    { resQueue = emptyRuntimeDataflowQueue priorityPlan frontier
    }
{-# INLINE emptyRuntimeEngineState #-}

runtimeEngineQueue ::
  Core.RuntimeState topology (RuntimeEngineState ctx prop boundary evidence) carrier factor ->
  RuntimeDataflowQueue ctx prop boundary evidence
runtimeEngineQueue =
  resQueue . Core.rsEngine
{-# INLINE runtimeEngineQueue #-}

setRuntimeEngineQueue ::
  RuntimeDataflowQueue ctx prop boundary evidence ->
  Core.RuntimeState topology (RuntimeEngineState ctx prop boundary evidence) carrier factor ->
  Core.RuntimeState topology (RuntimeEngineState ctx prop boundary evidence) carrier factor
setRuntimeEngineQueue queue =
  Core.mapRuntimeEngineSection
    ( \engineState ->
        engineState {resQueue = queue}
    )
{-# INLINE setRuntimeEngineQueue #-}

mapRuntimeEngineQueue ::
  (RuntimeDataflowQueue ctx prop boundary evidence -> RuntimeDataflowQueue ctx prop boundary evidence) ->
  Core.RuntimeState topology (RuntimeEngineState ctx prop boundary evidence) carrier factor ->
  Core.RuntimeState topology (RuntimeEngineState ctx prop boundary evidence) carrier factor
mapRuntimeEngineQueue update state =
  setRuntimeEngineQueue (update (runtimeEngineQueue state)) state
{-# INLINE mapRuntimeEngineQueue #-}
