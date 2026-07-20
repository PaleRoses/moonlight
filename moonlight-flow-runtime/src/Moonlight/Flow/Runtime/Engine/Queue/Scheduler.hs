{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Runtime.Engine.Queue.Scheduler
  ( runtimeDataflowPriorityPlan,
    runtimeDataflowScheduledPhase,
    runtimeDataflowProgressTime,
  )
where

import Moonlight.Delta.Time
  ( timedAt,
    timedValue,
  )
import Moonlight.Differential.Runtime.Schedule
  ( ScheduleError,
    SchedulePriorityPlan,
    mkSchedulePriorityPlan,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    retimeRelationalCarrierPhase,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (..),
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( RuntimeDataflowOp,
    ScheduledRuntimeDataflowOp,
    runtimeDataflowContractPhase,
    runtimeDataflowOpContract,
  )

runtimeDataflowPriorityPlan ::
  Either
    (ScheduleError RelationalPhase)
    (SchedulePriorityPlan RelationalPhase)
runtimeDataflowPriorityPlan =
  mkSchedulePriorityPlan
    [ PhaseProject,
      PhaseSubsumption,
      PhaseRestrict,
      PhaseAmalgamate,
      PhaseIndex,
      PhaseVisible,
      PhaseObstruction,
      PhaseJoin
    ]
{-# INLINE runtimeDataflowPriorityPlan #-}

runtimeDataflowScheduledPhase ::
  RuntimeDataflowOp ctx prop boundary evidence ->
  RelationalPhase
runtimeDataflowScheduledPhase =
  runtimeDataflowContractPhase . runtimeDataflowOpContract
{-# INLINE runtimeDataflowScheduledPhase #-}

runtimeDataflowProgressTime ::
  ScheduledRuntimeDataflowOp ctx prop boundary evidence ->
  RelationalCarrierTime ctx
runtimeDataflowProgressTime scheduledOp =
  retimeRelationalCarrierPhase
    (runtimeDataflowScheduledPhase (timedValue scheduledOp))
    (timedAt scheduledOp)
{-# INLINE runtimeDataflowProgressTime #-}
