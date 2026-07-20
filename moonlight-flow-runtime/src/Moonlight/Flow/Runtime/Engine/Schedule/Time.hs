module Moonlight.Flow.Runtime.Engine.Schedule.Time
  ( allocateExecutionTime,
    allocateExecutionTimeForContract,
    allocateExecutionTimeForDataflowOp,
  )
where

import Moonlight.Differential.Time
  ( nextFrontierStamp,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    mkRelationalCarrierTime,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase,
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( RuntimeDataflowContract,
    RuntimeDataflowOp,
    runtimeDataflowContractPhase,
    runtimeDataflowOpContext,
    runtimeDataflowOpContract,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
  )

allocateExecutionTime ::
  RelationalPhase ->
  ctx ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr, RelationalCarrierTime ctx)
allocateExecutionTime phaseValue contextValue runtime =
  case nextFrontierStamp (Core.rcsNextFrontierStamp clock0) of
    Nothing ->
      Left (RuntimeFrontierStampOverflow (Core.rcsNextFrontierStamp clock0))
    Just nextStamp ->
      Right
        ( runtime
            { rdrState =
                Core.setRuntimeClockState
                  clock0 {Core.rcsNextFrontierStamp = nextStamp}
                  state0
            },
          mkRelationalCarrierTime
            contextValue
            (Core.rcsQuotientEpoch clock0)
            (Core.rcsLiveEpoch clock0)
            phaseValue
            (Core.rcsNextFrontierStamp clock0)
        )
  where
    state0 =
      rdrState runtime

    clock0 =
      Core.rsClock state0
{-# INLINE allocateExecutionTime #-}

allocateExecutionTimeForContract ::
  RuntimeDataflowContract ctx Carrier prop ->
  ctx ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr, RelationalCarrierTime ctx)
allocateExecutionTimeForContract contract =
  allocateExecutionTime (runtimeDataflowContractPhase contract)
{-# INLINE allocateExecutionTimeForContract #-}

allocateExecutionTimeForDataflowOp ::
  RuntimeDataflowOp ctx prop boundary evidence ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr, RelationalCarrierTime ctx)
allocateExecutionTimeForDataflowOp op =
  allocateExecutionTimeForContract
    (runtimeDataflowOpContract op)
    (runtimeDataflowOpContext op)
{-# INLINE allocateExecutionTimeForDataflowOp #-}
