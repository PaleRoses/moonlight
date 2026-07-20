module Moonlight.Flow.Runtime.Engine.Step.Settle
  ( settleRuntime,
    settleRuntimeFixedPoint,
    settleRuntimeFixedPointBounded,
  )
where

import Moonlight.Differential.Runtime.Settle
  ( RuntimeSettleStep (..),
    runRuntimeSettleLoop,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Runtime.Engine.Queue.Types
  ( runtimeDataflowQueueEmpty,
  )
import Moonlight.Flow.Runtime.Engine.State
  ( runtimeEngineQueue,
  )
import Moonlight.Flow.Runtime.Engine.Step.Drain
  ( drainRuntimeDataflowQueue,
  )
import Moonlight.Flow.Runtime.Engine.Step.Flush
  ( flushRuntimeOnce,
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
  )

settleRuntime ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop,
    Semigroup evidence
  ) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
settleRuntime =
  settleRuntimeFixedPoint
{-# INLINE settleRuntime #-}

settleRuntimeFixedPoint ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop,
    Semigroup evidence
  ) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
settleRuntimeFixedPoint =
  settleRuntimeFixedPointBounded 64
{-# INLINE settleRuntimeFixedPoint #-}

settleRuntimeFixedPointBounded ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop,
    Semigroup evidence
  ) =>
  Int ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
settleRuntimeFixedPointBounded iterationLimit runtime
  | iterationLimit <= 0 =
      Left (RuntimeFixedPointIterationLimitExceeded iterationLimit)
  | otherwise = do
      settleResult <-
        runRuntimeSettleLoop
          iterationLimit
          runtimeSettleStep
          runtime
      case settleResult of
        Left _budgetExhausted ->
          Left (RuntimeFixedPointIterationLimitExceeded iterationLimit)
        Right settledRuntime ->
          Right settledRuntime
{-# INLINE settleRuntimeFixedPointBounded #-}

runtimeSettleStep ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop,
    Semigroup evidence
  ) =>
  RuntimeSettleStep
    (Either (RelationalRuntimeError ctx prop boundary evidence))
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
    ()
runtimeSettleStep =
  RuntimeSettleStep
    { rssDrain = drainRuntimeDataflowQueue,
      rssFlush = flushRuntimeOnce,
      rssQuiescent = runtimeQueueSettled,
      rssResidual = const ()
    }
{-# INLINE runtimeSettleStep #-}

runtimeQueueSettled ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Bool
runtimeQueueSettled =
  runtimeDataflowQueueEmpty . runtimeEngineQueue . rdrState
{-# INLINE runtimeQueueSettled #-}
