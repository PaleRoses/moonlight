{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Runtime.Settle
  ( RuntimeSettleStep (..),
    RuntimeScopedSettleStep (..),
    runRuntimeSettleLoop,
    runRuntimeSettleLoopScoped,
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Differential.Runtime.Error
  ( RuntimeSettleBudgetExhausted (..),
  )
import Moonlight.Differential.Time
  ( RuntimeScope,
  )

type RuntimeSettleStep :: (Type -> Type) -> Type -> Type -> Type
data RuntimeSettleStep m state residual = RuntimeSettleStep
  { rssDrain :: state -> m state,
    rssFlush :: state -> m state,
    rssQuiescent :: state -> Bool,
    rssResidual :: state -> residual
  }

type RuntimeScopedSettleStep :: (Type -> Type) -> Type -> Type -> Type
data RuntimeScopedSettleStep m state residual = RuntimeScopedSettleStep
  { rssScopedDrain :: (RuntimeScope -> Bool) -> state -> m state,
    rssScopedFlush :: (RuntimeScope -> Bool) -> state -> m state,
    rssScopedQuiescent :: (RuntimeScope -> Bool) -> state -> Bool,
    rssScopedResidual :: (RuntimeScope -> Bool) -> state -> residual
  }

runRuntimeSettleLoop ::
  Monad m =>
  Int ->
  RuntimeSettleStep m state residual ->
  state ->
  m (Either (RuntimeSettleBudgetExhausted residual) state)
runRuntimeSettleLoop iterationLimit settleStep =
  runRuntimeSettleLoopWith
    iterationLimit
    (rssDrain settleStep)
    (rssFlush settleStep)
    (rssQuiescent settleStep)
    (rssResidual settleStep)
{-# INLINE runRuntimeSettleLoop #-}

runRuntimeSettleLoopScoped ::
  Monad m =>
  (RuntimeScope -> Bool) ->
  Int ->
  RuntimeScopedSettleStep m state residual ->
  state ->
  m (Either (RuntimeSettleBudgetExhausted residual) state)
runRuntimeSettleLoopScoped keepScope iterationLimit settleStep =
  runRuntimeSettleLoopWith
    iterationLimit
    (rssScopedDrain settleStep keepScope)
    (rssScopedFlush settleStep keepScope)
    (rssScopedQuiescent settleStep keepScope)
    (rssScopedResidual settleStep keepScope)
{-# INLINE runRuntimeSettleLoopScoped #-}

runRuntimeSettleLoopWith ::
  Monad m =>
  Int ->
  (state -> m state) ->
  (state -> m state) ->
  (state -> Bool) ->
  (state -> residual) ->
  state ->
  m (Either (RuntimeSettleBudgetExhausted residual) state)
runRuntimeSettleLoopWith iterationLimit drain flush quiescent residual state0 =
  go 0 state0
  where
    go iteration state
      | quiescent state =
          pure (Right state)
      | iteration >= iterationLimit =
          pure
            ( Left
                RuntimeSettleBudgetExhausted
                  { rsbeIterationLimit = iterationLimit,
                    rsbeResidual = residual state
                  }
            )
      | otherwise = do
          drained <- drain state
          flushed <- flush drained
          go (iteration + 1) flushed
{-# INLINE runRuntimeSettleLoopWith #-}
