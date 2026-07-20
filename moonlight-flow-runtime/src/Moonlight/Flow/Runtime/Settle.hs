module Moonlight.Flow.Runtime.Settle
  ( settleRuntime,
    settleRuntimeFixedPoint,
    settleRuntimeFixedPointBounded,
  )
where

import Data.Bifunctor
  ( first,
  )
import Moonlight.Flow.Runtime.Engine.Step.Settle qualified as EngineSettle
import Moonlight.Flow.Runtime.Types
  ( Runtime (..),
    RuntimeApplyError (..),
  )

settleRuntime ::
  Runtime ctx prop ->
  Either (RuntimeApplyError ctx prop) (Runtime ctx prop)
settleRuntime =
  settleRuntimeFixedPoint
{-# INLINE settleRuntime #-}

settleRuntimeFixedPoint ::
  Runtime ctx prop ->
  Either (RuntimeApplyError ctx prop) (Runtime ctx prop)
settleRuntimeFixedPoint (Runtime kernel) =
  Runtime
    <$> first
      RuntimeApplyRejected
      (EngineSettle.settleRuntimeFixedPoint kernel)
{-# INLINE settleRuntimeFixedPoint #-}

settleRuntimeFixedPointBounded ::
  Int ->
  Runtime ctx prop ->
  Either (RuntimeApplyError ctx prop) (Runtime ctx prop)
settleRuntimeFixedPointBounded iterationLimit (Runtime kernel) =
  Runtime
    <$> first
      RuntimeApplyRejected
      (EngineSettle.settleRuntimeFixedPointBounded iterationLimit kernel)
{-# INLINE settleRuntimeFixedPointBounded #-}
