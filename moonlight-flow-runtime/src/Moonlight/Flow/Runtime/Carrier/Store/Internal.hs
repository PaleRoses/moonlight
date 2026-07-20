module Moonlight.Flow.Runtime.Carrier.Store.Internal
  ( runtimeCarrierStoreOperator,
    currentCarrierMaybeAtRouting,
    carrierStoreAtRouting,
    replaceCarrierStore,
  )
where

import Moonlight.Delta.Operator
  ( Operator,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierStore,
    CarrierStoreError,
    CarrierStoreTouch,
    carrierStoreOperator,
  )
import Moonlight.Flow.Carrier.View.Query
  ( carrierCurrentDeltaLatestTraceNow,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    RuntimeEnv (..),
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( RuntimeRouting,
    Shard,
    routeIndexShard,
  )
import Moonlight.Flow.Runtime.Carrier.State
  ( lookupRuntimeIndexState,
    replaceRuntimeIndexState,
  )

runtimeCarrierStoreOperator ::
  (Ord ctx, Ord prop) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Operator
    (RelationalCarrierTime ctx)
    (CarrierStore ctx Carrier prop boundary evidence)
    (RelationalCarrierDelta ctx Carrier prop boundary evidence)
    (CarrierStoreTouch ctx Carrier prop)
    (CarrierStoreError ctx Carrier prop boundary evidence)
runtimeCarrierStoreOperator runtime =
  carrierStoreOperator (reContextLattice (rdrEnv runtime))
{-# INLINE runtimeCarrierStoreOperator #-}

currentCarrierMaybeAtRouting ::
  (Ord ctx, Ord prop) =>
  RuntimeRouting ctx prop ->
  CarrierAddr ctx Carrier prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (Maybe (RelationalCarrierDelta ctx Carrier prop boundary evidence))
currentCarrierMaybeAtRouting routing addr runtime = do
  (_shard, indexState) <-
    carrierStoreAtRouting routing addr runtime
  pure (carrierCurrentDeltaLatestTraceNow addr indexState)
{-# INLINE currentCarrierMaybeAtRouting #-}

carrierStoreAtRouting ::
  (Ord ctx, Ord prop) =>
  RuntimeRouting ctx prop ->
  CarrierAddr ctx Carrier prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (Shard, CarrierStore ctx Carrier prop boundary evidence)
carrierStoreAtRouting routing addr runtime = do
  shard <-
    case routeIndexShard addr routing of
      Nothing ->
        Left (RuntimeMissingIndexRoute addr)
      Just shardValue ->
        Right shardValue
  case lookupRuntimeIndexState shard (rdrState runtime) of
    Nothing ->
      Left (RuntimeMissingIndexShard shard)
    Just indexState ->
      Right (shard, indexState)
{-# INLINE carrierStoreAtRouting #-}

replaceCarrierStore ::
  Shard ->
  CarrierStore ctx Carrier prop boundary evidence ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
replaceCarrierStore shard indexState runtime =
  runtime
    { rdrState =
        replaceRuntimeIndexState shard indexState (rdrState runtime)
    }
{-# INLINE replaceCarrierStore #-}
