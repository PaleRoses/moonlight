module Moonlight.Flow.Runtime.Engine.Dispatch.Shard
  ( stepProject,
    stepRestrict,
    stepIndex,
    flushProjectOps,
    flushRestrictOps,
    flushIndexOps,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Moonlight.Delta.Operator
  ( OpResult (..),
    Operator (..),
  )
import Moonlight.Delta.Time
  ( Timed (..),
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
  )
import Moonlight.Flow.Carrier.Engine.Project
  ( CarrierProjectState,
  )
import Moonlight.Flow.Carrier.Morphism.Core.Program
  ( CarrierMorphismRuntime,
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierStore,
  )
import Moonlight.Flow.Model.Event
  ( LocalRelationalEvent,
  )
import Moonlight.Flow.Runtime.Carrier.State
  ( RuntimeShardRegistry (..),
    runtimeShardRegistry,
    setRuntimeShardRegistry,
  )
import Moonlight.Flow.Runtime.Carrier.Store.Internal
  ( runtimeCarrierStoreOperator,
  )
import Moonlight.Flow.Runtime.Engine.Dispatch.Carrier
  ( applyTouchesAndScheduleFanout,
    enqueueCarrierFanout,
    enqueueCarrierFanoutChecked,
    enqueueCarrierStoreOnly,
    enqueueCarrierStoreOnlyChecked,
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
    RelationalRuntimeOpFailure (..),
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnv (..),
    RuntimeEnvelope (..),
  )
import Moonlight.Flow.Runtime.Kernel.Operators
  ( RuntimeCarrierOperators (..),
  )
import Moonlight.Flow.Runtime.Time
  ( RuntimeEventTime,
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( Shard (..),
    shardKey,
  )

data RuntimeShardSlot ctx prop boundary evidence stateValue = RuntimeShardSlot
  { rssValues ::
      RuntimeShardRegistry ctx prop boundary evidence ->
      IntMap.IntMap stateValue,
    rssReplaceValues ::
      IntMap.IntMap stateValue ->
      RuntimeShardRegistry ctx prop boundary evidence ->
      RuntimeShardRegistry ctx prop boundary evidence
  }

projectShardSlot :: RuntimeShardSlot ctx prop boundary evidence (CarrierProjectState ctx prop boundary evidence)
projectShardSlot =
  RuntimeShardSlot
    { rssValues = rsrProjectOps,
      rssReplaceValues = \values registry -> registry {rsrProjectOps = values}
    }

restrictShardSlot ::
  RuntimeShardSlot
    ctx
    prop
    boundary
    evidence
    (CarrierMorphismRuntime ctx Carrier prop boundary evidence)
restrictShardSlot =
  RuntimeShardSlot
    { rssValues = rsrRestrictOps,
      rssReplaceValues = \values registry -> registry {rsrRestrictOps = values}
    }

indexShardSlot :: RuntimeShardSlot ctx prop boundary evidence (CarrierStore ctx Carrier prop boundary evidence)
indexShardSlot =
  RuntimeShardSlot
    { rssValues = rsrIndexOps,
      rssReplaceValues = \values registry -> registry {rsrIndexOps = values}
    }

lookupShardState ::
  RuntimeShardSlot ctx prop boundary evidence stateValue ->
  Shard ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Maybe stateValue
lookupShardState slot shard =
  IntMap.lookup (shardKey shard)
    . rssValues slot
    . runtimeShardRegistry
    . rdrState
{-# INLINE lookupShardState #-}

insertShardState ::
  RuntimeShardSlot ctx prop boundary evidence stateValue ->
  Shard ->
  stateValue ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
insertShardState slot shard stateValue runtime =
  runtime
    { rdrState =
        setRuntimeShardRegistry
          ( replaceShardValue
              slot
              shard
              stateValue
              (runtimeShardRegistry (rdrState runtime))
          )
          (rdrState runtime)
    }
{-# INLINE insertShardState #-}

replaceShardValue ::
  RuntimeShardSlot ctx prop boundary evidence stateValue ->
  Shard ->
  stateValue ->
  RuntimeShardRegistry ctx prop boundary evidence ->
  RuntimeShardRegistry ctx prop boundary evidence
replaceShardValue slot shard stateValue registry =
  rssReplaceValues
    slot
    (IntMap.insert (shardKey shard) stateValue (rssValues slot registry))
    registry
{-# INLINE replaceShardValue #-}

stepProject ::
  (Ord ctx, Ord prop) =>
  Shard ->
  Timed (RuntimeEventTime ctx) LocalRelationalEvent ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
stepProject =
  stepShard
    projectShardSlot
    RuntimeMissingProjectShard
    (rcoProjectOperator . reCarrierOperators . rdrEnv)
    (\shard _ err _ -> Left (RuntimeOpFailure (RelationalRuntimeProjectOperatorError shard err)))
    (\timedInput -> enqueueCarrierFanoutChecked (timedAt timedInput))
{-# INLINE stepProject #-}

stepRestrict ::
  (Ord ctx, Ord prop) =>
  Shard ->
  Timed (RuntimeEventTime ctx) (RelationalCarrierDelta ctx Carrier prop boundary evidence) ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
stepRestrict =
  stepShard
    restrictShardSlot
    RuntimeMissingRestrictShard
    (rcoRestrictOperator . reCarrierOperators . rdrEnv)
    (\_shard _timedInput _err runtime -> Right runtime)
    (\timedInput -> enqueueCarrierStoreOnlyChecked (timedAt timedInput))
{-# INLINE stepRestrict #-}

stepIndex ::
  (Ord ctx, Ord prop) =>
  Shard ->
  Timed (RuntimeEventTime ctx) (RelationalCarrierDelta ctx Carrier prop boundary evidence) ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
stepIndex =
  stepShard
    indexShardSlot
    RuntimeMissingIndexShard
    runtimeCarrierStoreOperator
    (\shard _ err _ -> Left (RuntimeOpFailure (RelationalRuntimeCarrierStoreOperatorError shard err)))
    (const applyTouchesAndScheduleFanout)
{-# INLINE stepIndex #-}

stepShard ::
  RuntimeShardSlot ctx prop boundary evidence stateValue ->
  (Shard -> RelationalRuntimeError ctx prop boundary evidence) ->
  ( RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
    Operator (RuntimeEventTime ctx) stateValue input output operatorErr
  ) ->
  ( Shard ->
    Timed (RuntimeEventTime ctx) input ->
    operatorErr ->
    RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
    Either
      (RelationalRuntimeError ctx prop boundary evidence)
      (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
  ) ->
  ( Timed (RuntimeEventTime ctx) input ->
    [Timed (RuntimeEventTime ctx) output] ->
    RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
    Either
      (RelationalRuntimeError ctx prop boundary evidence)
      (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
  ) ->
  Shard ->
  Timed (RuntimeEventTime ctx) input ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
stepShard registryField missingShard runtimeOperator onOperatorError enqueueOutputs shard timedInput runtime =
  case lookupShardState registryField shard runtime of
    Nothing ->
      Left (missingShard shard)
    Just stateValue ->
      case opStep (runtimeOperator runtime) stateValue timedInput of
        Left err ->
          onOperatorError shard timedInput err runtime
        Right result ->
          enqueueOutputs
            timedInput
            (orEmit result)
            (insertShardState registryField shard (orState result) runtime)
{-# INLINE stepShard #-}

flushProjectOps ::
  (Ord ctx, Ord prop) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
flushProjectOps =
  flushShardOps
    projectShardSlot
    ( flushShard
        projectShardSlot
        (rcoProjectOperator . reCarrierOperators . rdrEnv)
        (\shard err -> RuntimeOpFailure (RelationalRuntimeProjectOperatorError shard err))
        enqueueCarrierFanout
    )
{-# INLINE flushProjectOps #-}

flushRestrictOps ::
  (Ord ctx, Ord prop) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
flushRestrictOps =
  flushShardOps
    restrictShardSlot
    ( flushShard
        restrictShardSlot
        (rcoRestrictOperator . reCarrierOperators . rdrEnv)
        (\shard err -> RuntimeOpFailure (RelationalRuntimeRestrictOperatorError shard err))
        enqueueCarrierStoreOnly
    )
{-# INLINE flushRestrictOps #-}

flushIndexOps ::
  (Ord ctx, Ord prop) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
flushIndexOps =
  flushShardOps
    indexShardSlot
    ( flushShard
        indexShardSlot
        runtimeCarrierStoreOperator
        (\shard err -> RuntimeOpFailure (RelationalRuntimeCarrierStoreOperatorError shard err))
        applyTouchesAndScheduleFanout
    )
{-# INLINE flushIndexOps #-}

flushShardOps ::
  RuntimeShardSlot ctx prop boundary evidence stateValue ->
  ( Shard ->
    stateValue ->
    RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
    Either
      (RelationalRuntimeError ctx prop boundary evidence)
      (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
  ) ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
flushShardOps registryField flushOne runtime =
  IntMap.foldlWithKey'
    ( \acc shardKey stateValue ->
        acc >>= flushOne (Shard shardKey) stateValue
    )
    (Right runtime)
    (rssValues registryField (runtimeShardRegistry (rdrState runtime)))
{-# INLINE flushShardOps #-}

flushShard ::
  RuntimeShardSlot ctx prop boundary evidence stateValue ->
  ( RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
    Operator (RuntimeEventTime ctx) stateValue input output operatorErr
  ) ->
  (Shard -> operatorErr -> RelationalRuntimeError ctx prop boundary evidence) ->
  ( [Timed (RuntimeEventTime ctx) output] ->
    RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
    Either
      (RelationalRuntimeError ctx prop boundary evidence)
      (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
  ) ->
  Shard ->
  stateValue ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
flushShard registryField runtimeOperator onOperatorError enqueueOutputs shard stateValue runtime =
  case opFlush (runtimeOperator runtime) stateValue of
    Left err ->
      Left (onOperatorError shard err)
    Right result ->
      enqueueOutputs
        (orEmit result)
        (insertShardState registryField shard (orState result) runtime)
{-# INLINE flushShard #-}
