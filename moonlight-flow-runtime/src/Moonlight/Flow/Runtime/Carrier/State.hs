{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Runtime.Carrier.State
  ( RuntimeShardRegistry (..),
    RuntimeCarrierState (..),
    emptyRuntimeCarrierState,
    runtimeShardRegistry,
    setRuntimeShardRegistry,
    mapRuntimeShardRegistry,
    runtimeProjectOps,
    runtimeRestrictOps,
    runtimeIndexOps,
    lookupRuntimeProjectState,
    lookupRuntimeRestrictState,
    lookupRuntimeIndexState,
    replaceRuntimeProjectState,
    replaceRuntimeRestrictState,
    replaceRuntimeIndexState,
    runtimeVisibleCache,
    setRuntimeVisibleCache,
    mapRuntimeVisibleCache,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Kind
  ( Type,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
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
import Moonlight.Flow.Carrier.View.Cache
  ( VisibleSectionCache,
    emptyVisibleSectionCache,
  )
import Moonlight.Flow.Carrier.View.Section
  ( RelationalSection,
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Topology.Routing
  ( Shard,
    shardKey,
  )

type RuntimeShardRegistry :: Type -> Type -> Type -> Type -> Type
data RuntimeShardRegistry ctx prop boundary evidence = RuntimeShardRegistry
  { rsrProjectOps :: !(IntMap (CarrierProjectState ctx prop boundary evidence)),
    rsrRestrictOps :: !(IntMap (CarrierMorphismRuntime ctx Carrier prop boundary evidence)),
    rsrIndexOps :: !(IntMap (CarrierStore ctx Carrier prop boundary evidence))
  }

type RuntimeCarrierState :: Type -> Type -> Type -> Type -> Type
data RuntimeCarrierState ctx prop boundary evidence = RuntimeCarrierState
  { rcsShardRegistry :: !(RuntimeShardRegistry ctx prop boundary evidence),
    rcsVisibleCache :: !(VisibleSectionCache ctx (RelationalSection ctx Carrier prop))
  }

emptyRuntimeCarrierState ::
  Int ->
  IntMap (CarrierProjectState ctx prop boundary evidence) ->
  IntMap (CarrierMorphismRuntime ctx Carrier prop boundary evidence) ->
  IntMap (CarrierStore ctx Carrier prop boundary evidence) ->
  RuntimeCarrierState ctx prop boundary evidence
emptyRuntimeCarrierState visibleCacheBudget projectOps restrictOps indexOps =
  RuntimeCarrierState
    { rcsShardRegistry =
        RuntimeShardRegistry
          { rsrProjectOps = projectOps,
            rsrRestrictOps = restrictOps,
            rsrIndexOps = indexOps
          },
      rcsVisibleCache = emptyVisibleSectionCache visibleCacheBudget
    }
{-# INLINE emptyRuntimeCarrierState #-}

runtimeShardRegistry ::
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor ->
  RuntimeShardRegistry ctx prop boundary evidence
runtimeShardRegistry =
  rcsShardRegistry . Core.rsCarrier
{-# INLINE runtimeShardRegistry #-}

setRuntimeShardRegistry ::
  RuntimeShardRegistry ctx prop boundary evidence ->
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor ->
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor
setRuntimeShardRegistry registry =
  Core.mapRuntimeCarrierSection
    ( \carrierState ->
        carrierState {rcsShardRegistry = registry}
    )
{-# INLINE setRuntimeShardRegistry #-}

mapRuntimeShardRegistry ::
  (RuntimeShardRegistry ctx prop boundary evidence -> RuntimeShardRegistry ctx prop boundary evidence) ->
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor ->
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor
mapRuntimeShardRegistry update state =
  setRuntimeShardRegistry (update (runtimeShardRegistry state)) state
{-# INLINE mapRuntimeShardRegistry #-}

runtimeProjectOps ::
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor ->
  IntMap (CarrierProjectState ctx prop boundary evidence)
runtimeProjectOps =
  rsrProjectOps . runtimeShardRegistry
{-# INLINE runtimeProjectOps #-}

runtimeRestrictOps ::
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor ->
  IntMap (CarrierMorphismRuntime ctx Carrier prop boundary evidence)
runtimeRestrictOps =
  rsrRestrictOps . runtimeShardRegistry
{-# INLINE runtimeRestrictOps #-}

runtimeIndexOps ::
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor ->
  IntMap (CarrierStore ctx Carrier prop boundary evidence)
runtimeIndexOps =
  rsrIndexOps . runtimeShardRegistry
{-# INLINE runtimeIndexOps #-}

lookupRuntimeProjectState ::
  Shard ->
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor ->
  Maybe (CarrierProjectState ctx prop boundary evidence)
lookupRuntimeProjectState shard =
  IntMap.lookup (shardKey shard) . runtimeProjectOps
{-# INLINE lookupRuntimeProjectState #-}

lookupRuntimeRestrictState ::
  Shard ->
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor ->
  Maybe (CarrierMorphismRuntime ctx Carrier prop boundary evidence)
lookupRuntimeRestrictState shard =
  IntMap.lookup (shardKey shard) . runtimeRestrictOps
{-# INLINE lookupRuntimeRestrictState #-}

lookupRuntimeIndexState ::
  Shard ->
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor ->
  Maybe (CarrierStore ctx Carrier prop boundary evidence)
lookupRuntimeIndexState shard =
  IntMap.lookup (shardKey shard) . runtimeIndexOps
{-# INLINE lookupRuntimeIndexState #-}

replaceRuntimeProjectState ::
  Shard ->
  CarrierProjectState ctx prop boundary evidence ->
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor ->
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor
replaceRuntimeProjectState shard projectState =
  mapRuntimeShardRegistry
    ( \registry ->
        registry
          { rsrProjectOps =
              IntMap.insert (shardKey shard) projectState (rsrProjectOps registry)
          }
    )
{-# INLINE replaceRuntimeProjectState #-}

replaceRuntimeRestrictState ::
  Shard ->
  CarrierMorphismRuntime ctx Carrier prop boundary evidence ->
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor ->
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor
replaceRuntimeRestrictState shard restrictState =
  mapRuntimeShardRegistry
    ( \registry ->
        registry
          { rsrRestrictOps =
              IntMap.insert (shardKey shard) restrictState (rsrRestrictOps registry)
          }
    )
{-# INLINE replaceRuntimeRestrictState #-}

replaceRuntimeIndexState ::
  Shard ->
  CarrierStore ctx Carrier prop boundary evidence ->
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor ->
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor
replaceRuntimeIndexState shard indexState =
  mapRuntimeShardRegistry
    ( \registry ->
        registry
          { rsrIndexOps =
              IntMap.insert (shardKey shard) indexState (rsrIndexOps registry)
          }
    )
{-# INLINE replaceRuntimeIndexState #-}

runtimeVisibleCache ::
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor ->
  VisibleSectionCache ctx (RelationalSection ctx Carrier prop)
runtimeVisibleCache =
  rcsVisibleCache . Core.rsCarrier
{-# INLINE runtimeVisibleCache #-}

setRuntimeVisibleCache ::
  VisibleSectionCache ctx (RelationalSection ctx Carrier prop) ->
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor ->
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor
setRuntimeVisibleCache cache =
  Core.mapRuntimeCarrierSection
    ( \carrierState ->
        carrierState {rcsVisibleCache = cache}
    )
{-# INLINE setRuntimeVisibleCache #-}

mapRuntimeVisibleCache ::
  (VisibleSectionCache ctx (RelationalSection ctx Carrier prop) -> VisibleSectionCache ctx (RelationalSection ctx Carrier prop)) ->
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor ->
  Core.RuntimeState topology engine (RuntimeCarrierState ctx prop boundary evidence) factor
mapRuntimeVisibleCache update state =
  setRuntimeVisibleCache (update (runtimeVisibleCache state)) state
{-# INLINE mapRuntimeVisibleCache #-}
