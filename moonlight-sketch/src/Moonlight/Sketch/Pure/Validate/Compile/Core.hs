module Moonlight.Sketch.Pure.Validate.Compile.Core
  ( NormalForm (..),
    SchemaIdentity (..),
    ValidatorCache (..),
    ComponentId (..),
    RefComponentGraph (..),
    RegistryIdentity (..),
    CompiledSchemaEnv (..),
    RootRuntimeKey (..),
    CompiledEnvCache (..),
    RootRuntimeCache (..),
    CachePolicy (..),
    CompiledSchemaCache (..),
    CacheInterpreter (..),
    EvictionPolicy (..),
    MetricsPolicy (..),
    CacheMetrics,
    defaultCachePolicy,
    mkCachePolicy,
    mkCachePolicyWithInterpreters,
    unboundedInterpreter,
    boundedLruInterpreter,
    mkCompiledSchemaCache,
    emptyCompiledSchemaCache,
    compiledSchemaCacheSizes,
    compiledSchemaCacheMetrics,
    tickCache,
    mkRegistryIdentity,
    mkNormalForm,
    mkSchemaIdentity,
    lookupValidatorCache,
    TypedCacheOps (..),
    TypedCacheFilter (..),
  )
where

import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Sketch.Pure.Hash (schemaHash)
import Moonlight.Sketch.Pure.Normalize (normalize)
import Moonlight.Sketch.Pure.Types
  ( RefId,
    SchemaHash,
    SchemaNode,
    SchemaRegistry (..),
  )
import Moonlight.Sketch.Pure.Validate.Cache
  ( CacheInterpreter (..),
    CacheMetrics,
    CacheStore (..),
    EvictionPolicy (..),
    MetricsPolicy (..),
    cacheStoreDeleteWhere,
    cacheStoreInsert,
    cacheStoreLookup,
    cacheStoreMetrics,
    cacheStoreSize,
    emptyCacheStore,
  )
import Moonlight.Sketch.Pure.Validate.Core
  ( Validator,
    ValidatorRuntime,
  )

type NormalForm :: Type
newtype NormalForm = NormalForm
  { unNormalForm :: SchemaNode
  }
  deriving stock (Eq, Ord, Show)

type SchemaIdentity :: Type
data SchemaIdentity = SchemaIdentity
  { siHash :: !SchemaHash,
    siNormalForm :: !NormalForm
  }
  deriving stock (Eq, Ord, Show)

type ValidatorCache :: Type
newtype ValidatorCache = ValidatorCache
  { unValidatorCache :: Map.Map SchemaIdentity Validator
  }

type ComponentId :: Type
newtype ComponentId = ComponentId
  { unComponentId :: Int
  }
  deriving stock (Eq, Ord, Show)

type RefComponentGraph :: Type
data RefComponentGraph = RefComponentGraph
  { rcgDefinitions :: Map.Map RefId SchemaIdentity,
    rcgComponentByRef :: Map.Map RefId ComponentId,
    rcgMembersByComponent :: Map.Map ComponentId (Set.Set RefId)
  }

type RegistryIdentity :: Type
newtype RegistryIdentity = RegistryIdentity
  { unRegistryIdentity :: Map.Map RefId SchemaIdentity
  }
  deriving stock (Eq, Ord, Show)

type CompiledSchemaEnv :: Type
data CompiledSchemaEnv = CompiledSchemaEnv
  { cseRegistryIdentity :: RegistryIdentity,
    cseRegistry :: SchemaRegistry,
    cseComponentGraph :: RefComponentGraph,
    cseComponentValidators :: Map.Map ComponentId (Map.Map RefId Validator)
  }

type RootRuntimeKey :: Type
data RootRuntimeKey = RootRuntimeKey
  { rrkRegistryIdentity :: RegistryIdentity,
    rrkSchemaIdentity :: SchemaIdentity
  }
  deriving stock (Eq, Ord, Show)

type CompiledEnvCache :: Type
newtype CompiledEnvCache = CompiledEnvCache
  (CacheStore RegistryIdentity CompiledSchemaEnv)

type RootRuntimeCache :: Type
newtype RootRuntimeCache = RootRuntimeCache
  (CacheStore RootRuntimeKey ValidatorRuntime)

type TypedCacheOps :: Type -> Type -> Type -> Constraint
class Ord key => TypedCacheOps cache key value | cache -> key value where
  tcLookup :: CacheInterpreter -> Int -> key -> cache -> (Maybe value, cache)
  tcInsert :: CacheInterpreter -> Int -> key -> value -> cache -> (cache, Set.Set key)
  tcSize :: cache -> Int
  tcMetrics :: cache -> CacheMetrics

instance TypedCacheOps CompiledEnvCache RegistryIdentity CompiledSchemaEnv where
  tcLookup ci tick key (CompiledEnvCache store) =
    let (found, touched) = cacheStoreLookup tick key (store {csInterpreter = ci})
     in (found, CompiledEnvCache touched)
  tcInsert ci tick key val (CompiledEnvCache store) =
    let (updated, evicted) = cacheStoreInsert tick key val (store {csInterpreter = ci})
     in (CompiledEnvCache updated, evicted)
  tcSize (CompiledEnvCache store) = cacheStoreSize store
  tcMetrics (CompiledEnvCache store) = cacheStoreMetrics store

instance TypedCacheOps RootRuntimeCache RootRuntimeKey ValidatorRuntime where
  tcLookup ci tick key (RootRuntimeCache store) =
    let (found, touched) = cacheStoreLookup tick key (store {csInterpreter = ci})
     in (found, RootRuntimeCache touched)
  tcInsert ci tick key val (RootRuntimeCache store) =
    let (updated, evicted) = cacheStoreInsert tick key val (store {csInterpreter = ci})
     in (RootRuntimeCache updated, evicted)
  tcSize (RootRuntimeCache store) = cacheStoreSize store
  tcMetrics (RootRuntimeCache store) = cacheStoreMetrics store

type TypedCacheFilter :: Type -> Type -> Type -> Constraint
class TypedCacheOps cache key value => TypedCacheFilter cache key value where
  tcDeleteWhere :: CacheInterpreter -> (key -> Bool) -> cache -> cache

instance TypedCacheFilter RootRuntimeCache RootRuntimeKey ValidatorRuntime where
  tcDeleteWhere ci predicate (RootRuntimeCache store) =
    RootRuntimeCache (cacheStoreDeleteWhere predicate (store {csInterpreter = ci}))

type CachePolicy :: Type
data CachePolicy = CachePolicy
  { cpCompiledEnvInterpreter :: CacheInterpreter,
    cpRootRuntimeInterpreter :: CacheInterpreter
  }

defaultCachePolicy :: CachePolicy
defaultCachePolicy = mkCachePolicy 64 1024

unboundedInterpreter :: MetricsPolicy -> CacheInterpreter
unboundedInterpreter metricsPolicy =
  CacheInterpreter
    { ciEvictionPolicy = KeepAll,
      ciMetricsPolicy = metricsPolicy
    }

boundedLruInterpreter :: Int -> MetricsPolicy -> CacheInterpreter
boundedLruInterpreter maxEntries metricsPolicy =
  CacheInterpreter
    { ciEvictionPolicy = LruBound (max 0 maxEntries),
      ciMetricsPolicy = metricsPolicy
    }

mkCachePolicy :: Int -> Int -> CachePolicy
mkCachePolicy maxCompiledEnvs maxRootRuntimes =
  mkCachePolicyWithInterpreters
    (boundedLruInterpreter maxCompiledEnvs MetricsDisabled)
    (boundedLruInterpreter maxRootRuntimes MetricsDisabled)

mkCachePolicyWithInterpreters :: CacheInterpreter -> CacheInterpreter -> CachePolicy
mkCachePolicyWithInterpreters compiledEnvInterpreter rootRuntimeInterpreter =
  CachePolicy
    { cpCompiledEnvInterpreter = compiledEnvInterpreter,
      cpRootRuntimeInterpreter = rootRuntimeInterpreter
    }

type CompiledSchemaCache :: Type
data CompiledSchemaCache = CompiledSchemaCache
  { cscPolicy :: CachePolicy,
    cscClock :: !Int,
    cscCompiledEnvCache :: CompiledEnvCache,
    cscRootRuntimeCache :: RootRuntimeCache
  }

emptyCompiledSchemaCache :: CompiledSchemaCache
emptyCompiledSchemaCache = mkCompiledSchemaCache defaultCachePolicy

mkCompiledSchemaCache :: CachePolicy -> CompiledSchemaCache
mkCompiledSchemaCache cachePolicy =
  CompiledSchemaCache
    { cscPolicy = cachePolicy,
      cscClock = 0,
      cscCompiledEnvCache =
        CompiledEnvCache
          (emptyCacheStore (cpCompiledEnvInterpreter cachePolicy)),
      cscRootRuntimeCache =
        RootRuntimeCache
          (emptyCacheStore (cpRootRuntimeInterpreter cachePolicy))
    }

compiledSchemaCacheSizes :: CompiledSchemaCache -> (Int, Int)
compiledSchemaCacheSizes compiledSchemaCache =
  ( tcSize (cscCompiledEnvCache compiledSchemaCache),
    tcSize (cscRootRuntimeCache compiledSchemaCache)
  )

compiledSchemaCacheMetrics :: CompiledSchemaCache -> (CacheMetrics, CacheMetrics)
compiledSchemaCacheMetrics compiledSchemaCache =
  ( tcMetrics (cscCompiledEnvCache compiledSchemaCache),
    tcMetrics (cscRootRuntimeCache compiledSchemaCache)
  )

mkRegistryIdentity :: SchemaRegistry -> RegistryIdentity
mkRegistryIdentity registry =
  RegistryIdentity (Map.map mkSchemaIdentity (srSchemas registry))

mkNormalForm :: SchemaNode -> NormalForm
mkNormalForm = NormalForm . normalize

mkSchemaIdentity :: SchemaNode -> SchemaIdentity
mkSchemaIdentity schemaNode =
  let normalForm = mkNormalForm schemaNode
      normalizedSchema = unNormalForm normalForm
   in
    SchemaIdentity
      { siHash = schemaHash normalizedSchema,
        siNormalForm = normalForm
      }

lookupValidatorCache :: ValidatorCache -> SchemaIdentity -> Maybe Validator
lookupValidatorCache validatorCache schemaIdentity =
  Map.lookup schemaIdentity (unValidatorCache validatorCache)

tickCache :: CompiledSchemaCache -> (Int, CompiledSchemaCache)
tickCache compiledSchemaCache =
  let nextTick = cscClock compiledSchemaCache + 1
   in (nextTick, compiledSchemaCache {cscClock = nextTick})
