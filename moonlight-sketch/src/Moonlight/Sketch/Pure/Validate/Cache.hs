module Moonlight.Sketch.Pure.Validate.Cache
  ( CacheEntry (..),
    EvictionPolicy (..),
    MetricsPolicy (..),
    CacheInterpreter (..),
    CacheMetrics (..),
    CacheStore (..),
    emptyCacheStore,
    cacheStoreSize,
    cacheStoreMetrics,
    cacheStoreLookup,
    cacheStoreInsert,
    cacheStoreDeleteWhere,
    emptyCacheMetrics,
  )
where

import Data.Kind (Type)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Optics (Lens', lens, over)

type CacheEntry :: Type -> Type
data CacheEntry a = CacheEntry
  { ceLastAccess :: !Int,
    ceValue :: a
  }

type EvictionPolicy :: Type
data EvictionPolicy
  = KeepAll
  | LruBound !Int
  deriving stock (Eq, Ord, Show)

type MetricsPolicy :: Type
data MetricsPolicy
  = MetricsDisabled
  | MetricsEnabled
  deriving stock (Eq, Ord, Show)

type CacheInterpreter :: Type
data CacheInterpreter = CacheInterpreter
  { ciEvictionPolicy :: EvictionPolicy,
    ciMetricsPolicy :: MetricsPolicy
  }
  deriving stock (Eq, Ord, Show)

type CacheMetrics :: Type
data CacheMetrics = CacheMetrics
  { cmHits :: !Int,
    cmMisses :: !Int,
    cmInserts :: !Int,
    cmEvictions :: !Int
  }
  deriving stock (Eq, Ord, Show)

type CacheStore :: Type -> Type -> Type
data CacheStore key value = CacheStore
  { csInterpreter :: CacheInterpreter,
    csEntries :: Map.Map key (CacheEntry value),
    csMetrics :: CacheMetrics
  }

cacheStoreEntriesLens :: Lens' (CacheStore key value) (Map.Map key (CacheEntry value))
cacheStoreEntriesLens =
  lens csEntries (\cacheStore entries -> cacheStore {csEntries = entries})

cacheStoreMetricsLens :: Lens' (CacheStore key value) CacheMetrics
cacheStoreMetricsLens =
  lens csMetrics (\cacheStore metrics -> cacheStore {csMetrics = metrics})

emptyCacheMetrics :: CacheMetrics
emptyCacheMetrics =
  CacheMetrics
    { cmHits = 0,
      cmMisses = 0,
      cmInserts = 0,
      cmEvictions = 0
    }

emptyCacheStore :: CacheInterpreter -> CacheStore key value
emptyCacheStore cacheInterpreter =
  CacheStore
    { csInterpreter = cacheInterpreter,
      csEntries = Map.empty,
      csMetrics = emptyCacheMetrics
    }

cacheStoreSize :: CacheStore key value -> Int
cacheStoreSize = Map.size . csEntries

cacheStoreMetrics :: CacheStore key value -> CacheMetrics
cacheStoreMetrics = csMetrics

cacheStoreLookup ::
  Ord key =>
  Int ->
  key ->
  CacheStore key value ->
  (Maybe value, CacheStore key value)
cacheStoreLookup tickValue cacheKey cacheStore =
  case Map.lookup cacheKey (csEntries cacheStore) of
    Just cacheEntry ->
      let touchedStore =
            over
              cacheStoreEntriesLens
              (Map.adjust (\entry -> entry {ceLastAccess = tickValue}) cacheKey)
              cacheStore
       in (Just (ceValue cacheEntry), recordMetric touchHit touchedStore)
    Nothing -> (Nothing, recordMetric touchMiss cacheStore)

cacheStoreInsert ::
  Ord key =>
  Int ->
  key ->
  value ->
  CacheStore key value ->
  (CacheStore key value, Set.Set key)
cacheStoreInsert tickValue cacheKey cacheValue cacheStore =
  let insertedEntries =
        Map.insert cacheKey (CacheEntry tickValue cacheValue) (csEntries cacheStore)
      (boundedEntries, evictedKeys) =
        applyEviction (ciEvictionPolicy (csInterpreter cacheStore)) insertedEntries
      insertedStore =
        recordMetric
          (\metrics -> metrics {cmInserts = cmInserts metrics + 1})
          (over cacheStoreEntriesLens (const boundedEntries) cacheStore)
      evictedStore =
        if Set.null evictedKeys
          then insertedStore
          else
            recordMetric
              (\metrics -> metrics {cmEvictions = cmEvictions metrics + Set.size evictedKeys})
              insertedStore
   in (evictedStore, evictedKeys)

cacheStoreDeleteWhere ::
  (key -> Bool) ->
  CacheStore key value ->
  CacheStore key value
cacheStoreDeleteWhere predicate cacheStore =
  let originalEntries = csEntries cacheStore
      filteredEntries = Map.filterWithKey (\cacheKey _ -> not (predicate cacheKey)) originalEntries
      removedCount = Map.size originalEntries - Map.size filteredEntries
      filteredStore = over cacheStoreEntriesLens (const filteredEntries) cacheStore
   in
    if removedCount <= 0
      then filteredStore
      else
        recordMetric
          (\metrics -> metrics {cmEvictions = cmEvictions metrics + removedCount})
          filteredStore

recordMetric ::
  (CacheMetrics -> CacheMetrics) ->
  CacheStore key value ->
  CacheStore key value
recordMetric updateMetrics cacheStore =
  case ciMetricsPolicy (csInterpreter cacheStore) of
    MetricsEnabled -> over cacheStoreMetricsLens updateMetrics cacheStore
    MetricsDisabled -> cacheStore

touchHit :: CacheMetrics -> CacheMetrics
touchHit metrics = metrics {cmHits = cmHits metrics + 1}

touchMiss :: CacheMetrics -> CacheMetrics
touchMiss metrics = metrics {cmMisses = cmMisses metrics + 1}

applyEviction ::
  Ord key =>
  EvictionPolicy ->
  Map.Map key (CacheEntry value) ->
  (Map.Map key (CacheEntry value), Set.Set key)
applyEviction evictionPolicy cacheEntries =
  case evictionPolicy of
    KeepAll -> (cacheEntries, Set.empty)
    LruBound maxEntries -> evictEntries maxEntries cacheEntries

evictEntries ::
  Ord key =>
  Int ->
  Map.Map key (CacheEntry value) ->
  (Map.Map key (CacheEntry value), Set.Set key)
evictEntries maxEntries cacheEntries
  | maxEntries <= 0 = (Map.empty, Set.fromList (Map.keys cacheEntries))
  | otherwise =
      let overflow = Map.size cacheEntries - maxEntries
       in
        if overflow <= 0
          then (cacheEntries, Set.empty)
          else
            let oldestKeys =
                  take overflow
                    (map fst (List.sortOn (ceLastAccess . snd) (Map.toList cacheEntries)))
                trimmedEntries =
                  List.foldl'
                    (flip Map.delete)
                    cacheEntries
                    oldestKeys
             in (trimmedEntries, Set.fromList oldestKeys)
