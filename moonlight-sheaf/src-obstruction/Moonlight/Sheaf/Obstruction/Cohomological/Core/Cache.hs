module Moonlight.Sheaf.Obstruction.Cohomological.Core.Cache
  ( ObstructionCacheKey (..),
    CohomologicalCache,
    emptyCohomologicalCache,
    lookupCachedObstruction,
    insertCachedObstruction,
    insertCachedObstructionForDependencies,
    invalidateCachedObstructions,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Sheaf.Obstruction.Cohomological.Types (RegionScale)

type ObstructionCacheKey :: Type -> Type
data ObstructionCacheKey purpose = ObstructionCacheKey
  { ockQueryFingerprint :: !Int,
    ockRegionFingerprint :: !Int,
    ockScale :: !RegionScale,
    ockPurpose :: !purpose,
    ockEnvironmentFingerprint :: !(Maybe Int)
  }
  deriving stock (Eq, Ord, Show, Read)

type CachedObstructionEntry :: Type -> Type
data CachedObstructionEntry summary = CachedObstructionEntry
  { cachedObstructionDependenciesInternal :: !IntSet,
    cachedObstructionSummaryInternal :: !summary
  }
  deriving stock (Eq, Show, Read)

type CohomologicalCache :: Type -> Type -> Type
data CohomologicalCache purpose summary = CohomologicalCache
  { cohomologicalCacheEntriesInternal :: Map (ObstructionCacheKey purpose) (CachedObstructionEntry summary),
    cohomologicalCacheDependencyIndexInternal :: IntMap (Set (ObstructionCacheKey purpose))
  }
  deriving stock (Eq, Show, Read)

emptyCohomologicalCache :: CohomologicalCache purpose summary
emptyCohomologicalCache =
  CohomologicalCache
    { cohomologicalCacheEntriesInternal = Map.empty,
      cohomologicalCacheDependencyIndexInternal = IntMap.empty
    }

lookupCachedObstruction ::
  Ord purpose =>
  ObstructionCacheKey purpose ->
  CohomologicalCache purpose summary ->
  Maybe summary
lookupCachedObstruction cacheKey =
  fmap cachedObstructionSummaryInternal . Map.lookup cacheKey . cohomologicalCacheEntriesInternal

insertCachedObstruction ::
  Ord purpose =>
  ObstructionCacheKey purpose ->
  summary ->
  CohomologicalCache purpose summary ->
  CohomologicalCache purpose summary
insertCachedObstruction = insertCachedObstructionForDependencies IntSet.empty

insertCachedObstructionForDependencies ::
  Ord purpose =>
  IntSet ->
  ObstructionCacheKey purpose ->
  summary ->
  CohomologicalCache purpose summary ->
  CohomologicalCache purpose summary
insertCachedObstructionForDependencies dependencies cacheKey cachedObstruction cache =
  let cacheWithoutPriorEntry =
        maybe
          cache
          (\priorEntry -> removeCacheEntry cacheKey (cachedObstructionDependenciesInternal priorEntry) cache)
          (Map.lookup cacheKey (cohomologicalCacheEntriesInternal cache))
      updatedEntries =
        Map.insert
          cacheKey
          (CachedObstructionEntry dependencies cachedObstruction)
          (cohomologicalCacheEntriesInternal cacheWithoutPriorEntry)
      updatedDependencyIndex =
        insertCacheDependencies
          dependencies
          cacheKey
          (cohomologicalCacheDependencyIndexInternal cacheWithoutPriorEntry)
   in
    CohomologicalCache
      { cohomologicalCacheEntriesInternal = updatedEntries,
        cohomologicalCacheDependencyIndexInternal = updatedDependencyIndex
      }

invalidateCachedObstructions ::
  Ord purpose =>
  IntSet ->
  CohomologicalCache purpose summary ->
  CohomologicalCache purpose summary
invalidateCachedObstructions impactedClassKeys cache =
  let impactedCacheKeys =
        cacheDependentsOfMany
          impactedClassKeys
          (cohomologicalCacheDependencyIndexInternal cache)
   in
    Set.foldl'
      ( \currentCache cacheKey ->
          maybe
            currentCache
            (\entry -> removeCacheEntry cacheKey (cachedObstructionDependenciesInternal entry) currentCache)
            (Map.lookup cacheKey (cohomologicalCacheEntriesInternal currentCache))
      )
      cache
      impactedCacheKeys

removeCacheEntry ::
  Ord purpose =>
  ObstructionCacheKey purpose ->
  IntSet ->
  CohomologicalCache purpose summary ->
  CohomologicalCache purpose summary
removeCacheEntry cacheKey dependencies cache =
  CohomologicalCache
    { cohomologicalCacheEntriesInternal = Map.delete cacheKey (cohomologicalCacheEntriesInternal cache),
      cohomologicalCacheDependencyIndexInternal =
        removeCacheDependencies
          dependencies
          cacheKey
          (cohomologicalCacheDependencyIndexInternal cache)
    }

cacheDependentsOfMany :: Ord purpose => IntSet -> IntMap (Set (ObstructionCacheKey purpose)) -> Set (ObstructionCacheKey purpose)
cacheDependentsOfMany keys dependencyIndex =
  foldMap (\key -> IntMap.findWithDefault Set.empty key dependencyIndex) (IntSet.toAscList keys)

insertCacheDependencies :: Ord purpose => IntSet -> ObstructionCacheKey purpose -> IntMap (Set (ObstructionCacheKey purpose)) -> IntMap (Set (ObstructionCacheKey purpose))
insertCacheDependencies keys cacheKey dependencyIndex =
  IntSet.foldl'
    (\current key -> IntMap.insertWith Set.union key (Set.singleton cacheKey) current)
    dependencyIndex
    keys

removeCacheDependencies :: Ord purpose => IntSet -> ObstructionCacheKey purpose -> IntMap (Set (ObstructionCacheKey purpose)) -> IntMap (Set (ObstructionCacheKey purpose))
removeCacheDependencies keys cacheKey dependencyIndex =
  IntSet.foldl'
    (\current key -> IntMap.update (nonEmptySet . Set.delete cacheKey) key current)
    dependencyIndex
    keys

nonEmptySet :: Set value -> Maybe (Set value)
nonEmptySet values
  | Set.null values = Nothing
  | otherwise = Just values
