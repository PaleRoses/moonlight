{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.View.Cache
  ( VisibleContextKey (..),
    CachedVisibleEntry (..),
    VisibleSectionCache (..),
    VisibleCacheAccountingError (..),
    emptyVisibleSectionCache,
    visibleCacheBytesExact,
    validateVisibleCacheAccounting,
    lookupVisibleContext,
    lookupPinnedVisibleContext,
    insertVisibleContext,
    insertPinnedVisibleContext,
    updatePinnedVisibleContext,
    pinnedVisibleContextMember,
    dropLazyVisibleContext,
    dropPinnedVisibleContext,
    dropVisibleContext,
    evictVisibleUntilWithinBudget,
  )
where

import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( LiveEpoch,
    QuotientEpoch,
  )

data VisibleContextKey ctx = VisibleContextKey
  { vckQuotientEpoch :: !QuotientEpoch,
    vckLiveEpoch :: !LiveEpoch,
    vckContext :: !ctx
  }
  deriving stock (Eq, Ord, Show, Read)

data CachedVisibleEntry section = CachedVisibleEntry
  { cveSection :: !section,
    cveBytes :: !Int,
    cveLastTouched :: !Int
  }
  deriving stock (Eq, Show)

data VisibleSectionCache ctx section = VisibleSectionCache
  { vscEntries :: !(Map (VisibleContextKey ctx) (CachedVisibleEntry section)),
    vscPinned :: !(Map ctx (CachedVisibleEntry section)),
    vscBudgetBytes :: !Int,
    vscCurrentBytes :: !Int,
    vscClock :: !Int
  }
  deriving stock (Eq, Show)

data VisibleCacheAccountingError
  = VisibleCacheNegativeCurrentBytes !Int
  | VisibleCacheBytesMismatch !Int !Int
  deriving stock (Eq, Ord, Show, Read)

emptyVisibleSectionCache :: Int -> VisibleSectionCache ctx section
emptyVisibleSectionCache budgetBytes =
  VisibleSectionCache
    { vscEntries = Map.empty,
      vscPinned = Map.empty,
      vscBudgetBytes = max 0 budgetBytes,
      vscCurrentBytes = 0,
      vscClock = 0
    }

visibleCacheBytesExact ::
  VisibleSectionCache ctx section ->
  Int
visibleCacheBytesExact cache =
  sumEntryBytes (Map.elems (vscEntries cache))
    + sumEntryBytes (Map.elems (vscPinned cache))

sumEntryBytes :: [CachedVisibleEntry section] -> Int
sumEntryBytes =
  List.foldl'
    (\acc entry -> acc + cveBytes entry)
    0
{-# INLINE sumEntryBytes #-}

validateVisibleCacheAccounting ::
  VisibleSectionCache ctx section ->
  Either VisibleCacheAccountingError ()
validateVisibleCacheAccounting cache
  | vscCurrentBytes cache < 0 =
      Left (VisibleCacheNegativeCurrentBytes (vscCurrentBytes cache))
  | expectedBytes /= vscCurrentBytes cache =
      Left (VisibleCacheBytesMismatch expectedBytes (vscCurrentBytes cache))
  | otherwise =
      Right ()
  where
    expectedBytes =
      visibleCacheBytesExact cache

lookupVisibleContext ::
  Ord ctx =>
  VisibleContextKey ctx ->
  VisibleSectionCache ctx section ->
  (VisibleSectionCache ctx section, Maybe section)
lookupVisibleContext keyValue cache =
  case Map.lookup keyValue (vscEntries cache) of
    Nothing ->
      (cache, Nothing)
    Just entry ->
      let nextClock =
            vscClock cache + 1
          touchedEntry =
            entry {cveLastTouched = nextClock}
       in ( cache
              { vscEntries = Map.insert keyValue touchedEntry (vscEntries cache),
                vscClock = nextClock
              },
            Just (cveSection entry)
          )

lookupPinnedVisibleContext ::
  Ord ctx =>
  ctx ->
  VisibleSectionCache ctx section ->
  (VisibleSectionCache ctx section, Maybe section)
lookupPinnedVisibleContext contextValue cache =
  case Map.lookup contextValue (vscPinned cache) of
    Nothing ->
      (cache, Nothing)
    Just entry ->
      let nextClock =
            vscClock cache + 1
          touchedEntry =
            entry {cveLastTouched = nextClock}
       in ( cache
              { vscPinned = Map.insert contextValue touchedEntry (vscPinned cache),
                vscClock = nextClock
              },
            Just (cveSection entry)
          )

insertVisibleContext ::
  Ord ctx =>
  (section -> Int) ->
  VisibleContextKey ctx ->
  section ->
  VisibleSectionCache ctx section ->
  VisibleSectionCache ctx section
insertVisibleContext measureBytes keyValue sectionValue cache =
  evictVisibleUntilWithinBudget
    cache
      { vscEntries = Map.insert keyValue entryValue (vscEntries cache),
        vscCurrentBytes = vscCurrentBytes cache - priorBytes + entryBytes,
        vscClock = nextClock
      }
  where
    nextClock =
      vscClock cache + 1

    entryBytes =
      max 0 (measureBytes sectionValue)

    priorBytes =
      maybe 0 cveBytes (Map.lookup keyValue (vscEntries cache))

    entryValue =
      CachedVisibleEntry
        { cveSection = sectionValue,
          cveBytes = entryBytes,
          cveLastTouched = nextClock
        }

insertPinnedVisibleContext ::
  Ord ctx =>
  (section -> Int) ->
  ctx ->
  section ->
  VisibleSectionCache ctx section ->
  VisibleSectionCache ctx section
insertPinnedVisibleContext measureBytes contextValue sectionValue cache =
  evictVisibleUntilWithinBudget
    cacheWithoutLazy
      { vscPinned = Map.insert contextValue entryValue (vscPinned cacheWithoutLazy),
        vscCurrentBytes = vscCurrentBytes cacheWithoutLazy - priorBytes + entryBytes,
        vscClock = nextClock
      }
  where
    cacheWithoutLazy =
      dropLazyVisibleContext contextValue cache

    nextClock =
      vscClock cacheWithoutLazy + 1

    entryBytes =
      max 0 (measureBytes sectionValue)

    priorBytes =
      maybe 0 cveBytes (Map.lookup contextValue (vscPinned cacheWithoutLazy))

    entryValue =
      CachedVisibleEntry
        { cveSection = sectionValue,
          cveBytes = entryBytes,
          cveLastTouched = nextClock
        }

updatePinnedVisibleContext ::
  Ord ctx =>
  (section -> Int) ->
  ctx ->
  (section -> section) ->
  VisibleSectionCache ctx section ->
  VisibleSectionCache ctx section
updatePinnedVisibleContext measureBytes contextValue update cache =
  case Map.lookup contextValue (vscPinned cache) of
    Nothing ->
      cache
    Just entry ->
      let nextClock =
            vscClock cache + 1
          nextSection =
            update (cveSection entry)
          nextBytes =
            max 0 (measureBytes nextSection)
          nextEntry =
            CachedVisibleEntry
              { cveSection = nextSection,
                cveBytes = nextBytes,
                cveLastTouched = nextClock
              }
       in evictVisibleUntilWithinBudget
            cache
              { vscPinned = Map.insert contextValue nextEntry (vscPinned cache),
                vscCurrentBytes = vscCurrentBytes cache - cveBytes entry + nextBytes,
                vscClock = nextClock
              }

pinnedVisibleContextMember ::
  Ord ctx =>
  ctx ->
  VisibleSectionCache ctx section ->
  Bool
pinnedVisibleContextMember contextValue =
  Map.member contextValue . vscPinned
{-# INLINE pinnedVisibleContextMember #-}

dropLazyVisibleContext ::
  Ord ctx =>
  ctx ->
  VisibleSectionCache ctx section ->
  VisibleSectionCache ctx section
dropLazyVisibleContext contextValue cache =
  let (droppedEntries, keptEntries) =
        Map.partitionWithKey
          (\keyValue _entry -> vckContext keyValue == contextValue)
          (vscEntries cache)
      droppedBytes =
        sumEntryBytes (Map.elems droppedEntries)
   in cache
        { vscEntries = keptEntries,
          vscCurrentBytes = max 0 (vscCurrentBytes cache - droppedBytes)
        }
{-# INLINE dropLazyVisibleContext #-}

dropPinnedVisibleContext ::
  Ord ctx =>
  ctx ->
  VisibleSectionCache ctx section ->
  VisibleSectionCache ctx section
dropPinnedVisibleContext contextValue cache =
  case Map.lookup contextValue (vscPinned cache) of
    Nothing ->
      cache
    Just entry ->
      cache
        { vscPinned = Map.delete contextValue (vscPinned cache),
          vscCurrentBytes = max 0 (vscCurrentBytes cache - cveBytes entry)
        }
{-# INLINE dropPinnedVisibleContext #-}

dropVisibleContext ::
  Ord ctx =>
  ctx ->
  VisibleSectionCache ctx section ->
  VisibleSectionCache ctx section
dropVisibleContext contextValue =
  dropPinnedVisibleContext contextValue . dropLazyVisibleContext contextValue
{-# INLINE dropVisibleContext #-}

evictVisibleUntilWithinBudget ::
  Ord ctx =>
  VisibleSectionCache ctx section ->
  VisibleSectionCache ctx section
evictVisibleUntilWithinBudget cache
  | vscCurrentBytes cache <= vscBudgetBytes cache =
      cache
  | otherwise =
      case leastRecentlyUsedEvictable cache of
        Nothing ->
          cache
        Just (keyValue, entry) ->
          evictVisibleUntilWithinBudget
            cache
              { vscEntries = Map.delete keyValue (vscEntries cache),
                vscCurrentBytes = max 0 (vscCurrentBytes cache - cveBytes entry)
              }

leastRecentlyUsedEvictable ::
  VisibleSectionCache ctx section ->
  Maybe (VisibleContextKey ctx, CachedVisibleEntry section)
leastRecentlyUsedEvictable cache =
  List.foldl'
    chooseLeastRecentlyUsed
    Nothing
    (Map.toAscList (vscEntries cache))

chooseLeastRecentlyUsed ::
  Maybe (key, CachedVisibleEntry section) ->
  (key, CachedVisibleEntry section) ->
  Maybe (key, CachedVisibleEntry section)
chooseLeastRecentlyUsed best candidate@(_keyValue, entry) =
  case best of
    Nothing ->
      Just candidate
    Just (_, bestEntry)
      | cveLastTouched entry < cveLastTouched bestEntry ->
          Just candidate
      | otherwise ->
          best
{-# INLINE chooseLeastRecentlyUsed #-}
