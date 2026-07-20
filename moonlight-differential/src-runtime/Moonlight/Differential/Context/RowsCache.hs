{-# LANGUAGE DerivingStrategies #-}

-- | LRU byte-budgeted context-rows cache; pinned entries survive drops and
-- block eviction (the cache may sit over budget while pins hold —
-- 'crcOverBudgetBytes' observes it), and 'withPinnedContext' is pure state
-- threading, not a bracket: a short-circuit in the underlying monad abandons
-- the pinned state with the pin held.
module Moonlight.Differential.Context.RowsCache
  ( ContextRowsKey,
    contextRowsKey,
    crkBaseRevision,
    crkOverlayEpoch,
    crkPlanFingerprint,
    crkContext,
    CachedRowsEntry,
    creRows,
    creBytes,
    creLastTouched,
    ContextRowsCache,
    crcEntries,
    crcPinned,
    crcBudgetBytes,
    crcCurrentBytes,
    crcOverBudgetBytes,
    crcClock,
    ContextRowsRuntime (..),
    ContextRowsSourceSelectionError (..),
    emptyContextRowsCache,
    cachedContextSet,
    resizeContextRowsCache,
    dropContextRowsWhere,
    dropContextRowsFor,
    getContextRows,
    insertContextRows,
    withPinnedContext,
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, gets, modify', put)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.List
  ( sortOn,
  )
import Data.Set (Set)
import Data.Set qualified as Set
import Numeric.Natural
  ( Natural,
  )

data ContextRowsKey ctx = ContextRowsKey
  { contextRowsKeyBaseRevisionRaw :: !Int,
    contextRowsKeyOverlayEpochRaw :: !Int,
    contextRowsKeyPlanFingerprintRaw :: !Int,
    contextRowsKeyContextRaw :: !ctx
  }
  deriving stock (Eq, Ord, Show)

data CachedRowsEntry rows = CachedRowsEntry
  { cachedRowsEntryRowsRaw :: !rows,
    cachedRowsEntryBytesRaw :: !Natural,
    cachedRowsEntryLastTouchedRaw :: !Int
  }
  deriving stock (Eq, Show)

data ContextRowsCache ctx rows = ContextRowsCache
  { contextRowsCacheEntriesRaw :: !(Map (ContextRowsKey ctx) (CachedRowsEntry rows)),
    contextRowsCachePinCountsRaw :: !(Map (ContextRowsKey ctx) Int),
    contextRowsCacheBudgetBytesRaw :: !Natural,
    contextRowsCacheCurrentBytesRaw :: !Natural,
    contextRowsCacheClockRaw :: !Int
  }
  deriving stock (Eq, Show)

data ContextRowsRuntime m ctx rows = ContextRowsRuntime
  { crrKeyFor :: ctx -> ContextRowsKey ctx,
    crrChooseRestrictionSource :: Set ctx -> ctx -> m (Maybe ctx),
    crrMaterializeRootRows :: ctx -> m rows,
    crrDeriveByRestriction :: ctx -> ctx -> rows -> m rows,
    crrRowsBytes :: rows -> Natural
  }

data ContextRowsSourceSelectionError ctx
  = ContextRowsRestrictionSourceIsTarget !ctx
  | ContextRowsRestrictionSourceOutsideCachedDomain !ctx !ctx
  deriving stock (Eq, Show)

contextRowsKey :: Int -> Int -> Int -> ctx -> ContextRowsKey ctx
contextRowsKey baseRevision overlayEpoch planFingerprint contextValue =
  ContextRowsKey
    { contextRowsKeyBaseRevisionRaw = baseRevision,
      contextRowsKeyOverlayEpochRaw = overlayEpoch,
      contextRowsKeyPlanFingerprintRaw = planFingerprint,
      contextRowsKeyContextRaw = contextValue
    }
{-# INLINE contextRowsKey #-}

crkBaseRevision :: ContextRowsKey ctx -> Int
crkBaseRevision =
  contextRowsKeyBaseRevisionRaw
{-# INLINE crkBaseRevision #-}

crkOverlayEpoch :: ContextRowsKey ctx -> Int
crkOverlayEpoch =
  contextRowsKeyOverlayEpochRaw
{-# INLINE crkOverlayEpoch #-}

crkPlanFingerprint :: ContextRowsKey ctx -> Int
crkPlanFingerprint =
  contextRowsKeyPlanFingerprintRaw
{-# INLINE crkPlanFingerprint #-}

crkContext :: ContextRowsKey ctx -> ctx
crkContext =
  contextRowsKeyContextRaw
{-# INLINE crkContext #-}

creRows :: CachedRowsEntry rows -> rows
creRows =
  cachedRowsEntryRowsRaw
{-# INLINE creRows #-}

creBytes :: CachedRowsEntry rows -> Natural
creBytes =
  cachedRowsEntryBytesRaw
{-# INLINE creBytes #-}

creLastTouched :: CachedRowsEntry rows -> Int
creLastTouched =
  cachedRowsEntryLastTouchedRaw
{-# INLINE creLastTouched #-}

crcEntries :: ContextRowsCache ctx rows -> Map (ContextRowsKey ctx) (CachedRowsEntry rows)
crcEntries =
  contextRowsCacheEntriesRaw
{-# INLINE crcEntries #-}

crcPinned :: ContextRowsCache ctx rows -> Set (ContextRowsKey ctx)
crcPinned =
  Map.keysSet . Map.filter (> 0) . contextRowsCachePinCountsRaw
{-# INLINE crcPinned #-}

crcBudgetBytes :: ContextRowsCache ctx rows -> Natural
crcBudgetBytes =
  contextRowsCacheBudgetBytesRaw
{-# INLINE crcBudgetBytes #-}

crcCurrentBytes :: ContextRowsCache ctx rows -> Natural
crcCurrentBytes =
  contextRowsCacheCurrentBytesRaw
{-# INLINE crcCurrentBytes #-}

crcOverBudgetBytes :: ContextRowsCache ctx rows -> Natural
crcOverBudgetBytes cache =
  naturalDifference
    (contextRowsCacheCurrentBytesRaw cache)
    (contextRowsCacheBudgetBytesRaw cache)
{-# INLINE crcOverBudgetBytes #-}

crcClock :: ContextRowsCache ctx rows -> Int
crcClock =
  contextRowsCacheClockRaw
{-# INLINE crcClock #-}

emptyContextRowsCache :: Natural -> ContextRowsCache ctx rows
emptyContextRowsCache budgetBytes =
  ContextRowsCache
    { contextRowsCacheEntriesRaw = Map.empty,
      contextRowsCachePinCountsRaw = Map.empty,
      contextRowsCacheBudgetBytesRaw = budgetBytes,
      contextRowsCacheCurrentBytesRaw = 0,
      contextRowsCacheClockRaw = 0
    }

cachedContextSet :: Ord ctx => ContextRowsCache ctx rows -> Set ctx
cachedContextSet =
  Set.fromList . fmap (contextRowsKeyContextRaw . fst) . Map.toAscList . contextRowsCacheEntriesRaw

resizeContextRowsCache ::
  Ord ctx =>
  Natural ->
  ContextRowsCache ctx rows ->
  ContextRowsCache ctx rows
resizeContextRowsCache budgetBytes cache =
  evictAfterResize
    cache
      { contextRowsCacheBudgetBytesRaw = budgetBytes
      }
{-# INLINE resizeContextRowsCache #-}

dropContextRowsWhere ::
  Ord ctx =>
  (ContextRowsKey ctx -> Bool) ->
  ContextRowsCache ctx rows ->
  (Set (ContextRowsKey ctx), ContextRowsCache ctx rows)
dropContextRowsWhere shouldDrop cache =
  ( refusedKeys,
    cache
      { contextRowsCacheEntriesRaw = keptEntries,
        contextRowsCacheCurrentBytesRaw =
          naturalDifference (contextRowsCacheCurrentBytesRaw cache) droppedBytes
      }
  )
  where
    pinnedKeys =
      crcPinned cache

    (droppedEntries, keptEntries) =
      Map.partitionWithKey
        (\keyValue _entry -> shouldDrop keyValue && not (Set.member keyValue pinnedKeys))
        (contextRowsCacheEntriesRaw cache)

    refusedKeys =
      Set.filter
        (\keyValue -> shouldDrop keyValue && Map.member keyValue (contextRowsCacheEntriesRaw cache))
        pinnedKeys

    droppedBytes =
      Map.foldl'
        (\acc entry -> acc + cachedRowsEntryBytesRaw entry)
        0
        droppedEntries
{-# INLINE dropContextRowsWhere #-}

dropContextRowsFor ::
  Ord ctx =>
  Set ctx ->
  ContextRowsCache ctx rows ->
  (Set (ContextRowsKey ctx), ContextRowsCache ctx rows)
dropContextRowsFor contexts =
  dropContextRowsWhere
    (\keyValue -> Set.member (contextRowsKeyContextRaw keyValue) contexts)
{-# INLINE dropContextRowsFor #-}

getContextRows ::
  (Monad m, Ord ctx) =>
  ContextRowsRuntime m ctx rows ->
  ctx ->
  StateT
    (ContextRowsCache ctx rows)
    m
    (Either (ContextRowsSourceSelectionError ctx) rows)
getContextRows runtime targetContext = do
  let targetKey = crrKeyFor runtime targetContext
  cached <- lookupAndTouch targetKey
  case cached of
    Just rows -> pure (Right rows)
    Nothing -> do
      availableRows <- gets (availableContextRowsForRuntime runtime)
      maybeSourceContext <-
        lift
          ( crrChooseRestrictionSource
              runtime
              (Map.keysSet availableRows)
              targetContext
          )
      case validateRestrictionSourceSelection targetContext availableRows maybeSourceContext of
        Left obstruction ->
          pure (Left obstruction)
        Right selectedSource -> do
          rows <-
            case selectedSource of
              Nothing ->
                lift (crrMaterializeRootRows runtime targetContext)
              Just (sourceContext, (sourceKey, sourceRows)) -> do
                modify' (touchKnownContextRowsKey sourceKey)
                lift (crrDeriveByRestriction runtime sourceContext targetContext sourceRows)
          insertContextRows runtime targetContext rows
          pure (Right rows)

validateRestrictionSourceSelection ::
  Ord ctx =>
  ctx ->
  Map ctx value ->
  Maybe ctx ->
  Either (ContextRowsSourceSelectionError ctx) (Maybe (ctx, value))
validateRestrictionSourceSelection targetContext availableRows maybeSourceContext =
  case maybeSourceContext of
    Nothing ->
      Right Nothing
    Just sourceContext
      | sourceContext == targetContext ->
          Left (ContextRowsRestrictionSourceIsTarget targetContext)
      | otherwise ->
          maybe
            (Left (ContextRowsRestrictionSourceOutsideCachedDomain sourceContext targetContext))
            (\sourceValue -> Right (Just (sourceContext, sourceValue)))
            (Map.lookup sourceContext availableRows)

insertContextRows ::
  (Monad m, Ord ctx) =>
  ContextRowsRuntime m ctx rows ->
  ctx ->
  rows ->
  StateT (ContextRowsCache ctx rows) m ()
insertContextRows runtime contextValue rows = do
  cache <- get
  let keyValue = crrKeyFor runtime contextValue
      rowBytes = crrRowsBytes runtime rows
      nextClock = contextRowsCacheClockRaw cache + 1
      priorBytes = maybe 0 cachedRowsEntryBytesRaw (Map.lookup keyValue (contextRowsCacheEntriesRaw cache))
      entry =
        CachedRowsEntry
          { cachedRowsEntryRowsRaw = rows,
            cachedRowsEntryBytesRaw = rowBytes,
            cachedRowsEntryLastTouchedRaw = nextClock
          }
      cacheInserted =
        cache
          { contextRowsCacheEntriesRaw = Map.insert keyValue entry (contextRowsCacheEntriesRaw cache),
            contextRowsCacheCurrentBytesRaw =
              naturalDifference (contextRowsCacheCurrentBytesRaw cache) priorBytes + rowBytes,
            contextRowsCacheClockRaw = nextClock
          }
  put (evictUntilWithinBudget cacheInserted)

withPinnedContext ::
  (Monad m, Ord ctx) =>
  ContextRowsRuntime m ctx rows ->
  ctx ->
  StateT (ContextRowsCache ctx rows) m a ->
  StateT (ContextRowsCache ctx rows) m a
withPinnedContext runtime contextValue action = do
  let keyValue = crrKeyFor runtime contextValue
  modify' (pinContextRowsKey keyValue)
  result <- action
  modify' (evictUntilWithinBudget . unpinContextRowsKey keyValue)
  pure result

pinContextRowsKey ::
  Ord ctx =>
  ContextRowsKey ctx ->
  ContextRowsCache ctx rows ->
  ContextRowsCache ctx rows
pinContextRowsKey keyValue cache =
  cache
    { contextRowsCachePinCountsRaw =
        Map.insertWith (+) keyValue 1 (contextRowsCachePinCountsRaw cache)
    }
{-# INLINE pinContextRowsKey #-}

unpinContextRowsKey ::
  Ord ctx =>
  ContextRowsKey ctx ->
  ContextRowsCache ctx rows ->
  ContextRowsCache ctx rows
unpinContextRowsKey keyValue cache =
  cache
    { contextRowsCachePinCountsRaw =
        Map.update decrement keyValue (contextRowsCachePinCountsRaw cache)
    }
  where
    decrement :: Int -> Maybe Int
    decrement count
      | count <= 1 =
          Nothing
      | otherwise =
          Just (count - 1)
{-# INLINE unpinContextRowsKey #-}

lookupAndTouch ::
  (Monad m, Ord ctx) =>
  ContextRowsKey ctx ->
  StateT (ContextRowsCache ctx rows) m (Maybe rows)
lookupAndTouch keyValue = do
  cache <- get
  case Map.lookup keyValue (contextRowsCacheEntriesRaw cache) of
    Nothing -> pure Nothing
    Just entry -> do
      let nextClock = contextRowsCacheClockRaw cache + 1
          touchedEntry = entry {cachedRowsEntryLastTouchedRaw = nextClock}
      put
        cache
          { contextRowsCacheEntriesRaw = Map.insert keyValue touchedEntry (contextRowsCacheEntriesRaw cache),
            contextRowsCacheClockRaw = nextClock
          }
      pure (Just (cachedRowsEntryRowsRaw entry))

availableContextRowsForRuntime ::
  Ord ctx =>
  ContextRowsRuntime m ctx rows ->
  ContextRowsCache ctx rows ->
  Map ctx (ContextRowsKey ctx, rows)
availableContextRowsForRuntime runtime =
  Map.fromAscList
    . fmap
      ( \(keyValue, entry) ->
          ( contextRowsKeyContextRaw keyValue,
            (keyValue, cachedRowsEntryRowsRaw entry)
          )
      )
    . filter (currentKey . fst)
    . Map.toAscList
    . contextRowsCacheEntriesRaw
  where
    currentKey keyValue =
      crrKeyFor runtime (contextRowsKeyContextRaw keyValue) == keyValue

touchKnownContextRowsKey ::
  Ord ctx =>
  ContextRowsKey ctx ->
  ContextRowsCache ctx rows ->
  ContextRowsCache ctx rows
touchKnownContextRowsKey keyValue cache =
  let nextClock = contextRowsCacheClockRaw cache + 1
   in cache
        { contextRowsCacheEntriesRaw =
            Map.adjust
              (\entry -> entry {cachedRowsEntryLastTouchedRaw = nextClock})
              keyValue
              (contextRowsCacheEntriesRaw cache),
          contextRowsCacheClockRaw = nextClock
        }

evictUntilWithinBudget ::
  Ord ctx =>
  ContextRowsCache ctx rows ->
  ContextRowsCache ctx rows
evictUntilWithinBudget cache
  | contextRowsCacheCurrentBytesRaw cache <= contextRowsCacheBudgetBytesRaw cache = cache
  | otherwise =
      case leastRecentlyUsedEvictable cache of
        Nothing -> cache
        Just (keyValue, entry) ->
          evictUntilWithinBudget
            cache
              { contextRowsCacheEntriesRaw = Map.delete keyValue (contextRowsCacheEntriesRaw cache),
                contextRowsCacheCurrentBytesRaw =
                  naturalDifference
                    (contextRowsCacheCurrentBytesRaw cache)
                    (cachedRowsEntryBytesRaw entry)
              }

leastRecentlyUsedEvictable ::
  Ord ctx =>
  ContextRowsCache ctx rows ->
  Maybe (ContextRowsKey ctx, CachedRowsEntry rows)
leastRecentlyUsedEvictable cache =
  foldr choose Nothing (Map.toAscList (contextRowsCacheEntriesRaw cache))
  where
    choose candidate@(keyValue, entry) best
      | Map.member keyValue (contextRowsCachePinCountsRaw cache) = best
      | otherwise =
          case best of
            Nothing -> Just candidate
            Just (_, bestEntry)
              | cachedRowsEntryLastTouchedRaw entry < cachedRowsEntryLastTouchedRaw bestEntry -> Just candidate
              | otherwise -> best

evictAfterResize ::
  Ord ctx =>
  ContextRowsCache ctx rows ->
  ContextRowsCache ctx rows
evictAfterResize cache
  | contextRowsCacheCurrentBytesRaw cache <= contextRowsCacheBudgetBytesRaw cache =
      cache
  | otherwise =
      cache
        { contextRowsCacheEntriesRaw =
            Map.withoutKeys (contextRowsCacheEntriesRaw cache) victimKeys,
          contextRowsCacheCurrentBytesRaw =
            naturalDifference (contextRowsCacheCurrentBytesRaw cache) victimBytes
        }
  where
    evictableEntries =
      sortOn
        (\(keyValue, entry) -> (cachedRowsEntryLastTouchedRaw entry, keyValue))
        ( filter
            (\(keyValue, _) -> Map.notMember keyValue (contextRowsCachePinCountsRaw cache))
            (Map.toAscList (contextRowsCacheEntriesRaw cache))
        )

    (victimKeys, victimBytes) =
      foldl' selectVictim (Set.empty, 0) evictableEntries

    selectVictim selected@(keys, bytes) (keyValue, entry)
      | naturalDifference (contextRowsCacheCurrentBytesRaw cache) bytes
          <= contextRowsCacheBudgetBytesRaw cache =
          selected
      | otherwise =
          ( Set.insert keyValue keys,
            bytes + cachedRowsEntryBytesRaw entry
          )
{-# INLINE evictAfterResize #-}

naturalDifference :: Natural -> Natural -> Natural
naturalDifference minuend subtrahend
  | minuend <= subtrahend =
      0
  | otherwise =
      minuend - subtrahend
{-# INLINE naturalDifference #-}
