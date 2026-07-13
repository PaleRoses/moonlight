{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Trace.Indexed
  ( IndexedTrace,
    itNextId,
    itEntries,
    itIndexes,
    TraceIndexOps (..),
    IndexedTraceError (..),
    emptyIndexedTrace,
    emptyIndexedTraceWithOps,
    lookupIndexedTraceEntry,
    lookupIndexedTraceEntryAt,
    indexedTraceEntriesForKeys,
    indexedTraceEntriesForKeysChecked,
    insertIndexedTraceEntry,
    insertIndexedTraceEntryAt,
    deleteIndexedTraceEntryAt,
    validateIndexedTraceEntryMap,
    validateIndexedTraceIndexes,
    applyIndexedTraceRewrite,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Kind
  ( Type,
  )
import Moonlight.Differential.Internal.Trace.Indexed
  ( IndexedTrace (..),
  )
import Moonlight.Differential.Trace.Id
  ( TraceId,
    initialTraceId,
    maxTraceId,
    nextTraceId,
    traceIdKey,
  )

type TraceIndexOps :: Type -> Type -> Type -> Type
data TraceIndexOps entry indexes indexError = TraceIndexOps
  { tioEntryId :: !(entry -> TraceId),
    tioEmptyIndexes :: !indexes,
    tioInsertIndexes :: !(TraceId -> entry -> indexes -> indexes),
    tioDeleteIndexes :: !(TraceId -> entry -> indexes -> indexes),
    tioValidateIndexes :: !(IntMap entry -> indexes -> [indexError])
  }

type IndexedTraceError :: Type
data IndexedTraceError
  = IndexedTraceEntryKeyMismatch !Int !TraceId
  | IndexedTraceEntryKeyCollision !Int
  | IndexedTraceEntryMissing !Int
  deriving stock (Eq, Ord, Show)

emptyIndexedTrace ::
  TraceId ->
  indexes ->
  IndexedTrace entry indexes
emptyIndexedTrace nextId indexes =
  IndexedTrace
    { indexedTraceNextIdRaw = nextId,
      indexedTraceEntriesRaw = IntMap.empty,
      indexedTraceIndexesRaw = indexes
    }

emptyIndexedTraceWithOps ::
  TraceIndexOps entry indexes indexError ->
  IndexedTrace entry indexes
emptyIndexedTraceWithOps ops =
  emptyIndexedTrace initialTraceId (tioEmptyIndexes ops)

itNextId :: IndexedTrace entry indexes -> TraceId
itNextId =
  indexedTraceNextIdRaw
{-# INLINE itNextId #-}

itEntries :: IndexedTrace entry indexes -> IntMap entry
itEntries =
  indexedTraceEntriesRaw
{-# INLINE itEntries #-}

itIndexes :: IndexedTrace entry indexes -> indexes
itIndexes =
  indexedTraceIndexesRaw
{-# INLINE itIndexes #-}

lookupIndexedTraceEntry ::
  TraceId ->
  IndexedTrace entry indexes ->
  Maybe entry
lookupIndexedTraceEntry traceId =
  IntMap.lookup (traceIdKey traceId) . indexedTraceEntriesRaw

lookupIndexedTraceEntryAt ::
  Int ->
  IndexedTrace entry indexes ->
  Either IndexedTraceError entry
lookupIndexedTraceEntryAt traceKey traceValue =
  case IntMap.lookup traceKey (indexedTraceEntriesRaw traceValue) of
    Nothing ->
      Left (IndexedTraceEntryMissing traceKey)
    Just entry ->
      Right entry

indexedTraceEntriesForKeys ::
  IntSet ->
  IndexedTrace entry indexes ->
  [entry]
indexedTraceEntriesForKeys keys traceValue =
  [ entry
  | traceKey <- IntSet.toAscList keys,
    Just entry <- [IntMap.lookup traceKey (indexedTraceEntriesRaw traceValue)]
  ]

indexedTraceEntriesForKeysChecked ::
  IntSet ->
  IndexedTrace entry indexes ->
  Either IndexedTraceError (IntMap entry)
indexedTraceEntriesForKeysChecked keys traceValue =
  IntSet.foldl'
    collectEntry
    (Right IntMap.empty)
    keys
  where
    collectEntry eitherEntries traceKey = do
      entries <- eitherEntries
      entry <- lookupIndexedTraceEntryAt traceKey traceValue
      Right (IntMap.insert traceKey entry entries)

insertIndexedTraceEntry ::
  TraceIndexOps entry indexes indexError ->
  entry ->
  IndexedTrace entry indexes ->
  Either IndexedTraceError (IndexedTrace entry indexes)
insertIndexedTraceEntry ops entry =
  insertIndexedTraceEntryAt ops (traceIdKey (tioEntryId ops entry)) entry

insertIndexedTraceEntryAt ::
  TraceIndexOps entry indexes indexError ->
  Int ->
  entry ->
  IndexedTrace entry indexes ->
  Either IndexedTraceError (IndexedTrace entry indexes)
insertIndexedTraceEntryAt ops traceKey entry traceValue = do
  ensureIndexedTraceEntryKey ops traceKey entry
  if IntMap.member traceKey (indexedTraceEntriesRaw traceValue)
    then Left (IndexedTraceEntryKeyCollision traceKey)
    else
      let traceId =
            tioEntryId ops entry
       in Right
            traceValue
              { indexedTraceNextIdRaw =
                  maxTraceId
                    (indexedTraceNextIdRaw traceValue)
                    (nextTraceId traceId),
                indexedTraceEntriesRaw =
                  IntMap.insert traceKey entry (indexedTraceEntriesRaw traceValue),
                indexedTraceIndexesRaw =
                  tioInsertIndexes ops traceId entry (indexedTraceIndexesRaw traceValue)
              }

deleteIndexedTraceEntryAt ::
  TraceIndexOps entry indexes indexError ->
  Int ->
  IndexedTrace entry indexes ->
  Either IndexedTraceError (IndexedTrace entry indexes)
deleteIndexedTraceEntryAt ops traceKey traceValue =
  case IntMap.lookup traceKey (indexedTraceEntriesRaw traceValue) of
    Nothing ->
      Left (IndexedTraceEntryMissing traceKey)
    Just entry -> do
      ensureIndexedTraceEntryKey ops traceKey entry
      let traceId =
            tioEntryId ops entry
      Right
        traceValue
          { indexedTraceEntriesRaw =
              IntMap.delete traceKey (indexedTraceEntriesRaw traceValue),
            indexedTraceIndexesRaw =
              tioDeleteIndexes ops traceId entry (indexedTraceIndexesRaw traceValue)
          }

validateIndexedTraceEntryMap ::
  TraceIndexOps entry indexes indexError ->
  IntMap entry ->
  Either IndexedTraceError ()
validateIndexedTraceEntryMap ops =
  IntMap.foldlWithKey'
    ( \result traceKey entry ->
        result *> ensureIndexedTraceEntryKey ops traceKey entry
    )
    (Right ())

validateIndexedTraceIndexes ::
  TraceIndexOps entry indexes indexError ->
  IndexedTrace entry indexes ->
  Either [indexError] ()
validateIndexedTraceIndexes ops traceValue =
  case tioValidateIndexes ops (indexedTraceEntriesRaw traceValue) (indexedTraceIndexesRaw traceValue) of
    [] ->
      Right ()
    errors ->
      Left errors

applyIndexedTraceRewrite ::
  TraceIndexOps entry indexes indexError ->
  IntMap entry ->
  IntMap entry ->
  IndexedTrace entry indexes ->
  Either IndexedTraceError (IndexedTrace entry indexes)
applyIndexedTraceRewrite ops compacted summaries trace0 = do
  validateIndexedTraceEntryMap ops compacted
  validateIndexedTraceEntryMap ops summaries
  traceWithoutCompacted <-
    IntMap.foldlWithKey'
      deleteCompactedEntry
      (Right trace0)
      compacted
  IntMap.foldlWithKey'
    insertSummaryEntry
    (Right traceWithoutCompacted)
    summaries
  where
    deleteCompactedEntry eitherTrace traceKey _entry = do
      traceValue <- eitherTrace
      deleteIndexedTraceEntryAt ops traceKey traceValue

    insertSummaryEntry eitherTrace traceKey entry = do
      traceValue <- eitherTrace
      insertIndexedTraceEntryAt ops traceKey entry traceValue

ensureIndexedTraceEntryKey ::
  TraceIndexOps entry indexes indexError ->
  Int ->
  entry ->
  Either IndexedTraceError ()
ensureIndexedTraceEntryKey ops traceKey entry =
  let actualId =
        tioEntryId ops entry
   in if traceKey == traceIdKey actualId
        then Right ()
        else Left (IndexedTraceEntryKeyMismatch traceKey actualId)
