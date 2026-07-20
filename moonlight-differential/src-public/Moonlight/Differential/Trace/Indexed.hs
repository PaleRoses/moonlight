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
    validateIndexedTraceCursor,
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
import Data.Bifunctor
  ( first,
  )
import Data.Foldable
  ( traverse_,
  )
import Data.Kind
  ( Type,
  )
import Moonlight.Differential.Internal.Trace.Indexed
  ( IndexedTrace (..),
    TraceIdCursor (..),
  )
import Moonlight.Differential.Trace.Id
  ( TraceId,
    TraceIdError (..),
    initialTraceId,
    nextTraceId,
    traceIdKey,
    validateTraceId,
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
  | IndexedTraceEntryIdInvalid !TraceIdError
  | IndexedTraceIdsExhausted
  | IndexedTraceNextIdNotAfterEntry !TraceId !TraceId
  deriving stock (Eq, Ord, Show)

emptyIndexedTrace ::
  TraceId ->
  indexes ->
  IndexedTrace entry indexes
emptyIndexedTrace nextId indexes =
  IndexedTrace
    { indexedTraceNextIdRaw = TraceIdAvailable nextId,
      indexedTraceEntriesRaw = IntMap.empty,
      indexedTraceIndexesRaw = indexes
    }

emptyIndexedTraceWithOps ::
  TraceIndexOps entry indexes indexError ->
  IndexedTrace entry indexes
emptyIndexedTraceWithOps ops =
  emptyIndexedTrace initialTraceId (tioEmptyIndexes ops)

itNextId :: IndexedTrace entry indexes -> Either IndexedTraceError TraceId
itNextId traceValue =
  case indexedTraceNextIdRaw traceValue of
    TraceIdAvailable nextId ->
      Right nextId
    TraceIdsExhausted ->
      Left IndexedTraceIdsExhausted
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
  IntMap.fromDistinctAscList
    <$> traverse collectEntry (IntSet.toAscList keys)
  where
    collectEntry traceKey = do
      entry <- lookupIndexedTraceEntryAt traceKey traceValue
      Right (traceKey, entry)

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
  traceId <- ensureIndexedTraceEntryKey ops traceKey entry
  if IntMap.member traceKey (indexedTraceEntriesRaw traceValue)
    then Left (IndexedTraceEntryKeyCollision traceKey)
    else do
      nextCursor <- advanceTraceIdCursor traceId (indexedTraceNextIdRaw traceValue)
      Right
        traceValue
          { indexedTraceNextIdRaw = nextCursor,
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
      traceId <- ensureIndexedTraceEntryKey ops traceKey entry
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
        result *> (() <$ ensureIndexedTraceEntryKey ops traceKey entry)
    )
    (Right ())

validateIndexedTraceCursor ::
  TraceIndexOps entry indexes indexError ->
  IndexedTrace entry indexes ->
  Either IndexedTraceError ()
validateIndexedTraceCursor ops traceValue =
  case indexedTraceNextIdRaw traceValue of
    TraceIdsExhausted ->
      traverse_ validateEntryId entries
    TraceIdAvailable nextId -> do
      _ <- first IndexedTraceEntryIdInvalid (validateTraceId nextId)
      traverse_ (ensureNextIdAfterEntry nextId) entries
  where
    entries =
      IntMap.elems (indexedTraceEntriesRaw traceValue)

    validateEntryId entry =
      first IndexedTraceEntryIdInvalid (validateTraceId (tioEntryId ops entry))

    ensureNextIdAfterEntry nextId entry =
      do
        entryId <- validateEntryId entry
        if traceIdKey entryId < traceIdKey nextId
          then Right ()
          else Left (IndexedTraceNextIdNotAfterEntry nextId entryId)

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
  Either IndexedTraceError TraceId
ensureIndexedTraceEntryKey ops traceKey entry =
  let actualId =
        tioEntryId ops entry
   in if traceKey == traceIdKey actualId
        then first IndexedTraceEntryIdInvalid (validateTraceId actualId)
        else Left (IndexedTraceEntryKeyMismatch traceKey actualId)

advanceTraceIdCursor :: TraceId -> TraceIdCursor -> Either IndexedTraceError TraceIdCursor
advanceTraceIdCursor insertedId cursor =
  case cursor of
    TraceIdsExhausted ->
      Right TraceIdsExhausted
    TraceIdAvailable nextId
      | traceIdKey insertedId < traceIdKey nextId ->
          Right cursor
      | otherwise ->
          case nextTraceId insertedId of
            Right successor ->
              Right (TraceIdAvailable successor)
            Left TraceIdExhausted ->
              Right TraceIdsExhausted
            Left idError ->
              Left (IndexedTraceEntryIdInvalid idError)
