{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Store.Engine.Replay
  ( CarrierReplayField (..),
    replayCarrierStore,
    replayCarrierTraceEntries,
    compareCarrierStoreReplay,
    validateCarrierStore,
  )
where

import Control.Monad
  ( unless,
  )
import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import Moonlight.Differential.Trace.Indexed
  ( IndexedTraceError (..),
    itEntries,
    itIndexes,
    itNextId,
    validateIndexedTraceEntryMap,
    validateIndexedTraceIndexes,
    validateIndexedTraceCursor,
  )
import Moonlight.Flow.Carrier.Store.Engine.Commit
  ( commitCarrierTraceEntry,
    emptyCarrierStore,
  )
import Moonlight.Flow.Carrier.Store.Core.Error
  ( CarrierReplayField (..),
    CarrierStoreError (..),
  )
import Moonlight.Flow.Carrier.Store.Core.State
  ( CarrierStore,
    CarrierTrace,
    CarrierTraceEntry (..),
    ccpIndexes,
    ccpSnapshots,
    cstTrace,
    cstViews,
    cvCurrent,
    cvFacts,
    traceIdKey,
  )
import Moonlight.Flow.Carrier.Store.Journal.Trace
  ( carrierTraceIndexOps,
  )
import Moonlight.FiniteLattice
  ( ContextLattice
  )

replayCarrierStore ::
  (Ord ctx, Ord carrier, Ord prop) =>
  ContextLattice ctx ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (CarrierStore ctx carrier prop boundary evidence)
replayCarrierStore latticeValue original =
  replayCarrierTraceEntries latticeValue (itEntries (cstTrace original))
{-# INLINE replayCarrierStore #-}

replayCarrierTraceEntries ::
  (Ord ctx, Ord carrier, Ord prop) =>
  ContextLattice ctx ->
  IntMap.IntMap (CarrierTraceEntry ctx carrier prop boundary evidence) ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (CarrierStore ctx carrier prop boundary evidence)
replayCarrierTraceEntries latticeValue =
  IntMap.foldlWithKey'
    (replayCarrierTraceEntry latticeValue)
    (Right emptyCarrierStore)
{-# INLINE replayCarrierTraceEntries #-}

replayCarrierTraceEntry ::
  (Ord ctx, Ord carrier, Ord prop) =>
  ContextLattice ctx ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (CarrierStore ctx carrier prop boundary evidence) ->
  Int ->
  CarrierTraceEntry ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (CarrierStore ctx carrier prop boundary evidence)
replayCarrierTraceEntry latticeValue eitherState entryKey entry = do
  replayed <- eitherState
  unless (entryKey == traceIdKey (cteId entry)) $
    Left (CarrierStoreReplayTraceKeyMismatch entryKey (cteId entry))
  first
    id
    (commitCarrierTraceEntry latticeValue (cteId entry) (cteDelta entry) replayed)
{-# INLINE replayCarrierTraceEntry #-}

compareCarrierStoreReplay ::
  ( Eq ctx,
    Eq carrier,
    Eq prop,
    Eq boundary,
    Eq evidence
  ) =>
  CarrierStore ctx carrier prop boundary evidence ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    ()
compareCarrierStoreReplay expected actual =
  compareCarrierTraceReplay expected actual
    *> compareCarrierReplayField CarrierReplayCurrent (ccpSnapshots . cvCurrent . cstViews) expected actual
    *> compareCarrierReplayField CarrierReplayIndexes (ccpIndexes . cvCurrent . cstViews) expected actual
    *> compareCarrierReplayField CarrierReplayFacts (cvFacts . cstViews) expected actual
{-# INLINE compareCarrierStoreReplay #-}

compareCarrierTraceReplay ::
  (Eq ctx, Eq carrier, Eq prop, Eq boundary, Eq evidence) =>
  CarrierStore ctx carrier prop boundary evidence ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    ()
compareCarrierTraceReplay expected actual =
  compareCarrierReplayField CarrierReplayTrace (itEntries . cstTrace) expected actual
    *> compareCarrierReplayField CarrierReplayTrace (itIndexes . cstTrace) expected actual
    *> compareCarrierTraceCursor (cstTrace expected) (cstTrace actual)
{-# INLINE compareCarrierTraceReplay #-}

compareCarrierTraceCursor ::
  CarrierTrace ctx carrier prop boundary evidence ->
  CarrierTrace ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    ()
compareCarrierTraceCursor expected actual =
  case (itNextId expected, itNextId actual) of
    (Left IndexedTraceIdsExhausted, _) ->
      Right ()
    (Right _, Left IndexedTraceIdsExhausted) ->
      Left (CarrierStoreReplayFieldMismatch CarrierReplayTrace)
    (Right expectedId, Right actualId)
      | expectedId >= actualId ->
          Right ()
      | otherwise ->
          Left (CarrierStoreReplayFieldMismatch CarrierReplayTrace)
    (Left expectedError, _) ->
      Left (CarrierStoreTraceMutationFailed expectedError)
    (_, Left actualError) ->
      Left (CarrierStoreTraceMutationFailed actualError)
{-# INLINE compareCarrierTraceCursor #-}

compareCarrierReplayField ::
  Eq value =>
  CarrierReplayField ->
  (CarrierStore ctx carrier prop boundary evidence -> value) ->
  CarrierStore ctx carrier prop boundary evidence ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    ()
compareCarrierReplayField field selector expected actual =
  if selector expected == selector actual
    then Right ()
    else Left (CarrierStoreReplayFieldMismatch field)

validateCarrierStore ::
  ( Ord ctx,
    Ord carrier,
    Ord prop,
    Eq boundary,
    Eq evidence
  ) =>
  ContextLattice ctx ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    ()
validateCarrierStore latticeValue indexState = do
  validateCarrierTraceIndexes (cstTrace indexState)
  replayed <- replayCarrierStore latticeValue indexState
  compareCarrierStoreReplay indexState replayed

validateCarrierTraceIndexes ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierTrace ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    ()
validateCarrierTraceIndexes traceValue =
  first CarrierStoreTraceMutationFailed (validateIndexedTraceEntryMap carrierTraceIndexOps (itEntries traceValue))
    *> first CarrierStoreTraceMutationFailed (validateIndexedTraceCursor carrierTraceIndexOps traceValue)
    *> case validateIndexedTraceIndexes carrierTraceIndexOps traceValue of
      Right () ->
        Right ()
      Left [] ->
        Right ()
      Left (firstError : remainingErrors) ->
        Left (CarrierStoreTraceIndexesInvalid (firstError :| remainingErrors))
