{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Store.Engine.Compact
  ( compactCarrierStoreBefore,
  )
where

import Data.Foldable qualified as Foldable
import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty
  ( NonEmpty,
  )
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Core
  ( QuotientEpoch,
  )
import Moonlight.Differential.Frontier
  ( frontierCutoffForScope,
  )
import Moonlight.Differential.Time
  ( RuntimeScope,
    rtContext,
  )
import Moonlight.Delta.Frontier
  ( upperFrontierPoints,
  )
import Moonlight.Differential.Trace.Compact
  ( PartitionedPrefixCompactionError (..),
    PartitionedPrefixCompactionOps (..),
    PartitionedPrefixCompactionResult (..),
    applyIndexedTraceCompactionPlan,
    planIndexedTraceCompactionBefore,
  )
import Moonlight.Differential.Trace.Indexed
  ( indexedTraceEntriesForKeysChecked,
    itIndexes,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Frontier
  ( RelDiffFrontier,
  )
import Moonlight.Flow.Carrier.Store.Engine.Commit
  ( putCarrierTrace,
    spliceCarrierAddressProjection,
  )
import Moonlight.Flow.Carrier.Store.Journal.Trace
  ( carrierTraceIndexOps,
  )
import Moonlight.Flow.Carrier.Store.Core.State
  ( CarrierStore (..),
    CarrierTrace,
    CarrierTraceEntry (..),
    ctiByAddr,
    carrierStoreSummaryEntryFromTraceEntry,
    traceIdKey,
  )
import Moonlight.Flow.Carrier.Store.Core.Error
  ( CarrierStoreError (..),
    CarrierSummaryError (..),
  )
import Moonlight.Flow.Carrier.Store.Engine.Replay
  ( replayCarrierTraceEntries,
  )
import Moonlight.Flow.Carrier.Store.Engine.Read
  ( CarrierHeldReads (..),
    carrierTraceEntryAfterReadFrontier,
  )
import Moonlight.Flow.Carrier.Core.Summary
  ( CarrierBatchSummaryOps (..),
    CarrierStoreSummaryEntry (..),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    RelationalRuntimeEpoch,
    relationalTimeQuotientEpoch,
    relationalTimeScope,
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchNull,
    composePlainRowPatch,
    emptyPlainRowPatch,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.FiniteLattice
  ( ContextLattice
  )
import Moonlight.FiniteLattice
  ( SupportBasis,
    emptySupport,
    supportUnion
  )


compactCarrierStoreBefore ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierBatchSummaryOps
    ctx
    carrier
    prop
    boundary
    evidence
    (CarrierStoreSummaryEntry ctx carrier prop boundary evidence) ->
  ContextLattice ctx ->
  CarrierHeldReads ctx carrier prop ->
  RelDiffFrontier ctx RelationalPhase ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (CarrierStore ctx carrier prop boundary evidence)
compactCarrierStoreBefore ops latticeValue heldReads frontier indexState = do
  prefixPlan <-
    mapPrefixError $
      planIndexedTraceCompactionBefore
        (carrierPrefixCompactionOps ops latticeValue)
        frontier
        (cstTrace indexState)

  validateCarrierCompactionHeldReads heldReads prefixPlan

  if IntMap.null (ppcrCompacted prefixPlan)
    then Right indexState
    else do
      trace1 <-
        first CarrierStoreTraceMutationFailed $
          applyIndexedTraceCompactionPlan
            carrierTraceIndexOps
            prefixPlan
            (cstTrace indexState)
      let touchedAddrs =
            compactedCarrierAddresses (ppcrCompacted prefixPlan)
          stateWithTrace =
            putCarrierTrace trace1 indexState
      Foldable.foldlM
        (rebuildCarrierProjectionAt latticeValue)
        stateWithTrace
        (Set.toAscList touchedAddrs)
{-# INLINE compactCarrierStoreBefore #-}

validateCarrierCompactionHeldReads ::
  Ord (CarrierAddr ctx carrier prop) =>
  CarrierHeldReads ctx carrier prop ->
  PartitionedPrefixCompactionResult (CarrierTraceEntry ctx carrier prop boundary evidence) ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    ()
validateCarrierCompactionHeldReads heldReads prefixPlan =
  validateEntryMap (ppcrCompacted prefixPlan)
    *> validateEntryMap (ppcrSummaries prefixPlan)
  where
    validateEntryMap =
      IntMap.foldl'
        ( \result entry ->
            result *> validateHeldEntry heldReads entry
        )
        (Right ())
{-# INLINE validateCarrierCompactionHeldReads #-}

validateHeldEntry ::
  Ord (CarrierAddr ctx carrier prop) =>
  CarrierHeldReads ctx carrier prop ->
  CarrierTraceEntry ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    ()
validateHeldEntry heldReads entry =
  case Map.lookup addr (chrReadsByAddr heldReads) of
    Nothing ->
      Right ()
    Just frontiers ->
      case Set.lookupMin (Set.filter (`carrierTraceEntryAfterReadFrontier` entry) frontiers) of
        Nothing ->
          Right ()
        Just frontier ->
          Left
            ( CarrierStoreCompactionWouldInvalidateHeldRead
                addr
                frontier
                (cteId entry)
            )
  where
    addr =
      deAddr (cteDelta entry)
{-# INLINE validateHeldEntry #-}

carrierPrefixCompactionOps ::
  Ord ctx =>
  CarrierBatchSummaryOps
    ctx
    carrier
    prop
    boundary
    evidence
    (CarrierStoreSummaryEntry ctx carrier prop boundary evidence) ->
  ContextLattice ctx ->
  PartitionedPrefixCompactionOps
    ctx
    RelationalRuntimeEpoch
    RelationalPhase
    (CarrierTraceEntry ctx carrier prop boundary evidence)
    (CarrierAddr ctx carrier prop)
    (QuotientEpoch, RuntimeScope)
    (CarrierSummaryError ctx)
carrierPrefixCompactionOps ops latticeValue =
  PartitionedPrefixCompactionOps
    { pcoBatchKey =
        traceIdKey . cteId,
      pcoBatchTime =
        deTime . cteDelta,
      pcoPartition =
        deAddr . cteDelta,
      pcoPartitionBlockedByPending =
        \addr pendingTime -> caContext addr == rtContext pendingTime,
      pcoGroup =
        \entry ->
          let entryTime =
                deTime (cteDelta entry)
           in (relationalTimeQuotientEpoch entryTime, relationalTimeScope entryTime),
      pcoSummarizeRun =
        summarizeCarrierRun ops latticeValue
    }
{-# INLINE carrierPrefixCompactionOps #-}

summarizeCarrierRun ::
  Ord ctx =>
  CarrierBatchSummaryOps
    ctx
    carrier
    prop
    boundary
    evidence
    (CarrierStoreSummaryEntry ctx carrier prop boundary evidence) ->
  ContextLattice ctx ->
  RelDiffFrontier ctx RelationalPhase ->
  CarrierAddr ctx carrier prop ->
  (QuotientEpoch, RuntimeScope) ->
  NonEmpty (CarrierTraceEntry ctx carrier prop boundary evidence) ->
  Either
    (CarrierSummaryError ctx)
    (Maybe (CarrierTraceEntry ctx carrier prop boundary evidence))
summarizeCarrierRun ops latticeValue frontier addr (_quotientEpoch, scopeValue) entries =
  let rows =
        consolidatedRunRows entries
   in if plainRowPatchNull rows
        then Right Nothing
        else do
          cutoffTime <-
            summaryCutoffTime addr scopeValue frontier

          summarySupport <-
            compactedSupport latticeValue entries

          let summaryEntries =
                fmap carrierStoreSummaryEntryFromTraceEntry entries
              summaryTraceId =
                cteId (NonEmpty.head entries)
              summaryDelta =
                RelationalCarrierDelta
                  { deAddr = addr,
                    deTime = cutoffTime,
                    deSupport = summarySupport,
                    deBoundary = cbsoSummaryBoundary ops addr summaryEntries,
                    deEvidence = cbsoSummaryEvidence ops addr summaryEntries,
                    deOrigin = cbsoSummaryOrigin ops addr summaryEntries,
                    deScope =
                      Foldable.foldl'
                        (<>)
                        mempty
                        (fmap (deScope . cteDelta) entries),
                    deRows = rows,
                    dePayload = ()
                  }

          Right
            ( Just
                CarrierTraceEntry
                  { cteId = summaryTraceId,
                    cteDelta = summaryDelta
                  }
            )
{-# INLINE summarizeCarrierRun #-}

summaryCutoffTime ::
  Ord ctx =>
  CarrierAddr ctx carrier prop ->
  RuntimeScope ->
  RelDiffFrontier ctx RelationalPhase ->
  Either (CarrierSummaryError ctx) (RelationalCarrierTime ctx)
summaryCutoffTime addr scopeValue frontier =
  case upperFrontierPoints (frontierCutoffForScope (caContext addr) scopeValue frontier) of
    [] ->
      Left (CarrierSummaryMissingVisibleCutoff (caContext addr))
    [cutoffTime] ->
      Right cutoffTime
    _multipleCutoffs ->
      Left (CarrierSummaryNonSingletonVisibleCutoff (caContext addr))
{-# INLINE summaryCutoffTime #-}

consolidatedRunRows ::
  NonEmpty (CarrierTraceEntry ctx carrier prop boundary evidence) ->
  RowDelta
consolidatedRunRows =
  Foldable.foldl'
    ( \acc entry ->
        composePlainRowPatch (deRows (cteDelta entry)) acc
    )
    emptyPlainRowPatch
{-# INLINE consolidatedRunRows #-}

rebuildCarrierProjectionAt ::
  (Ord ctx, Ord carrier, Ord prop) =>
  ContextLattice ctx ->
  CarrierStore ctx carrier prop boundary evidence ->
  CarrierAddr ctx carrier prop ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (CarrierStore ctx carrier prop boundary evidence)
rebuildCarrierProjectionAt latticeValue stateValue addr = do
  addressEntries <-
    traceEntriesForAddress addr (cstTrace stateValue)
  localProjection <-
    replayAddressTraceEntries
      latticeValue
      addressEntries
  spliceCarrierAddressProjection addr localProjection stateValue
{-# INLINE rebuildCarrierProjectionAt #-}

traceEntriesForAddress ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierTrace ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (IntMap (CarrierTraceEntry ctx carrier prop boundary evidence))
traceEntriesForAddress addr traceValue =
  first CarrierStoreTraceMutationFailed $
    indexedTraceEntriesForKeysChecked
      (Map.findWithDefault IntSet.empty addr (ctiByAddr (itIndexes traceValue)))
      traceValue
{-# INLINE traceEntriesForAddress #-}

replayAddressTraceEntries ::
  (Ord ctx, Ord carrier, Ord prop) =>
  ContextLattice ctx ->
  IntMap (CarrierTraceEntry ctx carrier prop boundary evidence) ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (CarrierStore ctx carrier prop boundary evidence)
replayAddressTraceEntries =
  replayCarrierTraceEntries
{-# INLINE replayAddressTraceEntries #-}

compactedCarrierAddresses ::
  (Ord ctx, Ord carrier, Ord prop) =>
  IntMap (CarrierTraceEntry ctx carrier prop boundary evidence) ->
  Set (CarrierAddr ctx carrier prop)
compactedCarrierAddresses =
  IntMap.foldl'
    ( \addrs entry ->
        Set.insert (deAddr (cteDelta entry)) addrs
    )
    Set.empty
{-# INLINE compactedCarrierAddresses #-}

compactedSupport ::
  Ord ctx =>
  ContextLattice ctx ->
  NonEmpty (CarrierTraceEntry ctx carrier prop boundary evidence) ->
  Either (CarrierSummaryError ctx) (SupportBasis ctx)
compactedSupport latticeValue =
  Foldable.foldlM
    ( \acc entry ->
        case supportUnion latticeValue acc (deSupport (cteDelta entry)) of
          Right supportValue ->
            Right supportValue
          Left lookupError ->
            Left (CarrierSummaryLatticeLookupFailed lookupError)
    )
    emptySupport
{-# INLINE compactedSupport #-}

mapPrefixError ::
  Either
    ( PartitionedPrefixCompactionError
        ctx
        RelationalRuntimeEpoch
        RelationalPhase
        (CarrierSummaryError ctx)
    )
    value ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    value
mapPrefixError eitherValue =
  case eitherValue of
    Right value ->
      Right value
    Left (PartitionedPrefixCompactionSummaryFailed (CarrierSummaryMissingVisibleCutoff ctx)) ->
      Left (CarrierStoreCompactionMissingVisibleCutoff ctx)
    Left other ->
      Left (CarrierStorePrefixCompactionFailed other)
{-# INLINE mapPrefixError #-}
