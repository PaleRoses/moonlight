module Moonlight.Flow.Carrier.View.Query
  ( visibleCarrierNow,
    visibleContextNow,
    visibleGlobalNow,
    visibleGlobalAcrossStores,
    carrierVisibleContextsNow,
    carrierCurrentRowsNow,
    carrierLiveBatchSummaryNow,
    carrierBoundaryLatestTraceNow,
    carrierCurrentDeltaLatestTraceNow,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Differential.Trace.Indexed
  ( itEntries,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDeltaP (..),
    RelationalCarrierDelta,
  )
import Moonlight.Flow.Carrier.Store.Projection.Current
  ( carrierCurrentRowsPlain,
  )
import Moonlight.Flow.Carrier.Fact.Ledger
  ( carrierLiveTraceEntriesAt,
  )
import Moonlight.Flow.Carrier.Store.Core.State
import Moonlight.Flow.Carrier.Core.Summary
  ( CarrierBatchSummary,
    CarrierBatchSummaryOps (..),
    CarrierStoreSummaryEntry,
    carrierBatchSummary,
  )
import Moonlight.Flow.Carrier.View.Section
  ( RelationalGlobalSection (..),
    RelationalSection (..),
    emptyVisibleGlobalSection,
    unionVisibleGlobalSections,
  )
import Moonlight.Delta.Signed
  ( Multiplicity
  )
import Moonlight.Differential.Row.Patch
  ( positivePlainRowPatchRows
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )

visibleCarrierNow ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierStore ctx carrier prop boundary evidence ->
  Map RowTupleKey Multiplicity
visibleCarrierNow addr indexState =
  maybe
    Map.empty
    (positivePlainRowPatchRows . carrierCurrentRowsPlain . csCurrentRows)
    (Map.lookup addr (ccpSnapshots (cvCurrent (cstViews indexState))))
{-# INLINE visibleCarrierNow #-}

visibleContextNow ::
  (Ord ctx, Ord carrier, Ord prop) =>
  ctx ->
  CarrierStore ctx carrier prop boundary evidence ->
  RelationalSection ctx carrier prop
visibleContextNow contextValue indexState =
  RelationalSection
    { rsCarriers =
        Map.fromAscList
          [ (addr, carrierCurrentRowsPlain (csCurrentRows snapshot))
          | addr <-
              Set.toAscList
                (Map.findWithDefault Set.empty contextValue (ciCurrentByContext (ccpIndexes (cvCurrent (cstViews indexState))))),
            Just snapshot <- [Map.lookup addr (ccpSnapshots (cvCurrent (cstViews indexState)))]
          ]
    }
{-# INLINE visibleContextNow #-}

visibleGlobalNow ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierStore ctx carrier prop boundary evidence ->
  RelationalGlobalSection ctx carrier prop
visibleGlobalNow indexState =
  RelationalGlobalSection
    { rgsContexts =
        Map.fromAscList
          [ (contextValue, visibleContextNow contextValue indexState)
          | contextValue <- Set.toAscList (carrierVisibleContextsNow indexState)
          ]
    }
{-# INLINE visibleGlobalNow #-}

visibleGlobalAcrossStores ::
  (Ord ctx, Ord carrier, Ord prop) =>
  IntMap (CarrierStore ctx carrier prop boundary evidence) ->
  RelationalGlobalSection ctx carrier prop
visibleGlobalAcrossStores =
  IntMap.foldl'
    ( \globalSection indexState ->
        unionVisibleGlobalSections globalSection (visibleGlobalNow indexState)
    )
    emptyVisibleGlobalSection
{-# INLINE visibleGlobalAcrossStores #-}

carrierVisibleContextsNow ::
  CarrierStore ctx carrier prop boundary evidence ->
  Set ctx
carrierVisibleContextsNow =
  Map.keysSet . ciCurrentByContext . ccpIndexes . cvCurrent . cstViews
{-# INLINE carrierVisibleContextsNow #-}

carrierCurrentRowsNow ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierStore ctx carrier prop boundary evidence ->
  Maybe RowDelta
carrierCurrentRowsNow addr indexState =
  ccrRows . csCurrentRows <$> Map.lookup addr (ccpSnapshots (cvCurrent (cstViews indexState)))
{-# INLINE carrierCurrentRowsNow #-}

carrierLiveBatchSummaryNow ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierBatchSummaryOps
    ctx
    carrier
    prop
    boundary
    evidence
    (CarrierStoreSummaryEntry ctx carrier prop boundary evidence) ->
  CarrierAddr ctx carrier prop ->
  CarrierStore ctx carrier prop boundary evidence ->
  Maybe (CarrierBatchSummary ctx carrier prop boundary evidence)
carrierLiveBatchSummaryNow ops addr indexState =
  carrierBatchSummary ops addr . fmap carrierStoreSummaryEntryFromTraceEntry <$> liveBatchEntries
  where
    liveBatchEntries =
      NonEmpty.nonEmpty (carrierLiveTraceEntriesAt addr indexState)
{-# INLINE carrierLiveBatchSummaryNow #-}

carrierBoundaryLatestTraceNow ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierStore ctx carrier prop boundary evidence ->
  Maybe boundary
carrierBoundaryLatestTraceNow addr indexState =
  deBoundary . cteDelta <$> carrierCurrentLatestTraceEntry addr indexState
{-# INLINE carrierBoundaryLatestTraceNow #-}

carrierCurrentDeltaLatestTraceNow ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierStore ctx carrier prop boundary evidence ->
  Maybe (RelationalCarrierDelta ctx carrier prop boundary evidence)
carrierCurrentDeltaLatestTraceNow addr indexState = do
  snapshot <- Map.lookup addr (ccpSnapshots (cvCurrent (cstViews indexState)))
  traceEntry <- carrierCurrentTraceEntryFromSnapshot snapshot indexState
  pure
    (cteDelta traceEntry)
      { deRows = ccrRows (csCurrentRows snapshot)
      }
{-# INLINE carrierCurrentDeltaLatestTraceNow #-}

carrierCurrentLatestTraceEntry ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierStore ctx carrier prop boundary evidence ->
  Maybe (CarrierTraceEntry ctx carrier prop boundary evidence)
carrierCurrentLatestTraceEntry addr indexState = do
  snapshot <- Map.lookup addr (ccpSnapshots (cvCurrent (cstViews indexState)))
  carrierCurrentTraceEntryFromSnapshot snapshot indexState
{-# INLINE carrierCurrentLatestTraceEntry #-}

carrierCurrentTraceEntryFromSnapshot ::
  CarrierSnapshot ctx carrier prop boundary evidence ->
  CarrierStore ctx carrier prop boundary evidence ->
  Maybe (CarrierTraceEntry ctx carrier prop boundary evidence)
carrierCurrentTraceEntryFromSnapshot snapshot indexState =
  IntMap.lookup
    (traceIdKey (csLatestTrace snapshot))
    (itEntries (cstTrace indexState))
{-# INLINE carrierCurrentTraceEntryFromSnapshot #-}
