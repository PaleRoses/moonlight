{-# LANGUAGE DerivingStrategies #-}

module Test.Moonlight.Flow.Runtime.Diagnostics.Validate.BatchRecompute
  ( CarrierBatchRecomputeError (..),
    RuntimeReferenceReplayError (..),
    batchCarrierStoreRecompute,
    batchVisibleGlobalRecompute,
    validateCarrierStoreBatchRecompute,
    validateRuntimeQuotientPatchReplay,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Control.Monad
  ( unless,
  )
import Moonlight.Core
  ( BoundaryOps,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierStore,
    CarrierStoreError,
    validateCarrierStore,
  )
import Moonlight.Flow.Carrier.View.Query
  ( visibleGlobalNow,
  )
import Moonlight.Flow.Carrier.View.Section
  ( RelationalGlobalSection,
    RelationalSection (..),
  )
import Moonlight.Flow.Model.Delta
  ( QuotientPatch
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchFromMultiplicityMap,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Runtime.Core.Replay.Policy
  ( RuntimeReplayDomain (..),
    RuntimeReplaySelection,
    runtimeReplaySelectionDomains,
  )
import Moonlight.Flow.Runtime.Engine.Patch.Apply
  ( applyQuotientPatch,
  )
import Moonlight.Flow.Runtime.Engine.Patch.Replay
  ( runtimeQuotientPatchReplaySelection,
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Carrier.Store.Read
  ( visibleCarrier,
    visibleContextUncached,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
  )
import Moonlight.FiniteLattice
  ( ContextLattice
  )


data CarrierBatchRecomputeError ctx carrier prop boundary evidence
  = CarrierBatchStoreValidationFailed
      !(CarrierStoreError ctx carrier prop boundary evidence)
  | CarrierBatchVisibleGlobalMismatch
      !(RelationalGlobalSection ctx carrier prop)
      !(RelationalGlobalSection ctx carrier prop)
  deriving stock (Eq, Show)

data RuntimeReferenceReplayError ctx prop boundary evidence joinErr
  = RuntimeReferenceSelectionFailed
      !(RelationalRuntimeError ctx prop boundary evidence)
  | RuntimeReferencePatchFailed
      !(RelationalRuntimeError ctx prop boundary evidence)
  | RuntimeReferenceSelectedDomainFailed
      !(RuntimeReplayDomain ctx prop)
      !(RelationalRuntimeError ctx prop boundary evidence)
  | RuntimeReferenceSelectedDomainMismatch
      !(RuntimeReplayDomain ctx prop)
      !(RelationalSection ctx Carrier prop)
      !(RelationalSection ctx Carrier prop)
  deriving stock (Eq, Show)

batchCarrierStoreRecompute ::
  (Ord ctx, Ord carrier, Ord prop, Eq boundary, Eq evidence, BoundaryOps boundary) =>
  ContextLattice ctx ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either
    (CarrierBatchRecomputeError ctx carrier prop boundary evidence)
    (CarrierStore ctx carrier prop boundary evidence)
batchCarrierStoreRecompute latticeValue indexState =
  first CarrierBatchStoreValidationFailed (validateCarrierStore latticeValue indexState)
    *> Right indexState
{-# INLINE batchCarrierStoreRecompute #-}

batchVisibleGlobalRecompute ::
  (Ord ctx, Ord carrier, Ord prop, Eq boundary, Eq evidence, BoundaryOps boundary) =>
  ContextLattice ctx ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either
    (CarrierBatchRecomputeError ctx carrier prop boundary evidence)
    (RelationalGlobalSection ctx carrier prop)
batchVisibleGlobalRecompute latticeValue indexState =
  visibleGlobalNow <$> batchCarrierStoreRecompute latticeValue indexState
{-# INLINE batchVisibleGlobalRecompute #-}

validateCarrierStoreBatchRecompute ::
  (Ord ctx, Ord carrier, Ord prop, Eq boundary, Eq evidence, BoundaryOps boundary) =>
  ContextLattice ctx ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either
    (CarrierBatchRecomputeError ctx carrier prop boundary evidence)
    ()
validateCarrierStoreBatchRecompute latticeValue indexState = do
  recomputedGlobal <-
    batchVisibleGlobalRecompute latticeValue indexState
  let currentGlobal =
        visibleGlobalNow indexState
  unless (currentGlobal == recomputedGlobal) $
    Left (CarrierBatchVisibleGlobalMismatch currentGlobal recomputedGlobal)
{-# INLINE validateCarrierStoreBatchRecompute #-}

validateRuntimeQuotientPatchReplay ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop,
    Semigroup evidence
  ) =>
  QuotientPatch ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RuntimeReferenceReplayError ctx prop boundary evidence joinErr)
    (RuntimeReplaySelection ctx prop)
validateRuntimeQuotientPatchReplay patch runtime0 actualRuntime = do
  selection <-
    first RuntimeReferenceSelectionFailed $
      runtimeQuotientPatchReplaySelection patch runtime0
  expectedRuntime <-
    first RuntimeReferencePatchFailed $
      applyQuotientPatch patch runtime0
  validateRuntimeReplaySelection selection expectedRuntime actualRuntime
  pure selection
{-# INLINE validateRuntimeQuotientPatchReplay #-}

validateRuntimeReplaySelection ::
  (Ord ctx, Ord prop) =>
  RuntimeReplaySelection ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RuntimeReferenceReplayError ctx prop boundary evidence joinErr)
    ()
validateRuntimeReplaySelection selection expectedRuntime actualRuntime =
  Foldable.traverse_
    (validateRuntimeReplayDomain expectedRuntime actualRuntime)
    (Set.toAscList (runtimeReplaySelectionDomains selection))
{-# INLINE validateRuntimeReplaySelection #-}

validateRuntimeReplayDomain ::
  (Ord ctx, Ord prop) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RuntimeReplayDomain ctx prop ->
  Either
    (RuntimeReferenceReplayError ctx prop boundary evidence joinErr)
    ()
validateRuntimeReplayDomain expectedRuntime actualRuntime domain = do
  expectedSection <-
    selectedRuntimeReplaySection domain expectedRuntime
  actualSection <-
    selectedRuntimeReplaySection domain actualRuntime
  unless (actualSection == expectedSection) $
    Left (RuntimeReferenceSelectedDomainMismatch domain expectedSection actualSection)
{-# INLINE validateRuntimeReplayDomain #-}

selectedRuntimeReplaySection ::
  (Ord ctx, Ord prop) =>
  RuntimeReplayDomain ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RuntimeReferenceReplayError ctx prop boundary evidence joinErr)
    (RelationalSection ctx Carrier prop)
selectedRuntimeReplaySection domain runtime =
  case domain of
    RuntimeReplayCarrierDomain addr ->
      fmap
        (RelationalSection . Map.singleton addr . plainRowPatchFromMultiplicityMap)
        (first (RuntimeReferenceSelectedDomainFailed domain) (visibleCarrier addr runtime))
    RuntimeReplayContextDomain contextValue ->
      Right (visibleContextUncached contextValue runtime)
{-# INLINE selectedRuntimeReplaySection #-}
