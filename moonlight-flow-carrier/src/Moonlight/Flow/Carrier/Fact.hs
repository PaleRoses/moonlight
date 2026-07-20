module Moonlight.Flow.Carrier.Fact
  ( CarrierFactRuntime (..),
    CarrierFactLedger,
    CarrierFactLedgerError,
    CarrierFactSeed (..),
    CarrierCurrentFactEvidence (..),
    CarrierFactCell,
    CarrierFactSection,
    RestrictedCarrierFactSection,
    ContextRestrictionEdge (..),
    CarrierFactRestrictionError (..),
    CarrierFactComparison (..),
    CarrierFactMergeError (..),
    CarrierFactCommonError (..),
    CarrierFactReconcileError (..),
    emptyCarrierFactLedger,
    carrierFactCellAt,
    carrierFactCellRows,
    carrierFactCellFacts,
    carrierFactRowsAt,
    carrierFactFactsAt,
    carrierLiveEvidenceAt,
    carrierFactAddressesNow,
    carrierFactContextsNow,
    carrierFactSectionContext,
    carrierFactSectionCellAt,
    carrierFactSectionCells,
    carrierFactSectionAddresses,
    restrictedCarrierFactSection,
    emptyCarrierFactSection,
    carrierFactSectionNow,
    restrictCarrierFactSection,
    compareCarrierFactSections,
    carrierFactComparisonEmpty,
    mergeCarrierFactSections,
    commonCarrierFactSection,
    reconcileCarrierFactRestriction,
    solveCarrierFactsAcross,
    reconcileCarrierFactsAcross,
  )
where

import Data.List.NonEmpty
  ( NonEmpty,
  )
import Moonlight.Core
  ( BoundaryOps,
  )
import Moonlight.Differential.Context.Restriction
  ( ContextRestrictionEdge (..),
  )
import Moonlight.Flow.Carrier.Fact.Internal.LedgerIndex
  ( CarrierFactRuntime (..),
  )
import Moonlight.Flow.Carrier.Fact.Ledger
import Moonlight.Flow.Carrier.Fact.Section
import Moonlight.Flow.Carrier.Store.Core.State
  ( CarrierStore,
  )

solveCarrierFactsAcross ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  CarrierFactRuntime ctx carrier prop boundary ->
  ContextRestrictionEdge ctx ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either
    (NonEmpty (CarrierFactRestrictionError ctx carrier prop boundary))
    (CarrierFactSection ctx carrier prop boundary evidence)
solveCarrierFactsAcross runtime edge store =
  restrictedCarrierFactSection
    <$> restrictCarrierFactSection
      runtime
      edge
      (carrierFactSectionNow (creSourceContext edge) store)
{-# INLINE solveCarrierFactsAcross #-}

reconcileCarrierFactsAcross ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  CarrierFactRuntime ctx carrier prop boundary ->
  ContextRestrictionEdge ctx ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either
    (CarrierFactReconcileError ctx carrier prop boundary)
    (CarrierFactSection ctx carrier prop boundary evidence)
reconcileCarrierFactsAcross runtime edge store = do
  restricted <-
    case restrictCarrierFactSection runtime edge (carrierFactSectionNow (creSourceContext edge) store) of
      Left restrictionErrors -> Left (CarrierFactReconcileRestrictionFailed restrictionErrors)
      Right restrictedSection -> Right restrictedSection
  reconcileCarrierFactRestriction
    runtime
    restricted
    (carrierFactSectionNow (creTargetContext edge) store)
{-# INLINE reconcileCarrierFactsAcross #-}
