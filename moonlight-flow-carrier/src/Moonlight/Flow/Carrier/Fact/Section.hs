{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Fact.Section
  ( CarrierFactCell,
    CarrierFactSection,
    RestrictedCarrierFactSection,
    CarrierFactRestrictionError (..),
    CarrierFactComparison (..),
    CarrierFactMergeError (..),
    CarrierFactCommonError (..),
    CarrierFactReconcileError (..),
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
    reconcileCarrierFactSectionWithProof,
  )
where

import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Moonlight.Core
  ( BoundaryOps,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
  )
import Moonlight.Flow.Carrier.Fact.Internal.LedgerIndex
  ( CarrierFactCell,
    CarrierFactLedger (..),
    CarrierFactSection (..),
    RestrictedCarrierFactSection (..),
    carrierFactCurrentCellReadout,
    carrierFactSectionAddressSet,
    mkCarrierFactSection,
  )
import Moonlight.Flow.Carrier.Fact.Internal.Compare
  ( CarrierFactComparison (..),
    carrierFactComparisonEmpty,
    compareCarrierFactSections,
  )
import Moonlight.Flow.Carrier.Fact.Internal.Reconcile
  ( CarrierFactCommonError (..),
    CarrierFactMergeError (..),
    CarrierFactReconcileError (..),
    commonCarrierFactSection,
    mergeCarrierFactSections,
    reconcileCarrierFactRestriction,
    reconcileCarrierFactSectionWithProof,
  )
import Moonlight.Flow.Carrier.Fact.Internal.Restrict
  ( CarrierFactRestrictionError (..),
    restrictCarrierFactSection,
  )
import Moonlight.Flow.Carrier.Store.Core.State
  ( CarrierStore (..),
    cstViews,
    cvFacts,
  )

carrierFactSectionContext ::
  CarrierFactSection ctx carrier prop boundary evidence ->
  ctx
carrierFactSectionContext =
  cfsContext
{-# INLINE carrierFactSectionContext #-}

carrierFactSectionCellAt ::
  Ord (CarrierAddr ctx carrier prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierFactSection ctx carrier prop boundary evidence ->
  Maybe (CarrierFactCell ctx carrier prop boundary evidence)
carrierFactSectionCellAt addr =
  Map.lookup addr . cfsCells
{-# INLINE carrierFactSectionCellAt #-}

carrierFactSectionCells ::
  CarrierFactSection ctx carrier prop boundary evidence ->
  Map (CarrierAddr ctx carrier prop) (CarrierFactCell ctx carrier prop boundary evidence)
carrierFactSectionCells =
  cfsCells
{-# INLINE carrierFactSectionCells #-}

carrierFactSectionAddresses ::
  CarrierFactSection ctx carrier prop boundary evidence ->
  Set (CarrierAddr ctx carrier prop)
carrierFactSectionAddresses =
  carrierFactSectionAddressSet
{-# INLINE carrierFactSectionAddresses #-}

restrictedCarrierFactSection ::
  RestrictedCarrierFactSection ctx carrier prop boundary evidence ->
  CarrierFactSection ctx carrier prop boundary evidence
restrictedCarrierFactSection =
  rcfsSection
{-# INLINE restrictedCarrierFactSection #-}

emptyCarrierFactSection ::
  ctx ->
  CarrierFactSection ctx carrier prop boundary evidence
emptyCarrierFactSection contextValue =
  CarrierFactSection
    { cfsContext = contextValue,
      cfsCells = Map.empty
    }
{-# INLINE emptyCarrierFactSection #-}

carrierFactSectionNow ::
  (Ord ctx, Ord prop, BoundaryOps boundary) =>
  ctx ->
  CarrierStore ctx carrier prop boundary evidence ->
  CarrierFactSection ctx carrier prop boundary evidence
carrierFactSectionNow contextValue store =
  mkCarrierFactSection
    contextValue
    ( Map.mapMaybe
        carrierFactCurrentCellReadout
        (Map.filterWithKey (\addr _cell -> caContext addr == contextValue) (cflCurrent (cvFacts (cstViews store))))
    )
{-# INLINE carrierFactSectionNow #-}
