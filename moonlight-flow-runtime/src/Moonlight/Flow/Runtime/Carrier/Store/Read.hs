{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Runtime.Carrier.Store.Read
  ( currentCarrierMaybe,
    currentCarrier,
    visibleCarrier,
    visibleContext,
    pinVisibleContext,
    unpinVisibleContext,
    visibleContextUncached,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
  )
import Moonlight.Flow.Carrier.View.Cache
  ( VisibleContextKey (..),
    dropPinnedVisibleContext,
    insertVisibleContext,
    insertPinnedVisibleContext,
    lookupVisibleContext,
    lookupPinnedVisibleContext,
  )
import Moonlight.Flow.Carrier.View.Query qualified as CarrierView
import Moonlight.Flow.Carrier.View.Section
  ( RelationalSection (..),
  )
import Moonlight.Delta.Signed
  ( Multiplicity
  )
import Moonlight.Differential.Row.Patch
  ( composePlainRowPatch
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.Flow.Runtime.Carrier.Store.Internal qualified as Internal
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    RuntimeEnv (..),
    rsRouting,
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Carrier.State
  ( runtimeIndexOps,
    runtimeVisibleCache,
    setRuntimeVisibleCache,
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core

currentCarrierMaybe ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (Maybe (RelationalCarrierDelta ctx Carrier prop boundary evidence))
currentCarrierMaybe addr runtime =
  Internal.currentCarrierMaybeAtRouting (rsRouting (rdrState runtime)) addr runtime
{-# INLINE currentCarrierMaybe #-}

currentCarrier ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelationalCarrierDelta ctx Carrier prop boundary evidence)
currentCarrier addr runtime = do
  maybeDelta <- currentCarrierMaybe addr runtime
  case maybeDelta of
    Nothing ->
      Left (RuntimeMissingCurrentCarrier addr)
    Just delta ->
      Right delta
{-# INLINE currentCarrier #-}

visibleCarrier ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (Map RowTupleKey Multiplicity)
visibleCarrier addr runtime = do
  (_shard, indexState) <-
    Internal.carrierStoreAtRouting
      (rsRouting (rdrState runtime))
      addr
      runtime
  pure (CarrierView.visibleCarrierNow addr indexState)
{-# INLINE visibleCarrier #-}

visibleContext ::
  (Ord ctx, Ord prop) =>
  ctx ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      RelationalSection ctx Carrier prop
    )
visibleContext contextValue runtime0 =
  let state0 =
        rdrState runtime0
      (cachePinnedTouched, maybePinnedSection) =
        lookupPinnedVisibleContext contextValue (runtimeVisibleCache state0)
      cacheKey =
        VisibleContextKey
          { vckQuotientEpoch = Core.rsQuotientEpoch state0,
            vckLiveEpoch = Core.rsLiveEpoch state0,
            vckContext = contextValue
          }
   in case maybePinnedSection of
        Just sectionValue ->
          Right
            ( runtime0 {rdrState = setRuntimeVisibleCache cachePinnedTouched state0},
              sectionValue
            )
        Nothing ->
          let (cacheTouched, maybeCachedSection) =
                lookupVisibleContext cacheKey cachePinnedTouched
           in case maybeCachedSection of
                Just sectionValue ->
                  Right
                    ( runtime0 {rdrState = setRuntimeVisibleCache cacheTouched state0},
                      sectionValue
                    )
                Nothing ->
                  let !sectionValue =
                        visibleContextUncached contextValue runtime0
                      !cacheInserted =
                        insertVisibleContext
                          (reVisibleSectionBytes (rdrEnv runtime0))
                          cacheKey
                          sectionValue
                          cacheTouched
                   in Right
                        ( runtime0 {rdrState = setRuntimeVisibleCache cacheInserted state0},
                          sectionValue
                        )
{-# INLINE visibleContext #-}

pinVisibleContext ::
  (Ord ctx, Ord prop) =>
  ctx ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
pinVisibleContext contextValue runtime =
  let state0 =
        rdrState runtime
      !sectionValue =
        visibleContextUncached contextValue runtime
      !cachePinned =
        insertPinnedVisibleContext
          (reVisibleSectionBytes (rdrEnv runtime))
          contextValue
          sectionValue
          (runtimeVisibleCache state0)
   in runtime {rdrState = setRuntimeVisibleCache cachePinned state0}
{-# INLINE pinVisibleContext #-}

unpinVisibleContext ::
  Ord ctx =>
  ctx ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
unpinVisibleContext contextValue runtime =
  let state0 =
        rdrState runtime
      !cacheUnpinned =
        dropPinnedVisibleContext contextValue (runtimeVisibleCache state0)
   in runtime {rdrState = setRuntimeVisibleCache cacheUnpinned state0}
{-# INLINE unpinVisibleContext #-}

visibleContextUncached ::
  (Ord ctx, Ord prop) =>
  ctx ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelationalSection ctx Carrier prop
visibleContextUncached contextValue runtime =
  RelationalSection
    { rsCarriers =
        IntMap.foldl'
          ( \acc indexState ->
              Map.unionWith
                composePlainRowPatch
                acc
                (rsCarriers (CarrierView.visibleContextNow contextValue indexState))
          )
          Map.empty
          (runtimeIndexOps (rdrState runtime))
    }
{-# INLINE visibleContextUncached #-}
