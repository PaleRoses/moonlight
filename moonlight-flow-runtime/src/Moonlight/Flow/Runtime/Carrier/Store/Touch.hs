{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Runtime.Carrier.Store.Touch
  ( applyTouches,
    invalidateLazyVisibleCache,
  )
where

import Data.Foldable
  ( foldlM,
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Delta.Time
  ( Timed (..),
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierStoreTouch (..),
  )
import Moonlight.Flow.Carrier.View.Cache
  ( dropLazyVisibleContext,
    pinnedVisibleContextMember,
    updatePinnedVisibleContext,
  )
import Moonlight.Flow.Carrier.View.Section
  ( deleteVisibleCarrierRows,
    setVisibleCarrierRows,
  )
import Moonlight.Flow.Runtime.Carrier.Core.Types
  ( CarrierCommitTrace (..),
  )
import Moonlight.Flow.Runtime.Carrier.Store.Internal
  ( currentCarrierMaybeAtRouting,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    RuntimeEnv (..),
    rsRouting,
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Carrier.State
  ( mapRuntimeVisibleCache,
    runtimeVisibleCache,
  )

applyTouches ::
  (Ord ctx, Ord prop) =>
  [Timed (RelationalCarrierTime ctx) (CarrierStoreTouch ctx Carrier prop)] ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      CarrierCommitTrace ctx prop
    )
applyTouches touches runtime = do
  runtimeMaintained <-
    maintainPinnedVisibleContexts touchedContexts touchedAddrs runtime
  Right
    ( runtimeMaintained,
      CarrierCommitTrace
        { cctTouchedContexts = touchedContexts,
          cctTouchedCarriers = touchedAddrs
        }
    )
  where
    !touchedContexts =
      Set.fromList (fmap (cstContext . timedValue) touches)

    !touchedAddrs =
      Set.fromList (fmap (cstAddr . timedValue) touches)
{-# INLINE applyTouches #-}

maintainPinnedVisibleContexts ::
  (Ord ctx, Ord prop) =>
  Set ctx ->
  Set (CarrierAddr ctx Carrier prop) ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
maintainPinnedVisibleContexts touchedContexts touchedAddrs runtime =
  foldlM
    refreshPinnedCarrier
    (invalidateLazyVisibleCache touchedContexts runtime)
    (Set.toAscList touchedAddrs)
{-# INLINE maintainPinnedVisibleContexts #-}

refreshPinnedCarrier ::
  (Ord ctx, Ord prop) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  CarrierAddr ctx Carrier prop ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
refreshPinnedCarrier runtime addr =
  if pinnedVisibleContextMember contextValue (runtimeVisibleCache (rdrState runtime))
    then do
      maybeCurrent <-
        currentCarrierMaybeAtRouting (rsRouting (rdrState runtime)) addr runtime
      let sectionUpdate =
            case maybeCurrent of
              Nothing ->
                deleteVisibleCarrierRows addr
              Just current ->
                setVisibleCarrierRows addr (deRows current)
      pure
        runtime
          { rdrState =
              mapRuntimeVisibleCache
                ( updatePinnedVisibleContext
                    (reVisibleSectionBytes (rdrEnv runtime))
                    contextValue
                    sectionUpdate
                )
                (rdrState runtime)
          }
    else Right runtime
  where
    contextValue =
      caContext addr
{-# INLINE refreshPinnedCarrier #-}

invalidateLazyVisibleCache ::
  (Ord ctx) =>
  Set ctx ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
invalidateLazyVisibleCache touchedContexts runtime =
  runtime
    { rdrState =
        mapRuntimeVisibleCache
          ( \cache ->
              Set.foldl'
                ( \cacheValue contextValue ->
                    dropLazyVisibleContext contextValue cacheValue
                )
                cache
                touchedContexts
          )
          (rdrState runtime)
    }
{-# INLINE invalidateLazyVisibleCache #-}
