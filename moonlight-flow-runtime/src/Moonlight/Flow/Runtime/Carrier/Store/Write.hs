{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Runtime.Carrier.Store.Write
  ( commitCarrierDelta,
    commitCarrierDeltas,
    clearCarrier,
    deltaAgainstCurrent,
    indexCarrierDelta,
    indexCarrierDeltaAtRouting,
    indexCarrierDeltas,
    commitCarrierDeltaAtRouting,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.Bifunctor
  ( first,
  )
import Data.Monoid
  ( Endo (..),
    appEndo,
  )
import Moonlight.Delta.Operator
  ( OpResult (..),
    Operator (..),
  )
import Moonlight.Delta.Time
  ( Timed (..),
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierStoreTouch,
  )
import Moonlight.Differential.Row.Patch
  ( emptyPlainRowPatch,
    negatePlainRowPatch,
    subtractPlainRowPatch,
  )
import Moonlight.Flow.Runtime.Carrier.Core.Types
  ( CarrierCommitTrace,
  )
import Moonlight.Flow.Runtime.Carrier.Store.Internal qualified as Internal
import Moonlight.Flow.Runtime.Carrier.Store.Read
  ( currentCarrierMaybe,
  )
import Moonlight.Flow.Runtime.Carrier.Store.Touch qualified as Touch
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    rsRouting,
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
    RelationalRuntimeOpFailure (..),
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( RuntimeRouting,
  )

commitCarrierDelta ::
  (Ord ctx, Ord prop) =>
  RelationalCarrierDelta ctx Carrier prop boundary evidence ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      CarrierCommitTrace ctx prop
    )
commitCarrierDelta delta runtime = do
  (runtimeIndexed, touches) <-
    indexCarrierDelta delta runtime
  Touch.applyTouches touches runtimeIndexed
{-# INLINE commitCarrierDelta #-}

commitCarrierDeltas ::
  (Ord ctx, Ord prop) =>
  [RelationalCarrierDelta ctx Carrier prop boundary evidence] ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      CarrierCommitTrace ctx prop
    )
commitCarrierDeltas deltas runtime = do
  (runtimeIndexed, touches) <-
    indexCarrierDeltas deltas runtime
  Touch.applyTouches touches runtimeIndexed
{-# INLINE commitCarrierDeltas #-}

clearCarrier ::
  (Ord ctx, Ord prop) =>
  RelationalCarrierDelta ctx Carrier prop boundary evidence ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      CarrierCommitTrace ctx prop
    )
clearCarrier currentSnapshot =
  commitCarrierDelta
    currentSnapshot
      { deRows =
          negatePlainRowPatch (deRows currentSnapshot)
      }
{-# INLINE clearCarrier #-}

deltaAgainstCurrent ::
  (Ord ctx, Ord prop) =>
  RelationalCarrierDelta ctx Carrier prop boundary evidence ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelationalCarrierDelta ctx Carrier prop boundary evidence)
deltaAgainstCurrent nextSnapshot runtime = do
  maybeCurrent <- currentCarrierMaybe (deAddr nextSnapshot) runtime
  let !currentRows =
        maybe emptyPlainRowPatch deRows maybeCurrent
  pure
    nextSnapshot
      { deRows =
          subtractPlainRowPatch
            (deRows nextSnapshot)
            currentRows
      }
{-# INLINE deltaAgainstCurrent #-}

commitCarrierDeltaAtRouting ::
  (Ord ctx, Ord prop) =>
  RuntimeRouting ctx prop ->
  RelationalCarrierDelta ctx Carrier prop boundary evidence ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      CarrierCommitTrace ctx prop
    )
commitCarrierDeltaAtRouting routing delta runtime = do
  (runtimeIndexed, touches) <-
    indexCarrierDeltaAtRouting routing delta runtime
  Touch.applyTouches touches runtimeIndexed
{-# INLINE commitCarrierDeltaAtRouting #-}

indexCarrierDelta ::
  (Ord ctx, Ord prop) =>
  RelationalCarrierDelta ctx Carrier prop boundary evidence ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      [Timed (RelationalCarrierTime ctx) (CarrierStoreTouch ctx Carrier prop)]
    )
indexCarrierDelta delta runtime =
  indexCarrierDeltaAtRouting (rsRouting (rdrState runtime)) delta runtime
{-# INLINE indexCarrierDelta #-}

indexCarrierDeltaAtRouting ::
  (Ord ctx, Ord prop) =>
  RuntimeRouting ctx prop ->
  RelationalCarrierDelta ctx Carrier prop boundary evidence ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      [Timed (RelationalCarrierTime ctx) (CarrierStoreTouch ctx Carrier prop)]
    )
indexCarrierDeltaAtRouting routing delta runtime = do
  (shard, indexState) <-
    Internal.carrierStoreAtRouting routing (deAddr delta) runtime
  result <-
    first
      (RuntimeOpFailure . RelationalRuntimeCarrierStoreOperatorError shard)
      (opStep (Internal.runtimeCarrierStoreOperator runtime) indexState (Timed (deTime delta) delta))
  pure
    ( Internal.replaceCarrierStore shard (orState result) runtime,
      orEmit result
    )
{-# INLINE indexCarrierDeltaAtRouting #-}

indexCarrierDeltas ::
  (Ord ctx, Ord prop) =>
  [RelationalCarrierDelta ctx Carrier prop boundary evidence] ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      [Timed (RelationalCarrierTime ctx) (CarrierStoreTouch ctx Carrier prop)]
    )
indexCarrierDeltas deltas runtime0 = do
  (runtimeIndexed, touchBuilder) <-
    foldM indexOne (runtime0, Endo id) deltas
  pure (runtimeIndexed, appEndo touchBuilder [])
  where
    indexOne ::
      (Ord ctx, Ord prop) =>
      ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
        Endo [Timed (RelationalCarrierTime ctx) (CarrierStoreTouch ctx Carrier prop)]
      ) ->
      RelationalCarrierDelta ctx Carrier prop boundary evidence ->
      Either
        (RelationalRuntimeError ctx prop boundary evidence)
        ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
          Endo [Timed (RelationalCarrierTime ctx) (CarrierStoreTouch ctx Carrier prop)]
        )
    indexOne (runtime, touches) delta = do
      (runtimeIndexed, emittedTouches) <-
        indexCarrierDelta delta runtime
      pure
        ( runtimeIndexed,
          touches <> Endo (emittedTouches <>)
        )
{-# INLINE indexCarrierDeltas #-}
