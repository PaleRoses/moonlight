{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Runtime.Carrier.Amalgamation
  ( amalgamateCarrierFamily,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe
  ( catMaybes,
  )
import Data.Set qualified as Set
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
import Moonlight.Differential.Carrier.Topology
  ( CarrierFamily,
    carrierFamilyCover,
    carrierFamilyMembers,
  )
import Moonlight.Flow.Carrier.Morphism.Amalgamation
  ( AmalgamationResult (..),
  )
import Moonlight.Flow.Carrier.Morphism.Engine
  ( runCarrierAmalgamation,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Runtime.Carrier.Core.Types
  ( CarrierCommitTrace,
  )
import Moonlight.Flow.Runtime.Carrier.Store
  ( commitCarrierDelta,
    currentCarrierMaybe,
    deltaAgainstCurrent,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
    RelationalRuntimeOpFailure (..),
  )

amalgamateCarrierFamily ::
  (Ord ctx, Ord prop, Semigroup evidence) =>
  RelationalCarrierTime ctx ->
  CarrierFamily ctx Carrier prop ->
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop RuntimeBoundary evidence)
    ( RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr,
      CarrierCommitTrace ctx prop
    )
amalgamateCarrierFamily eventTime family runtime = do
  snapshots <-
    traverse
      (`currentCarrierMaybe` runtime)
      (Set.toAscList (carrierFamilyMembers family))
  case NonEmpty.nonEmpty (catMaybes snapshots) of
    Nothing ->
      Right (runtime, mempty)
    Just deltas ->
      case runCarrierAmalgamation (carrierFamilyCover family) deltas of
        Left err ->
          Left (RuntimeOpFailure (RelationalRuntimeAmalgamationError family err))
        Right (ExactAmalgamatedDelta snapshot) ->
          insertSnapshot eventTime snapshot runtime
        Right (LowerBoundDelta snapshot) ->
          insertSnapshot eventTime snapshot runtime
        Right (ObstructedAmalgamation obstruction) ->
          Left (RuntimeOpFailure (RelationalRuntimeAmalgamationObstructed family obstruction))
{-# INLINE amalgamateCarrierFamily #-}

insertSnapshot ::
  (Ord ctx, Ord prop) =>
  RelationalCarrierTime ctx ->
  RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence ->
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop RuntimeBoundary evidence)
    ( RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr,
      CarrierCommitTrace ctx prop
    )
insertSnapshot eventTime snapshot runtime = do
  delta <-
    deltaAgainstCurrent snapshot {deTime = eventTime} runtime
  commitCarrierDelta
    delta
    runtime
{-# INLINE insertSnapshot #-}
