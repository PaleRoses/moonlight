{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Runtime.Carrier.Restrict
  ( restrictCarrier,
    lookupRestrictionProgram,
  )
where

import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( RestrictKey,
    rkSource,
    rkTarget,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Carrier.Morphism.Core.Program
  ( CarrierMorphismProgram,
    applyCarrierMorphismProgram,
    lookupCarrierMorphismRestrictionProgram,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (PhaseRestrict),
  )
import Moonlight.Flow.Runtime.Carrier.Core.Types
  ( CarrierCommitTrace,
  )
import Moonlight.Flow.Runtime.Carrier.Store
  ( clearCarrier,
    commitCarrierDelta,
    currentCarrierMaybe,
    deltaAgainstCurrent,
  )
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
  )
import Moonlight.Flow.Runtime.Carrier.State
  ( lookupRuntimeRestrictState,
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( Shard,
    routeCarrierShard,
  )

lookupRestrictionProgram ::
  (Ord ctx, Ord prop) =>
  RestrictKey ctx Carrier prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (Shard, CarrierMorphismProgram ctx Carrier prop boundary evidence)
lookupRestrictionProgram restrictKey runtime = do
  restrictShard <-
    case routeCarrierShard PhaseRestrict (rkSource restrictKey) (rsRouting (rdrState runtime)) of
      Nothing ->
        Left (RuntimeMissingRestrictRoute (rkSource restrictKey))
      Just shardValue ->
        Right shardValue
  restrictState <-
    case lookupRuntimeRestrictState restrictShard (rdrState runtime) of
      Nothing ->
        Left (RuntimeMissingRestrictShard restrictShard)
      Just stateValue ->
        Right stateValue
  case lookupCarrierMorphismRestrictionProgram restrictKey restrictState of
    Nothing ->
      Left (RuntimeMissingRestrictionProgram restrictKey)
    Just program ->
      Right (restrictShard, program)
{-# INLINE lookupRestrictionProgram #-}

restrictCarrier ::
  (Ord ctx, Ord prop) =>
  RelationalCarrierTime ctx ->
  RestrictKey ctx Carrier prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      CarrierCommitTrace ctx prop
    )
restrictCarrier eventTime restrictKey runtime0 = do
  (_restrictShard, program) <-
    lookupRestrictionProgram restrictKey runtime0
  maybeSourceSnapshot <-
    currentCarrierMaybe (rkSource restrictKey) runtime0
  case maybeSourceSnapshot of
    Nothing -> do
      maybeTargetSnapshot <-
        currentCarrierMaybe (rkTarget restrictKey) runtime0
      case maybeTargetSnapshot of
        Nothing ->
          Right (runtime0, mempty)
        Just targetSnapshot ->
          clearCarrier
            targetSnapshot {deTime = eventTime}
            runtime0
    Just sourceSnapshot ->
      case applyCarrierMorphismProgram eventTime sourceSnapshot program of
        Left _morphismError ->
          Right (runtime0, mempty)
        Right targetSnapshot -> do
          targetDelta <-
            deltaAgainstCurrent
              targetSnapshot {deTime = eventTime}
              runtime0
          commitCarrierDelta
            targetDelta
            runtime0
{-# INLINE restrictCarrier #-}
