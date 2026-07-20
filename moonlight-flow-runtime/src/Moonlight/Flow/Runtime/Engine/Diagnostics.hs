{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Runtime.Engine.Diagnostics
  ( RuntimeTouchExplanation (..),
    explainRuntimeFanout,
    explainRuntimeFanoutForTouchedCarriers,
    explainRuntimeTouch,
    explainRuntimeImpact,
  )
where

import Data.Kind
  ( Type,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Topology
  ( CarrierTopology,
    TouchKey,
    carrierTopologyTouchedBy,
  )
import Moonlight.Flow.Runtime.Topology.Lowering.Edge
  ( RuntimeTopologyFanoutStep,
    lowerTouchedCarrierFanoutSteps,
    lowerTouchedCarriersFanoutSteps,
  )
import Moonlight.Flow.Runtime.Topology.Lowering.Impact
  ( RuntimeImpact,
    RuntimeTouchCause (..),
    touchCausesForImpact,
  )

type RuntimeTouchExplanation :: Type -> Type -> Type -> Type -> Type
data RuntimeTouchExplanation ctx prop boundary evidence = RuntimeTouchExplanation
  { eteTouchKey :: !TouchKey,
    eteTouchedCarrier :: !(CarrierAddr ctx Carrier prop),
    eteCause :: !RuntimeTouchCause,
    eteFanout :: ![RuntimeTopologyFanoutStep ctx prop boundary evidence]
  }
  deriving stock (Eq, Show)

explainRuntimeFanout ::
  (Ord ctx, Ord prop) =>
  CarrierTopology ctx Carrier prop ->
  CarrierAddr ctx Carrier prop ->
  [RuntimeTopologyFanoutStep ctx prop boundary evidence]
explainRuntimeFanout =
  lowerTouchedCarrierFanoutSteps
{-# INLINE explainRuntimeFanout #-}

explainRuntimeFanoutForTouchedCarriers ::
  (Ord ctx, Ord prop) =>
  CarrierTopology ctx Carrier prop ->
  Set (CarrierAddr ctx Carrier prop) ->
  [RuntimeTopologyFanoutStep ctx prop boundary evidence]
explainRuntimeFanoutForTouchedCarriers =
  lowerTouchedCarriersFanoutSteps
{-# INLINE explainRuntimeFanoutForTouchedCarriers #-}

explainRuntimeTouch ::
  (Ord ctx, Ord prop) =>
  CarrierTopology ctx Carrier prop ->
  TouchKey ->
  [RuntimeTouchExplanation ctx prop boundary evidence]
explainRuntimeTouch carrierTopology touchKey =
  [ RuntimeTouchExplanation
      { eteTouchKey = touchKey,
        eteTouchedCarrier = addr,
        eteCause =
          RuntimeTouchCause
            { rtcTouchKeys = Set.singleton touchKey,
              rtcRelationalScope = mempty
            },
        eteFanout = explainRuntimeFanout carrierTopology addr
      }
  | addr <- Set.toAscList (carrierTopologyTouchedBy touchKey carrierTopology)
  ]
{-# INLINE explainRuntimeTouch #-}

explainRuntimeImpact ::
  (Ord ctx, Ord prop) =>
  CarrierTopology ctx Carrier prop ->
  RuntimeImpact ctx prop ->
  [RuntimeTouchExplanation ctx prop boundary evidence]
explainRuntimeImpact carrierTopology impact =
  [ RuntimeTouchExplanation
      { eteTouchKey = touchKey,
        eteTouchedCarrier = addr,
        eteCause = cause,
        eteFanout = explainRuntimeFanout carrierTopology addr
      }
  | (addr, cause) <- Map.toAscList (touchCausesForImpact carrierTopology impact),
    touchKey <- Set.toAscList (rtcTouchKeys cause)
  ]
{-# INLINE explainRuntimeImpact #-}
