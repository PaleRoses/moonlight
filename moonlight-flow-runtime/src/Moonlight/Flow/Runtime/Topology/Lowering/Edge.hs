{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Runtime.Topology.Lowering.Edge
  ( RuntimeTopologyFanoutStep (..),
    lowerCarrierEdge,
    lowerTouchedCarrierFanoutStep,
    lowerTouchedCarrierFanoutSteps,
    lowerTouchedCarriersFanoutSteps,
    lowerTouchedCarrierFanout,
    lowerTouchedCarriersFanout,
  )
where

import Data.Kind
  ( Type,
  )
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
  ( CarrierEdge (..),
    CarrierTopology,
    carrierTopologyFanoutFrom,
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( RuntimeDataflowContract,
    RuntimeDataflowOp,
    amalgamateCarrierFamilyDataflowOp,
    deriveSubsumedCarrierDataflowOp,
    restrictCarrierDataflowOp,
    runtimeDataflowOpContract,
  )

type RuntimeTopologyFanoutStep :: Type -> Type -> Type -> Type -> Type
data RuntimeTopologyFanoutStep ctx prop boundary evidence = RuntimeTopologyFanoutStep
  { rtfsTouchedCarrier :: !(CarrierAddr ctx Carrier prop),
    rtfsEdge :: !(CarrierEdge ctx Carrier prop),
    rtfsContract :: !(RuntimeDataflowContract ctx Carrier prop),
    rtfsDataflowOp :: !(RuntimeDataflowOp ctx prop boundary evidence)
  }
  deriving stock (Eq, Show)

lowerCarrierEdge ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  CarrierEdge ctx Carrier prop ->
  RuntimeDataflowOp ctx prop boundary evidence
lowerCarrierEdge _anchor edge =
  case edge of
    EdgeRestriction key ->
      restrictCarrierDataflowOp key
    EdgeSubsumption reuseId source target ->
      deriveSubsumedCarrierDataflowOp reuseId source target
    EdgeAmalgamation family ->
      amalgamateCarrierFamilyDataflowOp family
{-# INLINE lowerCarrierEdge #-}

lowerTouchedCarrierFanoutStep ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  CarrierEdge ctx Carrier prop ->
  RuntimeTopologyFanoutStep ctx prop boundary evidence
lowerTouchedCarrierFanoutStep addr edge =
  let op =
        lowerCarrierEdge addr edge
   in RuntimeTopologyFanoutStep
        { rtfsTouchedCarrier = addr,
          rtfsEdge = edge,
          rtfsContract = runtimeDataflowOpContract op,
          rtfsDataflowOp = op
        }
{-# INLINE lowerTouchedCarrierFanoutStep #-}

lowerTouchedCarrierFanoutSteps ::
  (Ord ctx, Ord prop) =>
  CarrierTopology ctx Carrier prop ->
  CarrierAddr ctx Carrier prop ->
  [RuntimeTopologyFanoutStep ctx prop boundary evidence]
lowerTouchedCarrierFanoutSteps graph addr =
  fmap
    (lowerTouchedCarrierFanoutStep addr)
    (Set.toAscList (carrierTopologyFanoutFrom addr graph))
{-# INLINE lowerTouchedCarrierFanoutSteps #-}

lowerTouchedCarriersFanoutSteps ::
  (Ord ctx, Ord prop) =>
  CarrierTopology ctx Carrier prop ->
  Set (CarrierAddr ctx Carrier prop) ->
  [RuntimeTopologyFanoutStep ctx prop boundary evidence]
lowerTouchedCarriersFanoutSteps graph =
  foldMap (lowerTouchedCarrierFanoutSteps graph)
    . reverse
    . Set.toAscList
{-# INLINE lowerTouchedCarriersFanoutSteps #-}

lowerTouchedCarrierFanout ::
  (Ord ctx, Ord prop) =>
  CarrierTopology ctx Carrier prop ->
  CarrierAddr ctx Carrier prop ->
  [RuntimeDataflowOp ctx prop boundary evidence]
lowerTouchedCarrierFanout graph =
  fmap rtfsDataflowOp . lowerTouchedCarrierFanoutSteps graph
{-# INLINE lowerTouchedCarrierFanout #-}

lowerTouchedCarriersFanout ::
  (Ord ctx, Ord prop) =>
  CarrierTopology ctx Carrier prop ->
  Set (CarrierAddr ctx Carrier prop) ->
  [RuntimeDataflowOp ctx prop boundary evidence]
lowerTouchedCarriersFanout graph =
  fmap rtfsDataflowOp . lowerTouchedCarriersFanoutSteps graph
{-# INLINE lowerTouchedCarriersFanout #-}
