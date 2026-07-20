{-# LANGUAGE DerivingStrategies #-}
module Moonlight.Flow.Runtime.Topology.ReuseEdges
  ( ReuseEdge (..),
    reuseTopologyEdgesFromPlanReuseState,
    reuseTopologyEdgeFromCarrierReuse,
    insertPlanReuseTopologyEdge,
    deletePlanReuseTopologyEdge,
    insertPlanReuseTopologyEdges,
    insertPlanReuseTopologyEdgesFor,
    deletePlanReuseTopologyEdgesFor,
  )
where
import Data.Maybe
  ( mapMaybe,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
import Moonlight.Flow.Carrier.Reuse
  ( PlanReuseState,
    planReuseCarrierReuses,
  )
import Moonlight.Flow.Carrier.Core.Topology
  ( CarrierEdge (..),
    CarrierTopology,
    deleteCarrierEdge,
    insertCarrierEdge,
  )
data ReuseEdge ctx prop = ReuseEdge
  { reReuseId :: !(CarrierReuseId ctx prop),
    reSource :: !(CarrierAddr ctx Carrier prop),
    reTarget :: !(CarrierAddr ctx Carrier prop)
  }
  deriving stock (Eq, Ord, Show)
reuseTopologyEdgesFromPlanReuseState ::
  PlanReuseState ctx prop ->
  [ReuseEdge ctx prop]
reuseTopologyEdgesFromPlanReuseState planReuse =
  mapMaybe reuseEdgeFromEntry (planReuseCarrierReuses planReuse)
{-# INLINE reuseTopologyEdgesFromPlanReuseState #-}
reuseEdgeFromEntry ::
  (CarrierReuseId ctx prop, CarrierReuse ctx prop) ->
  Maybe (ReuseEdge ctx prop)
reuseEdgeFromEntry (reuseId, reuse) =
  reuseTopologyEdgeFromCarrierReuse reuseId reuse
{-# INLINE reuseEdgeFromEntry #-}

reuseTopologyEdgeFromCarrierReuse ::
  CarrierReuseId ctx prop ->
  CarrierReuse ctx prop ->
  Maybe (ReuseEdge ctx prop)
reuseTopologyEdgeFromCarrierReuse reuseId reuse =
  ReuseEdge reuseId (rwSourceCarrier (cruWitness reuse))
    <$> carrierReuseExpectedTarget reuse
{-# INLINE reuseTopologyEdgeFromCarrierReuse #-}

insertPlanReuseTopologyEdges ::
  Ord ctx =>
  Ord prop =>
  PlanReuseState ctx prop ->
  CarrierTopology ctx Carrier prop ->
  CarrierTopology ctx Carrier prop
insertPlanReuseTopologyEdges planReuse graph =
  foldr insertPlanReuseTopologyEdge graph (reuseTopologyEdgesFromPlanReuseState planReuse)
{-# INLINE insertPlanReuseTopologyEdges #-}

insertPlanReuseTopologyEdgesFor ::
  (Ord ctx, Ord prop, Foldable f) =>
  f (CarrierReuse ctx prop) ->
  CarrierTopology ctx Carrier prop ->
  CarrierTopology ctx Carrier prop
insertPlanReuseTopologyEdgesFor reuses graph0 =
  foldr
    ( \reuse graph ->
        case reuseTopologyEdgeFromCarrierReuse (carrierReuseId reuse) reuse of
          Nothing ->
            graph
          Just edge ->
            insertPlanReuseTopologyEdge edge graph
    )
    graph0
    reuses
{-# INLINE insertPlanReuseTopologyEdgesFor #-}

deletePlanReuseTopologyEdgesFor ::
  (Ord ctx, Ord prop, Foldable f) =>
  f (CarrierReuse ctx prop) ->
  CarrierTopology ctx Carrier prop ->
  CarrierTopology ctx Carrier prop
deletePlanReuseTopologyEdgesFor reuses graph0 =
  foldr
    ( \reuse graph ->
        case reuseTopologyEdgeFromCarrierReuse (carrierReuseId reuse) reuse of
          Nothing ->
            graph
          Just edge ->
            deletePlanReuseTopologyEdge edge graph
    )
    graph0
    reuses
{-# INLINE deletePlanReuseTopologyEdgesFor #-}

insertPlanReuseTopologyEdge ::
  Ord ctx =>
  Ord prop =>
  ReuseEdge ctx prop ->
  CarrierTopology ctx Carrier prop ->
  CarrierTopology ctx Carrier prop
insertPlanReuseTopologyEdge edge =
  insertCarrierEdge
    (reSource edge)
    (EdgeSubsumption (reReuseId edge) (reSource edge) (reTarget edge))
{-# INLINE insertPlanReuseTopologyEdge #-}

deletePlanReuseTopologyEdge ::
  Ord ctx =>
  Ord prop =>
  ReuseEdge ctx prop ->
  CarrierTopology ctx Carrier prop ->
  CarrierTopology ctx Carrier prop
deletePlanReuseTopologyEdge edge =
  deleteCarrierEdge
    (reSource edge)
    (EdgeSubsumption (reReuseId edge) (reSource edge) (reTarget edge))
{-# INLINE deletePlanReuseTopologyEdge #-}
