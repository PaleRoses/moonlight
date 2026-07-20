{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Core.Topology
  ( TouchKey (..),
    CarrierEdge (..),
    CarrierTopology,
    emptyCarrierTopology,
    insertCarrierTouch,
    insertCarrierEdge,
    insertCarrierFamily,
    deleteCarrierEdge,
    deleteCarrierFamily,
    carrierTopologyEdges,
    carrierTopologyTouches,
    carrierTopologySubsumptionEdges,
    carrierTopologyRestrictionEdges,
    carrierTopologyHasFamilyMember,
    carrierTopologyAddresses,
    carrierTopologyDerivedOwners,
    carrierTopologyTouchedBy,
    carrierTopologyFanoutFrom,
    carrierTopologyTouchedAddresses,
  )
where

import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caCarrier,
    RestrictKey,
    rkSource,
    rkTarget,
  )
import Moonlight.Differential.Carrier.Topology
  ( CarrierFamily,
    carrierFamilyMembers,
    carrierFamilyTargets,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
  )
import Moonlight.Flow.Carrier.Core.Reuse
  ( CarrierReuseId,
  )

type TouchKey :: Type
data TouchKey
  = TouchAtom {-# UNPACK #-} !Int
  | TouchDep {-# UNPACK #-} !Int
  | TouchTopo {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show)

type CarrierEdge :: Type -> Type -> Type -> Type
data CarrierEdge ctx carrier prop
  = EdgeRestriction !(RestrictKey ctx carrier prop)
  | EdgeSubsumption !(CarrierReuseId ctx prop) !(CarrierAddr ctx carrier prop) !(CarrierAddr ctx carrier prop)
  | EdgeAmalgamation !(CarrierFamily ctx carrier prop)
  deriving stock (Eq, Ord, Show)

type CarrierTopology :: Type -> Type -> Type -> Type
data CarrierTopology ctx carrier prop = CarrierTopology
  { ctTouchIndex :: !(Map TouchKey (Set (CarrierAddr ctx carrier prop))),
    ctFanout :: !(Map (CarrierAddr ctx carrier prop) (Set (CarrierEdge ctx carrier prop)))
  }
  deriving stock (Eq, Show)

emptyCarrierTopology :: CarrierTopology ctx carrier prop
emptyCarrierTopology =
  CarrierTopology Map.empty Map.empty
{-# INLINE emptyCarrierTopology #-}

insertSet :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)
insertSet key value =
  Map.insertWith Set.union key (Set.singleton value)
{-# INLINE insertSet #-}

insertCarrierTouch ::
  (Ord ctx, Ord carrier, Ord prop) =>
  TouchKey ->
  CarrierAddr ctx carrier prop ->
  CarrierTopology ctx carrier prop ->
  CarrierTopology ctx carrier prop
insertCarrierTouch key addr topology =
  topology {ctTouchIndex = insertSet key addr (ctTouchIndex topology)}
{-# INLINE insertCarrierTouch #-}

insertCarrierEdge ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierEdge ctx carrier prop ->
  CarrierTopology ctx carrier prop ->
  CarrierTopology ctx carrier prop
insertCarrierEdge addr edge topology =
  topology {ctFanout = insertSet addr edge (ctFanout topology)}
{-# INLINE insertCarrierEdge #-}

insertCarrierFamily ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierFamily ctx carrier prop ->
  CarrierTopology ctx carrier prop ->
  CarrierTopology ctx carrier prop
insertCarrierFamily family topology0 =
  Set.foldl'
    (\topology addr -> insertCarrierEdge addr (EdgeAmalgamation family) topology)
    topology0
    (carrierFamilyMembers family)
{-# INLINE insertCarrierFamily #-}

deleteCarrierEdge ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierEdge ctx carrier prop ->
  CarrierTopology ctx carrier prop ->
  CarrierTopology ctx carrier prop
deleteCarrierEdge addr edge topology =
  topology
    { ctFanout =
        Map.update
          ( \edges ->
              let edges' =
                    Set.delete edge edges
               in if Set.null edges'
                    then Nothing
                    else Just edges'
          )
          addr
          (ctFanout topology)
    }
{-# INLINE deleteCarrierEdge #-}

deleteCarrierFamily ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierFamily ctx carrier prop ->
  CarrierTopology ctx carrier prop ->
  CarrierTopology ctx carrier prop
deleteCarrierFamily family topology0 =
  Set.foldl'
    (\topology addr -> deleteCarrierEdge addr (EdgeAmalgamation family) topology)
    topology0
    (carrierFamilyMembers family)
{-# INLINE deleteCarrierFamily #-}

carrierTopologyEdges ::
  CarrierTopology ctx carrier prop ->
  [(CarrierAddr ctx carrier prop, CarrierEdge ctx carrier prop)]
carrierTopologyEdges topology =
  [ (addr, edge)
  | (addr, edges) <- Map.toAscList (ctFanout topology),
    edge <- Set.toAscList edges
  ]
{-# INLINE carrierTopologyEdges #-}

carrierTopologyTouches ::
  CarrierTopology ctx carrier prop ->
  [(TouchKey, CarrierAddr ctx carrier prop)]
carrierTopologyTouches topology =
  [ (key, addr)
  | (key, addrs) <- Map.toAscList (ctTouchIndex topology),
    addr <- Set.toAscList addrs
  ]
{-# INLINE carrierTopologyTouches #-}

carrierTopologySubsumptionEdges ::
  CarrierTopology ctx Carrier prop ->
  [(CarrierReuseId ctx prop, CarrierAddr ctx Carrier prop, CarrierAddr ctx Carrier prop)]
carrierTopologySubsumptionEdges topology =
  [ (reuseId, source, target)
  | (_addr, EdgeSubsumption reuseId source target) <- carrierTopologyEdges topology
  ]
{-# INLINE carrierTopologySubsumptionEdges #-}

carrierTopologyRestrictionEdges ::
  CarrierTopology ctx Carrier prop ->
  [RestrictKey ctx Carrier prop]
carrierTopologyRestrictionEdges topology =
  [ restrictKeyValue
  | (_addr, EdgeRestriction restrictKeyValue) <- carrierTopologyEdges topology
  ]
{-# INLINE carrierTopologyRestrictionEdges #-}

carrierTopologyHasFamilyMember ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierFamily ctx carrier prop ->
  CarrierAddr ctx carrier prop ->
  CarrierTopology ctx carrier prop ->
  Bool
carrierTopologyHasFamilyMember family addr topology =
  Set.member
    (EdgeAmalgamation family)
    (Map.findWithDefault Set.empty addr (ctFanout topology))
{-# INLINE carrierTopologyHasFamilyMember #-}

carrierTopologyAddresses ::
  (Ord ctx, Ord prop) =>
  CarrierTopology ctx Carrier prop ->
  Set (CarrierAddr ctx Carrier prop)
carrierTopologyAddresses topology =
  Set.unions
    [ foldMap id (Map.elems (ctTouchIndex topology)),
      Map.keysSet (ctFanout topology),
      Set.unions [edgeAddresses edge | (_addr, edge) <- carrierTopologyEdges topology]
    ]
{-# INLINE carrierTopologyAddresses #-}

carrierTopologyDerivedOwners ::
  (Ord ctx, Ord prop) =>
  CarrierTopology ctx Carrier prop ->
  Map
    (CarrierAddr ctx Carrier prop)
    (Set (CarrierReuseId ctx prop, CarrierAddr ctx Carrier prop))
carrierTopologyDerivedOwners topology =
  Map.fromListWith
    Set.union
    [ (target, Set.singleton (reuseId, source))
    | (_addr, EdgeSubsumption reuseId source target) <- carrierTopologyEdges topology,
      isDerivedCarrier target
    ]
{-# INLINE carrierTopologyDerivedOwners #-}

carrierTopologyTouchedBy ::
  TouchKey ->
  CarrierTopology ctx carrier prop ->
  Set (CarrierAddr ctx carrier prop)
carrierTopologyTouchedBy key topology =
  Map.findWithDefault Set.empty key (ctTouchIndex topology)
{-# INLINE carrierTopologyTouchedBy #-}

carrierTopologyFanoutFrom ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierTopology ctx carrier prop ->
  Set (CarrierEdge ctx carrier prop)
carrierTopologyFanoutFrom addr topology =
  Map.findWithDefault Set.empty addr (ctFanout topology)
{-# INLINE carrierTopologyFanoutFrom #-}

carrierTopologyTouchedAddresses ::
  Ord (CarrierAddr ctx carrier prop) =>
  CarrierTopology ctx carrier prop ->
  Set (CarrierAddr ctx carrier prop)
carrierTopologyTouchedAddresses =
  foldMap id . Map.elems . ctTouchIndex
{-# INLINE carrierTopologyTouchedAddresses #-}

isDerivedCarrier :: CarrierAddr ctx Carrier prop -> Bool
isDerivedCarrier addr =
  case caCarrier addr of
    DerivedCarrier {} ->
      True
    QueryCarrier {} ->
      False
{-# INLINE isDerivedCarrier #-}

edgeAddresses ::
  (Ord ctx, Ord prop) =>
  CarrierEdge ctx Carrier prop ->
  Set (CarrierAddr ctx Carrier prop)
edgeAddresses edge =
  case edge of
    EdgeRestriction key ->
      Set.fromList [rkSource key, rkTarget key]
    EdgeSubsumption _reuseId source target ->
      Set.fromList [source, target]
    EdgeAmalgamation family ->
      Set.union
        (carrierFamilyMembers family)
        (carrierFamilyTargets family)
{-# INLINE edgeAddresses #-}
