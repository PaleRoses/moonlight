{-# LANGUAGE DerivingStrategies #-}
module Moonlight.Flow.Runtime.Topology.Subscription.Builder
  ( CarrierAddressing (..),
    SubscriptionBuildInput (..),
    CarrierSubscriptionBuildError (..),
    buildGeneratedRoutingSource,
  )
where
import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Map.Strict
  ( Map,
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
  ( TouchKey (..),
  )
import Moonlight.Flow.Runtime.Topology.Site.Types
  ( GeneratedRoutingSource (..),
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( Shard,
  )
import Moonlight.Flow.Runtime.Topology.Subscription
  ( AtomSubscriptionError,
    QueryAtomSubscription (..),
    buildAtomSubscribers,
  )
import Moonlight.Flow.Plan.Query.Core
data CarrierAddressing ctx prop = CarrierAddressing
  { caaAtom :: QueryId -> AtomId -> Maybe (CarrierAddr ctx Carrier prop),
    caaBag :: QueryId -> BagId -> Maybe (CarrierAddr ctx Carrier prop),
    caaSeparator :: QueryId -> BagId -> BagId -> Maybe (CarrierAddr ctx Carrier prop),
    caaRoot :: QueryId -> Maybe (CarrierAddr ctx Carrier prop)
  }
data SubscriptionBuildInput ctx prop boundary = SubscriptionBuildInput
  { sbiAtomSubscriptions :: ![QueryAtomSubscription],
    sbiQueryDecompositions :: !(Map QueryId DecompPlan),
    sbiAddressing :: !(CarrierAddressing ctx prop),
    sbiAtomTouchDeps :: !(IntMap.IntMap IntSet),
    sbiAtomTouchTopo :: !(IntMap.IntMap IntSet),
    sbiCarrierDeps :: !(Map (CarrierAddr ctx Carrier prop) IntSet),
    sbiCarrierTopo :: !(Map (CarrierAddr ctx Carrier prop) IntSet)
  }
data CarrierSubscriptionBuildError ctx prop
  = CarrierSubscriptionAtomError !AtomSubscriptionError
  | CarrierSubscriptionMissingDecomp !QueryId
  | CarrierSubscriptionMissingAtomCarrier !QueryId !AtomId
  | CarrierSubscriptionMissingBagCarrier !QueryId !BagId
  | CarrierSubscriptionMissingSeparatorCarrier !QueryId !BagId !BagId
  | CarrierSubscriptionMissingRootCarrier !QueryId
  | CarrierSubscriptionAtomOwnerMissing !QueryId !AtomId
  deriving stock (Eq, Ord, Show)
buildGeneratedRoutingSource ::
  (Ord ctx, Ord prop) =>
  SubscriptionBuildInput ctx prop boundary ->
  Map (CarrierAddr ctx Carrier prop) Shard ->
  Map (CarrierAddr ctx Carrier prop) Shard ->
  Either
    (CarrierSubscriptionBuildError ctx prop)
    (GeneratedRoutingSource ctx prop)
buildGeneratedRoutingSource input restrictShards indexShards = do
  (atomSubscribers, carrierTouches) <-
    buildCarrierTouchFacts input
  pure
    GeneratedRoutingSource
      { grsAtomSubscribers = atomSubscribers,
        grsCarrierTouches = carrierTouches,
        grsRestrictShardsByCarrier = restrictShards,
        grsIndexShardsByCarrier = indexShards
      }
{-# INLINE buildGeneratedRoutingSource #-}
buildCarrierTouchFacts ::
  (Ord ctx, Ord prop) =>
  SubscriptionBuildInput ctx prop boundary ->
  Either
    (CarrierSubscriptionBuildError ctx prop)
    ( IntMap.IntMap [(QueryId, AtomId)],
      Map TouchKey (Set (CarrierAddr ctx Carrier prop))
    )
buildCarrierTouchFacts input = do
  atomSubscribers <-
    first CarrierSubscriptionAtomError $
      buildAtomSubscribers (sbiAtomSubscriptions input)
  atomIndex <-
    buildAtomIndex input atomSubscribers
  pure
    ( atomSubscribers,
      carrierTouchIndex
        atomIndex
        (sbiAtomTouchDeps input)
        (sbiAtomTouchTopo input)
        (sbiCarrierDeps input)
        (sbiCarrierTopo input)
    )
{-# INLINE buildCarrierTouchFacts #-}
carrierTouchIndex ::
  (Ord ctx, Ord prop) =>
  IntMap.IntMap (Set (CarrierAddr ctx Carrier prop)) ->
  IntMap.IntMap IntSet ->
  IntMap.IntMap IntSet ->
  Map (CarrierAddr ctx Carrier prop) IntSet ->
  Map (CarrierAddr ctx Carrier prop) IntSet ->
  Map TouchKey (Set (CarrierAddr ctx Carrier prop))
carrierTouchIndex atomIndex atomDeps atomTopo carrierDeps carrierTopo =
  Map.unionsWith
    Set.union
    [ touchIndexFromIntMap TouchAtom atomIndex,
      atomScopedTouchIndex TouchDep atomIndex atomDeps,
      atomScopedTouchIndex TouchTopo atomIndex atomTopo,
      touchIndexFromIntMap TouchDep (invertIntSetMap carrierDeps),
      touchIndexFromIntMap TouchTopo (invertIntSetMap carrierTopo)
    ]
{-# INLINE carrierTouchIndex #-}
atomScopedTouchIndex ::
  (Ord ctx, Ord prop) =>
  (Int -> TouchKey) ->
  IntMap.IntMap (Set (CarrierAddr ctx Carrier prop)) ->
  IntMap.IntMap IntSet ->
  Map TouchKey (Set (CarrierAddr ctx Carrier prop))
atomScopedTouchIndex touchKey atomIndex atomTouches =
  IntMap.foldlWithKey'
    insertAtomTouches
    Map.empty
    atomTouches
  where
    insertAtomTouches acc atomKey touchKeys =
      case IntMap.lookup atomKey atomIndex of
        Nothing ->
          acc
        Just addrs ->
          IntSet.foldl'
            ( \acc' key ->
                Map.insertWith Set.union (touchKey key) addrs acc'
            )
            acc
            touchKeys
{-# INLINE atomScopedTouchIndex #-}
touchIndexFromIntMap ::
  (Ord ctx, Ord prop) =>
  (Int -> TouchKey) ->
  IntMap.IntMap (Set (CarrierAddr ctx Carrier prop)) ->
  Map TouchKey (Set (CarrierAddr ctx Carrier prop))
touchIndexFromIntMap touchKey =
  IntMap.foldlWithKey'
    ( \acc key addrs ->
        Map.insertWith Set.union (touchKey key) addrs acc
    )
    Map.empty
{-# INLINE touchIndexFromIntMap #-}
buildAtomIndex ::
  (Ord ctx, Ord prop) =>
  SubscriptionBuildInput ctx prop boundary ->
  IntMap.IntMap [(QueryId, AtomId)] ->
  Either
    (CarrierSubscriptionBuildError ctx prop)
    (IntMap.IntMap (Set (CarrierAddr ctx Carrier prop)))
buildAtomIndex input =
  IntMap.traverseWithKey (buildAtomSubscribersForKey input)
{-# INLINE buildAtomIndex #-}
buildAtomSubscribersForKey ::
  (Ord ctx, Ord prop) =>
  SubscriptionBuildInput ctx prop boundary ->
  Int ->
  [(QueryId, AtomId)] ->
  Either
    (CarrierSubscriptionBuildError ctx prop)
    (Set (CarrierAddr ctx Carrier prop))
buildAtomSubscribersForKey input _atomKey subscribers =
  Set.unions
    <$> traverse
      (uncurry (carriersForSubscribedAtom input))
      subscribers
{-# INLINE buildAtomSubscribersForKey #-}
carriersForSubscribedAtom ::
  (Ord ctx, Ord prop) =>
  SubscriptionBuildInput ctx prop boundary ->
  QueryId ->
  AtomId ->
  Either
    (CarrierSubscriptionBuildError ctx prop)
    (Set (CarrierAddr ctx Carrier prop))
carriersForSubscribedAtom input queryId atomId = do
  atomCarrier <-
    requireAtomCarrier input queryId atomId
  decomp <-
    case Map.lookup queryId (sbiQueryDecompositions input) of
      Nothing ->
        Left (CarrierSubscriptionMissingDecomp queryId)
      Just decompValue ->
        Right decompValue
  ownerBag <-
    case IntMap.lookup (atomIdKey atomId) (dpAtomOwner decomp) of
      Nothing ->
        Left (CarrierSubscriptionAtomOwnerMissing queryId atomId)
      Just bagId ->
        Right bagId
  bagCarrier <-
    requireBagCarrier input queryId ownerBag
  rootCarrier <-
    requireRootCarrier input queryId
  separatorCarriers <-
    traverse
      (uncurry (requireSeparatorCarrier input queryId))
      (upwardMessageCone decomp ownerBag)
  pure $
    Set.fromList
      ( atomCarrier
          : bagCarrier
          : rootCarrier
          : separatorCarriers
      )
{-# INLINE carriersForSubscribedAtom #-}
upwardMessageCone ::
  DecompPlan ->
  BagId ->
  [(BagId, BagId)]
upwardMessageCone decomp =
  go []
  where
    go acc child =
      case IntMap.lookup (unBagId child) (dpParent decomp) of
        Nothing ->
          reverse acc
        Just parent ->
          go ((child, parent) : acc) parent
{-# INLINE upwardMessageCone #-}
requireAtomCarrier ::
  SubscriptionBuildInput ctx prop boundary ->
  QueryId ->
  AtomId ->
  Either
    (CarrierSubscriptionBuildError ctx prop)
    (CarrierAddr ctx Carrier prop)
requireAtomCarrier input queryId atomId =
  case caaAtom (sbiAddressing input) queryId atomId of
    Nothing ->
      Left (CarrierSubscriptionMissingAtomCarrier queryId atomId)
    Just addr ->
      Right addr
{-# INLINE requireAtomCarrier #-}
requireBagCarrier ::
  SubscriptionBuildInput ctx prop boundary ->
  QueryId ->
  BagId ->
  Either
    (CarrierSubscriptionBuildError ctx prop)
    (CarrierAddr ctx Carrier prop)
requireBagCarrier input queryId bagId =
  case caaBag (sbiAddressing input) queryId bagId of
    Nothing ->
      Left (CarrierSubscriptionMissingBagCarrier queryId bagId)
    Just addr ->
      Right addr
{-# INLINE requireBagCarrier #-}
requireSeparatorCarrier ::
  SubscriptionBuildInput ctx prop boundary ->
  QueryId ->
  BagId ->
  BagId ->
  Either
    (CarrierSubscriptionBuildError ctx prop)
    (CarrierAddr ctx Carrier prop)
requireSeparatorCarrier input queryId child parent =
  case caaSeparator (sbiAddressing input) queryId child parent of
    Nothing ->
      Left (CarrierSubscriptionMissingSeparatorCarrier queryId child parent)
    Just addr ->
      Right addr
{-# INLINE requireSeparatorCarrier #-}
requireRootCarrier ::
  SubscriptionBuildInput ctx prop boundary ->
  QueryId ->
  Either
    (CarrierSubscriptionBuildError ctx prop)
    (CarrierAddr ctx Carrier prop)
requireRootCarrier input queryId =
  case caaRoot (sbiAddressing input) queryId of
    Nothing ->
      Left (CarrierSubscriptionMissingRootCarrier queryId)
    Just addr ->
      Right addr
{-# INLINE requireRootCarrier #-}
invertIntSetMap ::
  Ord value =>
  Map value IntSet ->
  IntMap.IntMap (Set value)
invertIntSetMap =
  Map.foldlWithKey'
    ( \acc value keys ->
        IntSet.foldl'
          ( \inner key ->
              IntMap.insertWith
                Set.union
                key
                (Set.singleton value)
                inner
          )
          acc
          keys
    )
    IntMap.empty
{-# INLINE invertIntSetMap #-}
