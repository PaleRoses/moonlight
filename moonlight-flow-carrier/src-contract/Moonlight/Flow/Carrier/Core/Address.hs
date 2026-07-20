{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    QueryCarrierNode (..),
    DerivedCarrierId (..),
    SubsumptionWitnessDigest (..),
    CarrierAddressBook (..),
    queryCarrier,
    queryRootCarrier,
    queryAtomCarrier,
    queryFactorCarrier,
    queryBagCarrier,
    queryBagBeliefCarrier,
    querySeparatorCarrier,
    derivedCarrier,
    queryCarrierAddr,
    queryRootAddr,
    queryAtomAddr,
    queryFactorAddr,
  )
where

import Data.Kind (Type)
import Moonlight.Core
  ( AtomId,
    QueryId,
  )
import Moonlight.Flow.Plan.Query.Core
  ( BagId,
    FactorNode (..),
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    CarrierProp,
    carrierAddr,
  )

type CarrierAddressBook :: Type -> Type -> Type
data CarrierAddressBook ctx prop = CarrierAddressBook
  { cabContextOfQuery :: QueryId -> ctx,
    cabPropOfQuery :: QueryId -> CarrierProp prop
  }

type SubsumptionWitnessDigest :: Type
newtype SubsumptionWitnessDigest = SubsumptionWitnessDigest
  { unSubsumptionWitnessDigest :: StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

type QueryCarrierNode :: Type
data QueryCarrierNode
  = QueryAtom !AtomId
  | QueryFactor !FactorNode
  deriving stock (Eq, Ord, Show, Read)

type DerivedCarrierId :: Type
data DerivedCarrierId = DerivedCarrierId
  { dciWitness :: !SubsumptionWitnessDigest,
    dciShape :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

type Carrier :: Type
data Carrier
  = QueryCarrier !QueryId !QueryCarrierNode
  | DerivedCarrier !DerivedCarrierId
  deriving stock (Eq, Ord, Show, Read)

queryCarrier ::
  QueryId ->
  QueryCarrierNode ->
  Carrier
queryCarrier =
  QueryCarrier
{-# INLINE queryCarrier #-}

queryRootCarrier :: QueryId -> Carrier
queryRootCarrier queryId =
  queryCarrier queryId (QueryFactor FactorNodeRoot)
{-# INLINE queryRootCarrier #-}

queryAtomCarrier :: QueryId -> AtomId -> Carrier
queryAtomCarrier queryId atomId =
  queryCarrier queryId (QueryAtom atomId)
{-# INLINE queryAtomCarrier #-}

queryFactorCarrier :: QueryId -> FactorNode -> Carrier
queryFactorCarrier queryId node =
  queryCarrier queryId (QueryFactor node)
{-# INLINE queryFactorCarrier #-}

queryBagCarrier :: QueryId -> BagId -> Carrier
queryBagCarrier queryId bagId =
  queryFactorCarrier queryId (FactorNodeBag bagId)
{-# INLINE queryBagCarrier #-}

queryBagBeliefCarrier :: QueryId -> BagId -> Carrier
queryBagBeliefCarrier queryId bagId =
  queryFactorCarrier queryId (FactorNodeBagBelief bagId)
{-# INLINE queryBagBeliefCarrier #-}

querySeparatorCarrier :: QueryId -> BagId -> BagId -> Carrier
querySeparatorCarrier queryId child parent =
  queryFactorCarrier queryId (FactorNodeSeparator child parent)
{-# INLINE querySeparatorCarrier #-}

derivedCarrier :: DerivedCarrierId -> Carrier
derivedCarrier =
  DerivedCarrier
{-# INLINE derivedCarrier #-}

queryCarrierAddr ::
  CarrierAddressBook ctx prop ->
  QueryId ->
  QueryCarrierNode ->
  CarrierAddr ctx Carrier prop
queryCarrierAddr book queryId node =
  carrierAddr
    (cabContextOfQuery book queryId)
    (cabPropOfQuery book queryId)
    (queryCarrier queryId node)
{-# INLINE queryCarrierAddr #-}

queryRootAddr ::
  CarrierAddressBook ctx prop ->
  QueryId ->
  CarrierAddr ctx Carrier prop
queryRootAddr book queryId =
  carrierAddr
    (cabContextOfQuery book queryId)
    (cabPropOfQuery book queryId)
    (queryRootCarrier queryId)
{-# INLINE queryRootAddr #-}

queryAtomAddr ::
  CarrierAddressBook ctx prop ->
  QueryId ->
  AtomId ->
  CarrierAddr ctx Carrier prop
queryAtomAddr book queryId atomId =
  carrierAddr
    (cabContextOfQuery book queryId)
    (cabPropOfQuery book queryId)
    (queryAtomCarrier queryId atomId)
{-# INLINE queryAtomAddr #-}

queryFactorAddr ::
  CarrierAddressBook ctx prop ->
  QueryId ->
  FactorNode ->
  CarrierAddr ctx Carrier prop
queryFactorAddr book queryId node =
  carrierAddr
    (cabContextOfQuery book queryId)
    (cabPropOfQuery book queryId)
    (queryFactorCarrier queryId node)
{-# INLINE queryFactorAddr #-}
