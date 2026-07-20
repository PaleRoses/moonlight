{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Store.Core.Read
  ( CarrierReadFrontier (..),
    CarrierReadCapability (..),
    carrierReadCapabilityFromFrontier,
    CarrierArrangedDelta (..),
    CarrierHeldReads (..),
    emptyCarrierHeldReads,
    insertCarrierHeldRead,
    carrierHeldReadsFromList,
    carrierReadFrontierFromTime,
    carrierReadFrontierCompatible,
    carrierReadCapabilityFromTime,
    carrierReadCapabilityFrontier,
    carrierReadCapabilityBoundaryDigest,
    downgradeCarrierReadCapability,
  )
where

import Data.Foldable qualified as Foldable
import Data.IntSet
  ( IntSet,
  )
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
import Moonlight.Core
  ( LiveEpoch,
    QuotientEpoch,
  )
import Moonlight.Differential.Time
  ( FrontierStamp,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    relationalTimeFrontierStamp,
    relationalTimeLiveEpoch,
    relationalTimeQuotientEpoch,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )

type CarrierReadFrontier :: Type
data CarrierReadFrontier = CarrierReadFrontier
  { crfQuotientEpoch :: !QuotientEpoch,
    crfLiveEpoch :: !LiveEpoch,
    crfFrontierStamp :: !FrontierStamp
  }
  deriving stock (Eq, Ord, Show)

type CarrierReadCapability :: Type -> Type -> Type -> Type
data CarrierReadCapability ctx carrier prop = CarrierReadCapability
  { crcAddr :: !(CarrierAddr ctx carrier prop),
    crcFrontier :: !CarrierReadFrontier,
    crcBoundaryDigest :: !StableDigest128
  }
  deriving stock (Eq, Show)

type CarrierArrangedDelta :: Type
data CarrierArrangedDelta = CarrierArrangedDelta
  { cadRows :: !RowDelta,
    cadTraceIds :: !IntSet
  }
  deriving stock (Eq, Show)

type CarrierHeldReads :: Type -> Type -> Type -> Type
newtype CarrierHeldReads ctx carrier prop = CarrierHeldReads
  { chrReadsByAddr :: Map (CarrierAddr ctx carrier prop) (Set CarrierReadFrontier)
  }
  deriving stock (Eq, Show)

emptyCarrierHeldReads :: CarrierHeldReads ctx carrier prop
emptyCarrierHeldReads =
  CarrierHeldReads
    { chrReadsByAddr = Map.empty
    }
{-# INLINE emptyCarrierHeldReads #-}

insertCarrierHeldRead ::
  Ord (CarrierAddr ctx carrier prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierReadFrontier ->
  CarrierHeldReads ctx carrier prop ->
  CarrierHeldReads ctx carrier prop
insertCarrierHeldRead addr frontier heldReads =
  heldReads
    { chrReadsByAddr =
        Map.insertWith
          Set.union
          addr
          (Set.singleton frontier)
          (chrReadsByAddr heldReads)
    }
{-# INLINE insertCarrierHeldRead #-}

carrierHeldReadsFromList ::
  Ord (CarrierAddr ctx carrier prop) =>
  [(CarrierAddr ctx carrier prop, CarrierReadFrontier)] ->
  CarrierHeldReads ctx carrier prop
carrierHeldReadsFromList =
  Foldable.foldl'
    ( \heldReads (addr, frontier) ->
        insertCarrierHeldRead addr frontier heldReads
    )
    emptyCarrierHeldReads
{-# INLINE carrierHeldReadsFromList #-}

carrierReadFrontierFromTime ::
  RelationalCarrierTime ctx ->
  CarrierReadFrontier
carrierReadFrontierFromTime timeValue =
  CarrierReadFrontier
    { crfQuotientEpoch = relationalTimeQuotientEpoch timeValue,
      crfLiveEpoch = relationalTimeLiveEpoch timeValue,
      crfFrontierStamp = relationalTimeFrontierStamp timeValue
    }
{-# INLINE carrierReadFrontierFromTime #-}

carrierReadFrontierCompatible ::
  CarrierReadFrontier ->
  CarrierReadFrontier ->
  Bool
carrierReadFrontierCompatible held current =
  crfQuotientEpoch held == crfQuotientEpoch current
    && crfLiveEpoch held == crfLiveEpoch current
    && crfFrontierStamp held <= crfFrontierStamp current
{-# INLINE carrierReadFrontierCompatible #-}

carrierReadCapabilityFromTime ::
  CarrierAddr ctx carrier prop ->
  StableDigest128 ->
  RelationalCarrierTime ctx ->
  CarrierReadCapability ctx carrier prop
carrierReadCapabilityFromTime addr boundaryDigestValue timeValue =
  carrierReadCapabilityFromFrontier
    addr
    boundaryDigestValue
    (carrierReadFrontierFromTime timeValue)
{-# INLINE carrierReadCapabilityFromTime #-}

carrierReadCapabilityFromFrontier ::
  CarrierAddr ctx carrier prop ->
  StableDigest128 ->
  CarrierReadFrontier ->
  CarrierReadCapability ctx carrier prop
carrierReadCapabilityFromFrontier addr boundaryDigestValue frontier =
  CarrierReadCapability
    { crcAddr = addr,
      crcFrontier = frontier,
      crcBoundaryDigest = boundaryDigestValue
    }
{-# INLINE carrierReadCapabilityFromFrontier #-}

carrierReadCapabilityFrontier ::
  CarrierReadCapability ctx carrier prop ->
  CarrierReadFrontier
carrierReadCapabilityFrontier =
  crcFrontier
{-# INLINE carrierReadCapabilityFrontier #-}

carrierReadCapabilityBoundaryDigest ::
  CarrierReadCapability ctx carrier prop ->
  StableDigest128
carrierReadCapabilityBoundaryDigest =
  crcBoundaryDigest
{-# INLINE carrierReadCapabilityBoundaryDigest #-}

downgradeCarrierReadCapability ::
  CarrierReadFrontier ->
  CarrierReadCapability ctx carrier prop ->
  Maybe (CarrierReadCapability ctx carrier prop)
downgradeCarrierReadCapability nextFrontier capability
  | carrierReadFrontierCompatible (crcFrontier capability) nextFrontier =
      Just capability {crcFrontier = nextFrontier}
  | otherwise =
      Nothing
{-# INLINE downgradeCarrierReadCapability #-}
