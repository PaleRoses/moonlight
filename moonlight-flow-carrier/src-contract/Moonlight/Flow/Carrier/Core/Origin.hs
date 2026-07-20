{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Core.Origin
  ( OriginEvent (..),
    DerivationRoute (..),
    RelationalOrigin (..),
    emptyDerivationRoute,
    singletonParentRoute,
    originHasRestriction,
    originConsRestriction,
    originAddParent,
    originMerge,
  )
where

import Data.List.NonEmpty
  ( NonEmpty,
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set

import Moonlight.Flow.Carrier.Core.Address
  ( SubsumptionWitnessDigest,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    RestrictKey,
  )
import Moonlight.Flow.Plan.Query.Core

data OriginEvent
  = OriginLocal !QueryId
  | OriginAtom !QueryId !AtomId
  | OriginFactor !QueryId !FactorNode
  | OriginSubsumed !SubsumptionWitnessDigest
  | OriginAmalgamated
  | OriginCompacted
  | OriginRestricted
  deriving stock (Eq, Ord, Show)

data DerivationRoute ctx carrier prop = DerivationRoute
  { drParents :: !(Set (CarrierAddr ctx carrier prop)),
    drRestrictions :: !(Set (RestrictKey ctx carrier prop))
  }
  deriving stock (Eq, Ord, Show)

data RelationalOrigin ctx carrier prop = RelationalOrigin
  { roEvent :: !OriginEvent,
    roRoute :: !(DerivationRoute ctx carrier prop)
  }
  deriving stock (Eq, Ord, Show)

emptyDerivationRoute ::
  DerivationRoute ctx carrier prop
emptyDerivationRoute =
  DerivationRoute
    { drParents = Set.empty,
      drRestrictions = Set.empty
    }
{-# INLINE emptyDerivationRoute #-}

singletonParentRoute ::
  CarrierAddr ctx carrier prop ->
  DerivationRoute ctx carrier prop
singletonParentRoute parent =
  emptyDerivationRoute
    { drParents = Set.singleton parent
    }
{-# INLINE singletonParentRoute #-}

originHasRestriction ::
  (Ord ctx, Ord carrier, Ord prop) =>
  RestrictKey ctx carrier prop ->
  RelationalOrigin ctx carrier prop ->
  Bool
originHasRestriction restrictKey =
  Set.member restrictKey . drRestrictions . roRoute
{-# INLINE originHasRestriction #-}

originConsRestriction ::
  (Ord ctx, Ord carrier, Ord prop) =>
  RestrictKey ctx carrier prop ->
  RelationalOrigin ctx carrier prop ->
  RelationalOrigin ctx carrier prop
originConsRestriction restrictKey origin =
  origin
    { roEvent = OriginRestricted,
      roRoute =
        (roRoute origin)
          { drRestrictions =
              Set.insert restrictKey (drRestrictions (roRoute origin))
          }
    }
{-# INLINE originConsRestriction #-}

originAddParent ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  RelationalOrigin ctx carrier prop ->
  RelationalOrigin ctx carrier prop
originAddParent parent origin =
  origin
    { roRoute =
        (roRoute origin)
          { drParents =
              Set.insert parent (drParents (roRoute origin))
          }
    }
{-# INLINE originAddParent #-}

originMerge ::
  (Ord ctx, Ord carrier, Ord prop) =>
  OriginEvent ->
  NonEmpty (RelationalOrigin ctx carrier prop) ->
  RelationalOrigin ctx carrier prop
originMerge event origins =
  RelationalOrigin
    { roEvent = event,
      roRoute =
        DerivationRoute
          { drParents =
              foldMap (drParents . roRoute) origins,
            drRestrictions =
              foldMap (drRestrictions . roRoute) origins
          }
    }
{-# INLINE originMerge #-}
