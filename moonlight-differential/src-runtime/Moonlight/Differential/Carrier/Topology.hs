{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Carrier.Topology
  ( CarrierCover,
    carrierCover,
    carrierCoverTarget,
    carrierCoverMembers,
    carrierCoverComplete,
    carrierCoverSupport,
    CarrierFamily,
    CarrierFamilyError (..),
    mkCarrierFamily,
    carrierFamilyTarget,
    carrierFamilyCover,
    carrierFamilyMembers,
    carrierFamilyTargets,
    carrierFamilyTargetContext,
    carrierFamilyProp,
    carrierFamilyCarrier,
  )
where

import Control.Monad
  ( unless,
  )
import Data.Foldable
  ( traverse_,
  )
import Data.Kind
  ( Type,
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    CarrierProp,
    caCarrier,
    caContext,
    caProp,
  )
import Moonlight.FiniteLattice
  ( SupportBasis
  )

-- | A context cover before it is attached to a concrete carrier address.
-- The smart constructor is total because the family constructor owns the
-- cross-field compatibility checks against addresses; 'ccSupport' is
-- caller-owned annotation whose lattice-coherence belongs to the layer that
-- owns the 'ContextLattice'.
type CarrierCover :: Type -> Type
data CarrierCover ctx = CarrierCover
  { ccTarget :: !ctx,
    ccMembers :: !(Set ctx),
    ccComplete :: !Bool,
    ccSupport :: !(SupportBasis ctx)
  }
  deriving stock (Eq, Ord, Show)

carrierCover :: ctx -> Set ctx -> Bool -> SupportBasis ctx -> CarrierCover ctx
carrierCover target members complete support =
  CarrierCover
    { ccTarget = target,
      ccMembers = members,
      ccComplete = complete,
      ccSupport = support
    }
{-# INLINE carrierCover #-}

carrierCoverTarget :: CarrierCover ctx -> ctx
carrierCoverTarget =
  ccTarget
{-# INLINE carrierCoverTarget #-}

carrierCoverMembers :: CarrierCover ctx -> Set ctx
carrierCoverMembers =
  ccMembers
{-# INLINE carrierCoverMembers #-}

carrierCoverComplete :: CarrierCover ctx -> Bool
carrierCoverComplete =
  ccComplete
{-# INLINE carrierCoverComplete #-}

carrierCoverSupport :: CarrierCover ctx -> SupportBasis ctx
carrierCoverSupport =
  ccSupport
{-# INLINE carrierCoverSupport #-}

type CarrierFamily :: Type -> Type -> Type -> Type
data CarrierFamily ctx carrier prop = CarrierFamily
  { carrierFamilyRecordTarget :: !(CarrierAddr ctx carrier prop),
    carrierFamilyRecordCover :: !(CarrierCover ctx),
    carrierFamilyRecordMembers :: !(Set (CarrierAddr ctx carrier prop))
  }
  deriving stock (Eq, Ord, Show)

type CarrierFamilyError :: Type -> Type -> Type -> Type
data CarrierFamilyError ctx carrier prop
  = CarrierFamilyTargetContextMismatch !(CarrierAddr ctx carrier prop) !ctx
  | CarrierFamilyEmptyMembers !(CarrierAddr ctx carrier prop)
  | CarrierFamilyMemberPropMismatch !(CarrierAddr ctx carrier prop) !(CarrierAddr ctx carrier prop)
  | CarrierFamilyMemberCarrierMismatch !(CarrierAddr ctx carrier prop) !(CarrierAddr ctx carrier prop)
  | CarrierFamilyMemberContextOutsideCover !(CarrierAddr ctx carrier prop) !ctx
  | CarrierFamilyIncompleteCover !(CarrierAddr ctx carrier prop) !(Set ctx) !(Set ctx)
  deriving stock (Eq, Ord, Show)

mkCarrierFamily ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierCover ctx ->
  Set (CarrierAddr ctx carrier prop) ->
  Either (CarrierFamilyError ctx carrier prop) (CarrierFamily ctx carrier prop)
mkCarrierFamily target cover members = do
  unless (carrierCoverTarget cover == caContext target) $
    Left (CarrierFamilyTargetContextMismatch target (carrierCoverTarget cover))
  unless (not (Set.null members)) $
    Left (CarrierFamilyEmptyMembers target)
  traverse_ validateMember (Set.toAscList members)
  unless (not (carrierCoverComplete cover) || memberContexts == carrierCoverMembers cover) $
    Left (CarrierFamilyIncompleteCover target (carrierCoverMembers cover) memberContexts)
  pure
    CarrierFamily
      { carrierFamilyRecordTarget = target,
        carrierFamilyRecordCover = cover,
        carrierFamilyRecordMembers = members
      }
  where
    memberContexts =
      Set.map caContext members

    validateMember member = do
      unless (caProp member == caProp target) $
        Left (CarrierFamilyMemberPropMismatch target member)
      unless (caCarrier member == caCarrier target) $
        Left (CarrierFamilyMemberCarrierMismatch target member)
      unless (Set.member (caContext member) (carrierCoverMembers cover)) $
        Left (CarrierFamilyMemberContextOutsideCover member (caContext member))
{-# INLINE mkCarrierFamily #-}

carrierFamilyTarget :: CarrierFamily ctx carrier prop -> CarrierAddr ctx carrier prop
carrierFamilyTarget =
  carrierFamilyRecordTarget
{-# INLINE carrierFamilyTarget #-}

carrierFamilyCover :: CarrierFamily ctx carrier prop -> CarrierCover ctx
carrierFamilyCover =
  carrierFamilyRecordCover
{-# INLINE carrierFamilyCover #-}

carrierFamilyMembers :: CarrierFamily ctx carrier prop -> Set (CarrierAddr ctx carrier prop)
carrierFamilyMembers =
  carrierFamilyRecordMembers
{-# INLINE carrierFamilyMembers #-}

carrierFamilyTargets ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierFamily ctx carrier prop ->
  Set (CarrierAddr ctx carrier prop)
carrierFamilyTargets family =
  Set.map
    (\addr -> addr {caContext = carrierCoverTarget (carrierFamilyCover family)})
    (carrierFamilyMembers family)
{-# INLINE carrierFamilyTargets #-}

carrierFamilyTargetContext :: CarrierFamily ctx carrier prop -> ctx
carrierFamilyTargetContext =
  caContext . carrierFamilyTarget
{-# INLINE carrierFamilyTargetContext #-}

carrierFamilyProp :: CarrierFamily ctx carrier prop -> CarrierProp prop
carrierFamilyProp =
  caProp . carrierFamilyTarget
{-# INLINE carrierFamilyProp #-}

carrierFamilyCarrier :: CarrierFamily ctx carrier prop -> carrier
carrierFamilyCarrier =
  caCarrier . carrierFamilyTarget
{-# INLINE carrierFamilyCarrier #-}
