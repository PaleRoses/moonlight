{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Presheaf.Transport
  ( CoverSectionTransport (..),
    CoverSectionTransportFailure (..),
    pullCoverSectionsAlong,
    pullCoverSectionsAlongPlan,
  )
where

import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism,
    Site (..),
  )
import Moonlight.Sheaf.Site.CoverBasis.Finite
  ( FiniteCoverBasis,
    FiniteCoverBasisFailure,
    finitePullbackCoverPlan,
  )
import Moonlight.Sheaf.Site.Plan
  ( CoverSlotKey (..),
    EffectiveCoverPlan,
    PullbackCoverPlan,
    PullbackCoverSlotPlan,
    pcpPulledCover,
    pcpSlotPlans,
    pcspOriginalSlot,
    pcspPulledSlot,
    pcspRestrictToOriginal,
  )

type CoverSectionTransport :: Type -> Type -> Type -> Type
data CoverSectionTransport obj mor value = CoverSectionTransport
  { cstCoverPlan :: !(EffectiveCoverPlan obj mor),
    cstSections :: !(IntMap value)
  }
  deriving stock (Eq, Show)

type CoverSectionTransportFailure :: Type -> Type -> Type -> Type -> Type
data CoverSectionTransportFailure obj mor value restrictionFailure
  = CoverSectionTransportCoverUnavailable
      !(CheckedMorphism obj mor)
      !(EffectiveCoverPlan obj mor)
      !(FiniteCoverBasisFailure obj mor)
  | CoverSectionTransportSectionMissing !CoverSlotKey
  | CoverSectionTransportRestrictionFailed
      !(CheckedMorphism obj mor)
      !value
      !restrictionFailure
  deriving stock (Eq, Show)

pullCoverSectionsAlong ::
  (Site site, Ord (SiteMorphism site)) =>
  FiniteCoverBasis site ->
  ( CheckedMorphism (SiteObject site) (SiteMorphism site) ->
    value ->
    Either restrictionFailure value
  ) ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  EffectiveCoverPlan (SiteObject site) (SiteMorphism site) ->
  IntMap value ->
  Either
    ( CoverSectionTransportFailure
        (SiteObject site)
        (SiteMorphism site)
        value
        restrictionFailure
    )
    (CoverSectionTransport (SiteObject site) (SiteMorphism site) value)
pullCoverSectionsAlong basis restrictAction morphismValue coverPlan sectionsBySlot = do
  pullbackPlan <-
    first
      (CoverSectionTransportCoverUnavailable morphismValue coverPlan)
      (finitePullbackCoverPlan basis morphismValue coverPlan)
  pullCoverSectionsAlongPlan restrictAction pullbackPlan sectionsBySlot

pullCoverSectionsAlongPlan ::
  (CheckedMorphism obj mor -> value -> Either restrictionFailure value) ->
  PullbackCoverPlan obj mor ->
  IntMap value ->
  Either
    (CoverSectionTransportFailure obj mor value restrictionFailure)
    (CoverSectionTransport obj mor value)
pullCoverSectionsAlongPlan restrictAction pullbackPlan sectionsBySlot = do
  pulledSections <-
    IntMap.fromList
      <$> traverse
        restrictPulledSlot
        (IntMap.elems (pcpSlotPlans pullbackPlan))
  pure
    CoverSectionTransport
      { cstCoverPlan = pcpPulledCover pullbackPlan,
        cstSections = pulledSections
      }
  where
    restrictPulledSlot slotPlan = do
      originalSection <-
        maybe
          (Left (CoverSectionTransportSectionMissing (pcspOriginalSlot slotPlan)))
          Right
          (IntMap.lookup (unCoverSlotKey (pcspOriginalSlot slotPlan)) sectionsBySlot)
      restrictedSection <-
        first
          ( CoverSectionTransportRestrictionFailed
              (pcspRestrictToOriginal slotPlan)
              originalSection
          )
          (restrictAction (pcspRestrictToOriginal slotPlan) originalSection)
      pure (unCoverSlotKey (pcspPulledSlot slotPlan), restrictedSection)
