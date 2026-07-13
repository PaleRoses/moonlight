{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Presheaf.Congruence
  ( PreparedCongruenceSiteModel,
    CongruenceFinitePresheaf,
    CongruencePresheafRestrictionFailure (..),
    CongruencePresheafBuildFailure (..),
    prepareCongruenceSiteModelWith,
    finiteCongruencePresheafFromRelations,
    finiteCongruencePresheafFromStalks,
  )
where

import Control.Monad (join)
import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core (DenseKey)
import Moonlight.Sheaf.Presheaf.Finite
  ( FinitePresheaf,
    FinitePresheafFailure,
    mkFinitePresheaf,
  )
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionKind (..),
  )
import Moonlight.Sheaf.Section.Stalk
  ( normalizeStalk,
    restrictStalk,
    stalkMismatches,
  )
import Moonlight.Sheaf.Section.Stalk.Congruence.Carrier
import Moonlight.Sheaf.Section.Stalk.Congruence.Mismatch
import Moonlight.Sheaf.Section.Stalk.Congruence.Model
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
  )

type PreparedCongruenceSiteModel :: Type -> Type -> Type -> Type -> Type
data PreparedCongruenceSiteModel carrier site rep atom = PreparedCongruenceSiteModel
  { pcsmSite :: !site,
    pcsmCarrier :: !(GlobalCarrier rep atom),
    pcsmPreparedModel :: !(PreparedCongruenceModel carrier (SiteObject site) rep atom),
    pcsmRestrictionsByMorphism :: !(Map (CheckedMorphism (SiteObject site) (SiteMorphism site)) (PreparedCongruenceRestriction carrier rep atom))
  }

type CongruenceFinitePresheaf :: Type -> Type -> Type -> Type -> Type
type CongruenceFinitePresheaf site carrier rep atom =
  FinitePresheaf
    site
    (PreparedCongruenceStalk carrier rep atom)
    (CongruenceMismatch rep atom)
    (CongruencePresheafRestrictionFailure (SiteObject site) (SiteMorphism site))

type CongruencePresheafRestrictionFailure :: Type -> Type -> Type
data CongruencePresheafRestrictionFailure obj mor
  = CongruencePresheafRestrictionMissing !(CheckedMorphism obj mor)
  deriving stock (Eq, Show)

type CongruencePresheafBuildFailure :: Type -> Type -> Type -> Type -> Type -> Type
data CongruencePresheafBuildFailure obj mor carrier rep atom
  = CongruencePresheafCarrierMismatch
      !CarrierId
      !CarrierId
      ![(rep, atom)]
      ![(rep, atom)]
  | CongruencePresheafUnknownCell !obj
  | CongruencePresheafVisibleMismatch !obj !IntSet !IntSet
  | CongruencePresheafStalkInvalid !(PreparedCongruenceBuildError obj atom)
  | CongruencePresheafFiniteInvalid
      !( FinitePresheafFailure
           obj
           mor
           (PreparedCongruenceStalk carrier rep atom)
           (CongruenceMismatch rep atom)
           (CongruencePresheafRestrictionFailure obj mor)
       )
  deriving stock (Eq, Show)

prepareCongruenceSiteModelWith ::
  (Site site, Ord (SiteMorphism site), DenseKey rep) =>
  site ->
  GlobalCarrier rep atom ->
  Map (SiteObject site) [rep] ->
  (CheckedMorphism (SiteObject site) (SiteMorphism site) -> IntMap rep) ->
  (forall carrier. PreparedCongruenceSiteModel carrier site rep atom -> result) ->
  Either (PreparedCongruenceBuildError (SiteObject site) atom) result
prepareCongruenceSiteModelWith site carrier visibleSupport restrictionCarrierMapAt continue =
  join $
    prepareCongruenceModelWith
      carrier
      (siteObjects site)
      visibleSupport
      (fmap snd specsByMorphism)
      (\preparedModelValue ->
        fmap continue $
          fmap (PreparedCongruenceSiteModel site carrier preparedModelValue . Map.fromList) $
            traverse
              (\(morphism, spec) ->
                fmap (\restriction -> (morphism, restriction)) $
                  preparedCongruenceRestrictionForSpec preparedModelValue spec
              )
              specsByMorphism
      )
  where
    specsByMorphism =
      fmap
        (\morphism -> (morphism, restrictionSpecFor morphism))
        (siteMorphisms site)

    restrictionSpecFor morphism =
      PreparedCongruenceRestrictionSpec
        { pcrsKind = PortalRestriction,
          pcrsSource = cmTarget morphism,
          pcrsTarget = cmSource morphism,
          pcrsCarrierMap = restrictionCarrierMapAt morphism
        }
{-# INLINEABLE prepareCongruenceSiteModelWith #-}

finiteCongruencePresheafFromRelations ::
  (Site site, Ord (SiteMorphism site), DenseKey rep) =>
  PreparedCongruenceSiteModel carrier site rep atom ->
  Map (SiteObject site) [EquivalenceRelation rep] ->
  Either
    (CongruencePresheafBuildFailure (SiteObject site) (SiteMorphism site) carrier rep atom)
    (CongruenceFinitePresheaf site carrier rep atom)
finiteCongruencePresheafFromRelations model relationFibers = do
  stalkFibers <-
    first CongruencePresheafStalkInvalid $
      Map.traverseWithKey
        (traverse . mkPreparedCongruenceStalkFromRelationAt (pcsmPreparedModel model))
        relationFibers
  first CongruencePresheafFiniteInvalid $
    mkFinitePresheaf
      (pcsmSite model)
      (restrictPreparedAlong model)
      (\_object -> stalkMismatches preparedCongruenceStalkAlgebra)
      (\_object -> normalizeStalk preparedCongruenceStalkAlgebra)
      stalkFibers
{-# INLINEABLE finiteCongruencePresheafFromRelations #-}

finiteCongruencePresheafFromStalks ::
  (Site site, Ord (SiteMorphism site), DenseKey rep, Eq atom) =>
  PreparedCongruenceSiteModel carrier site rep atom ->
  Map (SiteObject site) [CongruenceStalk rep atom] ->
  Either
    (CongruencePresheafBuildFailure (SiteObject site) (SiteMorphism site) carrier rep atom)
    (CongruenceFinitePresheaf site carrier rep atom)
finiteCongruencePresheafFromStalks model stalkFibers =
  Map.traverseWithKey (traverse . relationAt) stalkFibers
    >>= finiteCongruencePresheafFromRelations model
  where
    relationAt objectValue stalk =
      validateCarrier stalk
        *> validateVisible objectValue stalk
        *> pure (congruenceStalkRelation stalk)

    validateCarrier stalk
      | sameCarrier (pcsmCarrier model) (congruenceStalkCarrier stalk) =
          Right ()
      | otherwise =
          Left $
            CongruencePresheafCarrierMismatch
              (globalCarrierId (pcsmCarrier model))
              (globalCarrierId (congruenceStalkCarrier stalk))
              (carrierIndexedValues (pcsmCarrier model))
              (carrierIndexedValues (congruenceStalkCarrier stalk))

    validateVisible objectValue stalk =
      case preparedCongruenceVisibleAt objectValue (pcsmPreparedModel model) of
        Nothing ->
          Left (CongruencePresheafUnknownCell objectValue)
        Just expectedVisible
          | expectedVisible == congruenceStalkVisible stalk ->
              Right ()
          | otherwise ->
              Left (CongruencePresheafVisibleMismatch objectValue expectedVisible (congruenceStalkVisible stalk))
{-# INLINEABLE finiteCongruencePresheafFromStalks #-}

restrictPreparedAlong ::
  (Site site, Ord (SiteMorphism site), DenseKey rep) =>
  PreparedCongruenceSiteModel carrier site rep atom ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  PreparedCongruenceStalk carrier rep atom ->
  Either
    (CongruencePresheafRestrictionFailure (SiteObject site) (SiteMorphism site))
    (PreparedCongruenceStalk carrier rep atom)
restrictPreparedAlong model morphism stalk =
  maybe
    (Left (CongruencePresheafRestrictionMissing morphism))
    (\restriction -> Right (restrictStalk preparedCongruenceStalkAlgebra restriction stalk))
    (Map.lookup morphism (pcsmRestrictionsByMorphism model))
{-# INLINEABLE restrictPreparedAlong #-}
