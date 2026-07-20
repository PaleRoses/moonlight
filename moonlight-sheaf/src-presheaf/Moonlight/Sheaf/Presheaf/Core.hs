{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Presheaves as compiled restriction actions, with their law-failure
-- vocabulary.
module Moonlight.Sheaf.Presheaf.Core
  ( Presheaf (..),
    CompiledRestriction (..),
    PresheafLawFailure (..),
    compileRestrictionIndexFromPresheaf,
    compileAllSiteRestrictions,
    checkIdentityLawWith,
    checkCompositionLawWith,
    checkIdentityLaw,
    checkCompositionLaw,
  )
where

import Data.Kind (Constraint, Type)
import Moonlight.Sheaf.Kernel.Basis
  ( mkSheafBasis,
  )
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionKind,
    RestrictionParts (..),
  )
import Moonlight.Sheaf.Kernel.Basis (basisCells)
import Moonlight.Sheaf.Section.ObjectIndex
  ( mkObjectIndex,
  )
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    RestrictionIndexError,
    buildRestrictionIndex,
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra,
    stalkMismatches,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
    isIdentityMorphism,
  )

type Presheaf :: Type -> Type -> Constraint
class Site site => Presheaf site stalk where
  restrictAlong ::
    site ->
    CheckedMorphism (SiteObject site) (SiteMorphism site) ->
    stalk ->
    stalk

type CompiledRestriction :: Type -> Type
data CompiledRestriction site = CompiledRestriction
  { crSite :: site,
    crMorphism :: !(CheckedMorphism (SiteObject site) (SiteMorphism site))
  }

deriving stock instance
  (Eq site, Eq (SiteObject site), Eq (SiteMorphism site)) =>
  Eq (CompiledRestriction site)

deriving stock instance
  (Show site, Show (SiteObject site), Show (SiteMorphism site)) =>
  Show (CompiledRestriction site)

type PresheafLawFailure :: Type -> Type -> Type -> Type
data PresheafLawFailure obj mor mismatch
  = IdentityRestrictionMismatch !(CheckedMorphism obj mor) ![mismatch]
  | CompositionUndefined !(CheckedMorphism obj mor) !(CheckedMorphism obj mor)
  | CompositionRestrictionMismatch
      !(CheckedMorphism obj mor)
      !(CheckedMorphism obj mor)
      !(CheckedMorphism obj mor)
      ![mismatch]
  deriving stock (Eq, Show)

compileRestrictionIndexFromPresheaf ::
  (Site site, Eq (SiteMorphism site)) =>
  site ->
  (CheckedMorphism (SiteObject site) (SiteMorphism site) -> RestrictionKind) ->
  [CheckedMorphism (SiteObject site) (SiteMorphism site)] ->
  Either
    (RestrictionIndexError (SiteObject site))
    (RestrictionIndex (SiteObject site) (CompiledRestriction site))
compileRestrictionIndexFromPresheaf site kindAt morphisms =
  buildRestrictionIndex
    (mkObjectIndex (basisCells (mkSheafBasis (siteObjects site))))
    ( \checkedMorphism ->
        RestrictionParts
          { partKind = kindAt checkedMorphism,
            partSource = cmSource checkedMorphism,
            partTarget = cmTarget checkedMorphism,
            partWitness = CompiledRestriction site checkedMorphism
          }
    )
    (filter (not . isIdentityMorphism site) morphisms)

compileAllSiteRestrictions ::
  (Site site, Eq (SiteMorphism site)) =>
  site ->
  (CheckedMorphism (SiteObject site) (SiteMorphism site) -> RestrictionKind) ->
  Either
    (RestrictionIndexError (SiteObject site))
    (RestrictionIndex (SiteObject site) (CompiledRestriction site))
compileAllSiteRestrictions site kindAt =
  compileRestrictionIndexFromPresheaf site kindAt (siteMorphisms site)

checkIdentityLaw ::
  Presheaf site stalk =>
  StalkAlgebra witness stalk mismatch repairObstruction ->
  site ->
  SiteObject site ->
  stalk ->
  Either
    (PresheafLawFailure (SiteObject site) (SiteMorphism site) mismatch)
    ()
checkIdentityLaw stalkAlgebra site =
  checkIdentityLawWith stalkAlgebra site (restrictAlong site)

checkIdentityLawWith ::
  Site site =>
  StalkAlgebra witness stalk mismatch repairObstruction ->
  site ->
  (CheckedMorphism (SiteObject site) (SiteMorphism site) -> stalk -> stalk) ->
  SiteObject site ->
  stalk ->
  Either
    (PresheafLawFailure (SiteObject site) (SiteMorphism site) mismatch)
    ()
checkIdentityLawWith stalkAlgebra site restrictAction objectValue stalkValue =
  let identityMorphismValue = identityAt site objectValue
      restrictedValue =
        restrictAction identityMorphismValue stalkValue
      mismatches =
        stalkMismatches stalkAlgebra restrictedValue stalkValue
   in if null mismatches
        then Right ()
        else Left (IdentityRestrictionMismatch identityMorphismValue mismatches)

checkCompositionLaw ::
  Presheaf site stalk =>
  StalkAlgebra witness stalk mismatch repairObstruction ->
  site ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  stalk ->
  Either
    (PresheafLawFailure (SiteObject site) (SiteMorphism site) mismatch)
    ()
checkCompositionLaw stalkAlgebra site =
  checkCompositionLawWith stalkAlgebra site (restrictAlong site)

checkCompositionLawWith ::
  Site site =>
  StalkAlgebra witness stalk mismatch repairObstruction ->
  site ->
  (CheckedMorphism (SiteObject site) (SiteMorphism site) -> stalk -> stalk) ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  stalk ->
  Either
    (PresheafLawFailure (SiteObject site) (SiteMorphism site) mismatch)
    ()
checkCompositionLawWith stalkAlgebra site restrictAction outerMorphism innerMorphism stalkValue =
  case composeChecked site outerMorphism innerMorphism of
    Nothing ->
      Left (CompositionUndefined outerMorphism innerMorphism)
    Just compositeMorphism ->
      let sequentialRestriction =
            restrictAction innerMorphism
              (restrictAction outerMorphism stalkValue)
          directRestriction =
            restrictAction compositeMorphism stalkValue
          mismatches =
            stalkMismatches stalkAlgebra sequentialRestriction directRestriction
       in if null mismatches
            then Right ()
            else
              Left
                ( CompositionRestrictionMismatch
                    outerMorphism
                    innerMorphism
                    compositeMorphism
                    mismatches
                )
