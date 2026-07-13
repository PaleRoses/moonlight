{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Core
  ( CosheafLawFailure (..),
    checkCorestrictionIdentityLawWith,
    checkCorestrictionCompositionDefined,
    checkCorestrictionCompositionLawWith,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
  )

type CosheafLawFailure :: Type -> Type -> Type -> Type -> Type -> Type
data CosheafLawFailure obj mor value mismatch coreFailure
  = IdentityCorestrictionFailed !(CheckedMorphism obj mor) !value !coreFailure
  | IdentityCorestrictionMismatch !(CheckedMorphism obj mor) !value ![mismatch]
  | CompositionCorestrictionUndefined !(CheckedMorphism obj mor) !(CheckedMorphism obj mor)
  | CompositionCorestrictionFailed !(CheckedMorphism obj mor) !value !coreFailure
  | CompositionCorestrictionMismatch
      !(CheckedMorphism obj mor)
      !(CheckedMorphism obj mor)
      !(CheckedMorphism obj mor)
      !value
      ![mismatch]
  deriving stock (Eq, Show)

checkCorestrictionIdentityLawWith ::
  Site site =>
  (CheckedMorphism (SiteObject site) (SiteMorphism site) -> value -> Either coreFailure value) ->
  (SiteObject site -> value -> value -> [mismatch]) ->
  site ->
  SiteObject site ->
  value ->
  Either
    (CosheafLawFailure (SiteObject site) (SiteMorphism site) value mismatch coreFailure)
    ()
checkCorestrictionIdentityLawWith corestrictAction mismatchAt site objectValue costalkValue = do
  corestrictedValue <-
    first
      (IdentityCorestrictionFailed identityMorphism costalkValue)
      (corestrictAction identityMorphism costalkValue)
  let mismatches =
        mismatchAt objectValue corestrictedValue costalkValue
  if null mismatches
    then Right ()
    else Left (IdentityCorestrictionMismatch identityMorphism costalkValue mismatches)
  where
    identityMorphism =
      identityAt site objectValue

checkCorestrictionCompositionDefined ::
  Site site =>
  site ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  Either
    (CosheafLawFailure (SiteObject site) (SiteMorphism site) value mismatch coreFailure)
    (CheckedMorphism (SiteObject site) (SiteMorphism site))
checkCorestrictionCompositionDefined site outerMorphism innerMorphism =
  maybe
    (Left (CompositionCorestrictionUndefined outerMorphism innerMorphism))
    Right
    (composeChecked site outerMorphism innerMorphism)

checkCorestrictionCompositionLawWith ::
  Site site =>
  (CheckedMorphism (SiteObject site) (SiteMorphism site) -> value -> Either coreFailure value) ->
  (SiteObject site -> value -> value -> [mismatch]) ->
  site ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  value ->
  Either
    (CosheafLawFailure (SiteObject site) (SiteMorphism site) value mismatch coreFailure)
    ()
checkCorestrictionCompositionLawWith corestrictAction mismatchAt site outerMorphism innerMorphism sourceValue = do
  compositeMorphism <-
    checkCorestrictionCompositionDefined site outerMorphism innerMorphism
  innerValue <-
    first
      (CompositionCorestrictionFailed innerMorphism sourceValue)
      (corestrictAction innerMorphism sourceValue)
  sequentialValue <-
    first
      (CompositionCorestrictionFailed outerMorphism innerValue)
      (corestrictAction outerMorphism innerValue)
  directValue <-
    first
      (CompositionCorestrictionFailed compositeMorphism sourceValue)
      (corestrictAction compositeMorphism sourceValue)
  let mismatches =
        mismatchAt (cmTarget outerMorphism) sequentialValue directValue
  if null mismatches
    then Right ()
    else
      Left
        ( CompositionCorestrictionMismatch
            outerMorphism
            innerMorphism
            compositeMorphism
            sourceValue
            mismatches
        )
