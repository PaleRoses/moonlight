{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Image.Restrict
  ( ImageRestrictionFailure (..),
    ImagePullbackFailure (..),
    pullbackFinitePresheaf,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Sheaf.Presheaf.Finite
  ( FinitePresheaf (..),
    FinitePresheafFailure,
    finiteFiberAt,
    finiteFiberValues,
    mkFinitePresheaf,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
    siteMorphismUniverse,
  )
import Moonlight.Sheaf.Site.Construction.Map
  ( FiniteSiteMap,
    finiteSiteMapSource,
    siteMapMorphismImage,
    siteMapObjectImage,
  )

type ImageRestrictionFailure :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data ImageRestrictionFailure sourceObj sourceMor targetObj targetMor value restrictionFailure
  = ImageRestrictionMorphismImageMissing
      !(CheckedMorphism sourceObj sourceMor)
  | ImageRestrictionTargetMorphismOutsidePresheafSite
      !(CheckedMorphism sourceObj sourceMor)
      !(CheckedMorphism targetObj targetMor)
  | ImageRestrictionTargetFailed
      !(CheckedMorphism sourceObj sourceMor)
      !(CheckedMorphism targetObj targetMor)
      !value
      !restrictionFailure
  deriving stock (Eq, Show)

type ImagePullbackFailure :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data ImagePullbackFailure sourceObj sourceMor targetObj targetMor value mismatch restrictionFailure
  = ImagePullbackObjectImageMissing !sourceObj
  | ImagePullbackFiberMissing !sourceObj !targetObj
  | ImagePullbackPresheafInvalid
      !( FinitePresheafFailure
           sourceObj
           sourceMor
           value
           mismatch
           (ImageRestrictionFailure sourceObj sourceMor targetObj targetMor value restrictionFailure)
       )
  deriving stock (Eq, Show)

pullbackFinitePresheaf ::
  ( Site source,
    Site target,
    Ord (SiteMorphism source),
    Ord (SiteMorphism target),
    Ord value
  ) =>
  FiniteSiteMap source target ->
  FinitePresheaf target value mismatch restrictionFailure ->
  Either
    ( ImagePullbackFailure
        (SiteObject source)
        (SiteMorphism source)
        (SiteObject target)
        (SiteMorphism target)
        value
        mismatch
        restrictionFailure
    )
    ( FinitePresheaf
        source
        value
        mismatch
        ( ImageRestrictionFailure
            (SiteObject source)
            (SiteMorphism source)
            (SiteObject target)
            (SiteMorphism target)
            value
            restrictionFailure
        )
    )
pullbackFinitePresheaf siteMapValue targetPresheaf = do
  rawFibers <-
    Map.fromList
      <$> traverse
        rawFiberFor
        (siteObjects sourceSite)

  first ImagePullbackPresheafInvalid $
    mkFinitePresheaf
      sourceSite
      restrictAction
      mismatchAt
      normalizeAt
      rawFibers
  where
    sourceSite =
      finiteSiteMapSource siteMapValue

    targetSite =
      fpSite targetPresheaf

    targetMorphismSet =
      Set.fromList (siteMorphismUniverse targetSite)

    rawFiberFor sourceObject = do
      targetObject <- objectImageFor sourceObject
      targetFiber <-
        maybe
          (Left (ImagePullbackFiberMissing sourceObject targetObject))
          Right
          (finiteFiberAt targetObject targetPresheaf)
      pure (sourceObject, finiteFiberValues targetFiber)

    objectImageFor sourceObject =
      maybe
        (Left (ImagePullbackObjectImageMissing sourceObject))
        Right
        (siteMapObjectImage sourceObject siteMapValue)

    restrictAction sourceMorphism value = do
      targetMorphism <-
        maybe
          (Left (ImageRestrictionMorphismImageMissing sourceMorphism))
          Right
          (siteMapMorphismImage sourceMorphism siteMapValue)
      if Set.member targetMorphism targetMorphismSet
        then
          first
            (ImageRestrictionTargetFailed sourceMorphism targetMorphism value)
            (fpRestrict targetPresheaf targetMorphism value)
        else
          Left
            ( ImageRestrictionTargetMorphismOutsidePresheafSite
                sourceMorphism
                targetMorphism
            )

    mismatchAt sourceObject leftValue rightValue =
      case siteMapObjectImage sourceObject siteMapValue of
        Just targetObject ->
          fpMismatches targetPresheaf targetObject leftValue rightValue
        Nothing ->
          []

    normalizeAt sourceObject value =
      case siteMapObjectImage sourceObject siteMapValue of
        Just targetObject ->
          fpNormalize targetPresheaf targetObject value
        Nothing ->
          value
