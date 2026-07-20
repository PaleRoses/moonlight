{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Image.Adjunction
  ( FiniteImageAdjunction (..),
    FiniteImageAdjunctionFailure (..),
    FiniteImageTriangleFailure (..),
    FiniteSiteMapAdjunctionImageFailure (..),
    FiniteSiteMapUnitComponentFailure (..),
    FiniteSiteMapCounitComponentFailure (..),
    FiniteSiteMapLeftTriangleFailure (..),
    FiniteSiteMapRightTriangleFailure (..),
    finiteImageAdjunctionFromEvidence,
    finiteImageAdjunctionSatisfied,
    finiteSiteMapImageAdjunction,
    finiteSiteMapLeftTriangleFailures,
    finiteSiteMapRightTriangleFailures,
  )
where

import Data.Bifunctor (first)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Void (Void)
import Moonlight.Sheaf.Image.Direct
  ( DirectImageBuildFailure,
    DirectImageCone,
    DirectImageIndexObject (..),
    DirectImageMismatch,
    DirectImageRestrictionFailure,
    directImageConeAssignments,
    directImageConeValueAt,
    mkDirectImageCone,
    pushforwardFinitePresheaf,
  )
import Moonlight.Sheaf.Image.Restrict
  ( ImagePullbackFailure,
    ImageRestrictionFailure,
    pullbackFinitePresheaf,
  )
import Moonlight.Sheaf.Presheaf.Enumeration
  ( FiniteEnumerationBudget,
  )
import Moonlight.Sheaf.Presheaf.Finite
  ( FinitePresheaf (..),
    finiteFiberAt,
    finiteFiberValues,
  )
import Moonlight.Sheaf.Presheaf.Morphism
  ( FinitePresheafMorphism,
    FinitePresheafMorphismFailure,
    mkFinitePresheafMorphism,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
    siteMorphismUniverse,
  )
import Moonlight.Sheaf.Site.Construction.Map
  ( ContinuousSiteMap,
    FiniteSiteMap,
    continuousFiniteSiteMap,
    finiteSiteMapSource,
    finiteSiteMapTarget,
    siteMapObjectImage,
  )

data FiniteImageAdjunction
  unitSite
  counitSite
  unitSourceValue
  unitTargetValue
  unitSourceMismatch
  unitTargetMismatch
  unitSourceRestrictionFailure
  unitTargetRestrictionFailure
  counitSourceValue
  counitTargetValue
  counitSourceMismatch
  counitTargetMismatch
  counitSourceRestrictionFailure
  counitTargetRestrictionFailure
  leftTriangleFailure
  rightTriangleFailure = FiniteImageAdjunction
  { finiteImageAdjunctionUnit ::
      !( FinitePresheafMorphism
           unitSite
           unitSourceValue
           unitTargetValue
           unitSourceMismatch
           unitTargetMismatch
           unitSourceRestrictionFailure
           unitTargetRestrictionFailure
       ),
    finiteImageAdjunctionCounit ::
      !( FinitePresheafMorphism
           counitSite
           counitSourceValue
           counitTargetValue
           counitSourceMismatch
           counitTargetMismatch
           counitSourceRestrictionFailure
           counitTargetRestrictionFailure
       ),
    finiteImageAdjunctionLeftTriangleFailures :: ![leftTriangleFailure],
    finiteImageAdjunctionRightTriangleFailures :: ![rightTriangleFailure]
  }

data FiniteImageAdjunctionFailure imageFailure unitFailure counitFailure
  = FiniteImageAdjunctionImageInvalid !imageFailure
  | FiniteImageAdjunctionUnitInvalid !unitFailure
  | FiniteImageAdjunctionCounitInvalid !counitFailure
  deriving stock (Eq, Show)

data FiniteImageTriangleFailure obj mor value mismatch restrictionFailure
  = FiniteImageTriangleFiberMissing !obj
  | FiniteImageTriangleFirstMorphismMissing !obj !obj
  | FiniteImageTriangleSecondMorphismMissing !obj !obj
  | FiniteImageTriangleFirstRestrictionFailed !(CheckedMorphism obj mor) !value !restrictionFailure
  | FiniteImageTriangleSecondRestrictionFailed !(CheckedMorphism obj mor) !value !restrictionFailure
  | FiniteImageTriangleMismatch !obj !value !value ![mismatch]
  deriving stock (Eq, Show)

data FiniteSiteMapAdjunctionImageFailure
  sourceObj
  sourceMor
  targetObj
  targetMor
  targetValue
  targetMismatch
  targetRestrictionFailure
  sourceValue
  sourceMismatch
  sourceRestrictionFailure
  = FiniteSiteMapAdjunctionPullbackTargetFailed
      !(ImagePullbackFailure sourceObj sourceMor targetObj targetMor targetValue targetMismatch targetRestrictionFailure)
  | FiniteSiteMapAdjunctionPushforwardPulledTargetFailed
      !( DirectImageBuildFailure
           sourceObj
           sourceMor
           targetObj
           targetMor
           targetValue
           targetMismatch
           (ImageRestrictionFailure sourceObj sourceMor targetObj targetMor targetValue targetRestrictionFailure)
       )
  | FiniteSiteMapAdjunctionPushforwardSourceFailed
      !(DirectImageBuildFailure sourceObj sourceMor targetObj targetMor sourceValue sourceMismatch sourceRestrictionFailure)
  | FiniteSiteMapAdjunctionPullbackPushedSourceFailed
      !( ImagePullbackFailure
           sourceObj
           sourceMor
           targetObj
           targetMor
           (DirectImageCone sourceObj targetObj targetMor sourceValue)
           (DirectImageMismatch sourceObj targetObj targetMor sourceValue sourceMismatch)
           (DirectImageRestrictionFailure sourceObj targetObj targetMor)
       )
  deriving stock (Eq, Show)

data FiniteSiteMapUnitComponentFailure targetObj targetMor value targetRestrictionFailure
  = FiniteSiteMapUnitComponentRestrictionFailed !(CheckedMorphism targetObj targetMor) !value !targetRestrictionFailure
  deriving stock (Eq, Show)

data FiniteSiteMapCounitComponentFailure sourceObj targetObj targetMor sourceValue
  = FiniteSiteMapCounitObjectImageMissing !sourceObj
  | FiniteSiteMapCounitIdentityCoordinateMissing
      !sourceObj
      !targetObj
      !(DirectImageCone sourceObj targetObj targetMor sourceValue)
  deriving stock (Eq, Show)

data FiniteSiteMapLeftTriangleFailure sourceObj targetObj targetMor value mismatch restrictionFailure
  = FiniteSiteMapLeftTriangleFiberMissing !sourceObj
  | FiniteSiteMapLeftTriangleObjectImageMissing !sourceObj
  | FiniteSiteMapLeftTriangleUnitComponentFailed
      !sourceObj
      !value
      !(FiniteSiteMapUnitComponentFailure targetObj targetMor value restrictionFailure)
  | FiniteSiteMapLeftTriangleCounitComponentFailed
      !sourceObj
      !(DirectImageCone sourceObj targetObj targetMor value)
      !(FiniteSiteMapCounitComponentFailure sourceObj targetObj targetMor value)
  | FiniteSiteMapLeftTriangleMismatch !sourceObj !value !value ![mismatch]
  deriving stock (Eq, Show)

data FiniteSiteMapRightTriangleFailure sourceObj targetObj targetMor sourceValue sourceMismatch
  = FiniteSiteMapRightTriangleFiberMissing !targetObj
  | FiniteSiteMapRightTriangleUnitComponentFailed
      !targetObj
      !(DirectImageCone sourceObj targetObj targetMor sourceValue)
      !( FiniteSiteMapUnitComponentFailure
           targetObj
           targetMor
           (DirectImageCone sourceObj targetObj targetMor sourceValue)
           (DirectImageRestrictionFailure sourceObj targetObj targetMor)
       )
  | FiniteSiteMapRightTriangleCounitComponentFailed
      !targetObj
      !(DirectImageIndexObject sourceObj targetObj targetMor)
      !(FiniteSiteMapCounitComponentFailure sourceObj targetObj targetMor sourceValue)
  | FiniteSiteMapRightTriangleMismatch
      !targetObj
      !(DirectImageCone sourceObj targetObj targetMor sourceValue)
      !(DirectImageCone sourceObj targetObj targetMor sourceValue)
      ![DirectImageMismatch sourceObj targetObj targetMor sourceValue sourceMismatch]
  deriving stock (Eq, Show)

siteMapUnitComponentFor ::
  forall source target value restrictionFailure mismatch.
  ( Site source,
    Site target,
    Ord (SiteMorphism target)
  ) =>
  FiniteSiteMap source target ->
  FinitePresheaf target value mismatch restrictionFailure ->
  SiteObject target ->
  value ->
  Either
    (FiniteSiteMapUnitComponentFailure (SiteObject target) (SiteMorphism target) value restrictionFailure)
    (DirectImageCone (SiteObject source) (SiteObject target) (SiteMorphism target) value)
siteMapUnitComponentFor siteMapValue presheafValue =
  component
  where
    component targetObject value =
      mkDirectImageCone targetObject . Map.fromList
        <$> traverse
          (coordinate value)
          (unitIndexObjects targetObject)

    coordinate value indexObject =
      (indexObject,)
        <$> first
          (FiniteSiteMapUnitComponentRestrictionFailed (directImageIndexTargetMorphism indexObject) value)
          (fpRestrict presheafValue (directImageIndexTargetMorphism indexObject) value)

    unitIndexObjects targetObject =
      foldMap (sourceObjectIndexObjects targetObject) (siteObjects sourceSite)

    sourceObjectIndexObjects targetObject sourceObject =
      case siteMapObjectImage sourceObject siteMapValue of
        Nothing -> []
        Just sourceImage ->
          fmap
            (DirectImageIndexObject sourceObject)
            (morphismsWithEndpoints sourceImage targetObject)

    morphismsWithEndpoints sourceImage targetObject =
      Set.toAscList $
        Set.filter
          (\targetMorphism -> cmSource targetMorphism == sourceImage && cmTarget targetMorphism == targetObject)
          targetMorphismSet

    targetMorphismSet =
      Set.fromList (siteMorphismUniverse (finiteSiteMapTarget siteMapValue))

    sourceSite =
      finiteSiteMapSource siteMapValue

siteMapCounitComponentFor ::
  ( Site source,
    Site target,
    Ord (SiteMorphism target)
  ) =>
  FiniteSiteMap source target ->
  SiteObject source ->
  DirectImageCone (SiteObject source) (SiteObject target) (SiteMorphism target) value ->
  Either
    (FiniteSiteMapCounitComponentFailure (SiteObject source) (SiteObject target) (SiteMorphism target) value)
    value
siteMapCounitComponentFor siteMapValue sourceObject coneValue = do
  targetObject <-
    maybe
      (Left (FiniteSiteMapCounitObjectImageMissing sourceObject))
      Right
      (siteMapObjectImage sourceObject siteMapValue)
  let identityIndex =
        DirectImageIndexObject sourceObject (identityAt (finiteSiteMapTarget siteMapValue) targetObject)
  maybe
    (Left (FiniteSiteMapCounitIdentityCoordinateMissing sourceObject targetObject coneValue))
    Right
    (directImageConeValueAt identityIndex coneValue)

finiteSiteMapLeftTriangleFailures ::
  forall source target targetValue targetMismatch targetRestrictionFailure pulledRestrictionFailure.
  ( Site source,
    Site target,
    Ord (SiteMorphism target)
  ) =>
  FiniteSiteMap source target ->
  FinitePresheaf target targetValue targetMismatch targetRestrictionFailure ->
  FinitePresheaf source targetValue targetMismatch pulledRestrictionFailure ->
  [ FiniteSiteMapLeftTriangleFailure
      (SiteObject source)
      (SiteObject target)
      (SiteMorphism target)
      targetValue
      targetMismatch
      targetRestrictionFailure
  ]
finiteSiteMapLeftTriangleFailures siteMapValue targetPresheaf pulledTarget =
  foldMap objectFailures (siteObjects (finiteSiteMapSource siteMapValue))
  where
    unitComponent =
      siteMapUnitComponentFor siteMapValue targetPresheaf

    objectFailures sourceObject =
      case finiteFiberAt sourceObject pulledTarget of
        Nothing -> [FiniteSiteMapLeftTriangleFiberMissing sourceObject]
        Just fiber ->
          case siteMapObjectImage sourceObject siteMapValue of
            Nothing -> [FiniteSiteMapLeftTriangleObjectImageMissing sourceObject]
            Just targetObject ->
              foldMap (valueFailures sourceObject targetObject) (finiteFiberValues fiber)

    valueFailures sourceObject targetObject value =
      case unitComponent targetObject value of
        Left failure -> [FiniteSiteMapLeftTriangleUnitComponentFailed sourceObject value failure]
        Right coneValue ->
          case siteMapCounitComponentFor siteMapValue sourceObject coneValue of
            Left failure -> [FiniteSiteMapLeftTriangleCounitComponentFailed sourceObject coneValue failure]
            Right roundTrip ->
              case fpMismatches pulledTarget sourceObject roundTrip value of
                [] -> []
                mismatches -> [FiniteSiteMapLeftTriangleMismatch sourceObject value roundTrip mismatches]

finiteSiteMapRightTriangleFailures ::
  forall source target sourceValue sourceMismatch.
  ( Site source,
    Site target,
    Ord (SiteMorphism target)
  ) =>
  FiniteSiteMap source target ->
  FinitePresheaf
    target
    (DirectImageCone (SiteObject source) (SiteObject target) (SiteMorphism target) sourceValue)
    (DirectImageMismatch (SiteObject source) (SiteObject target) (SiteMorphism target) sourceValue sourceMismatch)
    (DirectImageRestrictionFailure (SiteObject source) (SiteObject target) (SiteMorphism target)) ->
  [ FiniteSiteMapRightTriangleFailure
      (SiteObject source)
      (SiteObject target)
      (SiteMorphism target)
      sourceValue
      sourceMismatch
  ]
finiteSiteMapRightTriangleFailures siteMapValue pushedSource =
  foldMap objectFailures (siteObjects (finiteSiteMapTarget siteMapValue))
  where
    unitComponent =
      siteMapUnitComponentFor siteMapValue pushedSource

    objectFailures targetObject =
      case finiteFiberAt targetObject pushedSource of
        Nothing -> [FiniteSiteMapRightTriangleFiberMissing targetObject]
        Just fiber -> foldMap (coneFailures targetObject) (finiteFiberValues fiber)

    coneFailures targetObject coneValue =
      case unitComponent targetObject coneValue of
        Left failure -> [FiniteSiteMapRightTriangleUnitComponentFailed targetObject coneValue failure]
        Right coneOfCones ->
          case Map.traverseWithKey counitAt (directImageConeAssignments coneOfCones) of
            Left (indexObject, failure) ->
              [FiniteSiteMapRightTriangleCounitComponentFailed targetObject indexObject failure]
            Right assignments ->
              let roundTrip = mkDirectImageCone targetObject assignments
               in case fpMismatches pushedSource targetObject roundTrip coneValue of
                    [] -> []
                    mismatches ->
                      [FiniteSiteMapRightTriangleMismatch targetObject coneValue roundTrip mismatches]

    counitAt indexObject innerCone =
      first
        (indexObject,)
        (siteMapCounitComponentFor siteMapValue (directImageIndexSourceObject indexObject) innerCone)

finiteImageAdjunctionFromEvidence ::
  Either unitFailure (FinitePresheafMorphism unitSite unitSourceValue unitTargetValue unitSourceMismatch unitTargetMismatch unitSourceRestrictionFailure unitTargetRestrictionFailure) ->
  Either counitFailure (FinitePresheafMorphism counitSite counitSourceValue counitTargetValue counitSourceMismatch counitTargetMismatch counitSourceRestrictionFailure counitTargetRestrictionFailure) ->
  [leftTriangleFailure] ->
  [rightTriangleFailure] ->
  Either
    (FiniteImageAdjunctionFailure Void unitFailure counitFailure)
    ( FiniteImageAdjunction
        unitSite
        counitSite
        unitSourceValue
        unitTargetValue
        unitSourceMismatch
        unitTargetMismatch
        unitSourceRestrictionFailure
        unitTargetRestrictionFailure
        counitSourceValue
        counitTargetValue
        counitSourceMismatch
        counitTargetMismatch
        counitSourceRestrictionFailure
        counitTargetRestrictionFailure
        leftTriangleFailure
        rightTriangleFailure
    )
finiteImageAdjunctionFromEvidence unitResult counitResult leftTriangleFailures rightTriangleFailures = do
  unitMorphism <- first FiniteImageAdjunctionUnitInvalid unitResult
  counitMorphism <- first FiniteImageAdjunctionCounitInvalid counitResult
  pure
    FiniteImageAdjunction
      { finiteImageAdjunctionUnit = unitMorphism,
        finiteImageAdjunctionCounit = counitMorphism,
        finiteImageAdjunctionLeftTriangleFailures = leftTriangleFailures,
        finiteImageAdjunctionRightTriangleFailures = rightTriangleFailures
      }

finiteImageAdjunctionSatisfied ::
  FiniteImageAdjunction
    unitSite
    counitSite
    unitSourceValue
    unitTargetValue
    unitSourceMismatch
    unitTargetMismatch
    unitSourceRestrictionFailure
    unitTargetRestrictionFailure
    counitSourceValue
    counitTargetValue
    counitSourceMismatch
    counitTargetMismatch
    counitSourceRestrictionFailure
    counitTargetRestrictionFailure
    leftTriangleFailure
    rightTriangleFailure ->
  Bool
finiteImageAdjunctionSatisfied adjunction =
  null (finiteImageAdjunctionLeftTriangleFailures adjunction)
    && null (finiteImageAdjunctionRightTriangleFailures adjunction)
{-# INLINE finiteImageAdjunctionSatisfied #-}

finiteSiteMapImageAdjunction ::
  forall source target targetValue targetMismatch targetRestrictionFailure sourceValue sourceMismatch sourceRestrictionFailure.
  ( Site source,
    Site target,
    Ord (SiteMorphism source),
    Ord (SiteMorphism target),
    Ord targetValue,
    Ord sourceValue
  ) =>
  FiniteEnumerationBudget ->
  ContinuousSiteMap source target ->
  FinitePresheaf target targetValue targetMismatch targetRestrictionFailure ->
  FinitePresheaf source sourceValue sourceMismatch sourceRestrictionFailure ->
  Either
    ( FiniteImageAdjunctionFailure
        ( FiniteSiteMapAdjunctionImageFailure
            (SiteObject source)
            (SiteMorphism source)
            (SiteObject target)
            (SiteMorphism target)
            targetValue
            targetMismatch
            targetRestrictionFailure
            sourceValue
            sourceMismatch
            sourceRestrictionFailure
        )
        ( FinitePresheafMorphismFailure
            (SiteObject target)
            (SiteMorphism target)
            targetValue
            (DirectImageCone (SiteObject source) (SiteObject target) (SiteMorphism target) targetValue)
            targetRestrictionFailure
            (DirectImageRestrictionFailure (SiteObject source) (SiteObject target) (SiteMorphism target))
            (DirectImageMismatch (SiteObject source) (SiteObject target) (SiteMorphism target) targetValue targetMismatch)
            (FiniteSiteMapUnitComponentFailure (SiteObject target) (SiteMorphism target) targetValue targetRestrictionFailure)
        )
        ( FinitePresheafMorphismFailure
            (SiteObject source)
            (SiteMorphism source)
            (DirectImageCone (SiteObject source) (SiteObject target) (SiteMorphism target) sourceValue)
            sourceValue
            ( ImageRestrictionFailure
                (SiteObject source)
                (SiteMorphism source)
                (SiteObject target)
                (SiteMorphism target)
                (DirectImageCone (SiteObject source) (SiteObject target) (SiteMorphism target) sourceValue)
                (DirectImageRestrictionFailure (SiteObject source) (SiteObject target) (SiteMorphism target))
            )
            sourceRestrictionFailure
            sourceMismatch
            (FiniteSiteMapCounitComponentFailure (SiteObject source) (SiteObject target) (SiteMorphism target) sourceValue)
        )
    )
    ( FiniteImageAdjunction
        target
        source
        targetValue
        (DirectImageCone (SiteObject source) (SiteObject target) (SiteMorphism target) targetValue)
        targetMismatch
        (DirectImageMismatch (SiteObject source) (SiteObject target) (SiteMorphism target) targetValue targetMismatch)
        targetRestrictionFailure
        (DirectImageRestrictionFailure (SiteObject source) (SiteObject target) (SiteMorphism target))
        (DirectImageCone (SiteObject source) (SiteObject target) (SiteMorphism target) sourceValue)
        sourceValue
        (DirectImageMismatch (SiteObject source) (SiteObject target) (SiteMorphism target) sourceValue sourceMismatch)
        sourceMismatch
        ( ImageRestrictionFailure
            (SiteObject source)
            (SiteMorphism source)
            (SiteObject target)
            (SiteMorphism target)
            (DirectImageCone (SiteObject source) (SiteObject target) (SiteMorphism target) sourceValue)
            (DirectImageRestrictionFailure (SiteObject source) (SiteObject target) (SiteMorphism target))
        )
        sourceRestrictionFailure
        ( FiniteSiteMapLeftTriangleFailure
            (SiteObject source)
            (SiteObject target)
            (SiteMorphism target)
            targetValue
            targetMismatch
            targetRestrictionFailure
        )
        ( FiniteSiteMapRightTriangleFailure
            (SiteObject source)
            (SiteObject target)
            (SiteMorphism target)
            sourceValue
            sourceMismatch
        )
    )
finiteSiteMapImageAdjunction budget continuousMap targetPresheaf sourcePresheaf = do
  pulledTarget <-
    first
      (FiniteImageAdjunctionImageInvalid . FiniteSiteMapAdjunctionPullbackTargetFailed)
      (pullbackFinitePresheaf siteMapValue targetPresheaf)
  pushedPulledTarget <-
    first
      (FiniteImageAdjunctionImageInvalid . FiniteSiteMapAdjunctionPushforwardPulledTargetFailed)
      (pushforwardFinitePresheaf budget continuousMap pulledTarget)
  pushedSource <-
    first
      (FiniteImageAdjunctionImageInvalid . FiniteSiteMapAdjunctionPushforwardSourceFailed)
      (pushforwardFinitePresheaf budget continuousMap sourcePresheaf)
  pulledPushedSource <-
    first
      (FiniteImageAdjunctionImageInvalid . FiniteSiteMapAdjunctionPullbackPushedSourceFailed)
      (pullbackFinitePresheaf siteMapValue pushedSource)
  unitMorphism <-
    first
      FiniteImageAdjunctionUnitInvalid
      (mkFinitePresheafMorphism targetPresheaf pushedPulledTarget (siteMapUnitComponentFor siteMapValue targetPresheaf))
  counitMorphism <-
    first
      FiniteImageAdjunctionCounitInvalid
      (mkFinitePresheafMorphism pulledPushedSource sourcePresheaf (siteMapCounitComponentFor siteMapValue))
  pure
    FiniteImageAdjunction
      { finiteImageAdjunctionUnit = unitMorphism,
        finiteImageAdjunctionCounit = counitMorphism,
        finiteImageAdjunctionLeftTriangleFailures =
          finiteSiteMapLeftTriangleFailures siteMapValue targetPresheaf pulledTarget,
        finiteImageAdjunctionRightTriangleFailures =
          finiteSiteMapRightTriangleFailures siteMapValue pushedSource
      }
  where
    siteMapValue =
      continuousFiniteSiteMap continuousMap
