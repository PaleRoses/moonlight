{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Site.Construction.Map
  ( FiniteSiteMap,
    ContinuousSiteMap,
    SiteMapFailure (..),
    CoverImageFailure (..),
    finiteSiteMapSource,
    finiteSiteMapTarget,
    siteMapObjectImage,
    siteMapMorphismImage,
    continuousFiniteSiteMap,
    coverImageAt,
    mkFiniteSiteMap,
    mkContinuousSiteMap,
  )
where

import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core
  ( duplicatesOrd,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoverConstructionError,
    CoveringFamily,
    Site (..),
    SiteLawFailure,
    coverArrows,
    coverTarget,
    mkCoveringFamily,
    siteMorphismUniverse,
  )
import Moonlight.Sheaf.Site.CoverBasis.Finite
  ( FiniteCoverBasis,
    FiniteCoverBasisFailure,
    finiteAllCoverPlans,
    finiteCoverPlanForCover,
  )
import Moonlight.Sheaf.Site.Plan
  ( EffectiveCoverPlan,
    effectiveCoverFamily,
  )
import Moonlight.Sheaf.Site.Class.Validation
  ( siteLawFailures,
  )

type FiniteSiteMap :: Type -> Type -> Type
data FiniteSiteMap source target = FiniteSiteMap
  { fsmSource :: !source,
    fsmTarget :: !target,
    fsmObjectImages ::
      !(Map (SiteObject source) (SiteObject target)),
    fsmMorphismImages ::
      !( Map
           (CheckedMorphism (SiteObject source) (SiteMorphism source))
           (CheckedMorphism (SiteObject target) (SiteMorphism target))
       )
  }

type ContinuousSiteMap :: Type -> Type -> Type
data ContinuousSiteMap source target = ContinuousSiteMap
  { csmFiniteSiteMap :: !(FiniteSiteMap source target),
    csmCoverImages ::
      !( Map
           (CoveringFamily (SiteObject source) (SiteMorphism source))
           (EffectiveCoverPlan (SiteObject target) (SiteMorphism target))
       )
  }

type SiteMapFailure :: Type -> Type -> Type -> Type -> Type
data SiteMapFailure sourceObj sourceMor targetObj targetMor
  = SiteMapSourceSiteInvalid !(NonEmpty (SiteLawFailure sourceObj sourceMor))
  | SiteMapTargetSiteInvalid !(NonEmpty (SiteLawFailure targetObj targetMor))
  | SiteMapDuplicateSourceObject !sourceObj
  | SiteMapDuplicateTargetObject !targetObj
  | SiteMapDuplicateSourceMorphism !(CheckedMorphism sourceObj sourceMor)
  | SiteMapDuplicateTargetMorphism !(CheckedMorphism targetObj targetMor)
  | SiteMapObjectImageMissing !sourceObj
  | SiteMapObjectImageUnknownSource !sourceObj
  | SiteMapObjectImageUnknownTarget !sourceObj !targetObj
  | SiteMapMorphismImageMissing !(CheckedMorphism sourceObj sourceMor)
  | SiteMapMorphismImageUnknownSource !(CheckedMorphism sourceObj sourceMor)
  | SiteMapMorphismImageUnknownTarget
      !(CheckedMorphism sourceObj sourceMor)
      !(CheckedMorphism targetObj targetMor)
  | SiteMapMorphismEndpointMismatch
      !(CheckedMorphism sourceObj sourceMor)
      !(CheckedMorphism targetObj targetMor)
      !targetObj
      !targetObj
  | SiteMapIdentityMismatch
      !sourceObj
      !(CheckedMorphism targetObj targetMor)
      !(CheckedMorphism targetObj targetMor)
  | SiteMapSourceCompositionUndefined
      !(CheckedMorphism sourceObj sourceMor)
      !(CheckedMorphism sourceObj sourceMor)
  | SiteMapTargetCompositionUndefined
      !(CheckedMorphism sourceObj sourceMor)
      !(CheckedMorphism sourceObj sourceMor)
      !(CheckedMorphism targetObj targetMor)
      !(CheckedMorphism targetObj targetMor)
  | SiteMapCompositionMismatch
      !(CheckedMorphism sourceObj sourceMor)
      !(CheckedMorphism sourceObj sourceMor)
      !(CheckedMorphism sourceObj sourceMor)
      !(CheckedMorphism targetObj targetMor)
      !(CheckedMorphism targetObj targetMor)
  deriving stock (Eq, Show)

type CoverImageFailure :: Type -> Type -> Type -> Type -> Type
data CoverImageFailure sourceObj sourceMor targetObj targetMor
  = CoverImageTargetObjectImageMissing
      !(CoveringFamily sourceObj sourceMor)
      !sourceObj
  | CoverImageArrowImageMissing
      !(CoveringFamily sourceObj sourceMor)
      !(CheckedMorphism sourceObj sourceMor)
  | CoverImageMappedCoverMalformed
      !(CoveringFamily sourceObj sourceMor)
      !(CoverConstructionError targetObj)
  | CoverImageNotInTargetBasis
      !(CoveringFamily sourceObj sourceMor)
      !(CoveringFamily targetObj targetMor)
      !(FiniteCoverBasisFailure targetObj targetMor)
  | CoverImageDuplicateSourceCover
      !(CoveringFamily sourceObj sourceMor)
  | CoverImageDuplicateTargetCover
      !(CoveringFamily targetObj targetMor)
  deriving stock (Eq, Show)

finiteSiteMapSource :: FiniteSiteMap source target -> source
finiteSiteMapSource =
  fsmSource
{-# INLINE finiteSiteMapSource #-}

finiteSiteMapTarget :: FiniteSiteMap source target -> target
finiteSiteMapTarget =
  fsmTarget
{-# INLINE finiteSiteMapTarget #-}

siteMapObjectImage ::
  Ord (SiteObject source) =>
  SiteObject source ->
  FiniteSiteMap source target ->
  Maybe (SiteObject target)
siteMapObjectImage sourceObject =
  Map.lookup sourceObject . fsmObjectImages
{-# INLINE siteMapObjectImage #-}

siteMapMorphismImage ::
  (Ord (SiteObject source), Ord (SiteMorphism source)) =>
  CheckedMorphism (SiteObject source) (SiteMorphism source) ->
  FiniteSiteMap source target ->
  Maybe (CheckedMorphism (SiteObject target) (SiteMorphism target))
siteMapMorphismImage sourceMorphism =
  Map.lookup sourceMorphism . fsmMorphismImages
{-# INLINE siteMapMorphismImage #-}

continuousFiniteSiteMap :: ContinuousSiteMap source target -> FiniteSiteMap source target
continuousFiniteSiteMap =
  csmFiniteSiteMap
{-# INLINE continuousFiniteSiteMap #-}

coverImageAt ::
  (Ord (SiteObject source), Ord (SiteMorphism source)) =>
  CoveringFamily (SiteObject source) (SiteMorphism source) ->
  ContinuousSiteMap source target ->
  Maybe (EffectiveCoverPlan (SiteObject target) (SiteMorphism target))
coverImageAt sourceCover =
  Map.lookup sourceCover . csmCoverImages
{-# INLINE coverImageAt #-}

mkFiniteSiteMap ::
  ( Site source,
    Site target,
    Ord (SiteMorphism source),
    Ord (SiteMorphism target)
  ) =>
  source ->
  target ->
  Map (SiteObject source) (SiteObject target) ->
  Map
    (CheckedMorphism (SiteObject source) (SiteMorphism source))
    (CheckedMorphism (SiteObject target) (SiteMorphism target)) ->
  Either
    ( SiteMapFailure
        (SiteObject source)
        (SiteMorphism source)
        (SiteObject target)
        (SiteMorphism target)
    )
    (FiniteSiteMap source target)
mkFiniteSiteMap source target objectImages morphismImages = do
  validateNoSiteFailures SiteMapSourceSiteInvalid source
  validateNoSiteFailures SiteMapTargetSiteInvalid target

  traverse_ (Left . SiteMapDuplicateSourceObject) (duplicatesOrd (siteObjects source))
  traverse_ (Left . SiteMapDuplicateTargetObject) (duplicatesOrd (siteObjects target))
  traverse_ (Left . SiteMapDuplicateSourceMorphism) (duplicatesOrd (siteMorphisms source))
  traverse_ (Left . SiteMapDuplicateTargetMorphism) (duplicatesOrd (siteMorphisms target))

  traverse_ validateObjectImageSourceKnown (Map.keys objectImages)
  traverse_ validateObjectImageTotal (siteObjects source)

  traverse_ validateMorphismImageSourceKnown (Map.keys morphismImages)
  traverse_ validateMorphismImageTotal sourceMorphismUniverse
  traverse_ validateMorphismImageKnown sourceMorphismUniverse
  traverse_ validateMorphismEndpoints sourceMorphismUniverse
  traverse_ validateIdentity (siteObjects source)
  traverse_ validateComposition composablePairs

  pure
    FiniteSiteMap
      { fsmSource = source,
        fsmTarget = target,
        fsmObjectImages = objectImages,
        fsmMorphismImages = morphismImages
      }
  where
    sourceObjectSet =
      Set.fromList (siteObjects source)

    targetObjectSet =
      Set.fromList (siteObjects target)

    sourceMorphismUniverse =
      siteMorphismUniverse source

    targetMorphismUniverse =
      siteMorphismUniverse target

    sourceMorphismSet =
      Set.fromList sourceMorphismUniverse

    targetMorphismSet =
      Set.fromList targetMorphismUniverse

    validateNoSiteFailures ::
      (Site site, Ord (SiteMorphism site)) =>
      (NonEmpty (SiteLawFailure (SiteObject site) (SiteMorphism site)) -> failure) ->
      site ->
      Either failure ()
    validateNoSiteFailures constructor siteValue =
      case NonEmpty.nonEmpty (siteLawFailures siteValue) of
        Nothing ->
          Right ()
        Just failures ->
          Left (constructor failures)

    validateObjectImageSourceKnown sourceObject =
      unless (Set.member sourceObject sourceObjectSet) $
        Left (SiteMapObjectImageUnknownSource sourceObject)

    validateObjectImageTotal sourceObject =
      case Map.lookup sourceObject objectImages of
        Nothing ->
          Left (SiteMapObjectImageMissing sourceObject)
        Just targetObject ->
          unless (Set.member targetObject targetObjectSet) $
            Left (SiteMapObjectImageUnknownTarget sourceObject targetObject)

    validateMorphismImageSourceKnown sourceMorphism =
      unless (Set.member sourceMorphism sourceMorphismSet) $
        Left (SiteMapMorphismImageUnknownSource sourceMorphism)

    validateMorphismImageTotal sourceMorphism =
      case Map.lookup sourceMorphism morphismImages of
        Nothing ->
          Left (SiteMapMorphismImageMissing sourceMorphism)
        Just _ ->
          Right ()

    validateMorphismImageKnown sourceMorphism = do
      targetMorphism <- morphismImageOrMissing sourceMorphism
      unless (Set.member targetMorphism targetMorphismSet) $
        Left (SiteMapMorphismImageUnknownTarget sourceMorphism targetMorphism)

    validateMorphismEndpoints sourceMorphism = do
      expectedSource <- objectImageOrMissing (cmSource sourceMorphism)
      expectedTarget <- objectImageOrMissing (cmTarget sourceMorphism)
      targetMorphism <- morphismImageOrMissing sourceMorphism
      unless
        ( cmSource targetMorphism == expectedSource
            && cmTarget targetMorphism == expectedTarget
        )
        ( Left
            ( SiteMapMorphismEndpointMismatch
                sourceMorphism
                targetMorphism
                expectedSource
                expectedTarget
            )
        )

    validateIdentity sourceObject = do
      targetObject <- objectImageOrMissing sourceObject
      let sourceIdentity =
            identityAt source sourceObject
          expectedTargetIdentity =
            identityAt target targetObject
      actualTargetIdentity <- morphismImageOrMissing sourceIdentity
      unless (actualTargetIdentity == expectedTargetIdentity) $
        Left
          ( SiteMapIdentityMismatch
              sourceObject
              expectedTargetIdentity
              actualTargetIdentity
          )

    validateComposition (outerMorphism, innerMorphism) = do
      sourceComposite <-
        maybe
          (Left (SiteMapSourceCompositionUndefined outerMorphism innerMorphism))
          Right
          (composeChecked source outerMorphism innerMorphism)
      targetOuter <- morphismImageOrMissing outerMorphism
      targetInner <- morphismImageOrMissing innerMorphism
      targetComposite <-
        maybe
          ( Left
              ( SiteMapTargetCompositionUndefined
                  outerMorphism
                  innerMorphism
                  targetOuter
                  targetInner
              )
          )
          Right
          (composeChecked target targetOuter targetInner)
      imageOfSourceComposite <- morphismImageOrMissing sourceComposite
      unless (imageOfSourceComposite == targetComposite) $
        Left
          ( SiteMapCompositionMismatch
              outerMorphism
              innerMorphism
              sourceComposite
              targetComposite
              imageOfSourceComposite
          )

    objectImageOrMissing sourceObject =
      maybe
        (Left (SiteMapObjectImageMissing sourceObject))
        Right
        (Map.lookup sourceObject objectImages)

    morphismImageOrMissing sourceMorphism =
      maybe
        (Left (SiteMapMorphismImageMissing sourceMorphism))
        Right
        (Map.lookup sourceMorphism morphismImages)

    composablePairs =
      [ (outerMorphism, innerMorphism)
      | outerMorphism <- sourceMorphismUniverse,
        innerMorphism <- sourceMorphismUniverse,
        cmSource outerMorphism == cmTarget innerMorphism
      ]

mkContinuousSiteMap ::
  ( Site source,
    Site target,
    Ord (SiteMorphism source),
    Ord (SiteMorphism target)
  ) =>
  FiniteCoverBasis source ->
  FiniteCoverBasis target ->
  FiniteSiteMap source target ->
  Either
    ( CoverImageFailure
        (SiteObject source)
        (SiteMorphism source)
        (SiteObject target)
        (SiteMorphism target)
    )
    (ContinuousSiteMap source target)
mkContinuousSiteMap sourceBasis targetBasis siteMapValue = do
  traverse_ (Left . CoverImageDuplicateSourceCover) (duplicatesOrd (fmap effectiveCoverFamily sourceCoverPlans))
  traverse_ (Left . CoverImageDuplicateTargetCover) (duplicatesOrd (fmap effectiveCoverFamily targetCoverPlans))
  coverImages <-
    Map.fromList
      <$> traverse coverImageEntry sourceCoverPlans

  pure
    ContinuousSiteMap
      { csmFiniteSiteMap = siteMapValue,
        csmCoverImages = coverImages
      }
  where
    sourceCoverPlans =
      finiteAllCoverPlans sourceBasis

    targetCoverPlans =
      finiteAllCoverPlans targetBasis

    coverImageEntry sourcePlan = do
      let sourceCover =
            effectiveCoverFamily sourcePlan
      mappedCover <-
        mapCover sourceCover
      targetPlan <-
        first
          (CoverImageNotInTargetBasis sourceCover mappedCover)
          (finiteCoverPlanForCover targetBasis mappedCover)
      pure (sourceCover, targetPlan)

    mapCover sourceCover = do
      targetObject <-
        maybe
          (Left (CoverImageTargetObjectImageMissing sourceCover (coverTarget sourceCover)))
          Right
          (siteMapObjectImage (coverTarget sourceCover) siteMapValue)
      mappedArrows <-
        traverse (mapCoverArrow sourceCover) (coverArrows sourceCover)
      first
        (CoverImageMappedCoverMalformed sourceCover)
        (mkCoveringFamily targetObject mappedArrows)

    mapCoverArrow sourceCover sourceArrow =
      maybe
        (Left (CoverImageArrowImageMissing sourceCover sourceArrow))
        Right
        (siteMapMorphismImage sourceArrow siteMapValue)
