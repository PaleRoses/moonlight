{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Image.ContextGalois
  ( ContextGaloisMap,
    ContextGaloisMapFailure (..),
    ContextRestrictRestrictionFailure (..),
    ContextRestrictPresheafFailure (..),
    ContextExtendRestrictionFailure (..),
    ContextExtendPresheafFailure (..),
    ContextImageUnitComponentFailure (..),
    ContextImageCounitComponentFailure (..),
    ContextImageAdjunctionImageFailure (..),
    ContextImageAdjunction,
    ContextImageAdjunctionFailure,
    contextGaloisSourceSite,
    contextGaloisTargetSite,
    contextGaloisLower,
    contextGaloisUpper,
    mkContextGaloisMap,
    restrictFiniteContextPresheaf,
    extendFiniteContextPresheaf,
    checkContextImageAdjunction,
  )
where

import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Sheaf.Image.Adjunction
  ( FiniteImageAdjunction (..),
    FiniteImageAdjunctionFailure (..),
    FiniteImageTriangleFailure (..),
  )
import Moonlight.Sheaf.Presheaf.Finite
  ( FinitePresheaf (..),
    FinitePresheafFailure,
    finiteFiberAt,
    finiteFiberValues,
    mkFinitePresheaf,
  )
import Moonlight.Sheaf.Presheaf.Morphism
  ( FinitePresheafMorphismFailure,
    mkFinitePresheafMorphism,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
  )
import Moonlight.Sheaf.Site.Construction.FiniteMeet
  ( FiniteMeetMorphism,
    FiniteMeetSite,
    finiteMeetMorphism,
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    ContextLatticeLookupError,
    leqContext
  )

data ContextGaloisMap sourceCtx targetCtx = ContextGaloisMap
  { cgmSourceLattice :: !(ContextLattice sourceCtx),
    cgmTargetLattice :: !(ContextLattice targetCtx),
    cgmSourceSite :: !(FiniteMeetSite sourceCtx),
    cgmTargetSite :: !(FiniteMeetSite targetCtx),
    cgmLower :: sourceCtx -> targetCtx,
    cgmUpper :: targetCtx -> sourceCtx
  }

data ContextGaloisMapFailure sourceCtx targetCtx
  = ContextGaloisSourceObjectLookupFailed
      !sourceCtx
      !(ContextLatticeLookupError sourceCtx)
  | ContextGaloisTargetObjectLookupFailed
      !targetCtx
      !(ContextLatticeLookupError targetCtx)
  | ContextGaloisLowerImageOutsideTargetSite
      !sourceCtx
      !targetCtx
  | ContextGaloisUpperImageOutsideSourceSite
      !targetCtx
      !sourceCtx
  | ContextGaloisLowerSourceOrderLookupFailed
      !sourceCtx
      !sourceCtx
      !(ContextLatticeLookupError sourceCtx)
  | ContextGaloisLowerTargetOrderLookupFailed
      !sourceCtx
      !sourceCtx
      !targetCtx
      !targetCtx
      !(ContextLatticeLookupError targetCtx)
  | ContextGaloisLowerNotMonotone
      !sourceCtx
      !sourceCtx
      !targetCtx
      !targetCtx
  | ContextGaloisUpperTargetOrderLookupFailed
      !targetCtx
      !targetCtx
      !(ContextLatticeLookupError targetCtx)
  | ContextGaloisUpperSourceOrderLookupFailed
      !targetCtx
      !targetCtx
      !sourceCtx
      !sourceCtx
      !(ContextLatticeLookupError sourceCtx)
  | ContextGaloisUpperNotMonotone
      !targetCtx
      !targetCtx
      !sourceCtx
      !sourceCtx
  | ContextGaloisAdjunctionTargetOrderLookupFailed
      !sourceCtx
      !targetCtx
      !targetCtx
      !(ContextLatticeLookupError targetCtx)
  | ContextGaloisAdjunctionSourceOrderLookupFailed
      !sourceCtx
      !targetCtx
      !sourceCtx
      !(ContextLatticeLookupError sourceCtx)
  | ContextGaloisAdjunctionFailed
      !sourceCtx
      !targetCtx
      !targetCtx
      !sourceCtx
      !Bool
      !Bool
  deriving stock (Eq, Show)

data ContextRestrictRestrictionFailure sourceCtx targetCtx value targetFailure
  = ContextRestrictMorphismMissing
      !(CheckedMorphism sourceCtx (FiniteMeetMorphism sourceCtx))
      !targetCtx
      !targetCtx
  | ContextRestrictTargetRestrictionFailed
      !(CheckedMorphism sourceCtx (FiniteMeetMorphism sourceCtx))
      !(CheckedMorphism targetCtx (FiniteMeetMorphism targetCtx))
      !value
      !targetFailure
  deriving stock (Eq, Show)

data ContextRestrictPresheafFailure sourceCtx targetCtx value mismatch targetFailure
  = ContextRestrictTargetSiteMismatch ![targetCtx] ![targetCtx]
  | ContextRestrictFiberMissing
      !sourceCtx
      !targetCtx
  | ContextRestrictPresheafInvalid
      !( FinitePresheafFailure
           sourceCtx
           (FiniteMeetMorphism sourceCtx)
           value
           mismatch
           (ContextRestrictRestrictionFailure sourceCtx targetCtx value targetFailure)
       )
  deriving stock (Eq, Show)

data ContextExtendRestrictionFailure sourceCtx targetCtx value sourceFailure
  = ContextExtendMorphismMissing
      !(CheckedMorphism targetCtx (FiniteMeetMorphism targetCtx))
      !sourceCtx
      !sourceCtx
  | ContextExtendSourceRestrictionFailed
      !(CheckedMorphism targetCtx (FiniteMeetMorphism targetCtx))
      !(CheckedMorphism sourceCtx (FiniteMeetMorphism sourceCtx))
      !value
      !sourceFailure
  deriving stock (Eq, Show)

data ContextExtendPresheafFailure sourceCtx targetCtx value mismatch sourceFailure
  = ContextExtendSourceSiteMismatch ![sourceCtx] ![sourceCtx]
  | ContextExtendFiberMissing
      !targetCtx
      !sourceCtx
  | ContextExtendPresheafInvalid
      !( FinitePresheafFailure
           targetCtx
           (FiniteMeetMorphism targetCtx)
           value
           mismatch
           (ContextExtendRestrictionFailure sourceCtx targetCtx value sourceFailure)
       )
  deriving stock (Eq, Show)

data ContextImageUnitComponentFailure sourceCtx sourceFailure
  = ContextImageUnitComponentMorphismMissing !sourceCtx !sourceCtx
  | ContextImageUnitComponentRestrictionFailed !(CheckedMorphism sourceCtx (FiniteMeetMorphism sourceCtx)) !sourceFailure
  deriving stock (Eq, Show)

data ContextImageCounitComponentFailure targetCtx targetFailure
  = ContextImageCounitComponentMorphismMissing !targetCtx !targetCtx
  | ContextImageCounitComponentRestrictionFailed !(CheckedMorphism targetCtx (FiniteMeetMorphism targetCtx)) !targetFailure
  deriving stock (Eq, Show)

data ContextImageAdjunctionImageFailure sourceCtx targetCtx value mismatch sourceFailure targetFailure
  = ContextImageAdjunctionExtendSourceFailed
      !(ContextExtendPresheafFailure sourceCtx targetCtx value mismatch sourceFailure)
  | ContextImageAdjunctionRestrictExtendedSourceFailed
      !( ContextRestrictPresheafFailure
           sourceCtx
           targetCtx
           value
           mismatch
           (ContextExtendRestrictionFailure sourceCtx targetCtx value sourceFailure)
       )
  | ContextImageAdjunctionRestrictTargetFailed
      !(ContextRestrictPresheafFailure sourceCtx targetCtx value mismatch targetFailure)
  | ContextImageAdjunctionExtendRestrictedTargetFailed
      !( ContextExtendPresheafFailure
           sourceCtx
           targetCtx
           value
           mismatch
           (ContextRestrictRestrictionFailure sourceCtx targetCtx value targetFailure)
       )
  deriving stock (Eq, Show)

type ContextImageAdjunction sourceCtx targetCtx value mismatch sourceFailure targetFailure =
  FiniteImageAdjunction
    (FiniteMeetSite sourceCtx)
    (FiniteMeetSite targetCtx)
    value
    value
    mismatch
    mismatch
    sourceFailure
    (ContextRestrictRestrictionFailure sourceCtx targetCtx value (ContextExtendRestrictionFailure sourceCtx targetCtx value sourceFailure))
    value
    value
    mismatch
    mismatch
    (ContextExtendRestrictionFailure sourceCtx targetCtx value (ContextRestrictRestrictionFailure sourceCtx targetCtx value targetFailure))
    targetFailure
    (FiniteImageTriangleFailure sourceCtx (FiniteMeetMorphism sourceCtx) value mismatch sourceFailure)
    (FiniteImageTriangleFailure targetCtx (FiniteMeetMorphism targetCtx) value mismatch targetFailure)

type ContextImageAdjunctionFailure sourceCtx targetCtx value mismatch sourceFailure targetFailure =
  FiniteImageAdjunctionFailure
    (ContextImageAdjunctionImageFailure sourceCtx targetCtx value mismatch sourceFailure targetFailure)
    ( FinitePresheafMorphismFailure
        sourceCtx
        (FiniteMeetMorphism sourceCtx)
        value
        value
        sourceFailure
        (ContextRestrictRestrictionFailure sourceCtx targetCtx value (ContextExtendRestrictionFailure sourceCtx targetCtx value sourceFailure))
        mismatch
        (ContextImageUnitComponentFailure sourceCtx sourceFailure)
    )
    ( FinitePresheafMorphismFailure
        targetCtx
        (FiniteMeetMorphism targetCtx)
        value
        value
        (ContextExtendRestrictionFailure sourceCtx targetCtx value (ContextRestrictRestrictionFailure sourceCtx targetCtx value targetFailure))
        targetFailure
        mismatch
        (ContextImageCounitComponentFailure targetCtx targetFailure)
    )

contextGaloisSourceSite :: ContextGaloisMap sourceCtx targetCtx -> FiniteMeetSite sourceCtx
contextGaloisSourceSite =
  cgmSourceSite
{-# INLINE contextGaloisSourceSite #-}

contextGaloisTargetSite :: ContextGaloisMap sourceCtx targetCtx -> FiniteMeetSite targetCtx
contextGaloisTargetSite =
  cgmTargetSite
{-# INLINE contextGaloisTargetSite #-}

contextGaloisLower :: ContextGaloisMap sourceCtx targetCtx -> sourceCtx -> targetCtx
contextGaloisLower =
  cgmLower
{-# INLINE contextGaloisLower #-}

contextGaloisUpper :: ContextGaloisMap sourceCtx targetCtx -> targetCtx -> sourceCtx
contextGaloisUpper =
  cgmUpper
{-# INLINE contextGaloisUpper #-}

mkContextGaloisMap ::
  (Ord sourceCtx, Ord targetCtx) =>
  ContextLattice sourceCtx ->
  ContextLattice targetCtx ->
  FiniteMeetSite sourceCtx ->
  FiniteMeetSite targetCtx ->
  (sourceCtx -> targetCtx) ->
  (targetCtx -> sourceCtx) ->
  Either
    (ContextGaloisMapFailure sourceCtx targetCtx)
    (ContextGaloisMap sourceCtx targetCtx)
-- The convention is lower s <=_target t iff s <=_source upper t.
-- Since FiniteMeetSite arrows are refinement arrows, the unit uses
-- upper (lower s) -> s and the counit uses t -> lower (upper t).
mkContextGaloisMap sourceLattice targetLattice sourceSite targetSite lower upper = do
  traverse_ validateSourceObject sourceObjects
  traverse_ validateTargetObject targetObjects
  traverse_ validateLowerImage sourceObjects
  traverse_ validateUpperImage targetObjects
  traverse_ validateLowerMonotone sourcePairs
  traverse_ validateUpperMonotone targetPairs
  traverse_ validateAdjunction adjunctionPairs
  pure
    ContextGaloisMap
      { cgmSourceLattice = sourceLattice,
        cgmTargetLattice = targetLattice,
        cgmSourceSite = sourceSite,
        cgmTargetSite = targetSite,
        cgmLower = lower,
        cgmUpper = upper
      }
  where
    sourceObjects =
      siteObjects sourceSite

    targetObjects =
      siteObjects targetSite

    sourceObjectSet =
      Set.fromList sourceObjects

    targetObjectSet =
      Set.fromList targetObjects

    sourcePairs =
      [(leftSource, rightSource) | leftSource <- sourceObjects, rightSource <- sourceObjects]

    targetPairs =
      [(leftTarget, rightTarget) | leftTarget <- targetObjects, rightTarget <- targetObjects]

    adjunctionPairs =
      [(sourceObject, targetObject) | sourceObject <- sourceObjects, targetObject <- targetObjects]

    validateSourceObject sourceObject = do
      _ <- first (ContextGaloisSourceObjectLookupFailed sourceObject) (leqContext sourceLattice sourceObject sourceObject)
      pure ()

    validateTargetObject targetObject = do
      _ <- first (ContextGaloisTargetObjectLookupFailed targetObject) (leqContext targetLattice targetObject targetObject)
      pure ()

    validateLowerImage sourceObject =
      let targetObject = lower sourceObject
       in unless (Set.member targetObject targetObjectSet) $
            Left (ContextGaloisLowerImageOutsideTargetSite sourceObject targetObject)

    validateUpperImage targetObject =
      let sourceObject = upper targetObject
       in unless (Set.member sourceObject sourceObjectSet) $
            Left (ContextGaloisUpperImageOutsideSourceSite targetObject sourceObject)

    validateLowerMonotone (leftSource, rightSource) = do
      sourceOrdered <-
        first
          (ContextGaloisLowerSourceOrderLookupFailed leftSource rightSource)
          (leqContext sourceLattice leftSource rightSource)
      let leftTarget = lower leftSource
          rightTarget = lower rightSource
      targetOrdered <-
        first
          (ContextGaloisLowerTargetOrderLookupFailed leftSource rightSource leftTarget rightTarget)
          (leqContext targetLattice leftTarget rightTarget)
      unless (not sourceOrdered || targetOrdered) $
        Left (ContextGaloisLowerNotMonotone leftSource rightSource leftTarget rightTarget)

    validateUpperMonotone (leftTarget, rightTarget) = do
      targetOrdered <-
        first
          (ContextGaloisUpperTargetOrderLookupFailed leftTarget rightTarget)
          (leqContext targetLattice leftTarget rightTarget)
      let leftSource = upper leftTarget
          rightSource = upper rightTarget
      sourceOrdered <-
        first
          (ContextGaloisUpperSourceOrderLookupFailed leftTarget rightTarget leftSource rightSource)
          (leqContext sourceLattice leftSource rightSource)
      unless (not targetOrdered || sourceOrdered) $
        Left (ContextGaloisUpperNotMonotone leftTarget rightTarget leftSource rightSource)

    validateAdjunction (sourceObject, targetObject) = do
      let lowerSource = lower sourceObject
          upperTarget = upper targetObject
      lowerSourceLeqTarget <-
        first
          (ContextGaloisAdjunctionTargetOrderLookupFailed sourceObject targetObject lowerSource)
          (leqContext targetLattice lowerSource targetObject)
      sourceLeqUpperTarget <-
        first
          (ContextGaloisAdjunctionSourceOrderLookupFailed sourceObject targetObject upperTarget)
          (leqContext sourceLattice sourceObject upperTarget)
      unless (lowerSourceLeqTarget == sourceLeqUpperTarget) $
        Left
          ( ContextGaloisAdjunctionFailed
              sourceObject
              targetObject
              lowerSource
              upperTarget
              lowerSourceLeqTarget
              sourceLeqUpperTarget
          )

restrictFiniteContextPresheaf ::
  (Ord sourceCtx, Ord targetCtx, Ord value) =>
  ContextGaloisMap sourceCtx targetCtx ->
  FinitePresheaf (FiniteMeetSite targetCtx) value mismatch targetFailure ->
  Either
    (ContextRestrictPresheafFailure sourceCtx targetCtx value mismatch targetFailure)
    ( FinitePresheaf
        (FiniteMeetSite sourceCtx)
        value
        mismatch
        (ContextRestrictRestrictionFailure sourceCtx targetCtx value targetFailure)
    )
restrictFiniteContextPresheaf galois targetPresheaf = do
  unless (fpSite targetPresheaf == targetSite) $
    Left
      ( ContextRestrictTargetSiteMismatch
          (siteObjects targetSite)
          (siteObjects (fpSite targetPresheaf))
      )
  transportFinitePresheaf
    sourceSite
    lower
    ContextRestrictFiberMissing
    (lowerMorphism galois)
    ContextRestrictMorphismMissing
    ContextRestrictTargetRestrictionFailed
    ContextRestrictPresheafInvalid
    targetPresheaf
  where
    sourceSite =
      cgmSourceSite galois

    targetSite =
      cgmTargetSite galois

    lower =
      cgmLower galois

extendFiniteContextPresheaf ::
  (Ord sourceCtx, Ord targetCtx, Ord value) =>
  ContextGaloisMap sourceCtx targetCtx ->
  FinitePresheaf (FiniteMeetSite sourceCtx) value mismatch sourceFailure ->
  Either
    (ContextExtendPresheafFailure sourceCtx targetCtx value mismatch sourceFailure)
    ( FinitePresheaf
        (FiniteMeetSite targetCtx)
        value
        mismatch
        (ContextExtendRestrictionFailure sourceCtx targetCtx value sourceFailure)
    )
extendFiniteContextPresheaf galois sourcePresheaf = do
  unless (fpSite sourcePresheaf == sourceSite) $
    Left
      ( ContextExtendSourceSiteMismatch
          (siteObjects sourceSite)
          (siteObjects (fpSite sourcePresheaf))
      )
  transportFinitePresheaf
    targetSite
    upper
    ContextExtendFiberMissing
    (upperMorphism galois)
    ContextExtendMorphismMissing
    ContextExtendSourceRestrictionFailed
    ContextExtendPresheafInvalid
    sourcePresheaf
  where
    sourceSite =
      cgmSourceSite galois

    targetSite =
      cgmTargetSite galois

    upper =
      cgmUpper galois

transportFinitePresheaf ::
  (Site outputSite, Site inputSite, Ord value) =>
  outputSite ->
  (SiteObject outputSite -> SiteObject inputSite) ->
  (SiteObject outputSite -> SiteObject inputSite -> outerFailure) ->
  ( CheckedMorphism (SiteObject outputSite) (SiteMorphism outputSite) ->
    Maybe (CheckedMorphism (SiteObject inputSite) (SiteMorphism inputSite))
  ) ->
  ( CheckedMorphism (SiteObject outputSite) (SiteMorphism outputSite) ->
    SiteObject inputSite ->
    SiteObject inputSite ->
    restrictionFailure
  ) ->
  ( CheckedMorphism (SiteObject outputSite) (SiteMorphism outputSite) ->
    CheckedMorphism (SiteObject inputSite) (SiteMorphism inputSite) ->
    value ->
    inputFailure ->
    restrictionFailure
  ) ->
  ( FinitePresheafFailure
      (SiteObject outputSite)
      (SiteMorphism outputSite)
      value
      mismatch
      restrictionFailure ->
    outerFailure
  ) ->
  FinitePresheaf inputSite value mismatch inputFailure ->
  Either outerFailure (FinitePresheaf outputSite value mismatch restrictionFailure)
transportFinitePresheaf outputSite objectImage fiberMissing morphismImage morphismMissing restrictionFailed invalidFailure inputPresheaf = do
  rawFibers <-
    Map.fromList
      <$> traverse rawFiberAtOutput (siteObjects outputSite)
  first invalidFailure $
    mkFinitePresheaf
      outputSite
      restrictAlongImage
      mismatchAtOutput
      normalizeAtOutput
      rawFibers
  where
    rawFiberAtOutput outputObject = do
      let inputObject = objectImage outputObject
      inputFiber <-
        note
          (fiberMissing outputObject inputObject)
          (finiteFiberAt inputObject inputPresheaf)
      pure (outputObject, finiteFiberValues inputFiber)

    restrictAlongImage outputMorphism value = do
      inputMorphism <-
        note
          (morphismMissing outputMorphism (objectImage (cmSource outputMorphism)) (objectImage (cmTarget outputMorphism)))
          (morphismImage outputMorphism)
      first
        (restrictionFailed outputMorphism inputMorphism value)
        (fpRestrict inputPresheaf inputMorphism value)

    mismatchAtOutput outputObject =
      fpMismatches inputPresheaf (objectImage outputObject)

    normalizeAtOutput outputObject =
      fpNormalize inputPresheaf (objectImage outputObject)

checkContextImageAdjunction ::
  (Ord sourceCtx, Ord targetCtx, Ord value) =>
  ContextGaloisMap sourceCtx targetCtx ->
  FinitePresheaf (FiniteMeetSite sourceCtx) value mismatch sourceFailure ->
  FinitePresheaf (FiniteMeetSite targetCtx) value mismatch targetFailure ->
  Either
    (ContextImageAdjunctionFailure sourceCtx targetCtx value mismatch sourceFailure targetFailure)
    (ContextImageAdjunction sourceCtx targetCtx value mismatch sourceFailure targetFailure)
checkContextImageAdjunction galois sourcePresheaf targetPresheaf = do
  extendedSource <-
    first
      (FiniteImageAdjunctionImageInvalid . ContextImageAdjunctionExtendSourceFailed)
      (extendFiniteContextPresheaf galois sourcePresheaf)
  restrictedExtendedSource <-
    first
      (FiniteImageAdjunctionImageInvalid . ContextImageAdjunctionRestrictExtendedSourceFailed)
      (restrictFiniteContextPresheaf galois extendedSource)
  restrictedTarget <-
    first
      (FiniteImageAdjunctionImageInvalid . ContextImageAdjunctionRestrictTargetFailed)
      (restrictFiniteContextPresheaf galois targetPresheaf)
  extendedRestrictedTarget <-
    first
      (FiniteImageAdjunctionImageInvalid . ContextImageAdjunctionExtendRestrictedTargetFailed)
      (extendFiniteContextPresheaf galois restrictedTarget)
  unitMorphism <-
    first
      FiniteImageAdjunctionUnitInvalid
      (mkFinitePresheafMorphism sourcePresheaf restrictedExtendedSource (contextUnitComponent galois sourcePresheaf))
  counitMorphism <-
    first
      FiniteImageAdjunctionCounitInvalid
      (mkFinitePresheafMorphism extendedRestrictedTarget targetPresheaf (contextCounitComponent galois targetPresheaf))
  pure
    FiniteImageAdjunction
      { finiteImageAdjunctionUnit = unitMorphism,
        finiteImageAdjunctionCounit = counitMorphism,
        finiteImageAdjunctionLeftTriangleFailures =
          concatMap extendTriangleFailuresAt (siteObjects targetSite),
        finiteImageAdjunctionRightTriangleFailures =
          concatMap restrictTriangleFailuresAt (siteObjects sourceSite)
      }
  where
    sourceSite =
      cgmSourceSite galois

    targetSite =
      cgmTargetSite galois

    lower =
      cgmLower galois

    upper =
      cgmUpper galois

    extendTriangleFailuresAt targetObject =
      let sourceObject = upper targetObject
          lowerUpperTarget = lower sourceObject
          sourceAfterUnit = upper lowerUpperTarget
       in triangleRestrictionFailures
            sourcePresheaf
            sourceObject
            sourceAfterUnit

    restrictTriangleFailuresAt sourceObject =
      let targetObject = lower sourceObject
          upperLowerSource = upper targetObject
          targetAfterUnit = lower upperLowerSource
       in triangleRestrictionFailures
            targetPresheaf
            targetObject
            targetAfterUnit

contextUnitComponent ::
  Ord sourceCtx =>
  ContextGaloisMap sourceCtx targetCtx ->
  FinitePresheaf (FiniteMeetSite sourceCtx) value mismatch sourceFailure ->
  sourceCtx ->
  value ->
  Either (ContextImageUnitComponentFailure sourceCtx sourceFailure) value
contextUnitComponent galois sourcePresheaf sourceObject value = do
  let unitTarget =
        cgmUpper galois (cgmLower galois sourceObject)
  unitMorphism <-
    note
      (ContextImageUnitComponentMorphismMissing sourceObject unitTarget)
      (finiteMeetMorphism (cgmSourceSite galois) unitTarget sourceObject)
  first
    (ContextImageUnitComponentRestrictionFailed unitMorphism)
    (fpRestrict sourcePresheaf unitMorphism value)

contextCounitComponent ::
  Ord targetCtx =>
  ContextGaloisMap sourceCtx targetCtx ->
  FinitePresheaf (FiniteMeetSite targetCtx) value mismatch targetFailure ->
  targetCtx ->
  value ->
  Either (ContextImageCounitComponentFailure targetCtx targetFailure) value
contextCounitComponent galois targetPresheaf targetObject value = do
  let counitDomain =
        cgmLower galois (cgmUpper galois targetObject)
  counitMorphism <-
    note
      (ContextImageCounitComponentMorphismMissing targetObject counitDomain)
      (finiteMeetMorphism (cgmTargetSite galois) targetObject counitDomain)
  first
    (ContextImageCounitComponentRestrictionFailed counitMorphism)
    (fpRestrict targetPresheaf counitMorphism value)

triangleRestrictionFailures ::
  Ord context =>
  FinitePresheaf (FiniteMeetSite context) value mismatch restrictionFailure ->
  context ->
  context ->
  [FiniteImageTriangleFailure context (FiniteMeetMorphism context) value mismatch restrictionFailure]
triangleRestrictionFailures presheaf objectValue turnObject =
  case
    ( finiteFiberAt objectValue presheaf,
      finiteMeetMorphism (fpSite presheaf) turnObject objectValue,
      finiteMeetMorphism (fpSite presheaf) objectValue turnObject
    )
    of
      (Nothing, _, _) ->
        [FiniteImageTriangleFiberMissing objectValue]
      (_, Nothing, _) ->
        [FiniteImageTriangleFirstMorphismMissing objectValue turnObject]
      (_, _, Nothing) ->
        [FiniteImageTriangleSecondMorphismMissing objectValue turnObject]
      (Just fiberValue, Just firstMorphism, Just secondMorphism) ->
        concatMap
          (checkTriangleValue firstMorphism secondMorphism)
          (finiteFiberValues fiberValue)
  where
    checkTriangleValue firstMorphism secondMorphism value =
      case fpRestrict presheaf firstMorphism value of
        Left failure ->
          [FiniteImageTriangleFirstRestrictionFailed firstMorphism value failure]
        Right afterFirst ->
          case fpRestrict presheaf secondMorphism afterFirst of
            Left failure ->
              [FiniteImageTriangleSecondRestrictionFailed secondMorphism afterFirst failure]
            Right afterSecond ->
              let mismatches =
                    fpMismatches presheaf objectValue afterSecond value
               in [FiniteImageTriangleMismatch objectValue value afterSecond mismatches | not (null mismatches)]

lowerMorphism ::
  Ord targetCtx =>
  ContextGaloisMap sourceCtx targetCtx ->
  CheckedMorphism sourceCtx (FiniteMeetMorphism sourceCtx) ->
  Maybe (CheckedMorphism targetCtx (FiniteMeetMorphism targetCtx))
lowerMorphism galois sourceMorphism =
  finiteMeetMorphism
    (cgmTargetSite galois)
    (cgmLower galois (cmSource sourceMorphism))
    (cgmLower galois (cmTarget sourceMorphism))
{-# INLINE lowerMorphism #-}

upperMorphism ::
  Ord sourceCtx =>
  ContextGaloisMap sourceCtx targetCtx ->
  CheckedMorphism targetCtx (FiniteMeetMorphism targetCtx) ->
  Maybe (CheckedMorphism sourceCtx (FiniteMeetMorphism sourceCtx))
upperMorphism galois targetMorphism =
  finiteMeetMorphism
    (cgmSourceSite galois)
    (cgmUpper galois (cmSource targetMorphism))
    (cgmUpper galois (cmTarget targetMorphism))
{-# INLINE upperMorphism #-}

note :: failure -> Maybe value -> Either failure value
note failure =
  maybe (Left failure) Right
{-# INLINE note #-}
