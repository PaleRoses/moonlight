{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Presheaf.Morphism
  ( FinitePresheafMorphism,
    FinitePresheafMorphismFailure (..),
    FinitePresheafMorphismCompositionComponentFailure (..),
    FinitePresheafMorphismCompositionFailure (..),
    finitePresheafMorphismSource,
    finitePresheafMorphismTarget,
    finitePresheafMorphismComponentMap,
    finitePresheafMorphismComponents,
    finitePresheafMorphismComponentAt,
    mkFinitePresheafMorphism,
    identityFinitePresheafMorphism,
    composeFinitePresheafMorphisms,
    composeAlignedFinitePresheafMorphisms,
  )
where

import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Sheaf.Presheaf.Finite
  ( FinitePresheaf (..),
    finiteFiberAt,
    finiteFiberContains,
    finiteFiberValues,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
  )

type FinitePresheafMorphism :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data FinitePresheafMorphism site sourceValue targetValue sourceMismatch targetMismatch sourceRestrictionFailure targetRestrictionFailure =
  FinitePresheafMorphism
    { fpmSource :: !(FinitePresheaf site sourceValue sourceMismatch sourceRestrictionFailure),
      fpmTarget :: !(FinitePresheaf site targetValue targetMismatch targetRestrictionFailure),
      fpmComponents :: !(Map (SiteObject site) (Map sourceValue targetValue))
    }

type FinitePresheafMorphismFailure :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data FinitePresheafMorphismFailure obj mor sourceValue targetValue sourceRestrictionFailure targetRestrictionFailure targetMismatch componentFailure
  = FinitePresheafMorphismObjectMismatch ![obj] ![obj]
  | FinitePresheafMorphismMorphismMismatch
      ![CheckedMorphism obj mor]
      ![CheckedMorphism obj mor]
  | FinitePresheafMorphismSourceFiberMissing !obj
  | FinitePresheafMorphismTargetFiberMissing !obj
  | FinitePresheafMorphismComponentFailed !obj !sourceValue !componentFailure
  | FinitePresheafMorphismComponentOutsideTargetFiber !obj !sourceValue !targetValue
  | FinitePresheafMorphismComponentMissing !obj !sourceValue
  | FinitePresheafMorphismSourceRestrictionFailed
      !(CheckedMorphism obj mor)
      !sourceValue
      !sourceRestrictionFailure
  | FinitePresheafMorphismTargetRestrictionFailed
      !(CheckedMorphism obj mor)
      !targetValue
      !targetRestrictionFailure
  | FinitePresheafMorphismNaturalityMismatch
      !(CheckedMorphism obj mor)
      !sourceValue
      !sourceValue
      !targetValue
      !targetValue
      ![targetMismatch]
  deriving stock (Eq, Show)

type FinitePresheafMorphismCompositionComponentFailure :: Type -> Type -> Type -> Type
data FinitePresheafMorphismCompositionComponentFailure obj sourceValue middleValue
  = FinitePresheafMorphismCompositionInnerComponentMissing !obj !sourceValue
  | FinitePresheafMorphismCompositionOuterComponentMissing !obj !middleValue
  deriving stock (Eq, Show)

type FinitePresheafMorphismCompositionFailure ::
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type
data FinitePresheafMorphismCompositionFailure
  obj
  mor
  sourceValue
  middleValue
  targetValue
  middleMismatch
  sourceRestrictionFailure
  middleRestrictionFailure
  targetRestrictionFailure
  targetMismatch
  = FinitePresheafMorphismCompositionObjectMismatch ![obj] ![obj]
  | FinitePresheafMorphismCompositionMorphismMismatch
      ![CheckedMorphism obj mor]
      ![CheckedMorphism obj mor]
  | FinitePresheafMorphismCompositionInnerMiddleFiberMissing !obj
  | FinitePresheafMorphismCompositionOuterMiddleFiberMissing !obj
  | FinitePresheafMorphismCompositionMiddleFiberMismatch !obj ![middleValue] ![middleValue]
  | FinitePresheafMorphismCompositionInnerMiddleRestrictionFailed
      !(CheckedMorphism obj mor)
      !middleValue
      !middleRestrictionFailure
  | FinitePresheafMorphismCompositionOuterMiddleRestrictionFailed
      !(CheckedMorphism obj mor)
      !middleValue
      !middleRestrictionFailure
  | FinitePresheafMorphismCompositionMiddleRestrictionMismatch
      !(CheckedMorphism obj mor)
      !middleValue
      !middleValue
      !middleValue
  | FinitePresheafMorphismCompositionMiddleMismatchRelationMismatch
      !obj
      !middleValue
      !middleValue
      ![middleMismatch]
      ![middleMismatch]
  | FinitePresheafMorphismCompositionInvalidResult
      !( FinitePresheafMorphismFailure
           obj
           mor
           sourceValue
           targetValue
           sourceRestrictionFailure
           targetRestrictionFailure
           targetMismatch
           (FinitePresheafMorphismCompositionComponentFailure obj sourceValue middleValue)
       )
  deriving stock (Eq, Show)

finitePresheafMorphismSource ::
  FinitePresheafMorphism site sourceValue targetValue sourceMismatch targetMismatch sourceRestrictionFailure targetRestrictionFailure ->
  FinitePresheaf site sourceValue sourceMismatch sourceRestrictionFailure
finitePresheafMorphismSource =
  fpmSource
{-# INLINE finitePresheafMorphismSource #-}

finitePresheafMorphismTarget ::
  FinitePresheafMorphism site sourceValue targetValue sourceMismatch targetMismatch sourceRestrictionFailure targetRestrictionFailure ->
  FinitePresheaf site targetValue targetMismatch targetRestrictionFailure
finitePresheafMorphismTarget =
  fpmTarget
{-# INLINE finitePresheafMorphismTarget #-}

finitePresheafMorphismComponentMap ::
  FinitePresheafMorphism site sourceValue targetValue sourceMismatch targetMismatch sourceRestrictionFailure targetRestrictionFailure ->
  Map (SiteObject site) (Map sourceValue targetValue)
finitePresheafMorphismComponentMap =
  fpmComponents
{-# INLINE finitePresheafMorphismComponentMap #-}

finitePresheafMorphismComponents ::
  FinitePresheafMorphism site sourceValue targetValue sourceMismatch targetMismatch sourceRestrictionFailure targetRestrictionFailure ->
  Map (SiteObject site) [(sourceValue, targetValue)]
finitePresheafMorphismComponents =
  fmap Map.toAscList . fpmComponents
{-# INLINE finitePresheafMorphismComponents #-}

finitePresheafMorphismComponentAt ::
  (Site site, Ord sourceValue) =>
  SiteObject site ->
  sourceValue ->
  FinitePresheafMorphism site sourceValue targetValue sourceMismatch targetMismatch sourceRestrictionFailure targetRestrictionFailure ->
  Maybe targetValue
finitePresheafMorphismComponentAt objectValue sourceValue morphismValue = do
  componentMap <-
    Map.lookup objectValue (fpmComponents morphismValue)
  Map.lookup sourceValue componentMap
{-# INLINEABLE finitePresheafMorphismComponentAt #-}

mkFinitePresheafMorphism ::
  forall site sourceValue targetValue sourceMismatch targetMismatch sourceRestrictionFailure targetRestrictionFailure componentFailure.
  (Site site, Eq (SiteMorphism site), Ord sourceValue, Ord targetValue) =>
  FinitePresheaf site sourceValue sourceMismatch sourceRestrictionFailure ->
  FinitePresheaf site targetValue targetMismatch targetRestrictionFailure ->
  (SiteObject site -> sourceValue -> Either componentFailure targetValue) ->
  Either
    ( FinitePresheafMorphismFailure
        (SiteObject site)
        (SiteMorphism site)
        sourceValue
        targetValue
        sourceRestrictionFailure
        targetRestrictionFailure
        targetMismatch
        componentFailure
    )
    (FinitePresheafMorphism site sourceValue targetValue sourceMismatch targetMismatch sourceRestrictionFailure targetRestrictionFailure)
mkFinitePresheafMorphism sourcePresheaf targetPresheaf componentAction = do
  validateSameSite
  componentMaps <-
    Map.fromList
      <$> traverse
        componentMapAtObject
        sourceObjects
  let morphismValue =
        FinitePresheafMorphism
          { fpmSource = sourcePresheaf,
            fpmTarget = targetPresheaf,
            fpmComponents = componentMaps
          }
  traverse_ (validateNaturality morphismValue) sourceMorphisms
  pure morphismValue
  where
    sourceSite =
      fpSite sourcePresheaf

    targetSite =
      fpSite targetPresheaf

    sourceObjects =
      siteObjects sourceSite

    targetObjects =
      siteObjects targetSite

    sourceMorphisms =
      siteMorphisms sourceSite

    targetMorphisms =
      siteMorphisms targetSite

    validateSameSite = do
      unless (sourceObjects == targetObjects) $
        Left (FinitePresheafMorphismObjectMismatch sourceObjects targetObjects)
      unless (sourceMorphisms == targetMorphisms) $
        Left (FinitePresheafMorphismMorphismMismatch sourceMorphisms targetMorphisms)

    componentMapAtObject objectValue = do
      sourceFiber <-
        maybe
          (Left (FinitePresheafMorphismSourceFiberMissing objectValue))
          Right
          (finiteFiberAt objectValue sourcePresheaf)
      targetFiber <-
        maybe
          (Left (FinitePresheafMorphismTargetFiberMissing objectValue))
          Right
          (finiteFiberAt objectValue targetPresheaf)
      componentEntries <-
        traverse
          (componentEntry objectValue targetFiber)
          (finiteFiberValues sourceFiber)
      pure (objectValue, Map.fromList componentEntries)

    componentEntry objectValue targetFiber sourceValue = do
      targetValue <-
        first
          (FinitePresheafMorphismComponentFailed objectValue sourceValue)
          (componentAction objectValue sourceValue)
      if finiteFiberContains targetValue targetFiber
        then Right (sourceValue, targetValue)
        else Left (FinitePresheafMorphismComponentOutsideTargetFiber objectValue sourceValue targetValue)

    validateNaturality morphismValue siteMorphism =
      traverse_
        (validateNaturalityAtValue morphismValue siteMorphism)
        =<< targetFiberValues siteMorphism

    targetFiberValues siteMorphism = do
      targetFiber <-
        maybe
          (Left (FinitePresheafMorphismSourceFiberMissing (cmTarget siteMorphism)))
          Right
          (finiteFiberAt (cmTarget siteMorphism) sourcePresheaf)
      pure (finiteFiberValues targetFiber)

    validateNaturalityAtValue morphismValue siteMorphism sourceValueAtTarget = do
      sourceRestricted <-
        first
          (FinitePresheafMorphismSourceRestrictionFailed siteMorphism sourceValueAtTarget)
          (fpRestrict sourcePresheaf siteMorphism sourceValueAtTarget)
      targetAfterSourceRestriction <-
        componentAt morphismValue (cmSource siteMorphism) sourceRestricted
      targetAtTarget <-
        componentAt morphismValue (cmTarget siteMorphism) sourceValueAtTarget
      targetRestricted <-
        first
          (FinitePresheafMorphismTargetRestrictionFailed siteMorphism targetAtTarget)
          (fpRestrict targetPresheaf siteMorphism targetAtTarget)
      let mismatches =
            fpMismatches targetPresheaf (cmSource siteMorphism) targetAfterSourceRestriction targetRestricted
      unless (null mismatches) $
        Left
          ( FinitePresheafMorphismNaturalityMismatch
              siteMorphism
              sourceValueAtTarget
              sourceRestricted
              targetAfterSourceRestriction
              targetRestricted
              mismatches
          )

    componentAt ::
      FinitePresheafMorphism site sourceValue targetValue sourceMismatch targetMismatch sourceRestrictionFailure targetRestrictionFailure ->
      SiteObject site ->
      sourceValue ->
      Either
        ( FinitePresheafMorphismFailure
            (SiteObject site)
            (SiteMorphism site)
            sourceValue
            targetValue
            sourceRestrictionFailure
            targetRestrictionFailure
            targetMismatch
            componentFailure
        )
        targetValue
    componentAt morphismValue objectValue sourceValue =
      maybe
        (Left (FinitePresheafMorphismComponentMissing objectValue sourceValue))
        Right
        (finitePresheafMorphismComponentAt objectValue sourceValue morphismValue)

identityFinitePresheafMorphism ::
  Ord value =>
  FinitePresheaf site value mismatch restrictionFailure ->
  FinitePresheafMorphism site value value mismatch mismatch restrictionFailure restrictionFailure
identityFinitePresheafMorphism presheafValue =
  FinitePresheafMorphism
    { fpmSource = presheafValue,
      fpmTarget = presheafValue,
      fpmComponents =
        fmap
          (Map.fromList . fmap (\value -> (value, value)) . finiteFiberValues)
          (fpFibers presheafValue)
    }
{-# INLINE identityFinitePresheafMorphism #-}

composeFinitePresheafMorphisms ::
  forall
    site
    sourceValue
    middleValue
    targetValue
    sourceMismatch
    middleMismatch
    targetMismatch
    sourceRestrictionFailure
    middleRestrictionFailure
    targetRestrictionFailure.
  (Site site, Eq (SiteMorphism site), Ord sourceValue, Ord middleValue, Ord targetValue) =>
  FinitePresheafMorphism
    site
    middleValue
    targetValue
    middleMismatch
    targetMismatch
    middleRestrictionFailure
    targetRestrictionFailure ->
  FinitePresheafMorphism
    site
    sourceValue
    middleValue
    sourceMismatch
    middleMismatch
    sourceRestrictionFailure
    middleRestrictionFailure ->
  Either
    ( FinitePresheafMorphismCompositionFailure
        (SiteObject site)
        (SiteMorphism site)
        sourceValue
        middleValue
        targetValue
        middleMismatch
        sourceRestrictionFailure
        middleRestrictionFailure
        targetRestrictionFailure
        targetMismatch
    )
    (FinitePresheafMorphism site sourceValue targetValue sourceMismatch targetMismatch sourceRestrictionFailure targetRestrictionFailure)
composeFinitePresheafMorphisms outerMorphism innerMorphism = do
  validateSameSite
  traverse_ validateMiddleFibers innerObjects
  traverse_ validateMiddleRestrictions innerMorphisms
  traverse_ validateMiddleMismatchRelation innerObjects
  first FinitePresheafMorphismCompositionInvalidResult $
    mkFinitePresheafMorphism
      (fpmSource innerMorphism)
      (fpmTarget outerMorphism)
      composedComponent
  where
    innerMiddle =
      fpmTarget innerMorphism

    outerMiddle =
      fpmSource outerMorphism

    innerSite =
      fpSite innerMiddle

    outerSite =
      fpSite outerMiddle

    innerObjects =
      siteObjects innerSite

    outerObjects =
      siteObjects outerSite

    innerMorphisms =
      siteMorphisms innerSite

    outerMorphisms =
      siteMorphisms outerSite

    validateSameSite = do
      unless (innerObjects == outerObjects) $
        Left (FinitePresheafMorphismCompositionObjectMismatch innerObjects outerObjects)
      unless (innerMorphisms == outerMorphisms) $
        Left (FinitePresheafMorphismCompositionMorphismMismatch innerMorphisms outerMorphisms)

    validateMiddleFibers objectValue = do
      innerValues <-
        finitePresheafFiberValuesOr
          FinitePresheafMorphismCompositionInnerMiddleFiberMissing
          objectValue
          innerMiddle
      outerValues <-
        finitePresheafFiberValuesOr
          FinitePresheafMorphismCompositionOuterMiddleFiberMissing
          objectValue
          outerMiddle
      unless (sameFiniteFiberValues innerValues outerValues) $
        Left
          ( FinitePresheafMorphismCompositionMiddleFiberMismatch
              objectValue
              innerValues
              outerValues
          )

    validateMiddleRestrictions morphismValue =
      traverse_ (validateMiddleRestrictionAtValue morphismValue)
        =<< finitePresheafFiberValuesOr
          FinitePresheafMorphismCompositionInnerMiddleFiberMissing
          (cmTarget morphismValue)
          innerMiddle

    validateMiddleRestrictionAtValue morphismValue middleValue = do
      innerRestricted <-
        first
          (FinitePresheafMorphismCompositionInnerMiddleRestrictionFailed morphismValue middleValue)
          (fpRestrict innerMiddle morphismValue middleValue)
      outerRestricted <-
        first
          (FinitePresheafMorphismCompositionOuterMiddleRestrictionFailed morphismValue middleValue)
          (fpRestrict outerMiddle morphismValue middleValue)
      unless (innerRestricted == outerRestricted) $
        Left
          ( FinitePresheafMorphismCompositionMiddleRestrictionMismatch
              morphismValue
              middleValue
              innerRestricted
              outerRestricted
          )

    validateMiddleMismatchRelation objectValue = do
      middleValues <-
        finitePresheafFiberValuesOr
          FinitePresheafMorphismCompositionInnerMiddleFiberMissing
          objectValue
          innerMiddle
      traverse_
        ( \leftValue ->
            traverse_
              (validateMiddleMismatchPair objectValue leftValue)
              middleValues
        )
        middleValues

    validateMiddleMismatchPair objectValue leftValue rightValue =
      let innerMismatches =
            fpMismatches innerMiddle objectValue leftValue rightValue
          outerMismatches =
            fpMismatches outerMiddle objectValue leftValue rightValue
       in unless (null innerMismatches == null outerMismatches) $
            Left
              ( FinitePresheafMorphismCompositionMiddleMismatchRelationMismatch
                  objectValue
                  leftValue
                  rightValue
                  innerMismatches
                  outerMismatches
              )

    composedComponent objectValue sourceValue = do
      middleValue <-
        maybe
          (Left (FinitePresheafMorphismCompositionInnerComponentMissing objectValue sourceValue))
          Right
          (finitePresheafMorphismComponentAt objectValue sourceValue innerMorphism)
      maybe
        (Left (FinitePresheafMorphismCompositionOuterComponentMissing objectValue middleValue))
        Right
        (finitePresheafMorphismComponentAt objectValue middleValue outerMorphism)

-- | Composition without middle reconciliation: sound only when both operands
-- were built against the same middle presheaf value; foreign middles must take
-- 'composeFinitePresheafMorphisms'.
composeAlignedFinitePresheafMorphisms ::
  forall
    site
    sourceValue
    middleValue
    targetValue
    sourceMismatch
    middleMismatch
    targetMismatch
    sourceRestrictionFailure
    middleRestrictionFailure
    targetRestrictionFailure.
  (Site site, Ord middleValue) =>
  FinitePresheafMorphism
    site
    middleValue
    targetValue
    middleMismatch
    targetMismatch
    middleRestrictionFailure
    targetRestrictionFailure ->
  FinitePresheafMorphism
    site
    sourceValue
    middleValue
    sourceMismatch
    middleMismatch
    sourceRestrictionFailure
    middleRestrictionFailure ->
  Either
    (FinitePresheafMorphismCompositionComponentFailure (SiteObject site) sourceValue middleValue)
    (FinitePresheafMorphism site sourceValue targetValue sourceMismatch targetMismatch sourceRestrictionFailure targetRestrictionFailure)
composeAlignedFinitePresheafMorphisms outerMorphism innerMorphism = do
  composedComponents <-
    Map.traverseWithKey
      composeObjectComponents
      (fpmComponents innerMorphism)
  pure
    FinitePresheafMorphism
      { fpmSource = fpmSource innerMorphism,
        fpmTarget = fpmTarget outerMorphism,
        fpmComponents = composedComponents
      }
  where
    composeObjectComponents objectValue =
      traverse (outerComponentAt objectValue)

    outerComponentAt objectValue middleValue =
      maybe
        (Left (FinitePresheafMorphismCompositionOuterComponentMissing objectValue middleValue))
        Right
        (finitePresheafMorphismComponentAt objectValue middleValue outerMorphism)

sameFiniteFiberValues :: Ord value => [value] -> [value] -> Bool
sameFiniteFiberValues leftValues rightValues =
  leftValues == rightValues
    || ( length leftValues == length rightValues
           && Set.fromList leftValues == Set.fromList rightValues
       )
{-# INLINE sameFiniteFiberValues #-}

finitePresheafFiberValuesOr ::
  Site site =>
  (SiteObject site -> failure) ->
  SiteObject site ->
  FinitePresheaf site value mismatch restrictionFailure ->
  Either failure [value]
finitePresheafFiberValuesOr failure objectValue presheafValue =
  maybe
    (Left (failure objectValue))
    (Right . finiteFiberValues)
    (finiteFiberAt objectValue presheafValue)
{-# INLINE finitePresheafFiberValuesOr #-}
