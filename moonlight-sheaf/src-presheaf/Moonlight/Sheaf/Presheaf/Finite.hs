{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Presheaf.Finite
  ( FiberKey (..),
    FiniteFiber (..),
    FinitePresheaf (..),
    FinitePresheafFailure (..),
    finiteFiberAt,
    finiteFiberValues,
    finiteFiberKeys,
    finiteFiberKeyIntSet,
    finiteFiberValueAt,
    finiteFiberKeyOf,
    finiteFiberContains,
    mkFinitePresheaf,
    validateFinitePresheafLaws,
    finitePresheafFromStalkAlgebra,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.IntSet (IntSet)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Void (Void)
import Moonlight.Core
  ( duplicatesOrd,
  )
import Moonlight.Core (DenseKey (..))
import Moonlight.Sheaf.Index.Dense
  ( DenseIndex,
    denseIndexContains,
    denseIndexKeyIntSet,
    denseIndexKeyOf,
    denseIndexKeys,
    denseIndexValueAt,
    denseIndexValues,
    mkDenseIndex,
  )
import Moonlight.Sheaf.Presheaf.Core
  ( CompiledRestriction (..),
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectIndex,
    mkObjectIndex,
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra,
    normalizeStalk,
    restrictStalk,
    stalkMismatches,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
  )

type FiberKey :: Type
newtype FiberKey = FiberKey
  { unFiberKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

instance DenseKey FiberKey where
  encodeDenseKey =
    unFiberKey
  {-# INLINE encodeDenseKey #-}

  decodeDenseKey =
    FiberKey
  {-# INLINE decodeDenseKey #-}

type FiniteFiber :: Type -> Type -> Type
data FiniteFiber obj value = FiniteFiber
  { ffObject :: !obj,
    ffValues :: !(DenseIndex FiberKey value)
  }
  deriving stock (Eq, Show)

type FinitePresheaf :: Type -> Type -> Type -> Type -> Type
data FinitePresheaf site value mismatch restrictionFailure = FinitePresheaf
  { fpSite :: !site,
    fpObjectIndex :: !(ObjectIndex (SiteObject site)),
    fpFibers :: !(Map (SiteObject site) (FiniteFiber (SiteObject site) value)),
    fpRestrict ::
      CheckedMorphism (SiteObject site) (SiteMorphism site) ->
      value ->
      Either restrictionFailure value,
    fpMismatches ::
      SiteObject site ->
      value ->
      value ->
      [mismatch],
    fpNormalize ::
      SiteObject site ->
      value ->
      value
  }

type FinitePresheafFailure :: Type -> Type -> Type -> Type -> Type -> Type
data FinitePresheafFailure obj mor value mismatch restrictionFailure
  = FiniteSiteDuplicateObject !obj
  | FiniteFiberMissing !obj
  | FiniteFiberUnknownObject !obj
  | FiniteFiberValueNotNormalized !obj !value !value
  | FiniteFiberDuplicateValue !obj !value
  | FiniteRestrictionSourceFiberMissing !(CheckedMorphism obj mor)
  | FiniteRestrictionTargetFiberMissing !(CheckedMorphism obj mor)
  | FiniteRestrictionFailed !(CheckedMorphism obj mor) !value !restrictionFailure
  | FiniteRestrictionOutsideFiber !(CheckedMorphism obj mor) !value !value
  | FiniteIdentityRestrictionMismatch !(CheckedMorphism obj mor) !value ![mismatch]
  | FiniteCompositionUndefined !(CheckedMorphism obj mor) !(CheckedMorphism obj mor)
  | FiniteCompositionRestrictionMismatch
      !(CheckedMorphism obj mor)
      !(CheckedMorphism obj mor)
      !(CheckedMorphism obj mor)
      !value
      ![mismatch]
  deriving stock (Eq, Show)

finiteFiberAt ::
  Site site =>
  SiteObject site ->
  FinitePresheaf site value mismatch restrictionFailure ->
  Maybe (FiniteFiber (SiteObject site) value)
finiteFiberAt objectValue =
  Map.lookup objectValue . fpFibers
{-# INLINE finiteFiberAt #-}

finiteFiberValues :: FiniteFiber obj value -> [value]
finiteFiberValues =
  denseIndexValues . ffValues
{-# INLINE finiteFiberValues #-}

finiteFiberKeys :: FiniteFiber obj value -> [FiberKey]
finiteFiberKeys =
  denseIndexKeys . ffValues
{-# INLINE finiteFiberKeys #-}

finiteFiberKeyIntSet :: FiniteFiber obj value -> IntSet
finiteFiberKeyIntSet =
  denseIndexKeyIntSet . finiteFiberKeys
{-# INLINE finiteFiberKeyIntSet #-}

finiteFiberValueAt :: FiberKey -> FiniteFiber obj value -> Maybe value
finiteFiberValueAt key =
  denseIndexValueAt key . ffValues
{-# INLINE finiteFiberValueAt #-}

finiteFiberKeyOf :: Ord value => value -> FiniteFiber obj value -> Maybe FiberKey
finiteFiberKeyOf value =
  denseIndexKeyOf value . ffValues
{-# INLINE finiteFiberKeyOf #-}

finiteFiberContains :: Ord value => value -> FiniteFiber obj value -> Bool
finiteFiberContains value =
  denseIndexContains value . ffValues
{-# INLINE finiteFiberContains #-}

mkFinitePresheaf ::
  (Site site, Ord value) =>
  site ->
  (CheckedMorphism (SiteObject site) (SiteMorphism site) -> value -> Either restrictionFailure value) ->
  (SiteObject site -> value -> value -> [mismatch]) ->
  (SiteObject site -> value -> value) ->
  Map (SiteObject site) [value] ->
  Either
    (FinitePresheafFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    (FinitePresheaf site value mismatch restrictionFailure)
mkFinitePresheaf site restrictAction mismatchAt normalizeAt rawFibers = do
  traverse_ (Left . FiniteSiteDuplicateObject) (duplicatesOrd (siteObjects site))
  traverse_ validateKnownFiberObject (Map.keys rawFibers)
  fibers <-
    Map.fromList
      <$> traverse buildObjectFiber (siteObjects site)
  let presheaf =
        FinitePresheaf
          { fpSite = site,
            fpObjectIndex = mkObjectIndex (siteObjects site),
            fpFibers = fibers,
            fpRestrict = restrictAction,
            fpMismatches = mismatchAt,
            fpNormalize = normalizeAt
          }
  validateFinitePresheafLaws presheaf
  pure presheaf
  where
    objectSet =
      Set.fromList (siteObjects site)

    validateKnownFiberObject objectValue =
      if Set.member objectValue objectSet
        then Right ()
        else Left (FiniteFiberUnknownObject objectValue)

    buildObjectFiber objectValue = do
      values <-
        maybe
          (Left (FiniteFiberMissing objectValue))
          Right
          (Map.lookup objectValue rawFibers)
      traverse_ (validateNormalized objectValue) values
      case duplicatesOrd values of
        duplicateValue : _ ->
          Left (FiniteFiberDuplicateValue objectValue duplicateValue)
        [] ->
          Right
            ( objectValue,
              FiniteFiber
                { ffObject = objectValue,
                  ffValues = mkDenseIndex values
                }
            )

    validateNormalized objectValue value =
      let normalizedValue = normalizeAt objectValue value
       in if normalizedValue == value
            then Right ()
            else Left (FiniteFiberValueNotNormalized objectValue value normalizedValue)

finitePresheafFromStalkAlgebra ::
  (Site site, Ord stalk) =>
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  site ->
  Map (SiteObject site) [stalk] ->
  Either
    (FinitePresheafFailure (SiteObject site) (SiteMorphism site) stalk mismatch Void)
    (FinitePresheaf site stalk mismatch Void)
finitePresheafFromStalkAlgebra stalkAlgebra site =
  mkFinitePresheaf
    site
    (\morphismValue stalkValue -> Right (restrictStalk stalkAlgebra (CompiledRestriction site morphismValue) stalkValue))
    (\_objectValue leftValue rightValue -> stalkMismatches stalkAlgebra leftValue rightValue)
    (\_objectValue stalkValue -> normalizeStalk stalkAlgebra stalkValue)

validateFinitePresheafLaws ::
  (Site site, Ord value) =>
  FinitePresheaf site value mismatch restrictionFailure ->
  Either
    (FinitePresheafFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    ()
validateFinitePresheafLaws presheaf = do
  traverse_ (validateRestrictionClosed presheaf) (siteMorphisms (fpSite presheaf))
  traverse_ (validateIdentity presheaf) (siteObjects (fpSite presheaf))
  traverse_ (validateComposition presheaf) composablePairs
  where
    morphismsByTarget =
      Map.fromListWith
        (flip (<>))
        [ (cmTarget morphismValue, [morphismValue])
        | morphismValue <- siteMorphisms (fpSite presheaf)
        ]

    composablePairs =
      [ (outerMorphism, innerMorphism)
      | outerMorphism <- siteMorphisms (fpSite presheaf),
        innerMorphism <- Map.findWithDefault [] (cmSource outerMorphism) morphismsByTarget
      ]

validateRestrictionClosed ::
  (Site site, Ord value) =>
  FinitePresheaf site value mismatch restrictionFailure ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  Either
    (FinitePresheafFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    ()
validateRestrictionClosed presheaf morphismValue = do
  sourceFiber <-
    maybe
      (Left (FiniteRestrictionSourceFiberMissing morphismValue))
      Right
      (finiteFiberAt (cmSource morphismValue) presheaf)
  targetFiber <-
    maybe
      (Left (FiniteRestrictionTargetFiberMissing morphismValue))
      Right
      (finiteFiberAt (cmTarget morphismValue) presheaf)
  traverse_
    (validateOneRestriction sourceFiber)
    (finiteFiberValues targetFiber)
  where
    validateOneRestriction sourceFiber value = do
      restrictedValue <-
        first
          (FiniteRestrictionFailed morphismValue value)
          (fpRestrict presheaf morphismValue value)
      if finiteFiberContains restrictedValue sourceFiber
        then Right ()
        else Left (FiniteRestrictionOutsideFiber morphismValue value restrictedValue)

validateIdentity ::
  Site site =>
  FinitePresheaf site value mismatch restrictionFailure ->
  SiteObject site ->
  Either
    (FinitePresheafFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    ()
validateIdentity presheaf objectValue = do
  fiberValue <-
    maybe
      (Left (FiniteFiberMissing objectValue))
      Right
      (finiteFiberAt objectValue presheaf)
  traverse_ validateValue (finiteFiberValues fiberValue)
  where
    identityMorphism =
      identityAt (fpSite presheaf) objectValue

    validateValue value = do
      restrictedValue <-
        first
          (FiniteRestrictionFailed identityMorphism value)
          (fpRestrict presheaf identityMorphism value)
      let mismatches =
            fpMismatches presheaf objectValue restrictedValue value
      if null mismatches
        then Right ()
        else Left (FiniteIdentityRestrictionMismatch identityMorphism value mismatches)

validateComposition ::
  Site site =>
  FinitePresheaf site value mismatch restrictionFailure ->
  ( CheckedMorphism (SiteObject site) (SiteMorphism site),
    CheckedMorphism (SiteObject site) (SiteMorphism site)
  ) ->
  Either
    (FinitePresheafFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    ()
validateComposition presheaf (outerMorphism, innerMorphism) =
  case composeChecked (fpSite presheaf) outerMorphism innerMorphism of
    Nothing ->
      Left (FiniteCompositionUndefined outerMorphism innerMorphism)
    Just compositeMorphism -> do
      targetFiber <-
        maybe
          (Left (FiniteRestrictionTargetFiberMissing outerMorphism))
          Right
          (finiteFiberAt (cmTarget outerMorphism) presheaf)
      traverse_
        (validateValue compositeMorphism)
        (finiteFiberValues targetFiber)
  where
    validateValue compositeMorphism value = do
      outerRestricted <-
        first
          (FiniteRestrictionFailed outerMorphism value)
          (fpRestrict presheaf outerMorphism value)
      sequentialRestricted <-
        first
          (FiniteRestrictionFailed innerMorphism outerRestricted)
          (fpRestrict presheaf innerMorphism outerRestricted)
      directRestricted <-
        first
          (FiniteRestrictionFailed compositeMorphism value)
          (fpRestrict presheaf compositeMorphism value)
      let mismatches =
            fpMismatches
              presheaf
              (cmSource innerMorphism)
              sequentialRestricted
              directRestricted
      if null mismatches
        then Right ()
        else
          Left
            ( FiniteCompositionRestrictionMismatch
                outerMorphism
                innerMorphism
                compositeMorphism
                value
                mismatches
            )
