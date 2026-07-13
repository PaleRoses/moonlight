{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Cosheaf.Finite
  ( CostalkKey (..),
    FiniteCostalk (..),
    FiniteCosheafAlgebra (..),
    CompiledCorestriction (..),
    FiniteCosheaf,
    fcSite,
    fcSiteIndex,
    fcCostalks,
    fcCorestrictions,
    FiniteCosheafFailure (..),
    finiteCostalkAt,
    finiteCostalkAtObjectKey,
    finiteCostalkValues,
    finiteCostalkKeys,
    finiteCostalkKeyIntSet,
    finiteCostalkValueAt,
    finiteCostalkKeyOf,
    mkFiniteCosheaf,
    compiledCorestrictionFor,
    corestrictCostalkKey,
    finiteCosheafCorestrictions,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core (DenseKey (..))
import Moonlight.Core (duplicatesOrd)
import Moonlight.Cosheaf.Core
  ( CosheafLawFailure (..),
    checkCorestrictionCompositionDefined,
    checkCorestrictionCompositionLawWith,
    checkCorestrictionIdentityLawWith,
  )
import Moonlight.Cosheaf.SiteIndex
  ( CosheafMorphismKey,
    CosheafSiteIndex,
    CosheafSiteIndexFailure,
    IndexedCosheafMorphism (..),
    buildCosheafSiteIndex,
    cosheafCompositionValidationBasis,
    cosheafIndexedMorphisms,
    cosheafMorphismKeyOf,
    cosheafSiteObjectIndex,
  )
import Moonlight.Sheaf.Index.Dense
  ( DenseIndex,
    denseIndexKeyIntSet,
    denseIndexKeyOf,
    denseIndexKeys,
    denseIndexValueAt,
    denseIndexValues,
    mkDenseIndex,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
  )

type CostalkKey :: Type
newtype CostalkKey = CostalkKey
  { unCostalkKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

instance DenseKey CostalkKey where
  encodeDenseKey =
    unCostalkKey
  {-# INLINE encodeDenseKey #-}

  decodeDenseKey =
    CostalkKey
  {-# INLINE decodeDenseKey #-}

type FiniteCostalk :: Type -> Type -> Type
data FiniteCostalk obj value = FiniteCostalk
  { fcostalkObject :: !obj,
    fcostalkValues :: !(DenseIndex CostalkKey value)
  }
  deriving stock (Eq, Show)

type FiniteCosheafAlgebra :: Type -> Type -> Type -> Type -> Type
data FiniteCosheafAlgebra site value mismatch coreFailure = FiniteCosheafAlgebra
  { fcaCorestrict ::
      CheckedMorphism (SiteObject site) (SiteMorphism site) ->
      value ->
      Either coreFailure value,
    fcaMismatches ::
      SiteObject site ->
      value ->
      value ->
      [mismatch],
    fcaNormalize ::
      SiteObject site ->
      value ->
      value
  }

type CompiledCorestriction :: Type -> Type -> Type
data CompiledCorestriction obj mor = CompiledCorestriction
  { ccMorphismKey :: !CosheafMorphismKey,
    ccMorphism :: !(CheckedMorphism obj mor),
    ccSourceObjectKey :: !ObjectKey,
    ccTargetObjectKey :: !ObjectKey,
    ccSourceToTarget :: !(IntMap CostalkKey)
  }
  deriving stock (Eq, Show)

type FiniteCosheaf :: Type -> Type -> Type
data FiniteCosheaf site value = FiniteCosheaf
  { fcSite :: !site,
    fcSiteIndex :: !(CosheafSiteIndex site),
    fcCostalks :: !(IntMap (FiniteCostalk (SiteObject site) value)),
    fcCorestrictions :: !(IntMap (CompiledCorestriction (SiteObject site) (SiteMorphism site)))
  }

deriving stock instance
  (Eq site, Eq value, Eq (SiteObject site), Eq (SiteMorphism site)) =>
  Eq (FiniteCosheaf site value)

deriving stock instance
  (Show site, Show value, Show (SiteObject site), Show (SiteMorphism site)) =>
  Show (FiniteCosheaf site value)

type FiniteCosheafFailure :: Type -> Type -> Type -> Type -> Type -> Type
data FiniteCosheafFailure obj mor value mismatch coreFailure
  = FiniteCostalkMissing !obj
  | FiniteCostalkUnknownObject !obj
  | FiniteCostalkValueNotNormalized !obj !value !value
  | FiniteCostalkDuplicateValue !obj !value
  | CosheafSiteIndexInvalid !(CosheafSiteIndexFailure obj mor)
  | FiniteCosheafObjectKeyMissing !obj
  | FiniteCorestrictionSourceCostalkMissing !(CheckedMorphism obj mor)
  | FiniteCorestrictionTargetCostalkMissing !(CheckedMorphism obj mor)
  | FiniteCorestrictionFailed !(CheckedMorphism obj mor) !value !coreFailure
  | FiniteCorestrictionOutsideCostalk !(CheckedMorphism obj mor) !value !value
  | FiniteCorestrictionIdentityMismatch !(CheckedMorphism obj mor) !value ![mismatch]
  | FiniteCorestrictionCompositionUndefined !(CheckedMorphism obj mor) !(CheckedMorphism obj mor)
  | FiniteCorestrictionCompositionMismatch
      !(CheckedMorphism obj mor)
      !(CheckedMorphism obj mor)
      !(CheckedMorphism obj mor)
      !value
      ![mismatch]
  deriving stock (Eq, Show)

finiteFailureFromLawFailure ::
  CosheafLawFailure obj mor value mismatch coreFailure ->
  FiniteCosheafFailure obj mor value mismatch coreFailure
finiteFailureFromLawFailure failureValue =
  case failureValue of
    IdentityCorestrictionFailed morphismValue value coreFailure ->
      FiniteCorestrictionFailed morphismValue value coreFailure
    IdentityCorestrictionMismatch morphismValue value mismatches ->
      FiniteCorestrictionIdentityMismatch morphismValue value mismatches
    CompositionCorestrictionUndefined outerMorphism innerMorphism ->
      FiniteCorestrictionCompositionUndefined outerMorphism innerMorphism
    CompositionCorestrictionFailed morphismValue value coreFailure ->
      FiniteCorestrictionFailed morphismValue value coreFailure
    CompositionCorestrictionMismatch outerMorphism innerMorphism compositeMorphism value mismatches ->
      FiniteCorestrictionCompositionMismatch
        outerMorphism
        innerMorphism
        compositeMorphism
        value
        mismatches

finiteCostalkAt ::
  Site site =>
  SiteObject site ->
  FiniteCosheaf site value ->
  Maybe (FiniteCostalk (SiteObject site) value)
finiteCostalkAt objectValue cosheaf = do
  objectKey <-
    denseIndexKeyOf objectValue (cosheafSiteObjectIndex (fcSiteIndex cosheaf))
  finiteCostalkAtObjectKey objectKey cosheaf
{-# INLINE finiteCostalkAt #-}

finiteCostalkAtObjectKey ::
  ObjectKey ->
  FiniteCosheaf site value ->
  Maybe (FiniteCostalk (SiteObject site) value)
finiteCostalkAtObjectKey objectKey =
  IntMap.lookup (unObjectKey objectKey) . fcCostalks
{-# INLINE finiteCostalkAtObjectKey #-}

finiteCostalkValues :: FiniteCostalk obj value -> [value]
finiteCostalkValues =
  denseIndexValues . fcostalkValues
{-# INLINE finiteCostalkValues #-}

finiteCostalkKeys :: FiniteCostalk obj value -> [CostalkKey]
finiteCostalkKeys =
  denseIndexKeys . fcostalkValues
{-# INLINE finiteCostalkKeys #-}

finiteCostalkKeyIntSet :: FiniteCostalk obj value -> IntSet
finiteCostalkKeyIntSet =
  denseIndexKeyIntSet . finiteCostalkKeys
{-# INLINE finiteCostalkKeyIntSet #-}

finiteCostalkValueAt :: CostalkKey -> FiniteCostalk obj value -> Maybe value
finiteCostalkValueAt key =
  denseIndexValueAt key . fcostalkValues
{-# INLINE finiteCostalkValueAt #-}

finiteCostalkKeyOf :: Ord value => value -> FiniteCostalk obj value -> Maybe CostalkKey
finiteCostalkKeyOf value =
  denseIndexKeyOf value . fcostalkValues
{-# INLINE finiteCostalkKeyOf #-}

mkFiniteCosheaf ::
  (Site site, Ord (SiteMorphism site), Ord value) =>
  site ->
  FiniteCosheafAlgebra site value mismatch coreFailure ->
  Map (SiteObject site) [value] ->
  Either
    (FiniteCosheafFailure (SiteObject site) (SiteMorphism site) value mismatch coreFailure)
    (FiniteCosheaf site value)
mkFiniteCosheaf site algebra rawCostalks = do
  traverse_ validateKnownCostalkObject (Map.keys rawCostalks)
  siteIndex <-
    first CosheafSiteIndexInvalid (buildCosheafSiteIndex site)
  costalks <-
    IntMap.fromList
      <$> traverse (buildObjectCostalk siteIndex) (siteObjects site)
  corestrictions <-
    IntMap.fromList
      <$> traverse (compileCorestriction costalks) (cosheafIndexedMorphisms siteIndex)
  let cosheaf =
        FiniteCosheaf
          { fcSite = site,
            fcSiteIndex = siteIndex,
            fcCostalks = costalks,
            fcCorestrictions = corestrictions
          }
  traverse_ (validateIdentity cosheaf) (siteObjects site)
  traverse_ (validateComposition cosheaf) (cosheafCompositionValidationBasis siteIndex)
  pure cosheaf
  where
    objectSet =
      Set.fromList (siteObjects site)

    validateKnownCostalkObject objectValue =
      if Set.member objectValue objectSet
        then Right ()
        else Left (FiniteCostalkUnknownObject objectValue)

    buildObjectCostalk siteIndex objectValue = do
      objectKey <-
        maybe
          (Left (FiniteCosheafObjectKeyMissing objectValue))
          Right
          (denseIndexKeyOf objectValue (cosheafSiteObjectIndex siteIndex))
      values <-
        maybe
          (Left (FiniteCostalkMissing objectValue))
          Right
          (Map.lookup objectValue rawCostalks)
      traverse_ (validateNormalized objectValue) values
      case duplicatesOrd values of
        duplicateValue : _ ->
          Left (FiniteCostalkDuplicateValue objectValue duplicateValue)
        [] ->
          Right
            ( unObjectKey objectKey,
              FiniteCostalk
                { fcostalkObject = objectValue,
                  fcostalkValues = mkDenseIndex values
                }
            )

    validateNormalized objectValue value =
      let normalizedValue = fcaNormalize algebra objectValue value
       in if normalizedValue == value
            then Right ()
            else Left (FiniteCostalkValueNotNormalized objectValue value normalizedValue)

    compileCorestriction costalks indexedMorphism = do
      sourceCostalk <-
        maybe
          (Left (FiniteCorestrictionSourceCostalkMissing morphismValue))
          Right
          (IntMap.lookup (unObjectKey (icmSourceObjectKey indexedMorphism)) costalks)
      targetCostalk <-
        maybe
          (Left (FiniteCorestrictionTargetCostalkMissing morphismValue))
          Right
          (IntMap.lookup (unObjectKey (icmTargetObjectKey indexedMorphism)) costalks)
      sourceToTarget <-
        IntMap.fromList
          <$> traverse (compileOneSource sourceCostalk targetCostalk) (finiteCostalkKeys sourceCostalk)
      pure
        ( encodeDenseKey (icmKey indexedMorphism),
          CompiledCorestriction
            { ccMorphismKey = icmKey indexedMorphism,
              ccMorphism = morphismValue,
              ccSourceObjectKey = icmSourceObjectKey indexedMorphism,
              ccTargetObjectKey = icmTargetObjectKey indexedMorphism,
              ccSourceToTarget = sourceToTarget
            }
        )
      where
        morphismValue =
          icmMorphism indexedMorphism

        compileOneSource sourceCostalk targetCostalk sourceKey = do
          sourceValue <-
            maybe
              (Left (FiniteCorestrictionSourceCostalkMissing morphismValue))
              Right
              (finiteCostalkValueAt sourceKey sourceCostalk)
          targetValue <-
            first
              (FiniteCorestrictionFailed morphismValue sourceValue)
              (fcaCorestrict algebra morphismValue sourceValue)
          targetKey <-
            maybe
              (Left (FiniteCorestrictionOutsideCostalk morphismValue sourceValue targetValue))
              Right
              (finiteCostalkKeyOf targetValue targetCostalk)
          pure (unCostalkKey sourceKey, targetKey)


    validateIdentity cosheaf objectValue = do
      costalkValue <-
        maybe
          (Left (FiniteCostalkMissing objectValue))
          Right
          (finiteCostalkAt objectValue cosheaf)
      traverse_ (validateIdentityValue objectValue) (finiteCostalkValues costalkValue)

    validateIdentityValue objectValue value = do
      first finiteFailureFromLawFailure $
        checkCorestrictionIdentityLawWith
          (fcaCorestrict algebra)
          (fcaMismatches algebra)
          site
          objectValue
          value

    validateComposition cosheaf (outerIndexed, innerIndexed) = do
      compositeMorphism <-
        first finiteFailureFromLawFailure $
          checkCorestrictionCompositionDefined site outerMorphism innerMorphism
      sourceCostalk <-
        maybe
          (Left (FiniteCorestrictionSourceCostalkMissing innerMorphism))
          Right
          (finiteCostalkAt (cmSource innerMorphism) cosheaf)
      case compiledDisagreements cosheaf compositeMorphism of
        Nothing ->
          traverse_
            (validateCompositionValue outerMorphism innerMorphism)
            (finiteCostalkValues sourceCostalk)
        Just disagreements ->
          traverse_
            ( \sourceKey ->
                traverse_
                  (validateCompositionValue outerMorphism innerMorphism)
                  (finiteCostalkValueAt sourceKey sourceCostalk)
            )
            disagreements
      where
        outerMorphism =
          icmMorphism outerIndexed

        innerMorphism =
          icmMorphism innerIndexed

        compiledDisagreements cosheaf' compositeMorphism = do
          innerCompiled <-
            IntMap.lookup (encodeDenseKey (icmKey innerIndexed)) (fcCorestrictions cosheaf')
          outerCompiled <-
            IntMap.lookup (encodeDenseKey (icmKey outerIndexed)) (fcCorestrictions cosheaf')
          compositeCompiled <- compiledCorestrictionFor compositeMorphism cosheaf'
          let innerMap = ccSourceToTarget innerCompiled
              outerMap = ccSourceToTarget outerCompiled
              compositeMap = ccSourceToTarget compositeCompiled
              agreesAt sourceKey midKey =
                case (IntMap.lookup (unCostalkKey midKey) outerMap, IntMap.lookup sourceKey compositeMap) of
                  (Just leftKey, Just rightKey) -> leftKey == rightKey
                  _ -> False
          pure
            ( if IntMap.foldrWithKey (\sourceKey midKey ok -> agreesAt sourceKey midKey && ok) True innerMap
                then []
                else
                  IntMap.foldrWithKey
                    ( \sourceKey midKey acc ->
                        if agreesAt sourceKey midKey
                          then acc
                          else CostalkKey sourceKey : acc
                    )
                    []
                    innerMap
            )

    validateCompositionValue outerMorphism innerMorphism value =
      first finiteFailureFromLawFailure $
        checkCorestrictionCompositionLawWith
          (fcaCorestrict algebra)
          (fcaMismatches algebra)
          site
          outerMorphism
          innerMorphism
          value

compiledCorestrictionFor ::
  (Site site, Ord (SiteMorphism site)) =>
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  FiniteCosheaf site value ->
  Maybe (CompiledCorestriction (SiteObject site) (SiteMorphism site))
compiledCorestrictionFor morphismValue cosheaf = do
  morphismKey <-
    cosheafMorphismKeyOf morphismValue (fcSiteIndex cosheaf)
  IntMap.lookup (encodeDenseKey morphismKey) (fcCorestrictions cosheaf)
{-# INLINE compiledCorestrictionFor #-}

corestrictCostalkKey ::
  (Site site, Ord (SiteMorphism site)) =>
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  CostalkKey ->
  FiniteCosheaf site value ->
  Maybe CostalkKey
corestrictCostalkKey morphismValue sourceKey cosheaf = do
  corestrictionValue <-
    compiledCorestrictionFor morphismValue cosheaf
  IntMap.lookup (unCostalkKey sourceKey) (ccSourceToTarget corestrictionValue)
{-# INLINE corestrictCostalkKey #-}

finiteCosheafCorestrictions ::
  FiniteCosheaf site value ->
  [CompiledCorestriction (SiteObject site) (SiteMorphism site)]
finiteCosheafCorestrictions =
  IntMap.elems . fcCorestrictions
{-# INLINE finiteCosheafCorestrictions #-}
