{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Linear
  ( LocalBasisKey (..),
    LinearCostalk (..),
    linearCostalkDimension,
    LinearCorestriction (..),
    LinearCosheaf,
    lcosSite,
    lcosSiteIndex,
    lcosCostalks,
    lcosCorestrictions,
    LinearCosheafAlgebra (..),
    LinearCosheafFailure (..),
    mkLinearCosheaf,
    linearCostalkAt,
    linearCostalkAtObjectKey,
    linearCorestrictionFor,
    linearCosheafCorestrictions,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Algebra (Semiring)
import Moonlight.Core (note)
import Moonlight.Core (encodeDenseKey)
import Moonlight.Core (duplicatesOrd)
import Moonlight.Cosheaf.Linear.Types
import Moonlight.Cosheaf.SiteIndex
  ( CosheafMorphismKey,
    CosheafSiteIndex,
    IndexedCosheafMorphism (..),
    buildCosheafSiteIndex,
    cosheafCompositionValidationBasis,
    cosheafIndexedMorphisms,
    cosheafMorphismKeyOf,
    cosheafSiteObjectIndex,
  )
import Moonlight.Homology
  ( BoundaryIncidence,
    composeBoundaryIncidence,
    identityBoundaryIncidenceOf,
    sourceCardinality,
    targetCardinality,
  )
import Moonlight.Sheaf.Index.Dense
  ( denseIndexKeyOf,
    mkDenseIndex,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
  )

lcosSite :: LinearCosheaf site basis coeff -> site
lcosSite =
  linearCosheafSiteInternal

lcosSiteIndex :: LinearCosheaf site basis coeff -> CosheafSiteIndex site
lcosSiteIndex =
  linearCosheafSiteIndexInternal

lcosCostalks ::
  LinearCosheaf site basis coeff ->
  IntMap (LinearCostalk (SiteObject site) basis)
lcosCostalks =
  linearCosheafCostalksInternal

lcosCorestrictions ::
  LinearCosheaf site basis coeff ->
  IntMap (LinearCorestriction (SiteObject site) (SiteMorphism site) coeff)
lcosCorestrictions =
  linearCosheafCorestrictionsInternal

mkLinearCosheaf ::
  forall site basis coeff matrixFailure.
  (Site site, Ord (SiteMorphism site), Ord basis, Eq coeff, Num coeff, Semiring coeff) =>
  site ->
  LinearCosheafAlgebra site coeff matrixFailure ->
  Map (SiteObject site) [basis] ->
  Either
    (LinearCosheafFailure (SiteObject site) (SiteMorphism site) basis coeff matrixFailure)
    (LinearCosheaf site basis coeff)
mkLinearCosheaf site algebra rawCostalks = do
  traverse_ (validateKnownCostalkObject objectSet) (Map.keys rawCostalks)
  siteIndex <-
    first LinearCosheafSiteIndexInvalid (buildCosheafSiteIndex site)
  costalks <-
    IntMap.fromList
      <$> traverse (buildLinearCostalk siteIndex rawCostalks) (siteObjects site)
  corestrictions <-
    IntMap.fromList
      <$> traverse (compileLinearCorestriction algebra costalks) (cosheafIndexedMorphisms siteIndex)
  let cosheaf =
        LinearCosheaf
          { linearCosheafSiteInternal = site,
            linearCosheafSiteIndexInternal = siteIndex,
            linearCosheafCostalksInternal = costalks,
            linearCosheafCorestrictionsInternal = corestrictions
          }
  traverse_ (validateLinearIdentity site cosheaf) (siteObjects site)
  traverse_ (validateLinearComposition site cosheaf) (cosheafCompositionValidationBasis siteIndex)
  pure cosheaf
  where
    objectSet :: Set (SiteObject site)
    objectSet =
      Set.fromList (siteObjects site)

linearCostalkAt ::
  Site site =>
  SiteObject site ->
  LinearCosheaf site basis coeff ->
  Maybe (LinearCostalk (SiteObject site) basis)
linearCostalkAt objectValue cosheaf = do
  objectKey <-
    denseIndexKeyOf objectValue (cosheafSiteObjectIndex (lcosSiteIndex cosheaf))
  linearCostalkAtObjectKey objectKey cosheaf
{-# INLINE linearCostalkAt #-}

linearCostalkAtObjectKey ::
  ObjectKey ->
  LinearCosheaf site basis coeff ->
  Maybe (LinearCostalk (SiteObject site) basis)
linearCostalkAtObjectKey objectKey =
  IntMap.lookup (unObjectKey objectKey) . lcosCostalks
{-# INLINE linearCostalkAtObjectKey #-}

linearCorestrictionFor ::
  (Site site, Ord (SiteMorphism site)) =>
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  LinearCosheaf site basis coeff ->
  Maybe (LinearCorestriction (SiteObject site) (SiteMorphism site) coeff)
linearCorestrictionFor morphismValue cosheaf = do
  morphismKey <-
    cosheafMorphismKeyOf morphismValue (lcosSiteIndex cosheaf)
  linearCorestrictionAtKey morphismKey cosheaf
{-# INLINE linearCorestrictionFor #-}

linearCorestrictionAtKey ::
  CosheafMorphismKey ->
  LinearCosheaf site basis coeff ->
  Maybe (LinearCorestriction (SiteObject site) (SiteMorphism site) coeff)
linearCorestrictionAtKey morphismKey =
  IntMap.lookup (encodeDenseKey morphismKey) . lcosCorestrictions
{-# INLINE linearCorestrictionAtKey #-}

linearCosheafCorestrictions ::
  LinearCosheaf site basis coeff ->
  [LinearCorestriction (SiteObject site) (SiteMorphism site) coeff]
linearCosheafCorestrictions =
  IntMap.elems . lcosCorestrictions
{-# INLINE linearCosheafCorestrictions #-}

validateKnownCostalkObject ::
  Ord obj =>
  Set obj ->
  obj ->
  Either (LinearCosheafFailure obj mor basis coeff matrixFailure) ()
validateKnownCostalkObject knownObjects objectValue =
  if Set.member objectValue knownObjects
    then Right ()
    else Left (LinearCostalkUnknownObject objectValue)

buildLinearCostalk ::
  forall site basis coeff matrixFailure.
  (Site site, Ord basis) =>
  CosheafSiteIndex site ->
  Map (SiteObject site) [basis] ->
  SiteObject site ->
  Either
    (LinearCosheafFailure (SiteObject site) (SiteMorphism site) basis coeff matrixFailure)
    (Int, LinearCostalk (SiteObject site) basis)
buildLinearCostalk siteIndex rawCostalks objectValue = do
  objectKey <-
    note
      (LinearCosheafObjectKeyMissing objectValue)
      (denseIndexKeyOf objectValue (cosheafSiteObjectIndex siteIndex))
  basisValues <-
    note
      (LinearCostalkMissing objectValue)
      (Map.lookup objectValue rawCostalks)
  case duplicatesOrd basisValues of
    duplicateBasis : _ ->
      Left (LinearCostalkDuplicateBasis objectValue duplicateBasis)
    [] ->
      Right
        ( unObjectKey objectKey,
          LinearCostalk
            { lcObjectKey = objectKey,
              lcObject = objectValue,
              lcBasis = mkDenseIndex basisValues
            }
        )

compileLinearCorestriction ::
  forall site basis coeff matrixFailure.
  LinearCosheafAlgebra site coeff matrixFailure ->
  IntMap (LinearCostalk (SiteObject site) basis) ->
  IndexedCosheafMorphism (SiteObject site) (SiteMorphism site) ->
  Either
    (LinearCosheafFailure (SiteObject site) (SiteMorphism site) basis coeff matrixFailure)
    (Int, LinearCorestriction (SiteObject site) (SiteMorphism site) coeff)
compileLinearCorestriction algebra costalks indexedMorphism = do
  sourceCostalk <-
    note
      (LinearCorestrictionSourceCostalkMissing morphismValue)
      (IntMap.lookup (unObjectKey (icmSourceObjectKey indexedMorphism)) costalks)
  targetCostalk <-
    note
      (LinearCorestrictionTargetCostalkMissing morphismValue)
      (IntMap.lookup (unObjectKey (icmTargetObjectKey indexedMorphism)) costalks)
  matrix <-
    first
      (LinearCorestrictionMatrixFailed morphismValue)
      (lcaCorestrictionMatrix algebra morphismValue)
  validateLinearMatrixShape
    morphismValue
    (linearCostalkDimension sourceCostalk)
    (linearCostalkDimension targetCostalk)
    matrix
  pure
    ( encodeDenseKey (icmKey indexedMorphism),
      LinearCorestriction
        { lcrMorphismKey = icmKey indexedMorphism,
          lcrMorphism = morphismValue,
          lcrSourceObjectKey = icmSourceObjectKey indexedMorphism,
          lcrTargetObjectKey = icmTargetObjectKey indexedMorphism,
          lcrMatrix = matrix
        }
    )
  where
    morphismValue :: CheckedMorphism (SiteObject site) (SiteMorphism site)
    morphismValue =
      icmMorphism indexedMorphism

validateLinearMatrixShape ::
  CheckedMorphism obj mor ->
  Int ->
  Int ->
  BoundaryIncidence coeff ->
  Either (LinearCosheafFailure obj mor basis coeff matrixFailure) ()
validateLinearMatrixShape morphismValue expectedSourceDimension expectedTargetDimension matrix =
  if sourceCardinality matrix == expectedSourceDimension
    && targetCardinality matrix == expectedTargetDimension
    then Right ()
    else
      Left
        ( LinearCorestrictionShapeMismatch
            morphismValue
            expectedSourceDimension
            expectedTargetDimension
            (sourceCardinality matrix)
            (targetCardinality matrix)
        )

validateLinearIdentity ::
  forall site basis coeff matrixFailure.
  (Site site, Ord (SiteMorphism site), Eq coeff, Num coeff) =>
  site ->
  LinearCosheaf site basis coeff ->
  SiteObject site ->
  Either
    (LinearCosheafFailure (SiteObject site) (SiteMorphism site) basis coeff matrixFailure)
    ()
validateLinearIdentity site cosheaf objectValue = do
  costalk <-
    note
      (LinearCostalkMissing objectValue)
      (linearCostalkAt objectValue cosheaf)
  identityCorestriction <-
    note
      (LinearCorestrictionIdentityMissing identityMorphism)
      (cosheafMorphismKeyOf identityMorphism (lcosSiteIndex cosheaf) >>= flip linearCorestrictionAtKey cosheaf)
  let actualMatrix =
        lcrMatrix identityCorestriction
      expectedMatrix =
        identityBoundaryIncidenceOf (fromIntegral (linearCostalkDimension costalk))
  if actualMatrix == expectedMatrix
    then Right ()
    else
      Left
        ( LinearCorestrictionIdentityMismatch
            identityMorphism
            actualMatrix
            expectedMatrix
        )
  where
    identityMorphism :: CheckedMorphism (SiteObject site) (SiteMorphism site)
    identityMorphism =
      identityAt site objectValue

validateLinearComposition ::
  forall site basis coeff matrixFailure.
  (Site site, Ord (SiteMorphism site), Eq coeff, Num coeff, Semiring coeff) =>
  site ->
  LinearCosheaf site basis coeff ->
  (IndexedCosheafMorphism (SiteObject site) (SiteMorphism site), IndexedCosheafMorphism (SiteObject site) (SiteMorphism site)) ->
  Either
    (LinearCosheafFailure (SiteObject site) (SiteMorphism site) basis coeff matrixFailure)
    ()
validateLinearComposition site cosheaf (outerIndexed, innerIndexed) = do
  outerCorestriction <-
    note
      (LinearCorestrictionCompositeMissing outerMorphism)
      (linearCorestrictionAtKey (icmKey outerIndexed) cosheaf)
  innerCorestriction <-
    note
      (LinearCorestrictionCompositeMissing innerMorphism)
      (linearCorestrictionAtKey (icmKey innerIndexed) cosheaf)
  case composeChecked site outerMorphism innerMorphism of
    Nothing ->
      Left (LinearCorestrictionCompositionUndefined outerMorphism innerMorphism)
    Just compositeMorphism -> do
      compositeKey <-
        note
          (LinearCorestrictionCompositeMissing compositeMorphism)
          (cosheafMorphismKeyOf compositeMorphism (lcosSiteIndex cosheaf))
      compositeCorestriction <-
        note
          (LinearCorestrictionCompositeMissing compositeMorphism)
          (linearCorestrictionAtKey compositeKey cosheaf)
      sequentialMatrix <-
        first
          (LinearCorestrictionCompositionShapeFailed outerMorphism innerMorphism)
          (composeBoundaryIncidence (lcrMatrix outerCorestriction) (lcrMatrix innerCorestriction))
      let directMatrix =
            lcrMatrix compositeCorestriction
      if sequentialMatrix == directMatrix
        then Right ()
        else
          Left
            ( LinearCorestrictionCompositionMismatch
                outerMorphism
                innerMorphism
                compositeMorphism
                sequentialMatrix
                directMatrix
            )
  where
    outerMorphism :: CheckedMorphism (SiteObject site) (SiteMorphism site)
    outerMorphism =
      icmMorphism outerIndexed

    innerMorphism :: CheckedMorphism (SiteObject site) (SiteMorphism site)
    innerMorphism =
      icmMorphism innerIndexed
