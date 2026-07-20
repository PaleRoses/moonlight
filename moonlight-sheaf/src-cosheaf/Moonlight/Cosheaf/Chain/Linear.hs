{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Cosheaf.Chain.Linear
  ( LinearCosheafChainSpec (..),
    CosheafBoundaryProvenance (..),
    LinearCosheafBoundaryFailure (..),
    LinearCosheafChainFailure (..),
    prepareLinearCosheafChainFromSupportPlan,
    prepareLinearCosheafChainFromLinearCosheafWithSupportPlan,
    prepareLinearCosheafChainFromLinearCosheaf,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (fold, traverse_)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Moonlight.Algebra
  ( MultiplicativeMonoid (..), Semiring,
  )
import Moonlight.Cosheaf.Chain.Coefficient
  ( CoefficientOps (..),
  )
import Moonlight.Cosheaf.Chain.Prepared
  ( BoundaryTerm (..),
    PreparedCosheafChain,
    PreparedCosheafChainFailure (..),
    buildPreparedCosheafBoundary,
    mkPreparedCosheafChain,
  )
import Moonlight.Cosheaf.Linear
  ( LinearCosheaf,
    lcosSite,
    lcrMatrix,
    linearCorestrictionFor,
    linearCostalkAt,
    linearCostalkDimension,
  )
import Moonlight.Cosheaf.Support
  ( scContains,
    supportCarrierItems,
  )
import Moonlight.Cosheaf.Support.Linear
  ( LinearCosheafSupportFailure (..),
    LinearCosheafSupportPlan,
    fullLinearCosheafSupportPlan,
    lcspCells,
    lcspCoordinates,
    lcspFaces,
    validateLinearCosheafSupportPlan,
  )
import Moonlight.Homology
  ( BoundaryIncidence,
    HomologicalDegree (..),
    boundaryCoefficient,
    boundaryEntries,
    sourceCardinality,
    sourceIndex,
    targetCardinality,
    targetIndex,
  )
import Moonlight.Sheaf.Kernel.Basis
  ( mkSheafBasis,
  )
import Moonlight.Sheaf.Operator.BuildError
  ( OperatorBasisRole (..),
    SheafOperatorBuildError,
  )
import Moonlight.Sheaf.Operator.LinearBasis
  ( LinearBasis,
    linearBasisCellSlot,
    mkLinearBasis,
  )
import Moonlight.Sheaf.Site.Analysis.Scaffold
  ( SiteBoundaryAlgebra (..),
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
  )

-- | Chain-facing block presentation of a linear cosheaf over a cellular/site
-- boundary algebra. This deliberately does not redefine the validated
-- site-indexed 'Moonlight.Cosheaf.Linear.LinearCosheaf'; it is the local block
-- cover used to assemble a coefficient-aware chain complex.
type LinearCosheafChainSpec :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data LinearCosheafChainSpec site cell face coefficient provenance coreFailure = LinearCosheafChainSpec
  { lccsSite :: !site,
    lccsBoundaryAlgebra :: !(SiteBoundaryAlgebra site cell face),
    lccsCostalkDimension :: cell -> Int,
    lccsCorestrictionBlock :: face -> Either coreFailure (BoundaryIncidence coefficient),
    lccsEntryProvenance :: face -> Int -> Int -> coefficient -> provenance
  }

-- | Local-to-global provenance for one emitted boundary term before duplicate
-- coefficients are glued. The prepared boundary interns the nonzero glued
-- entries into a provenance arena.
type CosheafBoundaryProvenance :: Type -> Type -> Type -> Type -> Type
data CosheafBoundaryProvenance cell face coefficient provenance = CosheafBoundaryProvenance
  { cbpFace :: !face,
    cbpSourceCell :: !cell,
    cbpTargetCell :: !cell,
    cbpSourceLocalIndex :: !Int,
    cbpTargetLocalIndex :: !Int,
    cbpOrientation :: !Int,
    cbpLocalCoefficient :: !coefficient,
    cbpFinalCoefficient :: !coefficient,
    cbpPayload :: !provenance
  }
  deriving stock (Eq, Show)

type LinearCosheafBoundaryFailure :: Type -> Type -> Type
data LinearCosheafBoundaryFailure object morphism
  = LinearCosheafBoundaryCostalkMissing !object
  | LinearCosheafBoundaryCorestrictionMissing !(CheckedMorphism object morphism)
  | LinearCosheafBoundaryFaceEndpointMismatch
      !(CheckedMorphism object morphism)
      !object
      !object
  deriving stock (Eq, Show)

type LinearCosheafChainFailure :: Type -> Type -> Type -> Type -> Type
data LinearCosheafChainFailure cell face coefficient coreFailure
  = LinearCosheafChainBasisFailed !(SheafOperatorBuildError cell)
  | LinearCosheafChainCostalkDimensionFailed !cell !coreFailure
  | LinearCosheafChainNegativeCostalkDimension !cell !Int
  | LinearCosheafChainCorestrictionFailed !face !coreFailure
  | LinearCosheafChainZeroFaceOrientation !face
  | LinearCosheafChainCellMissingFromBasis
      !HomologicalDegree
      !OperatorBasisRole
      !face
      !cell
  | LinearCosheafChainBlockShapeMismatch
      !face
      !cell
      !cell
      !Int
      !Int
      !Int
      !Int
  | LinearCosheafChainSupportFailed !(LinearCosheafSupportFailure cell face)
  | LinearCosheafChainPreparedFailed !(PreparedCosheafChainFailure cell coefficient)
  deriving stock (Eq, Show)

prepareLinearCosheafChainFromSupportPlan ::
  forall cell coefficient site face provenance coreFailure.
  (Ord cell, Ord face, Eq coefficient, Num coefficient, Semiring coefficient) =>
  CoefficientOps coefficient ->
  LinearCosheafSupportPlan cell face ->
  LinearCosheafChainSpec site cell face coefficient provenance coreFailure ->
  Either
    (LinearCosheafChainFailure cell face coefficient coreFailure)
    (PreparedCosheafChain site cell coefficient (CosheafBoundaryProvenance cell face coefficient provenance))
prepareLinearCosheafChainFromSupportPlan coefficientOps supportPlan spec = do
  first supportFailureToChainFailure $
    validateLinearCosheafSupportPlan siteValue boundaryAlgebra (lccsCostalkDimension spec) supportPlan
  basisByDegree <-
    Map.fromList
      <$> traverse basisAtDegree degrees
  boundaryByDegree <-
    Map.fromList
      <$> traverse (boundaryAtDegree basisByDegree) positiveDegrees
  first LinearCosheafChainPreparedFailed $
    mkPreparedCosheafChain
      coefficientOps
      siteValue
      maxDegree
      basisByDegree
      boundaryByDegree
  where
    siteValue =
      lccsSite spec

    boundaryAlgebra =
      lccsBoundaryAlgebra spec

    maxDegreeInt =
      fromIntegral (sbaDepth boundaryAlgebra siteValue)

    maxDegree =
      HomologicalDegree maxDegreeInt

    degrees =
      fmap HomologicalDegree [0 .. maxDegreeInt]

    positiveDegrees =
      fmap HomologicalDegree [1 .. maxDegreeInt]

    basisAtDegree degreeValue@(HomologicalDegree degreeInt) =
      fmap (degreeValue,) $
        first LinearCosheafChainBasisFailed $
          mkLinearBasis
            retainedDimension
            ( mkSheafBasis
                (filter retainedCell (sbaCellsAtDimension boundaryAlgebra siteValue degreeInt))
            )

    boundaryAtDegree basisByDegree degreeValue@(HomologicalDegree degreeInt) = do
      sourceBasis <-
        basisFromMap degreeValue basisByDegree
      targetBasis <-
        basisFromMap (HomologicalDegree (degreeInt - 1)) basisByDegree
      terms <-
        fold <$> traverse (boundaryTermsForFace degreeValue sourceBasis targetBasis) (facesAtDegree degreeInt)
      boundaryValue <-
        first LinearCosheafChainPreparedFailed $
          buildPreparedCosheafBoundary coefficientOps degreeValue sourceBasis targetBasis terms
      pure (degreeValue, boundaryValue)

    basisFromMap ::
      HomologicalDegree ->
      Map HomologicalDegree basis ->
      Either (LinearCosheafChainFailure cell face coefficient coreFailure) basis
    basisFromMap degreeValue basisByDegree =
      maybe
        (Left (LinearCosheafChainPreparedFailed (PreparedCosheafChainBasisMissing degreeValue)))
        Right
        (Map.lookup degreeValue basisByDegree)

    facesAtDegree degreeInt =
      filter
        ( \face ->
            retainedFace face
              && sbaCellDimension boundaryAlgebra (sbaFaceSource boundaryAlgebra face) == degreeInt
        )
        (sbaFaceMorphisms boundaryAlgebra siteValue)

    boundaryTermsForFace degreeValue sourceBasis targetBasis face = do
      let sourceCell = sbaFaceSource boundaryAlgebra face
          targetCell = sbaFaceTarget boundaryAlgebra face
          orientationValue = sbaFaceOrientation boundaryAlgebra face
      (sourceOffset, _expectedSourceDimension) <-
        basisSlot degreeValue OperatorSourceBasis sourceBasis face sourceCell
      (targetOffset, _expectedTargetDimension) <-
        basisSlot degreeValue OperatorTargetBasis targetBasis face targetCell
      blockValue <-
        first (LinearCosheafChainCorestrictionFailed face) $
          lccsCorestrictionBlock spec face
      validateOrientation face orientationValue
      validateBlockShape
        face
        sourceCell
        targetCell
        (lccsCostalkDimension spec sourceCell)
        (lccsCostalkDimension spec targetCell)
        blockValue
      supportedTerms <-
        traverse
          (boundaryTermForLocalEntry face sourceCell targetCell sourceOffset targetOffset orientationValue)
          (boundaryEntries blockValue)
      pure
        (mapMaybe id supportedTerms)

    basisSlot ::
      HomologicalDegree ->
      OperatorBasisRole ->
      LinearBasis cell ->
      face ->
      cell ->
      Either (LinearCosheafChainFailure cell face coefficient coreFailure) (Int, Int)
    basisSlot degreeValue role basis face cell =
      maybe
        (Left (LinearCosheafChainCellMissingFromBasis degreeValue role face cell))
        Right
        (linearBasisCellSlot cell basis)

    validateOrientation ::
      face ->
      Int ->
      Either (LinearCosheafChainFailure cell face coefficient coreFailure) ()
    validateOrientation face orientationValue =
      if orientationValue == 0
        then Left (LinearCosheafChainZeroFaceOrientation face)
        else Right ()

    validateBlockShape ::
      face ->
      cell ->
      cell ->
      Int ->
      Int ->
      BoundaryIncidence coefficient ->
      Either (LinearCosheafChainFailure cell face coefficient coreFailure) ()
    validateBlockShape face sourceCell targetCell expectedSourceDimension expectedTargetDimension blockValue =
      if sourceCardinality blockValue == expectedSourceDimension
        && targetCardinality blockValue == expectedTargetDimension
        then Right ()
        else
          Left
            ( LinearCosheafChainBlockShapeMismatch
                face
                sourceCell
                targetCell
                expectedSourceDimension
                expectedTargetDimension
                (sourceCardinality blockValue)
                (targetCardinality blockValue)
            )

    boundaryTermForLocalEntry face sourceCell targetCell sourceOffset targetOffset orientationValue entryValue =
      let sourceLocalIndexValue = sourceIndex entryValue
          targetLocalIndexValue = targetIndex entryValue
          localCoefficient = boundaryCoefficient entryValue
          finalCoefficient =
            mul
              (coFromInteger coefficientOps (fromIntegral orientationValue))
              localCoefficient
       in case retainedCoordinateIndex sourceCell sourceLocalIndexValue of
            Nothing ->
              Right Nothing
            Just retainedSourceIndex ->
              case retainedCoordinateIndex targetCell targetLocalIndexValue of
                Nothing ->
                  Left
                    ( LinearCosheafChainSupportFailed
                        ( LinearCosheafSupportBoundaryExits
                            face
                            sourceCell
                            sourceLocalIndexValue
                            targetCell
                            targetLocalIndexValue
                        )
                    )
                Just retainedTargetIndex ->
                  Right
                    ( Just
                        BoundaryTerm
                          { boundaryTermSourceIndex = sourceOffset + retainedSourceIndex,
                            boundaryTermTargetIndex = targetOffset + retainedTargetIndex,
                            boundaryTermCoefficient = finalCoefficient,
                            boundaryTermProvenance =
                              CosheafBoundaryProvenance
                                { cbpFace = face,
                                  cbpSourceCell = sourceCell,
                                  cbpTargetCell = targetCell,
                                  cbpSourceLocalIndex = sourceLocalIndexValue,
                                  cbpTargetLocalIndex = targetLocalIndexValue,
                                  cbpOrientation = orientationValue,
                                  cbpLocalCoefficient = localCoefficient,
                                  cbpFinalCoefficient = finalCoefficient,
                                  cbpPayload =
                                    lccsEntryProvenance
                                      spec
                                      face
                                      sourceLocalIndexValue
                                      targetLocalIndexValue
                                      localCoefficient
                                }
                          }
                    )

    retainedCoordinates =
      supportCarrierItems (lcspCoordinates supportPlan)

    coordinateIndexByCell =
      fmap
        (Map.fromList . flip zip [0 :: Int ..])
        coordinatesByCell

    coordinatesByCell =
      Map.fromListWith (flip (<>)) $
        fmap
          (\(cell, coordinateIndex) -> (cell, [coordinateIndex]))
          retainedCoordinates

    retainedDimension cell =
      maybe 0 Map.size (Map.lookup cell coordinateIndexByCell)

    retainedCoordinateIndex cell coordinateIndex =
      Map.lookup cell coordinateIndexByCell >>= Map.lookup coordinateIndex

    retainedCell cell =
      scContains (lcspCells supportPlan) cell

    retainedFace face =
      scContains (lcspFaces supportPlan) face

    supportFailureToChainFailure ::
      LinearCosheafSupportFailure cell face ->
      LinearCosheafChainFailure cell face coefficient coreFailure
    supportFailureToChainFailure supportFailure =
      case supportFailure of
        LinearCosheafSupportNegativeCostalkDimension cell dimensionValue ->
          LinearCosheafChainNegativeCostalkDimension cell dimensionValue
        _ ->
          LinearCosheafChainSupportFailed supportFailure

prepareLinearCosheafChainFromLinearCosheafWithSupportPlan ::
  forall site basis coefficient.
  (Site site, Ord (SiteMorphism site), Eq coefficient, Num coefficient, Semiring coefficient) =>
  CoefficientOps coefficient ->
  LinearCosheafSupportPlan
    (SiteObject site)
    (CheckedMorphism (SiteObject site) (SiteMorphism site)) ->
  SiteBoundaryAlgebra
    site
    (SiteObject site)
    (CheckedMorphism (SiteObject site) (SiteMorphism site)) ->
  LinearCosheaf site basis coefficient ->
  Either
    ( LinearCosheafChainFailure
        (SiteObject site)
        (CheckedMorphism (SiteObject site) (SiteMorphism site))
        coefficient
        (LinearCosheafBoundaryFailure (SiteObject site) (SiteMorphism site))
    )
    ( PreparedCosheafChain
        site
        (SiteObject site)
        coefficient
        ( CosheafBoundaryProvenance
            (SiteObject site)
            (CheckedMorphism (SiteObject site) (SiteMorphism site))
            coefficient
            (CheckedMorphism (SiteObject site) (SiteMorphism site), Int, Int, coefficient)
        )
    )
prepareLinearCosheafChainFromLinearCosheafWithSupportPlan coefficientOps supportPlan boundaryAlgebra cosheaf = do
  _ <- validateBoundaryAlgebraForLinearCosheaf boundaryAlgebra cosheaf
  prepareLinearCosheafChainFromSupportPlan
    coefficientOps
    supportPlan
    (linearCosheafChainSpec boundaryAlgebra cosheaf)

prepareLinearCosheafChainFromLinearCosheaf ::
  forall site basis coefficient.
  (Site site, Ord (SiteMorphism site), Eq coefficient, Num coefficient, Semiring coefficient) =>
  CoefficientOps coefficient ->
  SiteBoundaryAlgebra
    site
    (SiteObject site)
    (CheckedMorphism (SiteObject site) (SiteMorphism site)) ->
  LinearCosheaf site basis coefficient ->
  Either
    ( LinearCosheafChainFailure
        (SiteObject site)
        (CheckedMorphism (SiteObject site) (SiteMorphism site))
        coefficient
        (LinearCosheafBoundaryFailure (SiteObject site) (SiteMorphism site))
    )
    ( PreparedCosheafChain
        site
        (SiteObject site)
        coefficient
        ( CosheafBoundaryProvenance
            (SiteObject site)
            (CheckedMorphism (SiteObject site) (SiteMorphism site))
            coefficient
            (CheckedMorphism (SiteObject site) (SiteMorphism site), Int, Int, coefficient)
        )
    )
prepareLinearCosheafChainFromLinearCosheaf coefficientOps boundaryAlgebra cosheaf = do
  _ <- validateBoundaryAlgebraForLinearCosheaf boundaryAlgebra cosheaf
  supportPlan <-
    first LinearCosheafChainSupportFailed $
      fullLinearCosheafSupportPlan
        (lcosSite cosheaf)
        boundaryAlgebra
        (linearCosheafCostalkDimension cosheaf)
  prepareLinearCosheafChainFromSupportPlan
    coefficientOps
    supportPlan
    (linearCosheafChainSpec boundaryAlgebra cosheaf)

linearCosheafChainSpec ::
  (Site site, Ord (SiteMorphism site)) =>
  SiteBoundaryAlgebra
    site
    (SiteObject site)
    (CheckedMorphism (SiteObject site) (SiteMorphism site)) ->
  LinearCosheaf site basis coefficient ->
  LinearCosheafChainSpec
    site
    (SiteObject site)
    (CheckedMorphism (SiteObject site) (SiteMorphism site))
    coefficient
    (CheckedMorphism (SiteObject site) (SiteMorphism site), Int, Int, coefficient)
    (LinearCosheafBoundaryFailure (SiteObject site) (SiteMorphism site))
linearCosheafChainSpec boundaryAlgebra cosheaf =
  LinearCosheafChainSpec
    { lccsSite = lcosSite cosheaf,
      lccsBoundaryAlgebra = boundaryAlgebra,
      lccsCostalkDimension = linearCosheafCostalkDimension cosheaf,
      lccsCorestrictionBlock = linearCosheafCorestrictionBlock cosheaf,
      lccsEntryProvenance =
        \morphismValue sourceLocalIndex targetLocalIndex coefficientValue ->
          (morphismValue, sourceLocalIndex, targetLocalIndex, coefficientValue)
    }

linearCosheafCostalkDimension ::
  Site site =>
  LinearCosheaf site basis coefficient ->
  SiteObject site ->
  Int
linearCosheafCostalkDimension cosheaf cell =
  maybe 0 linearCostalkDimension (linearCostalkAt cell cosheaf)

linearCosheafCorestrictionBlock ::
  (Site site, Ord (SiteMorphism site)) =>
  LinearCosheaf site basis coefficient ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  Either (LinearCosheafBoundaryFailure (SiteObject site) (SiteMorphism site)) (BoundaryIncidence coefficient)
linearCosheafCorestrictionBlock cosheaf morphismValue =
  maybe
    (Left (LinearCosheafBoundaryCorestrictionMissing morphismValue))
    (Right . lcrMatrix)
    (linearCorestrictionFor morphismValue cosheaf)

validateBoundaryAlgebraForLinearCosheaf ::
  (Site site, Ord (SiteMorphism site)) =>
  SiteBoundaryAlgebra
    site
    (SiteObject site)
    (CheckedMorphism (SiteObject site) (SiteMorphism site)) ->
  LinearCosheaf site basis coefficient ->
  Either
    ( LinearCosheafChainFailure
        (SiteObject site)
        (CheckedMorphism (SiteObject site) (SiteMorphism site))
        coefficient
        (LinearCosheafBoundaryFailure (SiteObject site) (SiteMorphism site))
    )
    ()
validateBoundaryAlgebraForLinearCosheaf boundaryAlgebra cosheaf =
  traverse_ validateCellCostalk cellsValue
    *> traverse_ validateFace facesValue
  where
    siteValue =
      lcosSite cosheaf

    maxDegreeInt =
      fromIntegral (sbaDepth boundaryAlgebra siteValue)

    cellsValue =
      foldMap (sbaCellsAtDimension boundaryAlgebra siteValue) [0 .. maxDegreeInt]

    facesValue =
      sbaFaceMorphisms boundaryAlgebra siteValue

    validateCellCostalk cell =
      case linearCostalkAt cell cosheaf of
        Nothing ->
          Left (LinearCosheafChainCostalkDimensionFailed cell (LinearCosheafBoundaryCostalkMissing cell))
        Just _ ->
          Right ()

    validateFace face
      | cmSource face /= sbaFaceSource boundaryAlgebra face
          || cmTarget face /= sbaFaceTarget boundaryAlgebra face =
          Left
            ( LinearCosheafChainCorestrictionFailed
                face
                ( LinearCosheafBoundaryFaceEndpointMismatch
                    face
                    (sbaFaceSource boundaryAlgebra face)
                    (sbaFaceTarget boundaryAlgebra face)
                )
            )
      | otherwise =
          case linearCorestrictionFor face cosheaf of
            Nothing ->
              Left (LinearCosheafChainCorestrictionFailed face (LinearCosheafBoundaryCorestrictionMissing face))
            Just _ ->
              Right ()
