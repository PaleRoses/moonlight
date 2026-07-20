{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Sheaf Laplacians and the higher structures derived from coboundary
-- realizations.
module Moonlight.Sheaf.Cochain.Laplacian
  ( LaplacianKind (..),
    CoboundaryDerivedHigherStructure (..),
    PortalRestrictionHigherStructure (..),
    SemiringPropagationHigherStructure (..),
    RestrictionGraphPlanKind (..),
    RestrictionGraphPlan,
    SheafLaplacian,
    PackedSheafLaplacian,
    packedSheafLaplacianSymbolic,
    HigherStructureOf,
    LaplacianKindOf,
    slDegree,
    slDomainBasis,
    slCodomainBasis,
    slIncidence,
    slHigherStructure,
    slKind,
    eliminateSheafLaplacian,
    buildHodgeLaplacian0,
    buildHodgeLaplacian1,
    prepareRestrictionGraphPlanWithDimensions,
    prepareRestrictionGraphPlan,
    buildTarskiLaplacianFromPlan,
    buildPackedTarskiLaplacianFromPlan,
    buildTarskiLaplacian,
    buildSemiringLaplacianFromPlan,
    buildPackedSemiringLaplacianFromPlan,
    buildSemiringLaplacian,
    packSheafLaplacian,
    laplacianDomainCardinality,
    laplacianCodomainCardinality,
    laplacianIsSquare,
    laplacianIsSymmetric,
    laplacianSupportCells,
    laplacianQuadraticEnergy,
    laplacianApplySparse,
    laplacianCoordinateVectorWithCellCoordinates,
    laplacianResidualSquaredNormFromCoordinateVector,
    laplacianResidualSquareMapWithCellCoordinates,
    laplacianResidualSquareMap,
    laplacianResidualSquaredNorm,
    packedLaplacianDenseCoordinateVectorWithCellCoordinates,
    packedLaplacianResidualSquaredNormFromDenseCoordinates,
    packedLaplacianResidualVectorWithCellCoordinates,
    packedLaplacianResidualSquareMapWithCellCoordinates,
    packedLaplacianResidualSquaredNormWithCellCoordinates,
  )
where

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import Data.Vector.Unboxed qualified as Unboxed
import Moonlight.Homology
  ( BoundaryIncidence,
    addBoundaryIncidence,
    boundaryCoefficient,
    boundaryEntries,
    boundaryIncidenceApply,
    composeBoundaryIncidence,
    mapBoundaryCoefficients,
    sourceCardinality,
    sourceIndex,
    targetCardinality,
    targetIndex,
    transposeBoundaryIncidence,
  )
import Moonlight.LinAlg.Sparse
  ( PackedSparseOperator,
  )
import Moonlight.Sheaf.Operator.BuildError
  ( OperatorBasisRole (..),
    SheafOperatorBuildError (..),
  )
import Moonlight.Sheaf.Operator.GradedComplex
import Moonlight.Homology
  ( HomologicalDegree (..),
  )
import Moonlight.Sheaf.Operator.LinearBasis
  ( LinearBasis,
    linearBasisCardinality,
    linearBasisCellSlotOrError,
    linearBasisCells,
    linearBasisIndexedCoordinates,
    linearCoordinateCell,
    mkLinearBasis,
  )
import Moonlight.Sheaf.Operator.Sparse
  ( BoundaryPairConvention (..),
    applyPackedSparseOperatorDenseAsSheafOperator,
    liftBoundaryShape,
    mkBoundaryIncidenceFromPairs,
    packedSparseOperatorFromBoundary,
    validateLinearIncidenceShape,
  )
import Moonlight.Sheaf.Section.Store.State
import Moonlight.Sheaf.Section.Store.Types
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    sheafModelBasis,
    sheafModelRestrictions,
  )
import Moonlight.Sheaf.Section.Morphism
  ( Restriction,
    rSource,
    rTarget,
  )
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    portalRestrictions,
    restrictionEntries,
  )

data LaplacianKind
  = HodgeLaplacian
  | TarskiLaplacian
  | SemiringLaplacian
  deriving stock (Eq, Ord, Show)

data CoboundaryDerivedHigherStructure = CoboundaryDerivedHigherStructure
  deriving stock (Eq, Ord, Show)

data PortalRestrictionHigherStructure = PortalRestrictionHigherStructure
  deriving stock (Eq, Ord, Show)

data SemiringPropagationHigherStructure = SemiringPropagationHigherStructure
  deriving stock (Eq, Ord, Show)

data RestrictionGraphPlanKind (kind :: LaplacianKind) where
  TarskiRestrictionGraphPlan :: RestrictionGraphPlanKind 'TarskiLaplacian
  SemiringRestrictionGraphPlan :: RestrictionGraphPlanKind 'SemiringLaplacian

deriving stock instance Eq (RestrictionGraphPlanKind kind)

deriving stock instance Show (RestrictionGraphPlanKind kind)

data RestrictionGraphPlan owner (kind :: LaplacianKind) cell witness = RestrictionGraphPlan
  { rgpLinearBasisInternal :: !(LinearBasis cell),
    rgpRestrictionsInternal :: ![Restriction cell witness]
  }

type role RestrictionGraphPlan nominal nominal nominal representational

deriving stock instance (Eq cell, Eq witness) => Eq (RestrictionGraphPlan owner kind cell witness)

deriving stock instance (Show cell, Show witness) => Show (RestrictionGraphPlan owner kind cell witness)

type HigherStructureOf :: LaplacianKind -> Type
type family HigherStructureOf (kind :: LaplacianKind) where
  HigherStructureOf 'HodgeLaplacian = CoboundaryDerivedHigherStructure
  HigherStructureOf 'TarskiLaplacian = PortalRestrictionHigherStructure
  HigherStructureOf 'SemiringLaplacian = SemiringPropagationHigherStructure

type LaplacianKindOf :: LaplacianKind -> Constraint
class LaplacianKindOf (kind :: LaplacianKind) where
  laplacianKindValue :: Proxy kind -> LaplacianKind

instance LaplacianKindOf 'HodgeLaplacian where
  laplacianKindValue _ = HodgeLaplacian

instance LaplacianKindOf 'TarskiLaplacian where
  laplacianKindValue _ = TarskiLaplacian

instance LaplacianKindOf 'SemiringLaplacian where
  laplacianKindValue _ = SemiringLaplacian

data SheafLaplacian (kind :: LaplacianKind) cell = SheafLaplacian
  { sheafLaplacianDegreeInternal :: HomologicalDegree,
    sheafLaplacianDomainBasisInternal :: LinearBasis cell,
    sheafLaplacianCodomainBasisInternal :: LinearBasis cell,
    sheafLaplacianIncidenceInternal :: BoundaryIncidence Double,
    sheafLaplacianHigherStructureInternal :: HigherStructureOf kind
  }

slDegree :: SheafLaplacian kind cell -> HomologicalDegree
slDegree = sheafLaplacianDegreeInternal

slDomainBasis :: SheafLaplacian kind cell -> LinearBasis cell
slDomainBasis = sheafLaplacianDomainBasisInternal

slCodomainBasis :: SheafLaplacian kind cell -> LinearBasis cell
slCodomainBasis = sheafLaplacianCodomainBasisInternal

slIncidence :: SheafLaplacian kind cell -> BoundaryIncidence Double
slIncidence = sheafLaplacianIncidenceInternal

slHigherStructure :: SheafLaplacian kind cell -> HigherStructureOf kind
slHigherStructure = sheafLaplacianHigherStructureInternal

data PackedSheafLaplacian (kind :: LaplacianKind) cell = PackedSheafLaplacian
  { pslSymbolic :: !(SheafLaplacian kind cell),
    pslPackedOperator :: !(PackedSparseOperator Double)
  }

packedSheafLaplacianSymbolic :: PackedSheafLaplacian kind cell -> SheafLaplacian kind cell
packedSheafLaplacianSymbolic =
  pslSymbolic

slKind :: forall kind cell. LaplacianKindOf kind => SheafLaplacian kind cell -> LaplacianKind
slKind _ =
  laplacianKindValue (Proxy :: Proxy kind)

eliminateSheafLaplacian ::
  SheafLaplacian kind cell ->
  (HomologicalDegree -> LinearBasis cell -> LinearBasis cell -> BoundaryIncidence Double -> HigherStructureOf kind -> result) ->
  result
eliminateSheafLaplacian laplacian continue =
  continue
    (slDegree laplacian)
    (slDomainBasis laplacian)
    (slCodomainBasis laplacian)
    (slIncidence laplacian)
    (slHigherStructure laplacian)

mkSheafLaplacian ::
  HomologicalDegree ->
  LinearBasis cell ->
  LinearBasis cell ->
  BoundaryIncidence Double ->
  HigherStructureOf kind ->
  Either (SheafOperatorBuildError cell) (SheafLaplacian kind cell)
mkSheafLaplacian degree domainBasis codomainBasis incidence higherStructure = do
  validateLinearIncidenceShape domainBasis codomainBasis incidence
  Right
    SheafLaplacian
      { sheafLaplacianDegreeInternal = degree,
        sheafLaplacianDomainBasisInternal = domainBasis,
        sheafLaplacianCodomainBasisInternal = codomainBasis,
        sheafLaplacianIncidenceInternal = incidence,
        sheafLaplacianHigherStructureInternal = higherStructure
      }

buildHodgeLaplacian0 ::
  GradedComplex cell Int ->
  Either (SheafOperatorBuildError cell) (SheafLaplacian 'HodgeLaplacian cell)
buildHodgeLaplacian0 complex = do
  coboundary0 <- gradedOperatorAt (HomologicalDegree 0) complex
  let coboundaryZero =
        differentialIncidenceToDouble coboundary0
  hodgeIncidence <-
    liftBoundaryShape
      ( composeBoundaryIncidence
          (transposeBoundaryIncidence coboundaryZero)
          coboundaryZero
      )
  mkSheafLaplacian
    (HomologicalDegree 0)
    (gradedOperatorSourceBasis coboundary0)
    (gradedOperatorSourceBasis coboundary0)
    hodgeIncidence
    CoboundaryDerivedHigherStructure

buildHodgeLaplacian1 ::
  GradedComplex cell Int ->
  Either (SheafOperatorBuildError cell) (SheafLaplacian 'HodgeLaplacian cell)
buildHodgeLaplacian1 complex = do
  coboundary0 <- gradedOperatorAt (HomologicalDegree 0) complex
  coboundary1 <- gradedOperatorAt (HomologicalDegree 1) complex
  let coboundaryZero =
        differentialIncidenceToDouble coboundary0
      coboundaryOne =
        differentialIncidenceToDouble coboundary1
  faceTerm <-
    liftBoundaryShape
      ( composeBoundaryIncidence
          (transposeBoundaryIncidence coboundaryOne)
          coboundaryOne
      )
  vertexTerm <-
    liftBoundaryShape
      ( composeBoundaryIncidence
          coboundaryZero
          (transposeBoundaryIncidence coboundaryZero)
      )
  laplacianIncidence <-
    liftBoundaryShape (addBoundaryIncidence faceTerm vertexTerm)
  mkSheafLaplacian
    (HomologicalDegree 1)
    (gradedOperatorSourceBasis coboundary1)
    (gradedOperatorSourceBasis coboundary1)
    laplacianIncidence
    CoboundaryDerivedHigherStructure

prepareRestrictionGraphPlanWithDimensions ::
  Ord cell =>
  (cell -> Int) ->
  RestrictionGraphPlanKind kind ->
  SheafModel owner cell witness ->
  Either (SheafOperatorBuildError cell) (RestrictionGraphPlan owner kind cell witness)
prepareRestrictionGraphPlanWithDimensions stalkDimension planKind model = do
  linearBasis <- mkLinearBasis stalkDimension (sheafModelBasis model)
  pure
    RestrictionGraphPlan
      { rgpLinearBasisInternal = linearBasis,
        rgpRestrictionsInternal =
          case planKind of
            TarskiRestrictionGraphPlan ->
              tarskiRestrictionEntries (sheafModelRestrictions model)
            SemiringRestrictionGraphPlan ->
              restrictionEntries (sheafModelRestrictions model)
      }

prepareRestrictionGraphPlan ::
  Ord cell =>
  RestrictionGraphPlanKind kind ->
  SheafModel owner cell witness ->
  Either (SheafOperatorBuildError cell) (RestrictionGraphPlan owner kind cell witness)
prepareRestrictionGraphPlan =
  prepareRestrictionGraphPlanWithDimensions (const 1)

buildTarskiLaplacianFromPlan ::
  Ord cell =>
  RestrictionGraphPlan owner 'TarskiLaplacian cell witness ->
  Either (SheafOperatorBuildError cell) (SheafLaplacian 'TarskiLaplacian cell)
buildTarskiLaplacianFromPlan plan = do
  incidence <-
    restrictionGraphLaplacianIncidence
      (rgpRestrictionsInternal plan)
      (rgpLinearBasisInternal plan)
  mkSheafLaplacian
    (HomologicalDegree 0)
    (rgpLinearBasisInternal plan)
    (rgpLinearBasisInternal plan)
    incidence
    PortalRestrictionHigherStructure

buildPackedTarskiLaplacianFromPlan ::
  Ord cell =>
  RestrictionGraphPlan owner 'TarskiLaplacian cell witness ->
  Either (SheafOperatorBuildError cell) (PackedSheafLaplacian 'TarskiLaplacian cell)
buildPackedTarskiLaplacianFromPlan =
  buildTarskiLaplacianFromPlan >=> packSheafLaplacian

buildTarskiLaplacian ::
  Ord cell =>
  SheafModel owner cell witness ->
  Either (SheafOperatorBuildError cell) (SheafLaplacian 'TarskiLaplacian cell)
buildTarskiLaplacian model =
  prepareRestrictionGraphPlan TarskiRestrictionGraphPlan model >>= buildTarskiLaplacianFromPlan

buildSemiringLaplacianFromPlan ::
  Ord cell =>
  RestrictionGraphPlan owner 'SemiringLaplacian cell witness ->
  Either (SheafOperatorBuildError cell) (SheafLaplacian 'SemiringLaplacian cell)
buildSemiringLaplacianFromPlan plan = do
  incidence <-
    restrictionGraphPropagationIncidence
      (rgpRestrictionsInternal plan)
      (rgpLinearBasisInternal plan)
  mkSheafLaplacian
    (HomologicalDegree 0)
    (rgpLinearBasisInternal plan)
    (rgpLinearBasisInternal plan)
    incidence
    SemiringPropagationHigherStructure

buildPackedSemiringLaplacianFromPlan ::
  Ord cell =>
  RestrictionGraphPlan owner 'SemiringLaplacian cell witness ->
  Either (SheafOperatorBuildError cell) (PackedSheafLaplacian 'SemiringLaplacian cell)
buildPackedSemiringLaplacianFromPlan =
  buildSemiringLaplacianFromPlan >=> packSheafLaplacian

buildSemiringLaplacian ::
  Ord cell =>
  SheafModel owner cell witness ->
  Either (SheafOperatorBuildError cell) (SheafLaplacian 'SemiringLaplacian cell)
buildSemiringLaplacian model =
  prepareRestrictionGraphPlan SemiringRestrictionGraphPlan model >>= buildSemiringLaplacianFromPlan

packSheafLaplacian ::
  SheafLaplacian kind cell ->
  Either (SheafOperatorBuildError cell) (PackedSheafLaplacian kind cell)
packSheafLaplacian laplacian = do
  packedOperator <- packedSparseOperatorFromBoundary (slIncidence laplacian)
  pure
    PackedSheafLaplacian
      { pslSymbolic = laplacian,
        pslPackedOperator = packedOperator
      }

laplacianDomainCardinality :: SheafLaplacian kind cell -> Int
laplacianDomainCardinality =
  sourceCardinality . slIncidence

laplacianCodomainCardinality :: SheafLaplacian kind cell -> Int
laplacianCodomainCardinality =
  targetCardinality . slIncidence

laplacianIsSquare :: SheafLaplacian kind cell -> Bool
laplacianIsSquare laplacian =
  laplacianDomainCardinality laplacian == laplacianCodomainCardinality laplacian

laplacianIsSymmetric :: SheafLaplacian kind cell -> Bool
laplacianIsSymmetric laplacian =
  let coefficients = boundaryCoefficientByPair (slIncidence laplacian)
   in coefficients
        & Map.toList
        & all
          ( \(pair, value) ->
              Map.findWithDefault 0.0 (swap pair) coefficients == value
          )

laplacianSupportCells :: Ord cell => SheafLaplacian kind cell -> Set cell
laplacianSupportCells laplacian =
  linearBasisCells (slDomainBasis laplacian)
    <> linearBasisCells (slCodomainBasis laplacian)
    & Set.fromList

laplacianQuadraticEnergy :: SheafLaplacian kind cell -> Map Int Double -> Double
laplacianQuadraticEnergy laplacian vector =
  let applied = laplacianApplySparse laplacian vector
   in vector
        & Map.toList
        & fmap
          ( \(indexValue, vectorValue) ->
              vectorValue * Map.findWithDefault 0.0 indexValue applied
          )
        & sum

laplacianApplySparse :: SheafLaplacian kind cell -> Map Int Double -> Map Int Double
laplacianApplySparse laplacian =
  boundaryIncidenceApply (slIncidence laplacian)

laplacianCoordinateVectorWithCellCoordinates ::
  Ord cell =>
  (cell -> stalk -> [Double]) ->
  SheafLaplacian kind cell ->
  SheafModel owner cell witness ->
  TotalSectionStore owner cell stalk ->
  Either (SheafOperatorBuildError cell) (Map Int Double)
laplacianCoordinateVectorWithCellCoordinates stalkCoordinates laplacian =
  sectionCoordinateVector stalkCoordinates (slDomainBasis laplacian)

laplacianResidualSquareMapFromCoordinateVector ::
  Ord cell =>
  SheafLaplacian kind cell ->
  Map Int Double ->
  Map cell Double
laplacianResidualSquareMapFromCoordinateVector laplacian inputValues =
  residualSquareMapFromSparseValues
    (slCodomainBasis laplacian)
    (laplacianApplySparse laplacian inputValues)

laplacianResidualSquaredNormFromCoordinateVector ::
  Ord cell =>
  SheafLaplacian kind cell ->
  Map Int Double ->
  Double
laplacianResidualSquaredNormFromCoordinateVector laplacian =
  sum . Map.elems . laplacianResidualSquareMapFromCoordinateVector laplacian

laplacianResidualSquareMapWithCellCoordinates ::
  Ord cell =>
  (cell -> stalk -> [Double]) ->
  SheafLaplacian kind cell ->
  SheafModel owner cell witness ->
  TotalSectionStore owner cell stalk ->
  Either (SheafOperatorBuildError cell) (Map cell Double)
laplacianResidualSquareMapWithCellCoordinates stalkCoordinates laplacian model section = do
  inputValues <-
    laplacianCoordinateVectorWithCellCoordinates
      stalkCoordinates
      laplacian
      model
      section
  pure (laplacianResidualSquareMapFromCoordinateVector laplacian inputValues)

laplacianResidualSquareMap ::
  Ord cell =>
  (stalk -> [Double]) ->
  SheafLaplacian kind cell ->
  SheafModel owner cell witness ->
  TotalSectionStore owner cell stalk ->
  Either (SheafOperatorBuildError cell) (Map cell Double)
laplacianResidualSquareMap stalkCoordinates =
  laplacianResidualSquareMapWithCellCoordinates (const stalkCoordinates)

laplacianResidualSquaredNorm ::
  Ord cell =>
  (stalk -> [Double]) ->
  SheafLaplacian kind cell ->
  SheafModel owner cell witness ->
  TotalSectionStore owner cell stalk ->
  Either (SheafOperatorBuildError cell) Double
laplacianResidualSquaredNorm stalkCoordinates laplacian model section =
  laplacianResidualSquaredNormFromCoordinateVector laplacian
    <$> laplacianCoordinateVectorWithCellCoordinates (const stalkCoordinates) laplacian model section

packedLaplacianDenseCoordinateVectorWithCellCoordinates ::
  Ord cell =>
  (cell -> stalk -> [Double]) ->
  PackedSheafLaplacian kind cell ->
  SheafModel owner cell witness ->
  TotalSectionStore owner cell stalk ->
  Either (SheafOperatorBuildError cell) (Unboxed.Vector Double)
packedLaplacianDenseCoordinateVectorWithCellCoordinates stalkCoordinates packedLaplacian =
  sectionCoordinateVectorDense
    stalkCoordinates
    (slDomainBasis (pslSymbolic packedLaplacian))

packedLaplacianResidualVectorFromDenseCoordinates ::
  PackedSheafLaplacian kind cell ->
  Unboxed.Vector Double ->
  Either (SheafOperatorBuildError cell) (Unboxed.Vector Double)
packedLaplacianResidualVectorFromDenseCoordinates packedLaplacian =
  applyPackedSparseOperatorDenseAsSheafOperator
    OperatorDomainBasis
    (pslPackedOperator packedLaplacian)

packedLaplacianResidualSquaredNormFromDenseCoordinates ::
  PackedSheafLaplacian kind cell ->
  Unboxed.Vector Double ->
  Either (SheafOperatorBuildError cell) Double
packedLaplacianResidualSquaredNormFromDenseCoordinates packedLaplacian inputValues =
  residualSquaredNorm <$> packedLaplacianResidualVectorFromDenseCoordinates packedLaplacian inputValues

packedLaplacianResidualVectorWithCellCoordinates ::
  Ord cell =>
  (cell -> stalk -> [Double]) ->
  PackedSheafLaplacian kind cell ->
  SheafModel owner cell witness ->
  TotalSectionStore owner cell stalk ->
  Either (SheafOperatorBuildError cell) (Unboxed.Vector Double)
packedLaplacianResidualVectorWithCellCoordinates stalkCoordinates packedLaplacian model section = do
  inputValues <-
    packedLaplacianDenseCoordinateVectorWithCellCoordinates
      stalkCoordinates
      packedLaplacian
      model
      section
  packedLaplacianResidualVectorFromDenseCoordinates packedLaplacian inputValues

packedLaplacianResidualSquareMapWithCellCoordinates ::
  Ord cell =>
  (cell -> stalk -> [Double]) ->
  PackedSheafLaplacian kind cell ->
  SheafModel owner cell witness ->
  TotalSectionStore owner cell stalk ->
  Either (SheafOperatorBuildError cell) (Map cell Double)
packedLaplacianResidualSquareMapWithCellCoordinates stalkCoordinates packedLaplacian model section = do
  residualValues <-
    packedLaplacianResidualVectorWithCellCoordinates
      stalkCoordinates
      packedLaplacian
      model
      section
  residualSquareMapFromVector
    (slCodomainBasis (pslSymbolic packedLaplacian))
    residualValues

packedLaplacianResidualSquaredNormWithCellCoordinates ::
  Ord cell =>
  (cell -> stalk -> [Double]) ->
  PackedSheafLaplacian kind cell ->
  SheafModel owner cell witness ->
  TotalSectionStore owner cell stalk ->
  Either (SheafOperatorBuildError cell) Double
packedLaplacianResidualSquaredNormWithCellCoordinates stalkCoordinates packedLaplacian model section =
  packedLaplacianResidualSquaredNormFromDenseCoordinates packedLaplacian
    =<< packedLaplacianDenseCoordinateVectorWithCellCoordinates stalkCoordinates packedLaplacian model section

residualSquaredNorm :: Unboxed.Vector Double -> Double
residualSquaredNorm residualValues =
  Unboxed.sum (Unboxed.map (\residualValue -> residualValue * residualValue) residualValues)

sectionCoordinateVector ::
  Ord cell =>
  (cell -> stalk -> [Double]) ->
  LinearBasis cell ->
  SheafModel owner cell witness ->
  TotalSectionStore owner cell stalk ->
  Either (SheafOperatorBuildError cell) (Map Int Double)
sectionCoordinateVector stalkCoordinates basis model section =
  Map.fromList . concat <$> traverse cellCoordinates (linearBasisCells basis)
  where
    cellCoordinates cell = do
      (offsetValue, dimensionValue) <-
        linearBasisCellSlotOrError OperatorDomainBasis basis cell
      coordinates <- stalkCoordinatesForCell stalkCoordinates OperatorDomainBasis basis model section cell
      pure
        ( zipWith
            (\localIndex coordinateValue -> (offsetValue + localIndex, coordinateValue))
            [0 .. dimensionValue - 1]
            coordinates
        )

sectionCoordinateVectorDense ::
  Ord cell =>
  (cell -> stalk -> [Double]) ->
  LinearBasis cell ->
  SheafModel owner cell witness ->
  TotalSectionStore owner cell stalk ->
  Either (SheafOperatorBuildError cell) (Unboxed.Vector Double)
sectionCoordinateVectorDense stalkCoordinates basis model section = do
  coordinateChunks <-
    traverse
      (stalkCoordinatesForCell stalkCoordinates OperatorDomainBasis basis model section)
      (linearBasisCells basis)
  let vectorValue = Unboxed.fromList (concat coordinateChunks)
  if Unboxed.length vectorValue == linearBasisCardinality basis
    then Right vectorValue
    else
      Left
        ( OperatorVectorLengthMismatch
            OperatorDomainBasis
            (linearBasisCardinality basis)
            (Unboxed.length vectorValue)
        )

stalkCoordinatesForCell ::
  Ord cell =>
  (cell -> stalk -> [Double]) ->
  OperatorBasisRole ->
  LinearBasis cell ->
  SheafModel owner cell witness ->
  TotalSectionStore owner cell stalk ->
  cell ->
  Either (SheafOperatorBuildError cell) [Double]
stalkCoordinatesForCell stalkCoordinates basisRole basis model section cell = do
  (_, dimensionValue) <- linearBasisCellSlotOrError basisRole basis cell
  stalk <-
    first
      (OperatorSectionLookupFailure cell)
      (totalStalkAt model cell section)
  let projectedCoordinates = stalkCoordinates cell stalk
  if length projectedCoordinates == dimensionValue
    then Right projectedCoordinates
    else Left (OperatorStalkCoordinateDimensionMismatch cell dimensionValue (length projectedCoordinates))

residualSquareMapFromSparseValues ::
  Ord cell =>
  LinearBasis cell ->
  Map Int Double ->
  Map cell Double
residualSquareMapFromSparseValues basis values =
  Map.fromListWith (+)
    ( fmap
        ( \(indexValue, coordinate) ->
            let residualValue =
                  Map.findWithDefault 0.0 indexValue values
             in (linearCoordinateCell coordinate, residualValue * residualValue)
        )
        (linearBasisIndexedCoordinates basis)
    )

residualSquareMapFromVector ::
  Ord cell =>
  LinearBasis cell ->
  Unboxed.Vector Double ->
  Either (SheafOperatorBuildError cell) (Map cell Double)
residualSquareMapFromVector basis residualValues =
  Map.fromListWith (+)
    <$> traverse coordinateContribution (linearBasisIndexedCoordinates basis)
  where
    expectedLength =
      linearBasisCardinality basis
    actualLength =
      Unboxed.length residualValues
    coordinateContribution (indexValue, coordinate) =
      case residualValues Unboxed.!? indexValue of
        Just residualValue ->
          Right
            ( linearCoordinateCell coordinate,
              residualValue * residualValue
            )
        Nothing ->
          Left
            (OperatorVectorLengthMismatch OperatorCodomainBasis expectedLength actualLength)

boundaryCoefficientByPair :: BoundaryIncidence Double -> Map (Int, Int) Double
boundaryCoefficientByPair incidence =
  boundaryEntries incidence
    & fmap
      ( \entry ->
          ( (sourceIndex entry, targetIndex entry),
            boundaryCoefficient entry
          )
      )
    & Map.fromListWith (+)

tarskiRestrictionEntries ::
  RestrictionIndex cell witness ->
  [Restriction cell witness]
tarskiRestrictionEntries restrictions =
  case portalRestrictions restrictions of
    [] ->
      restrictionEntries restrictions
    portalEntries ->
      portalEntries

restrictionGraphLaplacianIncidence ::
  Ord cell =>
  [Restriction cell witness] ->
  LinearBasis cell ->
  Either (SheafOperatorBuildError cell) (BoundaryIncidence Double)
restrictionGraphLaplacianIncidence entries basis = do
  matrixTerms <-
    concat <$> traverse (laplacianTerms basis) entries
  let mergedTerms =
        Map.fromListWith (+) matrixTerms
  mkSquareIncidence basis mergedTerms

restrictionGraphPropagationIncidence ::
  Ord cell =>
  [Restriction cell witness] ->
  LinearBasis cell ->
  Either (SheafOperatorBuildError cell) (BoundaryIncidence Double)
restrictionGraphPropagationIncidence entries basis = do
  propagationTerms <-
    concat <$> traverse (propagationTermsForEntry basis) entries
  let identityTerms =
        linearBasisIndexedCoordinates basis
          & fmap
            ( \(indexValue, _) ->
                ((indexValue, indexValue), 1.0)
            )
      mergedTerms =
        Map.fromListWith (+) (identityTerms <> propagationTerms)
  mkSquareIncidence basis mergedTerms

laplacianTerms ::
  Ord cell =>
  LinearBasis cell ->
  Restriction cell witness ->
  Either (SheafOperatorBuildError cell) [((Int, Int), Double)]
laplacianTerms basis restriction = do
  sourceIndexValue <-
    scalarCellIndex OperatorSourceBasis basis (rSource restriction)
  targetIndexValue <-
    scalarCellIndex OperatorTargetBasis basis (rTarget restriction)
  pure
    [ ((sourceIndexValue, sourceIndexValue), 1.0),
      ((targetIndexValue, targetIndexValue), 1.0),
      ((sourceIndexValue, targetIndexValue), -1.0),
      ((targetIndexValue, sourceIndexValue), -1.0)
    ]

propagationTermsForEntry ::
  Ord cell =>
  LinearBasis cell ->
  Restriction cell witness ->
  Either (SheafOperatorBuildError cell) [((Int, Int), Double)]
propagationTermsForEntry basis restriction = do
  sourceIndexValue <-
    scalarCellIndex OperatorSourceBasis basis (rSource restriction)
  targetIndexValue <-
    scalarCellIndex OperatorTargetBasis basis (rTarget restriction)
  pure
    [((targetIndexValue, sourceIndexValue), 1.0)]

scalarCellIndex ::
  Ord cell =>
  OperatorBasisRole ->
  LinearBasis cell ->
  cell ->
  Either (SheafOperatorBuildError cell) Int
scalarCellIndex basisRole basis cell =
  linearBasisCellSlotOrError basisRole basis cell >>= scalarOffset
  where
    scalarOffset (offsetValue, dimensionValue)
      | dimensionValue == 1 =
          Right offsetValue
      | otherwise =
          Left (OperatorExpectedScalarCell cell dimensionValue)

mkSquareIncidence ::
  LinearBasis cell ->
  Map (Int, Int) Double ->
  Either (SheafOperatorBuildError cell) (BoundaryIncidence Double)
mkSquareIncidence basis = mkBoundaryIncidenceFromPairs
    (linearBasisCardinality basis)
    (linearBasisCardinality basis)
    RowColumnPairs

differentialIncidenceToDouble :: GradedOperator cell Int -> BoundaryIncidence Double
differentialIncidenceToDouble =
  mapBoundaryCoefficients fromIntegral . gradedOperatorIncidence
