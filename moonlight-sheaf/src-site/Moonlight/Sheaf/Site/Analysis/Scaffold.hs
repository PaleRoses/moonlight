{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Site.Analysis.Scaffold
  ( SiteBoundaryAlgebra (..),
    SiteComplexScaffold,
    scsSite,
    scsChainComplex,
    scsCellsByDimension,
    scsBasisRefs,
    scsCellByBasisRef,
    SiteMorseReduction,
    smrScaffold,
    smrMorseComplex,
    smrCriticalCellByReducedBasis,
    smrRefinedMorseComplex,
    smrRefinedCriticalCellByReducedBasis,
    defaultSiteCellHeuristic,
    grothendieckBoundaryAlgebra,
    grothendieckBoundaryCells,
    grothendieckChainComplexFromSite,
    mkGrothendieckComplexScaffold,
    mkNerveComplexScaffold,
    mkSiteComplexScaffoldFromCells,
    mkSiteComplexScaffold,
    nerveBoundaryAlgebra,
    reduceSiteComplex,
    reduceSiteComplexWith,
    siteBoundaryCells,
    siteChainComplexFromSite,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Homology
  ( BasisCellRef (..),
    BoundaryIncidence,
    FiniteChainComplex,
    HomologicalDegree (..),
    HomologyFailure (..),
    TopologyInputObstruction (..),
    MorseComplex (..),
    RefinedMorseComplex (..),
    acyclicMatching,
    emptyBoundaryIncidence,
    emptyBoundaryIncidenceOf,
    incidenceMatrixAt,
    materializeIncidenceBoundary,
    maxHomologicalDegree,
    morseComplex,
    mkFiniteChainComplexChecked,
    refinedMorseComplex,
    sourceCardinality,
  )
import Moonlight.Sheaf.Site.Grothendieck
  ( GrothendieckCell,
    GrothendieckFaceMorphism,
    GrothendieckSite,
    grothendieckCellDimension,
    grothendieckFaceMorphismOrientation,
    grothendieckFaceMorphismSource,
    grothendieckFaceMorphismTarget,
    grothendieckSiteDepth,
    grothendieckSiteCellsAtDimension,
    grothendieckSiteFaceMorphisms,
  )
import Moonlight.Sheaf.Site.Internal.Face (orientationMapForSourceDimension)
import Moonlight.Sheaf.Site.Construction.Nerve
  ( CellKey (..),
    FaceMorphism,
    faceMorphismOrientation,
    faceMorphismSource,
    faceMorphismTarget,
    NerveCell,
    NerveSite,
    nerveCellKey,
    nerveSiteDepth,
    siteCellsAtDimension,
    siteFaceMorphisms,
  )
import Numeric.Natural (Natural)

type SiteBoundaryAlgebra :: Type -> Type -> Type -> Type
data SiteBoundaryAlgebra site cell face = SiteBoundaryAlgebra
  { sbaDepth :: site -> Natural,
    sbaCellsAtDimension :: site -> Int -> [cell],
    sbaFaceMorphisms :: site -> [face],
    sbaFaceSource :: face -> cell,
    sbaFaceTarget :: face -> cell,
    sbaFaceOrientation :: face -> Int,
    sbaCellDimension :: cell -> Int
  }

type SiteComplexScaffold :: Type -> Type -> Type
data SiteComplexScaffold site cell = SiteComplexScaffold
  { siteComplexScaffoldSiteInternal :: site,
    siteComplexScaffoldChainInternal :: FiniteChainComplex Int,
    siteComplexScaffoldCellsByDimensionInternal :: Map Int [cell],
    siteComplexScaffoldBasisRefsInternal :: Map cell BasisCellRef,
    siteComplexScaffoldCellByBasisRefInternal :: Map BasisCellRef cell
  }

type SiteMorseReduction :: Type -> Type -> Type
data SiteMorseReduction site cell = SiteMorseReduction
  { siteMorseScaffoldInternal :: SiteComplexScaffold site cell,
    siteMorseComplexInternal :: MorseComplex Int,
    siteMorseCriticalCellByReducedBasisInternal :: Map BasisCellRef cell,
    siteMorseRefinedComplexInternal :: RefinedMorseComplex Rational,
    siteMorseRefinedCriticalCellByReducedBasisInternal :: Map BasisCellRef cell
  }

scsSite :: SiteComplexScaffold site cell -> site
scsSite = siteComplexScaffoldSiteInternal

scsChainComplex :: SiteComplexScaffold site cell -> FiniteChainComplex Int
scsChainComplex = siteComplexScaffoldChainInternal

scsCellsByDimension :: SiteComplexScaffold site cell -> Map Int [cell]
scsCellsByDimension = siteComplexScaffoldCellsByDimensionInternal

scsBasisRefs :: SiteComplexScaffold site cell -> Map cell BasisCellRef
scsBasisRefs = siteComplexScaffoldBasisRefsInternal

scsCellByBasisRef :: SiteComplexScaffold site cell -> Map BasisCellRef cell
scsCellByBasisRef = siteComplexScaffoldCellByBasisRefInternal

smrScaffold :: SiteMorseReduction site cell -> SiteComplexScaffold site cell
smrScaffold = siteMorseScaffoldInternal

smrMorseComplex :: SiteMorseReduction site cell -> MorseComplex Int
smrMorseComplex = siteMorseComplexInternal

smrCriticalCellByReducedBasis :: SiteMorseReduction site cell -> Map BasisCellRef cell
smrCriticalCellByReducedBasis = siteMorseCriticalCellByReducedBasisInternal

smrRefinedMorseComplex :: SiteMorseReduction site cell -> RefinedMorseComplex Rational
smrRefinedMorseComplex = siteMorseRefinedComplexInternal

smrRefinedCriticalCellByReducedBasis :: SiteMorseReduction site cell -> Map BasisCellRef cell
smrRefinedCriticalCellByReducedBasis = siteMorseRefinedCriticalCellByReducedBasisInternal

mkSiteComplexScaffold ::
  Ord cell =>
  SiteBoundaryAlgebra site cell face ->
  site ->
  Either HomologyFailure (SiteComplexScaffold site cell)
mkSiteComplexScaffold boundaryAlgebra siteValue = do
  chainComplexValue <- siteChainComplexFromSite boundaryAlgebra siteValue
  mkSiteComplexScaffoldFromCells siteValue (cellsByDimensionMap boundaryAlgebra siteValue) chainComplexValue

mkSiteComplexScaffoldFromCells ::
  Ord cell =>
  site ->
  Map Int [cell] ->
  FiniteChainComplex Int ->
  Either HomologyFailure (SiteComplexScaffold site cell)
mkSiteComplexScaffoldFromCells siteValue cellsByDimensionValue chainComplexValue = do
  traverse_ validateDistinctDimension (Map.toAscList cellsByDimensionValue)
  traverse_ validateDimensionCardinality dimensionsToValidate
  let inverseBasisRefs = inverseBasisRefMap basisRefsValue
      expectedBasisCardinality = sum (fmap length (Map.elems cellsByDimensionValue))
      actualBasisCardinality = Map.size basisRefsValue
  if actualBasisCardinality /= expectedBasisCardinality || Map.size inverseBasisRefs /= expectedBasisCardinality
    then Left (TopologyInputRejected (TopologyBasisCardinalityMismatch expectedBasisCardinality actualBasisCardinality))
    else
      Right
        SiteComplexScaffold
          { siteComplexScaffoldSiteInternal = siteValue,
            siteComplexScaffoldChainInternal = chainComplexValue,
            siteComplexScaffoldCellsByDimensionInternal = cellsByDimensionValue,
            siteComplexScaffoldBasisRefsInternal = basisRefsValue,
            siteComplexScaffoldCellByBasisRefInternal = inverseBasisRefs
          }
  where
    basisRefsValue =
      basisRefMap cellsByDimensionValue

    HomologicalDegree maximumChainDegree =
      maxHomologicalDegree chainComplexValue

    dimensionsToValidate =
      nubOrd (Map.keys cellsByDimensionValue <> enumerateBetween 0 maximumChainDegree)

    validateDistinctDimension :: Ord cellValue => (Int, [cellValue]) -> Either HomologyFailure ()
    validateDistinctDimension (dimensionValue, cells) =
      let distinctCount = Set.size (Set.fromList cells)
       in if distinctCount == length cells
            then Right ()
            else Left (TopologyInputRejected (TopologyDuplicateCells dimensionValue (length cells - distinctCount)))

    validateDimensionCardinality dimensionValue =
      let expectedCardinality = length (Map.findWithDefault [] dimensionValue cellsByDimensionValue)
          actualCardinality = sourceCardinality (incidenceMatrixAt chainComplexValue (HomologicalDegree dimensionValue))
       in if actualCardinality == expectedCardinality
            then Right ()
            else Left (ChainComplexShapeMismatch dimensionValue expectedCardinality actualCardinality)

mkNerveComplexScaffold ::
  NerveSite tag ->
  Either HomologyFailure (SiteComplexScaffold (NerveSite tag) (NerveCell tag))
mkNerveComplexScaffold =
  mkSiteComplexScaffold nerveBoundaryAlgebra

mkGrothendieckComplexScaffold ::
  GrothendieckSite system ->
  Either HomologyFailure (SiteComplexScaffold (GrothendieckSite system) (GrothendieckCell system))
mkGrothendieckComplexScaffold =
  mkSiteComplexScaffold grothendieckBoundaryAlgebra

reduceSiteComplex ::
  SiteComplexScaffold site cell ->
  Either HomologyFailure (SiteMorseReduction site cell)
reduceSiteComplex =
  reduceSiteComplexWith defaultSiteCellHeuristic

reduceSiteComplexWith ::
  (BasisCellRef -> Double) ->
  SiteComplexScaffold site cell ->
  Either HomologyFailure (SiteMorseReduction site cell)
reduceSiteComplexWith cellScore scaffoldValue = do
  let chainComplexValue = scsChainComplex scaffoldValue
  refinedMorseValue <- refinedMorseComplex chainComplexValue cellScore
  reduceSiteComplexWithRefined cellScore refinedMorseValue scaffoldValue

reduceSiteComplexWithRefined ::
  (BasisCellRef -> Double) ->
  RefinedMorseComplex Rational ->
  SiteComplexScaffold site cell ->
  Either HomologyFailure (SiteMorseReduction site cell)
reduceSiteComplexWithRefined cellScore refinedMorseValue scaffoldValue = do
  let chainComplexValue = scsChainComplex scaffoldValue
      matchingValue = acyclicMatching chainComplexValue cellScore
  morseValue <- morseComplex chainComplexValue matchingValue
  criticalCellsByReducedBasisValue <- criticalCellsByReducedBasis scaffoldValue morseValue
  refinedCriticalCellsByReducedBasisValue <-
    criticalCellsByBasisMap
      scaffoldValue
      (rmcReducedComplex refinedMorseValue)
      (rmcCriticalBasis refinedMorseValue)
  pure
    SiteMorseReduction
      { siteMorseScaffoldInternal = scaffoldValue,
        siteMorseComplexInternal = morseValue,
        siteMorseCriticalCellByReducedBasisInternal = criticalCellsByReducedBasisValue,
        siteMorseRefinedComplexInternal = refinedMorseValue,
        siteMorseRefinedCriticalCellByReducedBasisInternal = refinedCriticalCellsByReducedBasisValue
      }

defaultSiteCellHeuristic :: BasisCellRef -> Double
defaultSiteCellHeuristic basisCellRef =
  case cellDegree basisCellRef of
    HomologicalDegree degreeValue ->
      fromIntegral degreeValue + fromIntegral (cellIndex basisCellRef) / 1000000.0

siteChainComplexFromSite ::
  Ord cell =>
  SiteBoundaryAlgebra site cell face ->
  site ->
  Either HomologyFailure (FiniteChainComplex Int)
siteChainComplexFromSite boundaryAlgebra siteValue =
  let maxDimensionValue = fromIntegral (sbaDepth boundaryAlgebra siteValue)
      positiveDimensions = enumerateBetween 1 maxDimensionValue
   in Map.fromList
        <$> traverse (dimensionIncidence boundaryAlgebra siteValue) positiveDimensions
        >>= \incidenceByDimension ->
          mkFiniteChainComplexChecked
            (HomologicalDegree maxDimensionValue)
            ( \(HomologicalDegree dimensionValue) ->
                if dimensionValue <= 0
                  then zeroBoundary boundaryAlgebra siteValue
                  else Map.findWithDefault emptyBoundaryIncidence dimensionValue incidenceByDimension
            )

grothendieckChainComplexFromSite ::
  GrothendieckSite system ->
  Either HomologyFailure (FiniteChainComplex Int)
grothendieckChainComplexFromSite =
  siteChainComplexFromSite grothendieckBoundaryAlgebra

siteBoundaryCells ::
  Ord cell =>
  SiteBoundaryAlgebra site cell face ->
  site ->
  cell ->
  [cell]
siteBoundaryCells boundaryAlgebra siteValue sourceCell =
  sbaFaceMorphisms boundaryAlgebra siteValue
    & filter ((== sourceCell) . sbaFaceSource boundaryAlgebra)
    & fmap (sbaFaceTarget boundaryAlgebra)
    & nubOrd

grothendieckBoundaryCells ::
  GrothendieckSite system ->
  GrothendieckCell system ->
  [GrothendieckCell system]
grothendieckBoundaryCells =
  siteBoundaryCells grothendieckBoundaryAlgebra

nerveBoundaryAlgebra :: SiteBoundaryAlgebra (NerveSite tag) (NerveCell tag) (FaceMorphism tag)
nerveBoundaryAlgebra =
  SiteBoundaryAlgebra
    { sbaDepth = nerveSiteDepth,
      sbaCellsAtDimension = cellsAtNaturalDimension siteCellsAtDimension,
      sbaFaceMorphisms = siteFaceMorphisms,
      sbaFaceSource = faceMorphismSource,
      sbaFaceTarget = faceMorphismTarget,
      sbaFaceOrientation = faceMorphismOrientation,
      sbaCellDimension = nerveCellDimensionInt
    }

grothendieckBoundaryAlgebra ::
  SiteBoundaryAlgebra
    (GrothendieckSite system)
    (GrothendieckCell system)
    (GrothendieckFaceMorphism system)
grothendieckBoundaryAlgebra =
  SiteBoundaryAlgebra
    { sbaDepth = grothendieckSiteDepth,
      sbaCellsAtDimension = cellsAtNaturalDimension grothendieckSiteCellsAtDimension,
      sbaFaceMorphisms = grothendieckSiteFaceMorphisms,
      sbaFaceSource = grothendieckFaceMorphismSource,
      sbaFaceTarget = grothendieckFaceMorphismTarget,
      sbaFaceOrientation = grothendieckFaceMorphismOrientation,
      sbaCellDimension = grothendieckCellDimension
    }

nerveCellDimensionInt :: NerveCell tag -> Int
nerveCellDimensionInt =
  fromIntegral . ckDimension . nerveCellKey

criticalCellsByReducedBasis ::
  SiteComplexScaffold site cell ->
  MorseComplex Int ->
  Either HomologyFailure (Map BasisCellRef cell)
criticalCellsByReducedBasis scaffoldValue morseValue =
  criticalCellsByBasisMap scaffoldValue (mcReducedComplex morseValue) (mcCriticalBasis morseValue)

criticalCellsByBasisMap ::
  SiteComplexScaffold site cell ->
  FiniteChainComplex r ->
  Map BasisCellRef BasisCellRef ->
  Either HomologyFailure (Map BasisCellRef cell)
criticalCellsByBasisMap scaffoldValue reducedComplex criticalBasis =
  Map.fromList
    <$> traverse
      ( \reducedBasisRef ->
          fmap
            (\cellValue -> (reducedBasisRef, cellValue))
            (originalCellForBasisMap scaffoldValue criticalBasis reducedBasisRef)
      )
      (basisRefsOfComplex reducedComplex)

originalCellForBasisMap ::
  SiteComplexScaffold site cell ->
  Map BasisCellRef BasisCellRef ->
  BasisCellRef ->
  Either HomologyFailure cell
originalCellForBasisMap scaffoldValue criticalBasis reducedBasisRef =
  case Map.lookup reducedBasisRef criticalBasis of
    Nothing ->
      Left (MissingCriticalBasisProvenance reducedBasisRef)
    Just originalBasisRef ->
      maybe
        (Left (MissingCriticalBasisProvenance originalBasisRef))
        Right
        (Map.lookup originalBasisRef (scsCellByBasisRef scaffoldValue))

basisRefsOfComplex :: FiniteChainComplex r -> [BasisCellRef]
basisRefsOfComplex finiteComplex =
  let HomologicalDegree maxDegreeValue = maxHomologicalDegree finiteComplex
   in foldMap
        ( \degreeValue ->
            let homologicalDegreeValue = HomologicalDegree degreeValue
                cardinality = sourceCardinality (incidenceMatrixAt finiteComplex homologicalDegreeValue)
             in fmap
                  (BasisCellRef homologicalDegreeValue)
                  (enumerateFromZero cardinality)
        )
        (enumerateBetween 0 maxDegreeValue)

dimensionIncidence ::
  Ord cell =>
  SiteBoundaryAlgebra site cell face ->
  site ->
  Int ->
  Either HomologyFailure (Int, BoundaryIncidence Int)
dimensionIncidence boundaryAlgebra siteValue sourceDimensionValue =
  fmap
    (\incidenceValue -> (sourceDimensionValue, incidenceValue))
    (incidenceAtDimension boundaryAlgebra sourceDimensionValue siteValue)

incidenceAtDimension ::
  Ord cell =>
  SiteBoundaryAlgebra site cell face ->
  Int ->
  site ->
  Either HomologyFailure (BoundaryIncidence Int)
incidenceAtDimension boundaryAlgebra sourceDimensionValue siteValue =
  if sourceDimensionValue <= 0
    then Right emptyBoundaryIncidence
    else
      let boundaryRows = boundaryRowsForDimension boundaryAlgebra sourceDimensionValue siteValue
       in materializeIncidenceBoundary
            (\sourceCell -> Map.findWithDefault [] sourceCell boundaryRows)
            (sbaCellsAtDimension boundaryAlgebra siteValue sourceDimensionValue)
            (sbaCellsAtDimension boundaryAlgebra siteValue (sourceDimensionValue - 1))

cellsByDimensionMap ::
  SiteBoundaryAlgebra site cell face ->
  site ->
  Map Int [cell]
cellsByDimensionMap boundaryAlgebra siteValue =
  let maxDimensionValue = fromIntegral (sbaDepth boundaryAlgebra siteValue)
   in Map.fromList
        ( fmap
            ( \dimensionValue ->
                (dimensionValue, sbaCellsAtDimension boundaryAlgebra siteValue dimensionValue)
            )
            (enumerateBetween 0 maxDimensionValue)
        )

basisRefMap ::
  Ord cell =>
  Map Int [cell] ->
  Map cell BasisCellRef
basisRefMap cellsByDimensionValue =
  cellsByDimensionValue
    & Map.toAscList
    & foldMap refsAtDimension
    & Map.fromList

refsAtDimension ::
  (Int, [cell]) ->
  [(cell, BasisCellRef)]
refsAtDimension (dimensionValue, cellValues) =
  cellValues
    & zip (enumerateFromZero (length cellValues))
    & fmap
      ( \(cellIndexValue, cellValue) ->
          ( cellValue,
            BasisCellRef
              { cellDegree = HomologicalDegree dimensionValue,
                cellIndex = cellIndexValue
              }
          )
      )

boundaryRowsForDimension ::
  Ord cell =>
  SiteBoundaryAlgebra site cell face ->
  Int ->
  site ->
  Map cell [(Int, cell)]
boundaryRowsForDimension boundaryAlgebra sourceDimensionValue siteValue =
  orientationMapForDimension boundaryAlgebra sourceDimensionValue siteValue
    & Map.toList
    & fmap (\((sourceCell, targetCell), orientationValue) -> (sourceCell, [(orientationValue, targetCell)]))
    & Map.fromListWith (<>)

orientationMapForDimension ::
  Ord cell =>
  SiteBoundaryAlgebra site cell face ->
  Int ->
  site ->
  Map (cell, cell) Int
orientationMapForDimension boundaryAlgebra sourceDimensionValue =
  orientationMapForSourceDimension
    (sbaFaceSource boundaryAlgebra)
    (sbaCellDimension boundaryAlgebra)
    (sbaFaceTarget boundaryAlgebra)
    (sbaFaceOrientation boundaryAlgebra)
    sourceDimensionValue
    . sbaFaceMorphisms boundaryAlgebra

zeroBoundary ::
  SiteBoundaryAlgebra site cell face ->
  site ->
  BoundaryIncidence Int
zeroBoundary boundaryAlgebra siteValue =
  emptyBoundaryIncidenceOf
    (fromIntegral (length (sbaCellsAtDimension boundaryAlgebra siteValue 0)))
    0

inverseBasisRefMap :: Map cell BasisCellRef -> Map BasisCellRef cell
inverseBasisRefMap =
  Map.fromList . fmap (\(cellValue, basisCellRef) -> (basisCellRef, cellValue)) . Map.toList

cellsAtNaturalDimension ::
  (site -> Natural -> [cell]) ->
  site ->
  Int ->
  [cell]
cellsAtNaturalDimension cellsAt siteValue dimensionValue =
  if dimensionValue < 0
    then []
    else cellsAt siteValue (fromIntegral dimensionValue)

enumerateFromZero :: Int -> [Int]
enumerateFromZero upperBound
  | upperBound <= 0 = []
  | otherwise = [0 .. upperBound - 1]

enumerateBetween :: Int -> Int -> [Int]
enumerateBetween lowerBound upperBound =
  if upperBound < lowerBound
    then []
    else [lowerBound .. upperBound]
