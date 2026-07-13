{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Site.Analysis.Scaffold
  ( SiteBoundaryAlgebra (..),
    SiteComplexScaffold (..),
    SiteComplexScaffoldError (..),
    SiteMorseReduction (..),
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
import Data.Function ((&))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Homology
  ( BasisCellRef (..),
    BoundaryIncidence,
    FiniteChainComplex,
    HomologicalDegree (..),
    HomologyFailure,
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
  { scsSite :: site,
    scsChainComplex :: FiniteChainComplex Int,
    scsCellsByDimension :: Map Int [cell],
    scsBasisRefs :: Map cell BasisCellRef,
    scsCellByBasisRef :: Map BasisCellRef cell
  }

type SiteComplexScaffoldError :: Type
data SiteComplexScaffoldError
  = SiteComplexMorseFailure HomologyFailure
  | SiteComplexRefinedMorseFailure HomologyFailure
  | SiteReducedBasisWithoutOriginalBasis BasisCellRef
  | SiteOriginalBasisWithoutCell BasisCellRef
  deriving stock (Eq, Show)

type SiteMorseReduction :: Type -> Type -> Type
data SiteMorseReduction site cell = SiteMorseReduction
  { smrScaffold :: SiteComplexScaffold site cell,
    smrMorseComplex :: MorseComplex Int,
    smrCriticalCellByReducedBasis :: Map BasisCellRef cell,
    smrRefinedMorseComplex :: RefinedMorseComplex Rational,
    smrRefinedCriticalCellByReducedBasis :: Map BasisCellRef cell
  }

mkSiteComplexScaffold ::
  Ord cell =>
  SiteBoundaryAlgebra site cell face ->
  site ->
  Either HomologyFailure (SiteComplexScaffold site cell)
mkSiteComplexScaffold boundaryAlgebra siteValue = do
  chainComplexValue <- siteChainComplexFromSite boundaryAlgebra siteValue
  pure (mkSiteComplexScaffoldFromCells siteValue (cellsByDimensionMap boundaryAlgebra siteValue) chainComplexValue)

mkSiteComplexScaffoldFromCells ::
  Ord cell =>
  site ->
  Map Int [cell] ->
  FiniteChainComplex Int ->
  SiteComplexScaffold site cell
mkSiteComplexScaffoldFromCells siteValue cellsByDimensionValue chainComplexValue =
  SiteComplexScaffold
    { scsSite = siteValue,
      scsChainComplex = chainComplexValue,
      scsCellsByDimension = cellsByDimensionValue,
      scsBasisRefs = basisRefsValue,
      scsCellByBasisRef = inverseBasisRefMap basisRefsValue
    }
  where
    basisRefsValue =
      basisRefMap cellsByDimensionValue

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
  Either SiteComplexScaffoldError (SiteMorseReduction site cell)
reduceSiteComplex =
  reduceSiteComplexWith defaultSiteCellHeuristic

reduceSiteComplexWith ::
  (BasisCellRef -> Double) ->
  SiteComplexScaffold site cell ->
  Either SiteComplexScaffoldError (SiteMorseReduction site cell)
reduceSiteComplexWith cellScore scaffoldValue = do
  let chainComplexValue = scsChainComplex scaffoldValue
  refinedMorseValue <-
    either
      (Left . SiteComplexRefinedMorseFailure)
      Right
      (refinedMorseComplex chainComplexValue cellScore)
  reduceSiteComplexWithRefined cellScore refinedMorseValue scaffoldValue

reduceSiteComplexWithRefined ::
  (BasisCellRef -> Double) ->
  RefinedMorseComplex Rational ->
  SiteComplexScaffold site cell ->
  Either SiteComplexScaffoldError (SiteMorseReduction site cell)
reduceSiteComplexWithRefined cellScore refinedMorseValue scaffoldValue = do
  let chainComplexValue = scsChainComplex scaffoldValue
      matchingValue = acyclicMatching chainComplexValue cellScore
  morseValue <-
    either
      (Left . SiteComplexMorseFailure)
      Right
      (morseComplex chainComplexValue matchingValue)
  criticalCellsByReducedBasisValue <- criticalCellsByReducedBasis scaffoldValue morseValue
  refinedCriticalCellsByReducedBasisValue <-
    criticalCellsByBasisMap
      scaffoldValue
      (rmcReducedComplex refinedMorseValue)
      (rmcCriticalBasis refinedMorseValue)
  pure
    SiteMorseReduction
      { smrScaffold = scaffoldValue,
        smrMorseComplex = morseValue,
        smrCriticalCellByReducedBasis = criticalCellsByReducedBasisValue,
        smrRefinedMorseComplex = refinedMorseValue,
        smrRefinedCriticalCellByReducedBasis = refinedCriticalCellsByReducedBasisValue
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
  Either SiteComplexScaffoldError (Map BasisCellRef cell)
criticalCellsByReducedBasis scaffoldValue morseValue =
  criticalCellsByBasisMap scaffoldValue (mcReducedComplex morseValue) (mcCriticalBasis morseValue)

criticalCellsByBasisMap ::
  SiteComplexScaffold site cell ->
  FiniteChainComplex r ->
  Map BasisCellRef BasisCellRef ->
  Either SiteComplexScaffoldError (Map BasisCellRef cell)
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
  Either SiteComplexScaffoldError cell
originalCellForBasisMap scaffoldValue criticalBasis reducedBasisRef =
  case Map.lookup reducedBasisRef criticalBasis of
    Nothing ->
      Left (SiteReducedBasisWithoutOriginalBasis reducedBasisRef)
    Just originalBasisRef ->
      maybe
        (Left (SiteOriginalBasisWithoutCell originalBasisRef))
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
