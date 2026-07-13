{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Moonlight.Analysis.Resolution
  ( AnalysisDepth (..),
    NerveAnalysis (..),
    HasHomologicalLayer (..),
    HasSheafLayer (..),
    HasMicrosupportedLayer (..),
    HasClassifiedLayer (..),
    HomologicalLayer (..),
    ResolutionSheafInput (..),
    SheafLayer (..),
    MicrosupportLayer (..),
    ClassificationLayer (..),
    ResolutionBoundaryCore (..),
    ResolutionBoundaryAnalysis (..),
    ResolutionBundle (..),
    homologicalAnalysis,
    buildSheafAnalysis,
    buildMicrosupportedAnalysis,
    buildClassifiedAnalysis,
    buildResolutionBundle,
    chainComplexOf,
    basisRefsOf,
    bettiNumbers,
    sheafCohomology,
    resolutionBoundaryCore,
    resolutionBoundaryAnalysis,
    resolutionMorseComplex,
    resolutionSpectralPages,
    resolutionRepresentativeCocycles,
    resolutionParsingDepth,
    resolutionSourceNodes,
    resolutionSourcePoset,
    resolutionCellCount,
    resolutionCriticalMicrosupportNodes,
    resolutionMicrosupportFiberNodes,
    resolutionWitnessClassesBySourceNode,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as V
import Moonlight.Derived.Morse (posetSheafCohomologyDims)
import Moonlight.Derived.Site
  ( DerivedPoset
  , FinObjectId (..)
  , derivedPosetNodes
  , mkDerivedPosetFromOrderEdges
  )
import Moonlight.Homology
  ( BasisCellRef (..),
    Bidegree,
    BoundaryIncidence,
    FiniteChainComplex,
    HomologicalDegree (..),
    HomologyFailure (..),
    MorseComplex (..),
    RepresentativeCocycle,
    RefinedMorseComplex (..),
    SpectralPage,
    acyclicMatching,
    bidegreeFromTotalDegree,
    boundaryEntries,
    cellDegree,
    cellIndex,
    cohomologyBasisAt,
    computeRationalSpectralPages,
    convergenceDepth,
    emptyBoundaryIncidence,
    filteredReducedFiltration,
    filteredRefinedMorseComplex,
    freeBettiVector,
    frmcRefinedMorseComplex,
    incidenceMatrixAt,
    maxHomologicalDegree,
    morseComplex,
    sourceCardinality,
    sourceIndex,
    targetIndex,
  )

type AnalysisDepth :: Type
data AnalysisDepth
  = Homological
  | Sheaf
  | Microsupported
  | Classified

type HomologicalLayer :: Type -> Type
data HomologicalLayer cell = HomologicalLayer
  { hlChainComplex :: FiniteChainComplex Int,
    hlBasisRefs :: Map cell BasisCellRef
  }

type ResolutionSheafInput :: Type -> Type -> Type
data ResolutionSheafInput cell stalk = ResolutionSheafInput
  { rsiNodeIdByCell :: Map cell Int,
    rsiCoverPairs :: [(cell, cell)],
    rsiStalkCache :: Map cell stalk,
    rsiStalkDimensionsByNode :: Map Int Int,
    rsiComparableRestrictions :: Map (Int, Int) (BoundaryIncidence Int),
    rsiRawFiltrationByBasisCell :: Map BasisCellRef Int
  }

type ResolutionBoundaryCore :: Type
data ResolutionBoundaryCore = ResolutionBoundaryCore
  { rbcSourcePoset :: DerivedPoset,
    rbcStalkDimensions :: Map FinObjectId Int,
    rbcComparableRestrictions :: Map (FinObjectId, FinObjectId) (BoundaryIncidence Int),
    rbcBasisCellBySourceNode :: Map Int BasisCellRef
  }

type SheafLayer :: Type -> Type -> Type
data SheafLayer cell stalk = SheafLayer
  { slBoundaryCore :: ResolutionBoundaryCore,
    slStalkCache :: Map cell stalk,
    slBidegreesByBasisCell :: Map BasisCellRef Bidegree,
    slPosetCohomologyDims :: [Int],
    slCorrectedFiltrationByBasisCell :: Map BasisCellRef Int
  }

type MicrosupportLayer :: Type
data MicrosupportLayer = MicrosupportLayer
  { mlCriticalSourceNodes :: Set.Set Int,
    mlFiberSourceNodes :: [Int]
  }

type ClassificationLayer :: Type -> Type
newtype ClassificationLayer witness = ClassificationLayer
  { clWitnessClassesBySourceNode :: Map Int witness
  }

type NerveAnalysis :: AnalysisDepth -> Type -> Type -> Type -> Type
data NerveAnalysis (depth :: AnalysisDepth) cell stalk witness where
  HomologicalAnalysis ::
    HomologicalLayer cell ->
    NerveAnalysis 'Homological cell stalk witness
  SheafAnalysis ::
    NerveAnalysis 'Homological cell stalk witness ->
    SheafLayer cell stalk ->
    NerveAnalysis 'Sheaf cell stalk witness
  MicrosupportedAnalysis ::
    NerveAnalysis 'Sheaf cell stalk witness ->
    MicrosupportLayer ->
    NerveAnalysis 'Microsupported cell stalk witness
  ClassifiedAnalysis ::
    NerveAnalysis 'Microsupported cell stalk witness ->
    ClassificationLayer witness ->
    NerveAnalysis 'Classified cell stalk witness

type HasHomologicalLayer :: AnalysisDepth -> Constraint
class HasHomologicalLayer (depth :: AnalysisDepth) where
  homologicalLayerOf :: NerveAnalysis depth cell stalk witness -> HomologicalLayer cell

instance HasHomologicalLayer 'Homological where
  homologicalLayerOf (HomologicalAnalysis layerValue) =
    layerValue

instance HasHomologicalLayer 'Sheaf where
  homologicalLayerOf (SheafAnalysis previousLayer _) =
    homologicalLayerOf previousLayer

instance HasHomologicalLayer 'Microsupported where
  homologicalLayerOf (MicrosupportedAnalysis previousLayer _) =
    homologicalLayerOf previousLayer

instance HasHomologicalLayer 'Classified where
  homologicalLayerOf (ClassifiedAnalysis previousLayer _) =
    homologicalLayerOf previousLayer

type HasSheafLayer :: AnalysisDepth -> Constraint
class HasSheafLayer (depth :: AnalysisDepth) where
  sheafLayerOf :: NerveAnalysis depth cell stalk witness -> SheafLayer cell stalk

instance HasSheafLayer 'Sheaf where
  sheafLayerOf (SheafAnalysis _ layerValue) =
    layerValue

instance HasSheafLayer 'Microsupported where
  sheafLayerOf (MicrosupportedAnalysis previousLayer _) =
    sheafLayerOf previousLayer

instance HasSheafLayer 'Classified where
  sheafLayerOf (ClassifiedAnalysis previousLayer _) =
    sheafLayerOf previousLayer

type HasMicrosupportedLayer :: AnalysisDepth -> Constraint
class HasMicrosupportedLayer (depth :: AnalysisDepth) where
  microsupportLayerOf :: NerveAnalysis depth cell stalk witness -> MicrosupportLayer

instance HasMicrosupportedLayer 'Microsupported where
  microsupportLayerOf (MicrosupportedAnalysis _ layerValue) =
    layerValue

instance HasMicrosupportedLayer 'Classified where
  microsupportLayerOf (ClassifiedAnalysis previousLayer _) =
    microsupportLayerOf previousLayer

type HasClassifiedLayer :: AnalysisDepth -> Constraint
class HasClassifiedLayer (depth :: AnalysisDepth) where
  classifiedLayerOf :: NerveAnalysis depth cell stalk witness -> ClassificationLayer witness

instance HasClassifiedLayer 'Classified where
  classifiedLayerOf (ClassifiedAnalysis _ layerValue) =
    layerValue

type ResolutionBoundaryAnalysis :: Type
data ResolutionBoundaryAnalysis = ResolutionBoundaryAnalysis
  { rbaSourcePoset :: DerivedPoset,
    rbaStalkDimensions :: Map FinObjectId Int,
    rbaComparableRestrictions :: Map (FinObjectId, FinObjectId) (BoundaryIncidence Int),
    rbaBasisCellBySourceNode :: Map Int BasisCellRef,
    rbaBidegreesByBasisCell :: Map BasisCellRef Bidegree,
    rbaSpectralPages :: [SpectralPage Rational],
    rbaPosetCohomologyDims :: [Int]
  }

type ResolutionBundle :: AnalysisDepth -> Type -> Type -> Type -> Type
data ResolutionBundle (depth :: AnalysisDepth) cell stalk witness = ResolutionBundle
  { rbAnalysis :: NerveAnalysis depth cell stalk witness,
    rbBoundaryAnalysis :: ResolutionBoundaryAnalysis,
    rbMorseComplex :: MorseComplex Int
  }

homologicalAnalysis ::
  FiniteChainComplex Int ->
  Map cell BasisCellRef ->
  NerveAnalysis 'Homological cell stalk witness
homologicalAnalysis chainComplexValue basisRefsValue =
  HomologicalAnalysis
    HomologicalLayer
      { hlChainComplex = chainComplexValue,
        hlBasisRefs = basisRefsValue
      }

buildSheafAnalysis ::
  Ord cell =>
  NerveAnalysis 'Homological cell stalk witness ->
  ResolutionSheafInput cell stalk ->
  Either HomologyFailure (NerveAnalysis 'Sheaf cell stalk witness)
buildSheafAnalysis homologicalValue sheafInput = do
  nodeByCell <-
    Map.fromList
      <$> traverse
        (\cellValue ->
            case Map.lookup cellValue (rsiNodeIdByCell sheafInput) of
              Just nodeOrdinal ->
                Right (cellValue, FinObjectId nodeOrdinal)
              Nothing ->
                Left (InvalidTopologyInput "missing source node id for basis cell")
        )
        (Map.keys basisRefs)
  sourcePoset <- sourcePosetFromInput nodeByCell sheafInput
  let stalkDimensions =
        Map.fromList
          (fmap (\(nodeOrdinal, dimensionValue) -> (FinObjectId nodeOrdinal, dimensionValue)) (Map.toList (rsiStalkDimensionsByNode sheafInput)))
      comparableRestrictions =
        Map.fromList
          ( fmap
              (\((leftNode, rightNode), incidenceValue) -> ((FinObjectId leftNode, FinObjectId rightNode), incidenceValue))
              (Map.toList (rsiComparableRestrictions sheafInput))
          )
      basisCellBySourceNode =
        Map.fromList
          ( fmap
              (\(cellValue, basisCellRef) ->
                  ( nodeOrdinalForCell nodeByCell cellValue,
                    basisCellRef
                  )
              )
              (Map.toList basisRefs)
          )
      correctedFiltrationByBasisCell =
        coboundaryAwareFiltrationMap
          chainComplexValue
          (\basisCellRef -> Map.findWithDefault 0 basisCellRef (rsiRawFiltrationByBasisCell sheafInput))
      bidegreesByBasisCell =
        Map.fromList
          ( fmap
              (\basisCellRef ->
                  ( basisCellRef,
                    bidegreeFromTotalDegree
                      (Map.findWithDefault 0 basisCellRef correctedFiltrationByBasisCell)
                      (cellDegree basisCellRef)
                  )
              )
              (Map.elems basisRefs)
          )
  posetCohomologyDims <-
    posetSheafCohomologyDims
      sourcePoset
      (\nodeValue -> Map.findWithDefault 0 nodeValue stalkDimensions)
      (\nodePair -> Map.findWithDefault emptyBoundaryIncidence nodePair comparableRestrictions)
  let boundaryCore =
        ResolutionBoundaryCore
          { rbcSourcePoset = sourcePoset,
            rbcStalkDimensions = stalkDimensions,
            rbcComparableRestrictions = comparableRestrictions,
            rbcBasisCellBySourceNode = basisCellBySourceNode
          }
      sheafLayerValue =
        SheafLayer
          { slBoundaryCore = boundaryCore,
            slStalkCache = rsiStalkCache sheafInput,
            slBidegreesByBasisCell = bidegreesByBasisCell,
            slPosetCohomologyDims = posetCohomologyDims,
            slCorrectedFiltrationByBasisCell = correctedFiltrationByBasisCell
          }
  pure (SheafAnalysis homologicalValue sheafLayerValue)
  where
    HomologicalLayer
      { hlChainComplex = chainComplexValue,
        hlBasisRefs = basisRefs
      } = homologicalLayerOf homologicalValue

buildMicrosupportedAnalysis ::
  NerveAnalysis 'Sheaf cell stalk witness ->
  Set.Set Int ->
  [Int] ->
  NerveAnalysis 'Microsupported cell stalk witness
buildMicrosupportedAnalysis sheafValue criticalSourceNodes fiberSourceNodes =
  MicrosupportedAnalysis
    sheafValue
    MicrosupportLayer
      { mlCriticalSourceNodes = criticalSourceNodes,
        mlFiberSourceNodes = fiberSourceNodes
      }

buildClassifiedAnalysis ::
  NerveAnalysis 'Microsupported cell stalk witness ->
  Map Int witness ->
  NerveAnalysis 'Classified cell stalk witness
buildClassifiedAnalysis microsupportedValue witnessClassesBySourceNode =
  ClassifiedAnalysis microsupportedValue (ClassificationLayer witnessClassesBySourceNode)

buildResolutionBundle ::
  (HasHomologicalLayer depth, HasSheafLayer depth) =>
  NerveAnalysis depth cell stalk witness ->
  Either HomologyFailure (ResolutionBundle depth cell stalk witness)
buildResolutionBundle analysisValue = do
  let chainComplexValue = chainComplexOf analysisValue
      sheafLayerValue = sheafLayerOf analysisValue
      matchingValue = acyclicMatching chainComplexValue defaultBasisCellHeuristic
  morseValue <- morseComplex chainComplexValue matchingValue
  let correctedFiltrationByBasisCell = slCorrectedFiltrationByBasisCell sheafLayerValue
      correctedFiltration basisCellRef =
        Map.findWithDefault 0 basisCellRef correctedFiltrationByBasisCell
  filteredMorseValue <- filteredRefinedMorseComplex chainComplexValue correctedFiltration defaultBasisCellHeuristic
  let refinedMorseValue = frmcRefinedMorseComplex filteredMorseValue
      spectralFiltration = filteredReducedFiltration filteredMorseValue
  spectralPagesValue <- computeRationalSpectralPages (rmcReducedComplex refinedMorseValue) spectralFiltration
  let boundaryCore = slBoundaryCore sheafLayerValue
      boundaryAnalysisValue =
        ResolutionBoundaryAnalysis
          { rbaSourcePoset = rbcSourcePoset boundaryCore,
            rbaStalkDimensions = rbcStalkDimensions boundaryCore,
            rbaComparableRestrictions = rbcComparableRestrictions boundaryCore,
            rbaBasisCellBySourceNode = rbcBasisCellBySourceNode boundaryCore,
            rbaBidegreesByBasisCell = slBidegreesByBasisCell sheafLayerValue,
            rbaSpectralPages = spectralPagesValue,
            rbaPosetCohomologyDims = slPosetCohomologyDims sheafLayerValue
          }
  pure
    ResolutionBundle
      { rbAnalysis = analysisValue,
        rbBoundaryAnalysis = boundaryAnalysisValue,
        rbMorseComplex = morseValue
      }

chainComplexOf ::
  HasHomologicalLayer depth =>
  NerveAnalysis depth cell stalk witness ->
  FiniteChainComplex Int
chainComplexOf =
  hlChainComplex . homologicalLayerOf

basisRefsOf ::
  HasHomologicalLayer depth =>
  NerveAnalysis depth cell stalk witness ->
  Map cell BasisCellRef
basisRefsOf =
  hlBasisRefs . homologicalLayerOf

bettiNumbers ::
  HasHomologicalLayer depth =>
  NerveAnalysis depth cell stalk witness ->
  [Int]
bettiNumbers =
  freeBettiVector . chainComplexOf

sheafCohomology ::
  HasSheafLayer depth =>
  NerveAnalysis depth cell stalk witness ->
  [Int]
sheafCohomology =
  slPosetCohomologyDims . sheafLayerOf

resolutionBoundaryCore ::
  HasSheafLayer depth =>
  ResolutionBundle depth cell stalk witness ->
  ResolutionBoundaryCore
resolutionBoundaryCore =
  slBoundaryCore . sheafLayerOf . rbAnalysis

resolutionBoundaryAnalysis ::
  ResolutionBundle depth cell stalk witness ->
  ResolutionBoundaryAnalysis
resolutionBoundaryAnalysis =
  rbBoundaryAnalysis

resolutionMorseComplex ::
  ResolutionBundle depth cell stalk witness ->
  MorseComplex Int
resolutionMorseComplex =
  rbMorseComplex

resolutionSpectralPages ::
  ResolutionBundle depth cell stalk witness ->
  [SpectralPage Rational]
resolutionSpectralPages =
  rbaSpectralPages . rbBoundaryAnalysis

resolutionRepresentativeCocycles ::
  HasHomologicalLayer depth =>
  HomologicalDegree ->
  ResolutionBundle depth cell stalk witness ->
  [RepresentativeCocycle Rational Int]
resolutionRepresentativeCocycles degreeValue resolutionValue =
  cohomologyBasisAt (chainComplexOf (rbAnalysis resolutionValue)) degreeValue

resolutionParsingDepth ::
  ResolutionBundle depth cell stalk witness ->
  Int
resolutionParsingDepth =
  convergenceDepth . resolutionSpectralPages

resolutionSourceNodes ::
  ResolutionBundle depth cell stalk witness ->
  [Int]
resolutionSourceNodes resolutionValue =
  fmap
    (\(FinObjectId nodeOrdinal) -> nodeOrdinal)
    (V.toList (derivedPosetNodes (resolutionSourcePoset resolutionValue)))

resolutionSourcePoset ::
  ResolutionBundle depth cell stalk witness ->
  DerivedPoset
resolutionSourcePoset =
  rbaSourcePoset . rbBoundaryAnalysis

resolutionCellCount ::
  HasHomologicalLayer depth =>
  ResolutionBundle depth cell stalk witness ->
  Int
resolutionCellCount =
  Map.size . basisRefsOf . rbAnalysis

resolutionCriticalMicrosupportNodes ::
  HasMicrosupportedLayer depth =>
  ResolutionBundle depth cell stalk witness ->
  Set.Set Int
resolutionCriticalMicrosupportNodes =
  mlCriticalSourceNodes . microsupportLayerOf . rbAnalysis

resolutionMicrosupportFiberNodes ::
  HasMicrosupportedLayer depth =>
  ResolutionBundle depth cell stalk witness ->
  [Int]
resolutionMicrosupportFiberNodes =
  mlFiberSourceNodes . microsupportLayerOf . rbAnalysis

resolutionWitnessClassesBySourceNode ::
  HasClassifiedLayer depth =>
  ResolutionBundle depth cell stalk witness ->
  Map Int witness
resolutionWitnessClassesBySourceNode =
  clWitnessClassesBySourceNode . classifiedLayerOf . rbAnalysis

nodeOrdinalForCell :: Ord cell => Map cell FinObjectId -> cell -> Int
nodeOrdinalForCell nodeByCell cellValue =
  case Map.lookup cellValue nodeByCell of
    Just (FinObjectId nodeOrdinal) ->
      nodeOrdinal
    Nothing ->
      0

sourcePosetFromInput ::
  Ord cell =>
  Map cell FinObjectId ->
  ResolutionSheafInput cell stalk ->
  Either HomologyFailure DerivedPoset
sourcePosetFromInput nodeByCell sheafInput = do
  coverPairs <-
    traverse
      (\(leftCell, rightCell) ->
          case (Map.lookup leftCell nodeByCell, Map.lookup rightCell nodeByCell) of
            (Just leftNode, Just rightNode) ->
              Right (leftNode, rightNode)
            _ ->
              Left (InvalidTopologyInput "cover references cell without source node id")
      )
      (rsiCoverPairs sheafInput)
  first
    (InvalidTopologyInput . show)
    (mkDerivedPosetFromOrderEdges (Set.toAscList (Set.fromList (Map.elems nodeByCell))) coverPairs)

defaultBasisCellHeuristic :: BasisCellRef -> Double
defaultBasisCellHeuristic basisCellRef =
  case cellDegree basisCellRef of
    HomologicalDegree degreeValue ->
      fromIntegral degreeValue + fromIntegral (cellIndex basisCellRef) / 1000000.0

coboundaryAwareFiltrationMap ::
  FiniteChainComplex r ->
  (BasisCellRef -> Int) ->
  Map BasisCellRef Int
coboundaryAwareFiltrationMap finiteComplex rawFiltration =
  let HomologicalDegree maxDegree = maxHomologicalDegree finiteComplex
   in foldl'
        (assignAtDegree finiteComplex rawFiltration)
        Map.empty
        [0 .. maxDegree]

assignAtDegree ::
  FiniteChainComplex r ->
  (BasisCellRef -> Int) ->
  Map BasisCellRef Int ->
  Int ->
  Map BasisCellRef Int
assignAtDegree finiteComplex rawFiltration assigned degreeValue =
  let incidence = incidenceMatrixAt finiteComplex (HomologicalDegree degreeValue)
   in foldl'
        (\currentAssigned sourceCellIndex ->
            let cellRef = BasisCellRef (HomologicalDegree degreeValue) sourceCellIndex
                rawFiltrationValue = rawFiltration cellRef
                maxFaceFiltrationValue =
                  maximumFaceFiltration finiteComplex rawFiltration currentAssigned degreeValue sourceCellIndex
             in Map.insert cellRef (max rawFiltrationValue maxFaceFiltrationValue) currentAssigned
        )
        assigned
        [0 .. sourceCardinality incidence - 1]

maximumFaceFiltration ::
  FiniteChainComplex r ->
  (BasisCellRef -> Int) ->
  Map BasisCellRef Int ->
  Int ->
  Int ->
  Int
maximumFaceFiltration finiteComplex rawFiltration assigned degreeValue sourceCellIndex
  | degreeValue <= 0 =
      0
  | otherwise =
      let incidence = incidenceMatrixAt finiteComplex (HomologicalDegree degreeValue)
          faceDegree = HomologicalDegree (degreeValue - 1)
          faceRefs =
            fmap
              ((\faceIndex -> BasisCellRef faceDegree faceIndex) . targetIndex)
              (filter ((== sourceCellIndex) . sourceIndex) (boundaryEntries incidence))
          lookupFiltration cellRef =
            Map.findWithDefault (rawFiltration cellRef) cellRef assigned
       in foldl' (\accumulator faceRef -> max accumulator (lookupFiltration faceRef)) 0 faceRefs
