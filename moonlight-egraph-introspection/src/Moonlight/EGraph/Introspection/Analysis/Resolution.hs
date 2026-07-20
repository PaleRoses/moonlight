{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Introspection.Analysis.Resolution
  ( ResolutionKernel (..),
    ResolutionBundle (..),
    ResolutionBoundaryCore (..),
    ResolutionBoundaryAnalysis (..),
    ResolutionAnalysisAlg (..),
    RewriteSiteScaffold,
    buildResolutionBundle,
    resolutionBoundaryCore,
    resolutionDerivedComplex,
    resolutionSourceNodes,
    resolutionSourcePoset,
    resolutionCellCount,
    resolutionCriticalMicrosupportNodes,
    resolutionMicrosupportFiberNodes,
    resolutionWitnessClassesBySourceNode,
    resolutionPruningGapAt,
    resolutionCocycleRuleClasses,
    contextPosetFromRewriteSystem,
    derivedFromFiniteChainComplex,
  )
where

import Data.Bifunctor (first)
import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Vector qualified as V
import Moonlight.Core (ZipMatch (..), HasConstructorTag, Pattern, RegionNodeId (..), RewriteRuleId)
import Moonlight.Derived.Morse (posetSheafCohomologyDims)
import Moonlight.Derived.Pruning (pruningGapAt)
import Moonlight.Derived.Morse (hypercohomologyDims)
import Moonlight.Derived.Morse (MicrosupportResult (..))
import Moonlight.Derived.Complex (Derived, derivedPoset)
import Moonlight.Derived.Site (Criticality (..))
import Moonlight.Derived.Site
  ( DerivedPoset
  , FinObjectId (..)
  , derivedPosetNodes
  , derivedPosetTopoAsc
  , leqChecked
  , mkDerivedPosetFromOrderEdges
  )
import Moonlight.Sheaf.Site
  ( GrothendieckCell,
    GrothendieckSite,
    grothendieckCellSingleMorphism,
    grothendieckSiteCells,
    mkGrothendieckSite,
  )
import Moonlight.Sheaf.Site
  ( SiteComplexScaffold,
    SiteMorseReduction,
    mkGrothendieckComplexScaffold,
    reduceSiteComplexWith,
    scsBasisRefs,
    scsChainComplex,
    scsSite,
    smrMorseComplex,
    smrScaffold,
  )
import Moonlight.Derived.Site
  ( derivedFromFiniteChainComplex
  )
import Moonlight.Sheaf.Site.Analysis.Microsupport
  ( localMicrosupportFromGenerators
  )
import Moonlight.Sheaf.Site
  ( LinearizedRestrictionModel,
    buildLinearizedRestrictionModel,
    interfaceStalkBasisLinearization,
    linearizedRestrictionComparableRestrictions,
    linearizedRestrictionStalkDimensions,
  )
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteSystem, RewriteTag, rcObjects, rcOrdinal, rewriteRuleIdOf)
import Moonlight.Sheaf.Section.Linearize
  ( StalkLinearization
  )
import Moonlight.Sheaf.Site (allContexts, contextLeq)
import Moonlight.Sheaf.Site
  ( InterfaceStalk (..),
    WitnessClass,
    grothendieckStalkFromCell,
    witnessClass,
  )
import Moonlight.Homology
  ( BasisCellRef (..),
    Bidegree,
    basisCellNodeId,
    basisIndexCellMapAtDegree,
    BoundaryIncidence, boundaryEntries, sourceCardinality, sourceIndex, targetIndex,
    emptyBoundaryIncidence,
    FiniteChainComplex,
    HomologicalDegree (..),
    HomologyFailure (..),
    MorseComplex,
    RefinedMorseComplex (..),
    RepresentativeCocycle,
    SpectralPage,
    bidegreeFromTotalDegree,
    cellDegree,
    cellIndex,
    cohomologyBasisAt,
    computeRationalSpectralPages,
    convergenceDepth,
    filteredReducedFiltration,
    filteredRefinedMorseComplex,
    frmcRefinedMorseComplex,
    incidenceMatrixAt,
    inverseBasisRefMap,
    maxHomologicalDegree,
    representativeTerms,
  )
import Moonlight.LinAlg (GF2)
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteMorphism)
import Numeric.Natural (Natural)

type RewriteSiteScaffold :: (Type -> Type) -> Type
type RewriteSiteScaffold f =
  SiteComplexScaffold
    (GrothendieckSite (RewriteSystem f))
    (GrothendieckCell (RewriteSystem f))

type ResolutionBoundaryAnalysis :: Type
data ResolutionBoundaryAnalysis = ResolutionBoundaryAnalysis
  { rbaSourcePoset :: DerivedPoset,
    rbaStalkDimensions :: Map FinObjectId Int,
    rbaComparableRestrictions :: Map (FinObjectId, FinObjectId) (BoundaryIncidence Int),
    rbaBasisCellBySourceNode :: Map RegionNodeId BasisCellRef,
    rbaBidegreesByBasisCell :: Map BasisCellRef Bidegree,
    rbaSpectralPages :: [SpectralPage Rational],
    rbaPosetCohomologyDims :: [Int]
  }

type ResolutionBoundaryCore :: Type
data ResolutionBoundaryCore = ResolutionBoundaryCore
  { rbcSourcePoset :: !DerivedPoset,
    rbcStalkDimensions :: !(Map FinObjectId Int),
    rbcComparableRestrictions :: !(Map (FinObjectId, FinObjectId) (BoundaryIncidence Int)),
    rbcBasisCellBySourceNode :: !(Map RegionNodeId BasisCellRef)
  }

type ResolutionKernel :: (Type -> Type) -> Type
data ResolutionKernel f = ResolutionKernel
  { rkRewriteSystem :: !(RewriteSystem f),
    rkScaffold :: !(RewriteSiteScaffold f),
    rkDerivedComplex :: !(Derived GF2),
    rkStalkCache :: !(Map (GrothendieckCell (RewriteSystem f)) (InterfaceStalk (RewriteTag f))),
    rkMicrosupport :: !MicrosupportResult,
    rkCoreAnalysis :: !ResolutionCoreAnalysis
  }

type ResolutionAnalysisAlg :: (Type -> Type) -> (Type -> Type) -> Type
data ResolutionAnalysisAlg f m = ResolutionAnalysisAlg
  { raMorse :: m (MorseComplex Int),
    raSpectralPages :: m [SpectralPage Rational],
    raLerayProfile :: m (IntMap Int),
    raRepresentativeCocycles :: HomologicalDegree -> m [RepresentativeCocycle Rational Int],
    raBoundaryAnalysis :: m ResolutionBoundaryAnalysis,
    raParsingDepth :: m Int
  }

type ResolutionBundle :: (Type -> Type) -> Type
data ResolutionBundle f = ResolutionBundle
  { rbKernel :: !(ResolutionKernel f),
    rbAnalysis :: !(ResolutionAnalysisAlg f (Either HomologyFailure))
  }

resolutionBoundaryCore :: ResolutionBundle f -> ResolutionBoundaryCore
resolutionBoundaryCore =
  boundaryCoreFromAnalysis . rkCoreAnalysis . rbKernel

boundaryCoreFromAnalysis :: ResolutionCoreAnalysis -> ResolutionBoundaryCore
boundaryCoreFromAnalysis core =
  ResolutionBoundaryCore
    { rbcSourcePoset = rcaSourcePoset core,
      rbcStalkDimensions = rcaStalkDimensions core,
      rbcComparableRestrictions = rcaComparableRestrictions core,
      rbcBasisCellBySourceNode = rcaBasisCellBySourceNode core
    }

buildResolutionKernel ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure (ResolutionKernel f)
buildResolutionKernel rewriteSystem depthValue = do
  microsupportValue <- localMicrosupportFromGenerators rewriteSystem
  analysisScaffold <- mkGrothendieckComplexScaffold (mkGrothendieckSite rewriteSystem depthValue)
  let stalkCache = stalkCacheFromScaffold analysisScaffold
      originalComplex = scsChainComplex analysisScaffold
  derivedComplex <-
    first (InvalidTopologyInput . show) (derivedFromFiniteChainComplex originalComplex)
  coreAnalysis <-
    buildResolutionBoundaryAnalysisCore
      interfaceStalkBasisLinearization analysisScaffold derivedComplex originalComplex stalkCache
  pure
    ResolutionKernel
      { rkRewriteSystem = rewriteSystem,
        rkScaffold = analysisScaffold,
        rkDerivedComplex = derivedComplex,
        rkStalkCache = stalkCache,
        rkMicrosupport = microsupportValue,
        rkCoreAnalysis = coreAnalysis
      }

deriveAnalysis ::
  ResolutionKernel f ->
  ResolutionAnalysisAlg f (Either HomologyFailure)
deriveAnalysis kernel =
  let scaffold = rkScaffold kernel
      stalkCache = rkStalkCache kernel
      complex = scsChainComplex scaffold
      derivedComplex = rkDerivedComplex kernel
      coreAnalysis = rkCoreAnalysis kernel
      reductionResult =
        reduceSiteComplexWith (basisCellHeuristicFromCache scaffold stalkCache) scaffold
      morseResult = smrMorseComplex <$> reductionResult
      spectralResult = do
        reductionValue <- reductionResult
        extendCoreWithSpectral
          scaffold
          (basisCellHeuristicFromCache scaffold stalkCache)
          reductionValue
          coreAnalysis
   in ResolutionAnalysisAlg
        { raMorse = morseResult,
          raSpectralPages = fmap rbaSpectralPages spectralResult,
          raLerayProfile = first (BackendFailure . show) (hypercohomologyDims derivedComplex),
          raRepresentativeCocycles = \degree -> pure (cohomologyBasisAt complex degree),
          raBoundaryAnalysis = spectralResult,
          raParsingDepth = fmap (convergenceDepth . rbaSpectralPages) spectralResult
        }

buildResolutionBundle ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure (ResolutionBundle f)
buildResolutionBundle rewriteSystem depthValue = do
  kernel <- buildResolutionKernel rewriteSystem depthValue
  pure (ResolutionBundle kernel (deriveAnalysis kernel))

resolutionDerivedComplex :: ResolutionBundle f -> Derived GF2
resolutionDerivedComplex =
  rkDerivedComplex . rbKernel

resolutionCellCount :: ResolutionBundle f -> Int
resolutionCellCount =
  length . grothendieckSiteCells . scsSite . rkScaffold . rbKernel

resolutionSourceNodes :: ResolutionBundle f -> [RegionNodeId]
resolutionSourceNodes bundle =
  fmap
    (\(FinObjectId ordinalValue) -> RegionNodeId ordinalValue)
    (V.toList (derivedPosetNodes (resolutionSourcePoset bundle)))

resolutionWitnessClassesBySourceNode ::
  (HasConstructorTag f, ZipMatch f) =>
  ResolutionBundle f ->
  Map RegionNodeId WitnessClass
resolutionWitnessClassesBySourceNode bundle =
  let kernel = rbKernel bundle
      analysisScaffold = rkScaffold kernel
      stalkCache = rkStalkCache kernel
      cellByBasisRef = inverseBasisRefMap (scsBasisRefs analysisScaffold)
   in rcaBasisCellBySourceNode (rkCoreAnalysis kernel)
        & Map.toList
        & mapMaybe
          ( \(nodeIdValue, basisCellRef) ->
              Map.lookup basisCellRef cellByBasisRef
                & fmap
                  ( \cellValue ->
                      let stalkValue =
                            Map.findWithDefault
                              (grothendieckStalkFromCell cellValue)
                              cellValue
                              stalkCache
                       in (nodeIdValue, witnessClass (rsWitness stalkValue))
                  )
          )
        & Map.fromList

resolutionPruningGapAt ::
  HomologicalDegree ->
  RegionNodeId ->
  ResolutionBundle f ->
  Either HomologyFailure Double
resolutionPruningGapAt degreeValue (RegionNodeId ordinalValue) bundle =
  first (BackendFailure . show)
    ( pruningGapAt
        (resolutionSourcePoset bundle)
        degreeValue
        (FinObjectId ordinalValue)
        (resolutionDerivedComplex bundle)
    )

resolutionCriticalMicrosupportNodes ::
  ResolutionBundle f ->
  Set.Set RegionNodeId
resolutionCriticalMicrosupportNodes bundle =
  mrCriticalFibers (rkMicrosupport (rbKernel bundle))
    & foldr
      ( \(FinObjectId ordinalValue, criticalityValue) ->
          case criticalityValue of
            Critical ->
              Set.insert (RegionNodeId ordinalValue)
            NonCritical ->
              id
      )
      Set.empty

resolutionMicrosupportFiberNodes ::
  ResolutionBundle f ->
  [RegionNodeId]
resolutionMicrosupportFiberNodes bundle =
  fmap
    (\(FinObjectId ordinalValue, _) -> RegionNodeId ordinalValue)
    (mrCriticalFibers (rkMicrosupport (rbKernel bundle)))

resolutionCocycleRuleClasses ::
  Eq (RewriteMorphism f) =>
  ResolutionBundle f ->
  Either HomologyFailure [Set.Set RewriteRuleId]
resolutionCocycleRuleClasses bundle = do
  cocycleRepresentatives <- raRepresentativeCocycles (rbAnalysis bundle) (HomologicalDegree 1)
  let analysisScaffold = rkScaffold (rbKernel bundle)
      basisCellMap = basisIndexCellMapAtDegree (HomologicalDegree 1) (scsBasisRefs analysisScaffold)
      rewriteSystem = rkRewriteSystem (rbKernel bundle)
  pure (filter (not . Set.null)
    (fmap (cocycleRuleIds rewriteSystem basisCellMap) cocycleRepresentatives))

stalkCacheFromScaffold ::
  (HasConstructorTag f, ZipMatch f) =>
  RewriteSiteScaffold f ->
  Map (GrothendieckCell (RewriteSystem f)) (InterfaceStalk (RewriteTag f))
stalkCacheFromScaffold analysisScaffold =
  grothendieckSiteCells (scsSite analysisScaffold)
    & fmap (\cellValue -> (cellValue, grothendieckStalkFromCell cellValue))
    & Map.fromList

basisCellHeuristicFromCache ::
  RewriteSiteScaffold f ->
  Map (GrothendieckCell (RewriteSystem f)) (InterfaceStalk (RewriteTag f)) ->
  BasisCellRef ->
  Double
basisCellHeuristicFromCache analysisScaffold stalkCache basisCellRef =
  let cellByBasisRef = inverseBasisRefMap (scsBasisRefs analysisScaffold)
      fallbackWeight =
        case cellDegree basisCellRef of
          HomologicalDegree degreeValue ->
            fromIntegral degreeValue + fromIntegral (cellIndex basisCellRef) / 1000000.0
   in Map.lookup basisCellRef cellByBasisRef
        >>= (`Map.lookup` stalkCache)
        & fmap stalkWeightFromCache
        & maybe fallbackWeight id

stalkWeightFromCache :: InterfaceStalk (RewriteTag f) -> Double
stalkWeightFromCache stalkValue =
  fromIntegral
    ( Set.size (rsBoundNames stalkValue)
        + Set.size (rsDeletedNames stalkValue)
        + Set.size (rsCreatedNames stalkValue)
        + if rsGuarded stalkValue then 1 else 0
    )

type ResolutionCoreAnalysis :: Type
data ResolutionCoreAnalysis = ResolutionCoreAnalysis
  { rcaSourcePoset :: DerivedPoset,
    rcaStalkDimensions :: Map FinObjectId Int,
    rcaComparableRestrictions :: Map (FinObjectId, FinObjectId) (BoundaryIncidence Int),
    rcaBasisCellBySourceNode :: Map RegionNodeId BasisCellRef,
    rcaBidegreesByBasisCell :: Map BasisCellRef Bidegree,
    rcaPosetCohomologyDims :: [Int],
    rcaCorrectedFiltration :: BasisCellRef -> Int
  }

buildResolutionBoundaryAnalysisCore ::
  StalkLinearization (InterfaceStalk (RewriteTag f)) Int ->
  RewriteSiteScaffold f ->
  Derived GF2 ->
  FiniteChainComplex Int ->
  Map (GrothendieckCell (RewriteSystem f)) (InterfaceStalk (RewriteTag f)) ->
  Either HomologyFailure ResolutionCoreAnalysis
buildResolutionBoundaryAnalysisCore stalkLinearization analysisScaffold derivedComplex finiteComplex stalkCache = do
  let sourcePoset = derivedPoset derivedComplex
      stalksByNode =
        grothendieckSiteCells (scsSite analysisScaffold)
          & mapMaybe
            ( \cellValue ->
                Map.lookup cellValue stalkCache
                  & fmap
                    ( \stalkValue ->
                        ( FinObjectId (cellNodeIdFromScaffold analysisScaffold cellValue),
                          stalkValue
                        )
                    )
            )
          & Map.fromList
      topoIndexByNode =
        Map.fromList
          (zip (V.toList (derivedPosetTopoAsc sourcePoset)) [0 :: Int ..])
      basisCellBySourceNode =
        scsBasisRefs analysisScaffold
          & Map.toList
          & fmap
            ( \(cellValue, basisCellRef) ->
                ( RegionNodeId (cellNodeIdFromScaffold analysisScaffold cellValue),
                  basisCellRef
                )
            )
          & Map.fromList
      rawFiltrationIndex basisCellRef =
        Map.findWithDefault
          0
          (FinObjectId (basisCellNodeId finiteComplex basisCellRef))
          topoIndexByNode
      correctedFiltration =
        coboundaryAwareFiltration finiteComplex rawFiltrationIndex
      bidegreesByBasisCell =
        Map.elems (scsBasisRefs analysisScaffold)
          & fmap
            (\basisCellRef -> (basisCellRef, bidegreeFromTotalDegree (correctedFiltration basisCellRef) (cellDegree basisCellRef)))
          & Map.fromList
      linearizedRestrictions =
        buildLinearizedRestrictionModel
          stalksByNode
          (\upperNode lowerNode -> either (const False) id (leqChecked sourcePoset lowerNode upperNode))
          stalkLinearization
      stalkDimensions =
        linearizedRestrictionStalkDimensions linearizedRestrictions
      comparableRestrictions =
        linearizedRestrictionComparableRestrictions linearizedRestrictions
  posetCohomologyDims <-
    posetSheafCohomologyDims
      sourcePoset
      (\nodeValue -> Map.findWithDefault 0 nodeValue stalkDimensions)
      (\nodePair -> Map.findWithDefault emptyComparableBoundaryIncidence nodePair comparableRestrictions)
  pure
    ResolutionCoreAnalysis
      { rcaSourcePoset = sourcePoset,
        rcaStalkDimensions = stalkDimensions,
        rcaComparableRestrictions = comparableRestrictions,
        rcaBasisCellBySourceNode = basisCellBySourceNode,
        rcaBidegreesByBasisCell = bidegreesByBasisCell,
        rcaPosetCohomologyDims = posetCohomologyDims,
        rcaCorrectedFiltration = correctedFiltration
      }

extendCoreWithSpectral ::
  RewriteSiteScaffold f ->
  (BasisCellRef -> Double) ->
  SiteMorseReduction
    (GrothendieckSite (RewriteSystem f))
    (GrothendieckCell (RewriteSystem f)) ->
  ResolutionCoreAnalysis ->
  Either HomologyFailure ResolutionBoundaryAnalysis
extendCoreWithSpectral _analysisScaffold cellScore reductionValue coreAnalysis = do
  let sourceComplex = scsChainComplex (smrScaffold reductionValue)
      correctedFiltration = rcaCorrectedFiltration coreAnalysis
  filteredMorseValue <- filteredRefinedMorseComplex sourceComplex correctedFiltration cellScore
  let refinedMorseValue = frmcRefinedMorseComplex filteredMorseValue
      spectralComplex = rmcReducedComplex refinedMorseValue
      spectralFiltration = filteredReducedFiltration filteredMorseValue
  spectralPages <- computeRationalSpectralPages spectralComplex spectralFiltration
  pure
    ResolutionBoundaryAnalysis
      { rbaSourcePoset = rcaSourcePoset coreAnalysis,
        rbaStalkDimensions = rcaStalkDimensions coreAnalysis,
        rbaComparableRestrictions = rcaComparableRestrictions coreAnalysis,
        rbaBasisCellBySourceNode = rcaBasisCellBySourceNode coreAnalysis,
        rbaBidegreesByBasisCell = rcaBidegreesByBasisCell coreAnalysis,
        rbaSpectralPages = spectralPages,
        rbaPosetCohomologyDims = rcaPosetCohomologyDims coreAnalysis
      }

emptyComparableBoundaryIncidence :: BoundaryIncidence Int
emptyComparableBoundaryIncidence =
  emptyBoundaryIncidence

coboundaryAwareFiltration ::
  FiniteChainComplex r ->
  (BasisCellRef -> Int) ->
  BasisCellRef -> Int
coboundaryAwareFiltration finiteComplex rawFiltration =
  let HomologicalDegree maxDegree = maxHomologicalDegree finiteComplex
      corrected =
        foldl'
          (assignAtDegree finiteComplex rawFiltration)
          Map.empty
          [0 .. maxDegree]
   in \cellRef ->
        Map.findWithDefault (rawFiltration cellRef) cellRef corrected

assignAtDegree ::
  FiniteChainComplex r ->
  (BasisCellRef -> Int) ->
  Map BasisCellRef Int ->
  Int ->
  Map BasisCellRef Int
assignAtDegree finiteComplex rawFiltration assigned degreeValue =
  let incidence = incidenceMatrixAt finiteComplex (HomologicalDegree degreeValue)
   in foldl'
        ( \currentAssigned sourceCellIndex ->
            let cellRef = BasisCellRef (HomologicalDegree degreeValue) sourceCellIndex
                rawFilt = rawFiltration cellRef
                maxFaceFilt =
                  maximumFaceFiltration finiteComplex rawFiltration currentAssigned degreeValue sourceCellIndex
             in Map.insert cellRef (max rawFilt maxFaceFilt) currentAssigned
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
  | degreeValue <= 0 = 0
  | otherwise =
      let incidence = incidenceMatrixAt finiteComplex (HomologicalDegree degreeValue)
          faceDegree = HomologicalDegree (degreeValue - 1)
          faceRefs =
            fmap
              ((\faceIndex -> BasisCellRef faceDegree faceIndex) . targetIndex)
              (filter ((== sourceCellIndex) . sourceIndex) (boundaryEntries incidence))
          lookupFilt cellRef =
            Map.findWithDefault (rawFiltration cellRef) cellRef assigned
       in foldl' (\acc ref -> max acc (lookupFilt ref)) 0 faceRefs

resolutionSourcePoset :: ResolutionBundle f -> DerivedPoset
resolutionSourcePoset =
  rcaSourcePoset . rkCoreAnalysis . rbKernel

cellNodeIdFromScaffold ::
  RewriteSiteScaffold f ->
  GrothendieckCell (RewriteSystem f) ->
  Int
cellNodeIdFromScaffold analysisScaffold cellValue =
  let finite = scsChainComplex analysisScaffold
   in Map.lookup cellValue (scsBasisRefs analysisScaffold)
        & fmap (basisCellNodeId finite)
        & maybe 0 id

contextPosetFromRewriteSystem ::
  (HasConstructorTag f, ZipMatch f) =>
  RewriteSystem f ->
  Either HomologyFailure DerivedPoset
contextPosetFromRewriteSystem rewriteSystem =
  let contexts = allContexts rewriteSystem
      nodes =
        fmap (FinObjectId . rcOrdinal) contexts
      contextsByCardinality =
        IntMap.fromListWith (<>) [(length (rcObjects c), [c]) | c <- contexts]
      covers =
        [ (FinObjectId (rcOrdinal sm), FinObjectId (rcOrdinal lg))
          | (smCard, smCtxs) <- IntMap.toAscList contextsByCardinality,
            (lgCard, lgCtxs) <- IntMap.toAscList contextsByCardinality,
            lgCard > smCard,
            sm <- smCtxs,
            lg <- lgCtxs,
            contextLeq rewriteSystem sm lg
        ]
   in first (InvalidTopologyInput . show) (mkDerivedPosetFromOrderEdges nodes covers)

cocycleRuleIds ::
  Eq (RewriteMorphism f) =>
  RewriteSystem f ->
  Map Int (GrothendieckCell (RewriteSystem f)) ->
  RepresentativeCocycle Rational Int ->
  Set.Set RewriteRuleId
cocycleRuleIds rewriteSystem basisCellMap cocycleRepresentative =
  Set.fromList
    ( mapMaybe
        ( \(_, basisIndexValue) ->
            Map.lookup basisIndexValue basisCellMap
              >>= cocycleRuleIdOfCell rewriteSystem
        )
        (representativeTerms cocycleRepresentative)
    )

cocycleRuleIdOfCell ::
  Eq (RewriteMorphism f) =>
  RewriteSystem f ->
  GrothendieckCell (RewriteSystem f) ->
  Maybe RewriteRuleId
cocycleRuleIdOfCell rewriteSystem cellValue =
  grothendieckCellSingleMorphism cellValue
    >>= rewriteRuleIdOf rewriteSystem
