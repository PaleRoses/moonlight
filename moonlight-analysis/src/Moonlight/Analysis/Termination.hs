{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Analysis.Termination
  ( BoundedRatio,
    boundedRatioValue,
    CertificateLayer (..),
    GrowthLayer (..),
    HeuristicLayer (..),
    TerminationAnalysis (..),
    TerminationSummary (..),
    ConsistencySummary (..),
    ExecutionSummary (..),
    InfluenceSummary (..),
    RuntimeRelativeSummary (..),
    terminationFromSummary,
    enrichWithConsistency,
    enrichWithExecution,
    enrichWithInfluence,
    enrichWithRuntimeRelative,
  )
where

import Data.Kind (Type)
import Moonlight.Analysis.Cohomology (CoboundaryNilpotenceEvidence)
import Moonlight.Analysis.Homotopy (NerveHomotopyProfile (..))
import Moonlight.Probability.Core (Prob, mkProb, probValue)

type BoundedRatio :: Type
newtype BoundedRatio = BoundedRatio
  { boundedRatioProb :: Prob
  }
  deriving stock (Eq, Show, Read)

boundedRatioValue :: BoundedRatio -> Double
boundedRatioValue = probValue . boundedRatioProb

type CertificateLayer :: Type
data CertificateLayer = CertificateLayer
  { clAcyclic :: Bool,
    clCoboundaryNilpotenceEvidence :: CoboundaryNilpotenceEvidence,
    clObstructionFree :: Maybe Bool,
    clConfluent :: Maybe Bool
  }
  deriving stock (Eq, Show, Read)

type GrowthLayer :: Type
data GrowthLayer = GrowthLayer
  { glCellCount :: Int,
    glRestrictionCount :: Int,
    glExpansionRate :: Maybe Double,
    glSaturationDepth :: Maybe Int
  }
  deriving stock (Eq, Show, Read)

type HeuristicLayer :: Type
data HeuristicLayer = HeuristicLayer
  { hlConnectedComponents :: Int,
    hlCycleRank :: Int,
    hlRestrictionDensity :: Maybe Double,
    hlExecutionVertexCount :: Maybe Int,
    hlExecutionTransitionCount :: Maybe Int,
    hlExecutionDensity :: Maybe Double,
    hlInfluenceEdgeCount :: Maybe Int,
    hlInfluenceBoundedEdgeCount :: Maybe Int,
    hlInfluenceCooldownPressure :: Maybe Double,
    hlStaticDynamicGap :: Maybe Int,
    hlObservedGroundedMorphismGap :: Maybe Int,
    hlObservedGroundedChainCoverage :: Maybe BoundedRatio,
    hlRuntimeAmbiguityPressure :: Maybe BoundedRatio,
    hlRuntimeUnmappedGroundedNodeCount :: Maybe Int,
    hlSpectralGap :: Maybe Double,
    hlConsistencyRadius :: Maybe Double
  }
  deriving stock (Eq, Show, Read)

type TerminationAnalysis :: Type
data TerminationAnalysis = TerminationAnalysis
  { taCertificate :: CertificateLayer,
    taGrowth :: GrowthLayer,
    taHeuristic :: HeuristicLayer
  }
  deriving stock (Eq, Show, Read)

type TerminationSummary :: Type
data TerminationSummary = TerminationSummary
  { tsHomotopyProfile :: NerveHomotopyProfile,
    tsCellCount :: Int,
    tsRestrictionCount :: Int,
    tsCoboundaryNilpotenceEvidence :: CoboundaryNilpotenceEvidence
  }

type ConsistencySummary :: Type
data ConsistencySummary = ConsistencySummary
  { csConsistencyRatio :: Maybe Prob
  }

type ExecutionSummary :: Type
data ExecutionSummary = ExecutionSummary
  { esVertexCount :: Int,
    esTransitionCount :: Int
  }

type InfluenceSummary :: Type
data InfluenceSummary = InfluenceSummary
  { isEdgeCount :: Int,
    isBoundedEdgeCount :: Int,
    isCooldownPressure :: Maybe Double,
    isReferenceRestrictionCount :: Int
  }

type RuntimeRelativeSummary :: Type
data RuntimeRelativeSummary = RuntimeRelativeSummary
  { rrsGroundedMorphismCount :: Int,
    rrsGroundedChainCount :: Int,
    rrsGroundedNodeCoverage :: Int,
    rrsObservedGroundedMorphismCount :: Int,
    rrsObservedGroundedChainCount :: Int,
    rrsAmbiguousGroundedNodeCount :: Int,
    rrsUnmappedGroundedNodeCount :: Int
  }

terminationFromSummary :: TerminationSummary -> TerminationAnalysis
terminationFromSummary terminationSummary =
  let homotopyProfile = tsHomotopyProfile terminationSummary
      cycleRank = bettiAt 1 (nhpBettiVector homotopyProfile)
      cellCount = tsCellCount terminationSummary
      restrictionCount = tsRestrictionCount terminationSummary
   in TerminationAnalysis
        { taCertificate =
            CertificateLayer
              { clAcyclic = cycleRank == 0,
                clCoboundaryNilpotenceEvidence = tsCoboundaryNilpotenceEvidence terminationSummary,
                clObstructionFree = Nothing,
                clConfluent = Nothing
              },
          taGrowth =
            GrowthLayer
              { glCellCount = cellCount,
                glRestrictionCount = restrictionCount,
                glExpansionRate = Nothing,
                glSaturationDepth = Nothing
              },
          taHeuristic =
            HeuristicLayer
              { hlConnectedComponents = nhpConnectedComponents homotopyProfile,
                hlCycleRank = cycleRank,
                hlRestrictionDensity = restrictionDensity cellCount restrictionCount,
                hlExecutionVertexCount = Nothing,
                hlExecutionTransitionCount = Nothing,
                hlExecutionDensity = Nothing,
                hlInfluenceEdgeCount = Nothing,
                hlInfluenceBoundedEdgeCount = Nothing,
                hlInfluenceCooldownPressure = Nothing,
                hlStaticDynamicGap = Nothing,
                hlObservedGroundedMorphismGap = Nothing,
                hlObservedGroundedChainCoverage = Nothing,
                hlRuntimeAmbiguityPressure = Nothing,
                hlRuntimeUnmappedGroundedNodeCount = Nothing,
                hlSpectralGap = Nothing,
                hlConsistencyRadius = Nothing
              }
        }

enrichWithConsistency :: ConsistencySummary -> TerminationAnalysis -> TerminationAnalysis
enrichWithConsistency consistencySummary =
  overHeuristic
    (\heuristicLayer ->
        heuristicLayer
          { hlConsistencyRadius = fmap probValue (csConsistencyRatio consistencySummary)
          }
    )

enrichWithExecution :: ExecutionSummary -> TerminationAnalysis -> TerminationAnalysis
enrichWithExecution executionSummary =
  let vertexCount = esVertexCount executionSummary
      transitionCount = esTransitionCount executionSummary
   in overHeuristic
        (\heuristicLayer ->
            heuristicLayer
              { hlExecutionVertexCount = Just vertexCount,
                hlExecutionTransitionCount = Just transitionCount,
                hlExecutionDensity = restrictionDensity vertexCount transitionCount
              }
        )

enrichWithInfluence :: InfluenceSummary -> TerminationAnalysis -> TerminationAnalysis
enrichWithInfluence influenceSummary =
  overHeuristic
    (\heuristicLayer ->
        heuristicLayer
          { hlInfluenceEdgeCount = Just (isEdgeCount influenceSummary),
            hlInfluenceBoundedEdgeCount = Just (isBoundedEdgeCount influenceSummary),
            hlInfluenceCooldownPressure = isCooldownPressure influenceSummary,
            hlStaticDynamicGap = Just (isReferenceRestrictionCount influenceSummary - isEdgeCount influenceSummary)
          }
    )

enrichWithRuntimeRelative :: RuntimeRelativeSummary -> TerminationAnalysis -> TerminationAnalysis
enrichWithRuntimeRelative runtimeRelativeSummary =
  overHeuristic
    (\heuristicLayer ->
        heuristicLayer
          { hlObservedGroundedMorphismGap =
              Just (rrsGroundedMorphismCount runtimeRelativeSummary - rrsObservedGroundedMorphismCount runtimeRelativeSummary),
            hlObservedGroundedChainCoverage =
              densityOver
                (rrsGroundedChainCount runtimeRelativeSummary)
                (rrsObservedGroundedChainCount runtimeRelativeSummary),
            hlRuntimeAmbiguityPressure =
              densityOver
                (rrsGroundedNodeCoverage runtimeRelativeSummary)
                (rrsAmbiguousGroundedNodeCount runtimeRelativeSummary),
            hlRuntimeUnmappedGroundedNodeCount =
              Just (rrsUnmappedGroundedNodeCount runtimeRelativeSummary)
          }
    )

overHeuristic :: (HeuristicLayer -> HeuristicLayer) -> TerminationAnalysis -> TerminationAnalysis
overHeuristic updateHeuristic terminationAnalysis =
  terminationAnalysis
    { taHeuristic = updateHeuristic (taHeuristic terminationAnalysis)
    }

bettiAt :: Int -> [Int] -> Int
bettiAt dimensionValue bettiVector =
  case drop dimensionValue bettiVector of
    bettiValue : _ -> bettiValue
    [] -> 0

restrictionDensity :: Int -> Int -> Maybe Double
restrictionDensity cellCount restrictionCount =
  if cellCount <= 0
    then Nothing
    else Just (fromIntegral restrictionCount / fromIntegral cellCount)

densityOver :: Int -> Int -> Maybe BoundedRatio
densityOver totalCount numeratorCount =
  if totalCount <= 0 || numeratorCount < 0 || numeratorCount > totalCount
    then Nothing
    else mkBoundedRatio (fromIntegral numeratorCount / fromIntegral totalCount)

mkBoundedRatio :: Double -> Maybe BoundedRatio
mkBoundedRatio rawValue =
  either (const Nothing) (Just . BoundedRatio) (mkProb rawValue)
