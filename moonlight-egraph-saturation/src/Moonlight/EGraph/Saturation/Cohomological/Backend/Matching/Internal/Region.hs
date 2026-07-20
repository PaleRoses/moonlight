{-# LANGUAGE PatternSynonyms #-}

module Moonlight.EGraph.Saturation.Cohomological.Backend.Matching.Internal.Region
  ( carrierInitialRegionsForRequest,
    seededInitialRegionsForRequest,
  )
where

import Moonlight.EGraph.Saturation.Cohomological.Types (SheafCapabilityAtom)
import Data.Maybe (mapMaybe)
import Moonlight.Core
  ( HasConstructorTag,
    Pattern,
    ZipMatch,
  )
import Moonlight.Delta.Scope qualified as Delta
import Moonlight.EGraph.Introspection.Analysis.Resolution
  ( ResolutionBundle,
  )
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteMorphism)
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchingFrontier,
    MatchingRequest,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Frontier
  ( filterRegionsByMatchingFrontier,
    seedSurvivesMatchingFrontier,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Seed
  ( materializeSeedWithPruning,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Seed
  ( evidenceFromResolution,
    requestPruningGates,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance
  ( CohomologicalBackend (..),
  )
import Moonlight.Rewrite.Algebra
  ( cpqPrimaryPattern,
  )
import Moonlight.Saturation.Matching qualified as GenericMatching
import Moonlight.Sheaf.Pruning (pruningDecisionAllowed)
import Moonlight.Saturation.Obstruction.Cohomological.Prepared
  ( InitialRegionStage (..),
    PreparedInitialRegionBatch (..),
  )
import Moonlight.Saturation.Obstruction.Cohomological.Metrics.Pipeline
  ( pipelineMetricsFromList,
  )
import Moonlight.Saturation.Obstruction.Cohomological.Seed
  ( SeedInterpreter (..),
    SeedFrontierPlan,
    seedFrontierPlanFromList,
    seedFrontierPlanCount,
    seedFrontierPlanSeeds,
  )
import Moonlight.Sheaf.Obstruction
  ( CandidateRegionSeed,
    CohomologicalPruningGates (..),
    PruningEvidence (..),
    SectionCertificationAlgebra (socRegionCarrierPlan),
    buildPruningGates,
    carrierPlanItems,
  )

carrierInitialRegionsForRequest ::
  CohomologicalBackend owner c f ->
  MatchingFrontier ->
  (ClassId -> ClassId) ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  PreparedInitialRegionBatch ClassId
carrierInitialRegionsForRequest configuration matchingFrontier canonicalize request =
  let carrierRegions =
        carrierPlanItems
          ( socRegionCarrierPlan
              (cbContext configuration)
              request
              (cpqPrimaryPattern (GenericMatching.qrQuery request))
          )
      regions =
        filterRegionsByMatchingFrontier canonicalize matchingFrontier carrierRegions
      regionCount =
        fromIntegral (length regions)
   in PreparedInitialRegionBatch
        { pirbMetrics =
            pipelineMetricsFromList
              [ (InitialRegionSeeds, 0),
                (InitialRegionMaterializedRegions, regionCount),
                (InitialRegionAfterPruningGates, 0),
                (InitialRegionAfterFrontierFilter, 0),
                (InitialRegionAfterMaterialization, regionCount),
                (InitialRegionAfterMicrosupport, 0),
                (InitialRegionAfterContext, 0),
                (InitialRegionAfterSpectral, 0),
                (InitialRegionAfterLaplacian, 0)
              ],
          pirbRegions = regions
        }

seededInitialRegionsForRequest ::
  (HasConstructorTag f, ZipMatch f, Eq (RewriteMorphism f)) =>
  CohomologicalBackend owner c f ->
  SeedInterpreter (MatchingRequest owner c SheafCapabilityAtom f) (Pattern f) MatchingFrontier ClassId ->
  ResolutionBundle f ->
  MatchingFrontier ->
  (ClassId -> ClassId) ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  PreparedInitialRegionBatch ClassId
seededInitialRegionsForRequest configuration seedInterpreter resolutionValue matchingFrontier canonicalize request =
  let queryPattern = cpqPrimaryPattern (GenericMatching.qrQuery request)
      pruningEvidence =
        evidenceFromResolution (Just resolutionValue) (cbPolicy configuration) request
      pruningGates = requestPruningGates (Just resolutionValue) (cbPolicy configuration) request
      frontierPlan =
        seedPlanForMatchingFrontier seedInterpreter matchingFrontier request queryPattern
      frontierSeeds =
        seedFrontierPlanSeeds frontierPlan
      frontierSeedCount =
        seedFrontierPlanCount frontierPlan
      countSeedFamily selectEvidence =
        let familyEvidence = filter selectEvidence pruningEvidence
         in if null familyEvidence
              then frontierSeedCount
              else fromIntegral (length (filterSeedsByPruning (buildPruningGates familyEvidence) frontierSeeds))
      afterPruning =
        filterSeedsByPruning pruningGates frontierSeeds
      afterFrontierFilter =
        filter (seedSurvivesMatchingFrontier canonicalize matchingFrontier) afterPruning
      afterMaterialization =
        mapMaybe (materializeSeedWithPruning pruningGates seedInterpreter request queryPattern) afterFrontierFilter
      materializedRegions =
        filterRegionsByMatchingFrontier canonicalize matchingFrontier afterMaterialization
      passingMicrosupport = countSeedFamily isMicrosupportEvidence
      passingContext = countSeedFamily isContextualSeedEvidence
      passingSpectral = frontierSeedCount
      passingLaplacian = frontierSeedCount
   in PreparedInitialRegionBatch
        { pirbMetrics =
            pipelineMetricsFromList
              [ (InitialRegionSeeds, frontierSeedCount),
                (InitialRegionMaterializedRegions, fromIntegral (length materializedRegions)),
                (InitialRegionAfterPruningGates, fromIntegral (length afterPruning)),
                (InitialRegionAfterFrontierFilter, fromIntegral (length afterFrontierFilter)),
                (InitialRegionAfterMaterialization, fromIntegral (length afterMaterialization)),
                (InitialRegionAfterMicrosupport, passingMicrosupport),
                (InitialRegionAfterContext, passingContext),
                (InitialRegionAfterSpectral, passingSpectral),
                (InitialRegionAfterLaplacian, passingLaplacian)
              ],
          pirbRegions = materializedRegions
        }
  where
    isMicrosupportEvidence evidence =
      case evidence of
        MicrosupportNonCritical _ -> True
        _ -> False

    isContextualSeedEvidence evidence =
      case evidence of
        ContextRelevant _ -> True
        WitnessClassification _ -> True
        _ -> False

filterSeedsByPruning ::
  CohomologicalPruningGates ClassId ->
  [CandidateRegionSeed ClassId] ->
  [CandidateRegionSeed ClassId]
filterSeedsByPruning pruningGates =
  filter (pruningDecisionAllowed . cpgSeedDecision pruningGates)

seedPlanForMatchingFrontier ::
  SeedInterpreter (MatchingRequest owner c SheafCapabilityAtom f) (Pattern f) MatchingFrontier ClassId ->
  MatchingFrontier ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  Pattern f ->
  SeedFrontierPlan ClassId
seedPlanForMatchingFrontier seedInterpreter matchingFrontier request queryPattern =
  Delta.foldScope
    (seedFrontierPlanFromList [])
    (\rootKeys -> siSeedsForRootsPlan seedInterpreter rootKeys request queryPattern)
    (seedFrontierPlanFromList [])
    matchingFrontier
