module Moonlight.EGraph.Introspection.Analysis.Termination
  ( BoundedRatio,
    boundedRatioValue,
    CertificateLayer (..),
    GrowthLayer (..),
    HeuristicLayer (..),
    TerminationAnalysis (..),
    analyzeTermination,
    analyzeTerminationWithTrace,
    analyzeTerminationWithScheduler,
    analyzeTerminationWithSchedulerTrace,
    terminationFromGrothendieckSummary,
  )
where

import Data.Bifunctor (first)
import Data.Function ((&))
import Moonlight.Analysis.Termination
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
  )
import Moonlight.Analysis.Termination qualified as Generic
import Moonlight.Core (ZipMatch (..), HasConstructorTag, Pattern, RewriteRuleId)
import Moonlight.Control.Schedule (SchedulerConfig, defaultSchedulerConfig)
import Moonlight.Sheaf.Site
  ( executionTransitionCount,
    executionVertexCount,
    executionComplex,
  )
import Moonlight.EGraph.Introspection.Analysis.Relative
  ( RuntimeRelativeDiagnostics,
    rrdBase,
    rrdObservedGroundedMorphismCount,
    rrdObservedGroundedChainCount,
    rrdAmbiguousGroundedNodeCount,
    rrdUnmappedGroundedNodeCount,
    rdGroundedMorphismCount,
    rdGroundedChainCount,
    rdGroundedNodeCoverage,
    runtimeRelativeDiagnostics,
  )
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteSystem)
import Moonlight.EGraph.Introspection.Core.Rewrite.Successor
  ( rewriteInfluenceComplex,
  )
import Moonlight.Pale.Diagnostic.Section.Saturation (SaturationTrace)
import Moonlight.EGraph.Introspection.Analysis.Spectral
  ( GrothendieckConsistencyProfile (..),
    grothendieckConsistencyProfile,
  )
import Moonlight.Sheaf.Site
  ( GrothendieckStructuralSummary (..),
    summarizeGrothendieckSystem,
  )
import Moonlight.Control.Scheduling.Successor
  ( BackoffInfluenceEnvelope (..),
    InfluenceComplex (..),
    SchedulerInfluence (..),
  )
import Moonlight.Homology (HomologyFailure (BackendFailure))
import Numeric.Natural (Natural)

analyzeTermination ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure TerminationAnalysis
analyzeTermination =
  analyzeTerminationWithScheduler defaultSchedulerConfig

analyzeTerminationWithTrace ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  SaturationTrace RewriteRuleId ->
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure TerminationAnalysis
analyzeTerminationWithTrace =
  analyzeTerminationWithSchedulerTrace defaultSchedulerConfig

analyzeTerminationWithScheduler ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  SchedulerConfig RewriteRuleId ->
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure TerminationAnalysis
analyzeTerminationWithScheduler schedulerConfig rewriteSystem depthValue = do
  grothendieckSummary <- summarizeGrothendieckSystem rewriteSystem depthValue
  consistencyProfile <- grothendieckConsistencyProfile rewriteSystem depthValue
  executionValue <- first (BackendFailure . show) (executionComplex rewriteSystem)
  let baseAnalysis = terminationFromGrothendieckSummary grothendieckSummary
      influenceComplex = rewriteInfluenceComplex schedulerConfig rewriteSystem
      influences = fmap snd (ricEdgeInfluences influenceComplex)
      restrictionCount = glRestrictionCount (taGrowth baseAnalysis)
  pure
    ( baseAnalysis
        & Generic.enrichWithConsistency
            ConsistencySummary
              { csConsistencyRatio = gcpConsistencyRatio consistencyProfile
              }
        & Generic.enrichWithExecution
            ExecutionSummary
              { esVertexCount = executionVertexCount executionValue,
                esTransitionCount = executionTransitionCount executionValue
              }
        & Generic.enrichWithInfluence
            InfluenceSummary
              { isEdgeCount = length influences,
                isBoundedEdgeCount = length (filter isBoundedInfluence influences),
                isCooldownPressure = averageOfCooldown influences,
                isReferenceRestrictionCount = restrictionCount
              }
    )

analyzeTerminationWithSchedulerTrace ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  SchedulerConfig RewriteRuleId ->
  SaturationTrace RewriteRuleId ->
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure TerminationAnalysis
analyzeTerminationWithSchedulerTrace schedulerConfig saturationTrace rewriteSystem depthValue = do
  runtimeRelativeValue <- runtimeRelativeDiagnostics saturationTrace rewriteSystem depthValue
  baseAnalysis <- analyzeTerminationWithScheduler schedulerConfig rewriteSystem depthValue
  pure
    ( Generic.enrichWithRuntimeRelative
        (runtimeRelativeSummaryFrom runtimeRelativeValue)
        baseAnalysis
    )

terminationFromGrothendieckSummary :: GrothendieckStructuralSummary -> TerminationAnalysis
terminationFromGrothendieckSummary grothendieckSummary =
  Generic.terminationFromSummary
    TerminationSummary
      { tsHomotopyProfile = gssHomotopyProfile grothendieckSummary,
        tsCellCount = gssCellCount grothendieckSummary,
        tsRestrictionCount = gssFaceCount grothendieckSummary,
        tsCoboundaryNilpotenceEvidence = gssCoboundaryNilpotenceEvidence grothendieckSummary
      }

runtimeRelativeSummaryFrom :: RuntimeRelativeDiagnostics f -> RuntimeRelativeSummary
runtimeRelativeSummaryFrom runtimeRelativeValue =
  let baseRelativeValue = rrdBase runtimeRelativeValue
   in RuntimeRelativeSummary
        { rrsGroundedMorphismCount = rdGroundedMorphismCount baseRelativeValue,
          rrsGroundedChainCount = rdGroundedChainCount baseRelativeValue,
          rrsGroundedNodeCoverage = rdGroundedNodeCoverage baseRelativeValue,
          rrsObservedGroundedMorphismCount = rrdObservedGroundedMorphismCount runtimeRelativeValue,
          rrsObservedGroundedChainCount = rrdObservedGroundedChainCount runtimeRelativeValue,
          rrsAmbiguousGroundedNodeCount = rrdAmbiguousGroundedNodeCount runtimeRelativeValue,
          rrsUnmappedGroundedNodeCount = rrdUnmappedGroundedNodeCount runtimeRelativeValue
        }

isBoundedInfluence :: SchedulerInfluence -> Bool
isBoundedInfluence influenceValue =
  case influenceValue of
    DeterministicInfluence -> False
    BackoffInfluence _ -> True

cooldownContribution :: SchedulerInfluence -> Double
cooldownContribution influenceValue =
  case influenceValue of
    DeterministicInfluence -> 0.0
    BackoffInfluence envelope ->
      let boundedShare =
            if bieSharedOutgoingEdges envelope <= 0
              then 0.0
              else min 1.0 (fromIntegral (bieMatchLimit envelope) / fromIntegral (bieSharedOutgoingEdges envelope))
       in boundedShare / fromIntegral (bieCooldownRounds envelope + 1)

averageOfCooldown :: [SchedulerInfluence] -> Maybe Double
averageOfCooldown influences =
  let values = fmap cooldownContribution influences
   in if null values
        then Nothing
        else Just (sum values / fromIntegral (length values))
