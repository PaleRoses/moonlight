{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Moonlight.EGraph.Saturation.Cohomological.Backend.Seed
  ( evidenceFromResolution,
    requestPruningGates,
    requestPrefersCoarseRefinement,
    materializeSeedWithPruning,
    PatternOccurrence (..),
    CandidateRegionSeed (..),
    SeedInterpreter (..),
    refineSheafRegion,
  )
where

import Data.Function ((&))
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core (HasConstructorTag, RewriteRuleId)
import Moonlight.EGraph.Pure.Types (ClassId)
import Moonlight.Core
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchingFrontier,
  )
import Moonlight.Saturation.Obstruction.Cohomological.Seed
  ( SeedInterpreter (..),
  )
import Moonlight.EGraph.Saturation.Cohomological.Types
  ( PatternOccurrence (..),
    SheafCapabilityAtom,
    refineSheafRegion,
  )
import Moonlight.Sheaf.Site (overlappingContextPairs)
import Moonlight.EGraph.Introspection.Analysis.Resolution
  ( ResolutionBundle,
    ResolutionAnalysisAlg (raLerayProfile, raRepresentativeCocycles),
    ResolutionKernel (rkMicrosupport, rkRewriteSystem),
    rbAnalysis,
    rbKernel,
    resolutionCocycleRuleClasses,
    resolutionCriticalMicrosupportNodes,
    resolutionWitnessClassesBySourceNode,
  )
import Moonlight.EGraph.Introspection.Core.Rewrite
  ( RewriteContext,
    RewriteSystem,
    irsRuleId,
    irsSpan,
    rcOrdinal,
    rsRuleIdentities,
  )
import Moonlight.Sheaf.Site.Context.GeneratorCover (contextGenerators)
import Moonlight.Sheaf.Site
  ( contextLeq,
    systemMorphismsInContext,
  )
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchingRequest,
  )
import Moonlight.Saturation.Matching qualified as GenericMatching
import Moonlight.Sheaf.Pruning (pruningDecisionAllowed)
import Moonlight.Homology (HomologicalDegree (..))
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteMorphism)
import Moonlight.Sheaf.Obstruction
  ( CohomologicalPolicy (..),
  )
import Moonlight.Sheaf.Obstruction
  ( PruningEvidence (..),
    CohomologicalPruningGates (..),
    CandidateRegion (..),
    CandidateRegionSeed (..),
    buildPruningGates,
  )
import Moonlight.Sheaf.Obstruction
  ( microsupportResultPruningEvidence,
  )

evidenceFromResolution ::
  (HasConstructorTag f, ZipMatch f, Eq (RewriteMorphism f)) =>
  Maybe (ResolutionBundle f) ->
  CohomologicalPolicy ->
  MatchingRequest owner c SheafCapabilityAtom f a ->
  [PruningEvidence]
evidenceFromResolution maybeResolution policy request =
  case GenericMatching.qrPurpose request of
    GenericMatching.RewritePurpose rewriteRuleId ->
      maybe [] (evidenceForRewrite policy rewriteRuleId) maybeResolution
    _ -> []

requestPruningGates ::
  (HasConstructorTag f, ZipMatch f, Eq (RewriteMorphism f)) =>
  Maybe (ResolutionBundle f) ->
  CohomologicalPolicy ->
  MatchingRequest owner c SheafCapabilityAtom f a ->
  CohomologicalPruningGates ClassId
requestPruningGates maybeResolution policy request =
  buildPruningGates (evidenceFromResolution maybeResolution policy request)

requestPrefersCoarseRefinement ::
  Eq (RewriteMorphism f) =>
  Maybe (ResolutionBundle f) ->
  CohomologicalPolicy ->
  MatchingRequest owner c SheafCapabilityAtom f a ->
  Bool
requestPrefersCoarseRefinement maybeResolution policy request =
  case (GenericMatching.qrPurpose request, maybeResolution) of
    (GenericMatching.RewritePurpose rewriteRuleId, Just resolutionValue) ->
      policyAllowsCoarseRefinementBias policy
        && resolutionPrefersCoarseRefinement rewriteRuleId resolutionValue
    _ ->
      False

evidenceForRewrite ::
  (HasConstructorTag f, ZipMatch f, Eq (RewriteMorphism f)) =>
  CohomologicalPolicy ->
  RewriteRuleId ->
  ResolutionBundle f ->
  [PruningEvidence]
evidenceForRewrite policy rewriteRuleId resolutionValue =
  concat
    [ enabledEvidence (policyAllowsMicrosupportPruning policy) (microsupportEvidence resolutionValue),
      enabledEvidence (policyAllowsContextPruning policy) (contextRelevanceEvidence rewriteRuleId resolutionValue),
      enabledEvidence (policyAllowsWitnessPruning policy) (witnessEvidence resolutionValue)
    ]

enabledEvidence :: Bool -> [a] -> [a]
enabledEvidence enabled evidence =
  if enabled
    then evidence
    else []

policyAllowsMicrosupportPruning :: CohomologicalPolicy -> Bool
policyAllowsMicrosupportPruning = cpUseHierarchicalPruning

policyAllowsContextPruning :: CohomologicalPolicy -> Bool
policyAllowsContextPruning = cpUseHierarchicalPruning

policyAllowsWitnessPruning :: CohomologicalPolicy -> Bool
policyAllowsWitnessPruning = cpUseHierarchicalPruning

policyAllowsCoarseRefinementBias :: CohomologicalPolicy -> Bool
policyAllowsCoarseRefinementBias = cpUseHierarchicalPruning

microsupportEvidence :: ResolutionBundle f -> [PruningEvidence]
microsupportEvidence resolutionValue =
  let criticalNodes = resolutionCriticalMicrosupportNodes resolutionValue
   in [ evidence
        | evidence@(MicrosupportNonCritical _) <-
            microsupportResultPruningEvidence (rkMicrosupport (rbKernel resolutionValue)),
          not (Set.null criticalNodes)
      ]

contextRelevanceEvidence ::
  (HasConstructorTag f, ZipMatch f, Eq (RewriteMorphism f)) =>
  RewriteRuleId ->
  ResolutionBundle f ->
  [PruningEvidence]
contextRelevanceEvidence rewriteRuleId resolutionValue =
  case relevantContextOrdinals rewriteRuleId resolutionValue of
    Nothing -> []
    Just ordinals -> [ContextRelevant ordinals]

witnessEvidence :: (HasConstructorTag f, ZipMatch f) => ResolutionBundle f -> [PruningEvidence]
witnessEvidence resolutionValue =
  let witnessByNode = resolutionWitnessClassesBySourceNode resolutionValue
   in [WitnessClassification witnessByNode | not (Map.null witnessByNode)]

relevantContextOrdinals ::
  (HasConstructorTag f, ZipMatch f, Eq (RewriteMorphism f)) =>
  RewriteRuleId ->
  ResolutionBundle f ->
  Maybe (Set Int)
relevantContextOrdinals rewriteRuleId resolutionValue =
  let rewriteSystem = rkRewriteSystem (rbKernel resolutionValue)
      directlyRelevantOrdinals =
        ruleContextOrdinals rewriteRuleId rewriteSystem
      overlappedOrdinals =
        overlappingContextPairs rewriteSystem (contextGenerators rewriteSystem)
          & foldMap
            (\(leftContext, rightContext) ->
               let leftOrdinal = rcOrdinal leftContext
                   rightOrdinal = rcOrdinal rightContext
                in if Set.member leftOrdinal directlyRelevantOrdinals
                     || Set.member rightOrdinal directlyRelevantOrdinals
                     then Set.fromList [leftOrdinal, rightOrdinal]
                     else Set.empty
            )
      allRelevantOrdinals =
        directlyRelevantOrdinals <> overlappedOrdinals
   in if Set.null allRelevantOrdinals
        then Nothing
        else Just allRelevantOrdinals

ruleContextOrdinals ::
  (HasConstructorTag f, ZipMatch f, Eq (RewriteMorphism f)) =>
  RewriteRuleId ->
  RewriteSystem f ->
  Set Int
ruleContextOrdinals rewriteRuleId rewriteSystem =
  maybe
    Set.empty
    (foldMap (Set.singleton . rcOrdinal) . minimalVisibleRuleContexts rewriteSystem)
    (ruleSpanFor rewriteRuleId rewriteSystem)

minimalVisibleRuleContexts ::
  (HasConstructorTag f, ZipMatch f, Eq (RewriteMorphism f)) =>
  RewriteSystem f ->
  RewriteMorphism f ->
  [RewriteContext f]
minimalVisibleRuleContexts rewriteSystem ruleSpan =
  let visibleContexts =
        contextGenerators rewriteSystem
          & filter
            (\contextValue -> ruleSpan `elem` systemMorphismsInContext rewriteSystem contextValue)
   in filter
        ( \candidateContext ->
            not
              ( any
                  ( \otherContext ->
                      rcOrdinal otherContext /= rcOrdinal candidateContext
                        && contextLeq rewriteSystem otherContext candidateContext
                  )
                  visibleContexts
              )
        )
        visibleContexts

ruleSpanFor :: RewriteRuleId -> RewriteSystem f -> Maybe (RewriteMorphism f)
ruleSpanFor rewriteRuleId rewriteSystem =
  irsSpan
    <$> List.find
      (\identifiedSpan -> irsRuleId identifiedSpan == rewriteRuleId)
      (rsRuleIdentities rewriteSystem)

resolutionPrefersCoarseRefinement ::
  Eq (RewriteMorphism f) =>
  RewriteRuleId ->
  ResolutionBundle f ->
  Bool
resolutionPrefersCoarseRefinement rewriteRuleId resolutionValue =
  resolutionSupportsEarlyRefinement resolutionValue
    && resolutionTouchesRewriteRule rewriteRuleId resolutionValue

resolutionTouchesRewriteRule ::
  Eq (RewriteMorphism f) =>
  RewriteRuleId ->
  ResolutionBundle f ->
  Bool
resolutionTouchesRewriteRule rewriteRuleId resolutionValue =
  either (const False) (any (Set.member rewriteRuleId)) (resolutionCocycleRuleClasses resolutionValue)

resolutionSupportsEarlyRefinement :: ResolutionBundle f -> Bool
resolutionSupportsEarlyRefinement resolutionValue =
  let hasCocycles =
        either
          (const False)
          (not . null)
          (raRepresentativeCocycles (rbAnalysis resolutionValue) (HomologicalDegree 1))
      hasPositiveLeray =
        either
          (const False)
          (\leray -> IntMap.findWithDefault 0 1 leray > 0)
          (raLerayProfile (rbAnalysis resolutionValue))
   in hasCocycles && hasPositiveLeray

materializeSeedWithPruning ::
  CohomologicalPruningGates ClassId ->
  SeedInterpreter (MatchingRequest owner c SheafCapabilityAtom f) (Pattern f) MatchingFrontier ClassId ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  Pattern f ->
  CandidateRegionSeed ClassId ->
  Maybe (CandidateRegion ClassId)
materializeSeedWithPruning gates _ _ _ seedValue
  | not (pruningDecisionAllowed (cpgSeedDecision gates seedValue)) = Nothing
materializeSeedWithPruning gates seedInterpreter request queryPattern seedValue =
  siMaterializeSeed seedInterpreter request queryPattern seedValue
    >>= \regionValue ->
      if pruningDecisionAllowed (cpgRegionDecision gates regionValue)
        then Just regionValue
        else Nothing
