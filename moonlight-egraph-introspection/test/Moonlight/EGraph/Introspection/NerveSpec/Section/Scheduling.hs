{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Introspection.NerveSpec.Section.Scheduling
  ( tests,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.EGraph.Introspection.NerveSpec.Section.Prelude
import Moonlight.EGraph.Introspection.NerveSpec.Fixture
import Moonlight.EGraph.Test.Saturation (EGraphStrategyPhase (..))
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.Saturation.Substrate (SatGraph, TrivialContext)
import Moonlight.Sheaf.Context.Site (UnitContextSiteOwner)
import Moonlight.EGraph.Introspection.Core.Rewrite.Successor
  ( rewriteInfluenceComplex,
    rewriteSupportRuntimeOverlayFromTrace,
    rewriteSuccessorComplex,
    runtimeWeightedSuccessorComplexFromTrace,
  )
import Moonlight.EGraph.Introspection.Core.Rewrite.Successor qualified as RewriteSuccessor
import Data.List (find)
import Moonlight.Control.Scheduling.Tower (spectralSchedulingPriorityObservation, towerWarningClusters)
import Moonlight.Saturation.Context.Runtime.Report qualified as ContextReport
import Moonlight.Control.Schedule (SchedulerRefinement (..))
import Moonlight.Sheaf.Site qualified as SheafContextPresentation
import Moonlight.Pale.Diagnostic.Section.Saturation qualified as PaleSaturation
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Moonlight.Pale.Ghc.Expr (ScopeCtx)
import Moonlight.Pale.Diagnostic.Derived.Rewrite (RewriteOutcomeSummary (..))
import Moonlight.Pale.Diagnostic.Section.Rewrite (rosRuleId, rosScheduledCount, rtRuleId, rtsCount, rtsFromRule)
import Moonlight.Pale.Test.Site.Assertion (expectRight)

type IntrospectionTrace :: Type
data IntrospectionTrace
  = StructuralTrace
  | RewriteRuntimeTrace (PaleSaturation.SaturationTrace RewriteRuleId)

type IntrospectionSubject :: (Type -> Type) -> Type
data IntrospectionSubject f
  = RewriteSystemIntrospectionSubject (RewriteSystem f)
  | DerivedIntrospectionSubject [RewriteRule f]

type IntrospectedSaturation :: (Type -> Type) -> Type -> Type
data IntrospectedSaturation f result where
  PlainIntrospectedSaturation ::
    PlanSpec
      (EGraphU UnitContextSiteOwner ScopeCtx f () TrivialContext)
      (SatGraph (EGraphU UnitContextSiteOwner ScopeCtx f () TrivialContext))
      RewriteRuleId ->
    IntrospectedSaturation
      f
      (SaturationReport (EGraphU UnitContextSiteOwner ScopeCtx f () TrivialContext))
  StrategyIntrospectedSaturation ::
    Program () (EGraphStrategyPhase UnitContextSiteOwner ScopeCtx f () TrivialContext) ->
    IntrospectedSaturation
      f
      ( Report
          (EGraph f ())
          (SaturationReport (EGraphU UnitContextSiteOwner ScopeCtx f () TrivialContext))
          (PhaseSummary SaturationBudget SaturationTermination ())
      )

rewriteSystemIntrospectionSubject :: RewriteSystem f -> IntrospectionSubject f
rewriteSystemIntrospectionSubject =
  RewriteSystemIntrospectionSubject

derivedIntrospectionSubject :: [RewriteRule f] -> IntrospectionSubject f
derivedIntrospectionSubject =
  DerivedIntrospectionSubject

structuralPriorityProfile :: SchedulerConfig RewriteRuleId -> RewriteSystem ArithF -> PriorityProfile RewriteRuleId
structuralPriorityProfile schedulerConfig rewriteSystem =
  priorityProfileFromList
    [ (ruleId, priorityEvidence 1 0 0 nonCriticalPriorityRank)
    | (edgeValue, _) <- ricEdgeInfluences (rewriteInfluenceComplex schedulerConfig rewriteSystem),
      ruleId <- maybe [] pure (runtimeRuleIdOf (snRuntimeRuleIdentity (seSource edgeValue)))
    ]

runtimePriorityProfile ::
  SchedulerConfig RewriteRuleId ->
  PaleSaturation.SaturationTrace RewriteRuleId ->
  RewriteSystem ArithF ->
  PriorityProfile RewriteRuleId
runtimePriorityProfile schedulerConfig saturationTrace rewriteSystem =
  let runtimeOverlay = runtimeWeightedSuccessorComplexFromTrace saturationTrace rewriteSystem
   in structuralPriorityProfile schedulerConfig rewriteSystem
        <> priorityProfileFromList
          [ ( rtsFromRule transition,
              priorityEvidence 0 (rtsCount transition) 0 nonCriticalPriorityRank
            )
          | weightedEdge <- RewriteSuccessor.rwscWeightedEdges runtimeOverlay
          , transition <- NonEmpty.toList (rieTransitions (rweEvidence weightedEdge))
          ]
        <> priorityProfileFromList
          [ (rosRuleId outcomeStat, priorityEvidence 0 0 (rosScheduledCount outcomeStat) nonCriticalPriorityRank)
          | outcomeStat <- rosRuleStats (RewriteSuccessor.rwscOutcomeSummary runtimeOverlay)
          ]

schedulerConfigWithIntrospection ::
  IntrospectionTrace ->
  Maybe a ->
  SchedulerConfig RewriteRuleId ->
  RewriteSystem ArithF ->
  SchedulerConfig RewriteRuleId
schedulerConfigWithIntrospection introspectionTrace _ schedulerConfig rewriteSystem =
  withPriorityProfile
    ( case introspectionTrace of
        StructuralTrace ->
          structuralPriorityProfile schedulerConfig rewriteSystem
        RewriteRuntimeTrace saturationTrace ->
          runtimePriorityProfile schedulerConfig saturationTrace rewriteSystem
    )
    schedulerConfig

runIntrospectedSaturation introspectionMode introspectionSubject introspectedSaturation rawRules initialGraph =
  case introspectedSaturation of
    PlainIntrospectedSaturation saturationConfig ->
      saturateWithSchedulerRefinement
        (schedulerRefinement introspectionMode (rewriteSystemFor introspectionSubject))
        saturationConfig
        []
        rawRules
        initialGraph
    StrategyIntrospectedSaturation guideStrategy ->
      saturateByStrategyWithSchedulerRefinement
        (schedulerRefinement introspectionMode (rewriteSystemFor introspectionSubject))
        guideStrategy
        []
        rawRules
        initialGraph

runIntrospectedSaturation ::
  SchedulingAnalysisMode ->
  IntrospectionSubject ArithF ->
  IntrospectedSaturation ArithF result ->
  [RewriteRule ArithF] ->
  EGraph ArithF () ->
  Either
    (SaturationError (EGraphU UnitContextSiteOwner ScopeCtx ArithF () TrivialContext) RewriteRuleId)
    result

schedulerRefinement :: SchedulingAnalysisMode -> RewriteSystem ArithF -> SchedulerRefinement state RewriteRuleId
schedulerRefinement introspectionMode rewriteSystem =
  SchedulerRefinement
    { srPriorityObservation =
        const
          ( case introspectionMode of
              StructuralOnce ->
                structuralPriorityProfile defaultSchedulerConfig rewriteSystem
              RuntimeBetweenRounds ->
                structuralPriorityProfile defaultSchedulerConfig rewriteSystem
          ),
      srTracePolicyUpdate = id
    }

rewriteSystemFor :: IntrospectionSubject ArithF -> RewriteSystem ArithF
rewriteSystemFor introspectionSubject =
  case introspectionSubject of
    RewriteSystemIntrospectionSubject rewriteSystem ->
      rewriteSystem
    DerivedIntrospectionSubject rawRules ->
      mkIdentifiedRewriteSystem (fmap (expectSchedulingSpan . identifiedSpanFromEGraphRewriteRule) rawRules)

expectSchedulingSpan :: Show error => Either error value -> value
expectSchedulingSpan =
  either
    (\failure -> error ("scheduling rewrite span rejected: " <> show failure))
    id

runtimeRuleIdOf :: RuntimeRuleIdentity -> Maybe RewriteRuleId
runtimeRuleIdOf runtimeRuleIdentity =
  case runtimeRuleIdentity of
    UniqueRuntimeRuleIdentity ruleId -> Just ruleId
    _ -> Nothing

priorityMapFromProfile :: Ord group => PriorityProfile group -> Map.Map group PriorityEvidence
priorityMapFromProfile =
  Map.fromList . priorityProfileToList

rewriteRuleKey :: RewriteRuleId -> Int
rewriteRuleKey (RewriteRuleId ruleKey) = ruleKey

obstructionOverlay ::
  ObstructionOverlay
    RewriteRuleId
    (ObstructionClass (GrothendieckCell (RewriteSystem ArithF)) (CompositionWitness (RewriteTag ArithF)))
    (GrothendieckCell (RewriteSystem ArithF))
    (RewriteContext ArithF)
    (RewriteMorphism ArithF)
    RuntimeRuleIdentity
    (RewriteMorphism ArithF)
    (RewriteSuccessor.RewriteCompositionObstruction ArithF)
obstructionOverlay =
  ObstructionOverlay
    { ooDegree = ocDegree,
      ooSupportingCells = ocSupportingCells,
      ooNorm =
        \obstructionClass ->
          ocInterpretation obstructionClass
            & oiCellEvaluations
            & fmap (abs . snd)
            & sum,
      ooRulesForCell =
        \influenceComplex cellValue ->
          case grothendieckCellSingleMorphism cellValue of
            Nothing -> []
            Just spanValue ->
              rscNodes (ricSuccessorComplex influenceComplex)
                & filter ((== spanValue) . snRule)
                & fmap snRuntimeRuleIdentity
    }

tests :: TestTree
tests =
  testGroup
    "scheduling"
    [ testCase "execution complex stays dynamic while Grothendieck stays categorical" testExecutionComplex,
      testCase "rule successor complex tracks schedulable rule influence rather than state transitions" testSuccessorComplex,
      testCase "scheduler influence envelopes preserve scheduler policy without fake scalar scores" testSchedulerInfluence,
      testCase "introspection scheduling profiles lift structural and runtime rule influence into scheduler overlays" testIntrospectionScheduling,
      testCase "spectral scheduling overlay lifts obstruction layers into graded rule clusters" testSpectralSchedulingOverlay,
      testCase "tower scheduling prioritizes lower obstruction degree before higher-degree clusters" testSpectralSchedulingPriority,
      testCase "tower warning clusters isolate large cocycle norms" testTowerWarningClusters,
      testCase "support runtime overlays preserve support-local pressure while aggregating per-rule scheduling" testSupportRuntimeOverlay,
      testCase "introspection-aware saturation feeds adaptive scheduler overlays into the engine" testAdaptiveIntrospectionSaturation,
      testCase "introspection-aware strategy saturation reuses the adaptive scheduler hook at the orchestration layer" testAdaptiveIntrospectionStrategySaturation
    ]

testExecutionComplex :: Assertion
testExecutionComplex =
  case multiContextSystemResult of
    Left failure ->
      assertFailure (show failure)
    Right rewriteSystem ->
      case executionComplex rewriteSystem of
        Left failure ->
          assertFailure (show failure)
        Right executionComplexValue ->
          let familyValue = SheafContextPresentation.contextPresentation rewriteSystem
              crossContextGrothendieckMorphisms =
                [ morphismValue
                | morphismValue <- grothendieckMorphisms familyValue
                , gmSourceContext morphismValue /= gmTargetContext morphismValue
                ]
           in do
                assertEqual
                  "expected execution vertices to index visible object/context states only"
                  5
                  (executionVertexCount executionComplexValue)
                assertEqual
                  "expected execution transitions to track only visible in-context rule firings"
                  3
                  (executionTransitionCount executionComplexValue)
                assertEqual
                  "expected the execution support skeleton to carry the same vertex count"
                  5
                  (graphVertexCount (recUndirectedSupportSkeleton executionComplexValue))
                assertEqual
                  "expected the execution support skeleton to carry one edge per endpoint support"
                  3
                  (length (graphEdges (recUndirectedSupportSkeleton executionComplexValue)))
                assertBool
                  "expected execution transitions to remain within a single context"
                  ( all
                      (\transitionValue -> evContext (etSource transitionValue) == evContext (etTarget transitionValue))
                      (recTransitions executionComplexValue)
                  )
                assertBool
                  "expected the Grothendieck layer to retain cross-context structure absent from execution"
                  (not (null crossContextGrothendieckMorphisms))

testSuccessorComplex :: Assertion
testSuccessorComplex =
  let chainSuccessorComplex = rewriteSuccessorComplex acyclicChainSystem
      singleSuccessorComplex = rewriteSuccessorComplex singleRuleSystem
      singleGrothendieck1 =
        length
          (simplicesAtDimension (grothendieckNerve (SheafContextPresentation.contextPresentation singleRuleSystem) 1) 1)
   in do
        assertEqual
          "expected one successor node per visible rule/context pair"
          2
          (successorNodeCount chainSuccessorComplex)
        assertEqual
          "expected only the composable rule pair to induce scheduler-relevant successor influence"
          1
          (successorEdgeCount chainSuccessorComplex)
        assertEqual
          "expected a lone rule to have no rule-successor edge"
          0
          (successorEdgeCount singleSuccessorComplex)
        assertBool
          "expected the categorical layer to still expose the single rule as a 1-simplex"
          (singleGrothendieck1 > 0)

testSchedulerInfluence :: Assertion
testSchedulerInfluence =
  let schedulerConfig =
        SchedulerConfig
          { scOrder = BackoffByGroup (backoffConfig 1 2),
            scTracePolicy = TraceAll,
            scPriorityProfile = emptyPriorityProfile
          }
      influenceComplex = rewriteInfluenceComplex schedulerConfig acyclicChainSystem
      terminationResult = analyzeTerminationWithScheduler schedulerConfig acyclicChainSystem 2
   in do
        assertEqual
          "expected the influence layer to preserve the structural successor carrier"
          1
          (schedulerInfluenceEdgeCount influenceComplex)
        assertEqual
          "expected backoff policy to annotate the lone successor edge with an exact sharing envelope"
          [BackoffInfluence (BackoffInfluenceEnvelope {bieMatchLimit = 1, bieCooldownRounds = 2, bieSharedOutgoingEdges = 1})]
          (fmap snd (ricEdgeInfluences influenceComplex))
        case terminationResult of
          Left failure ->
            assertFailure (show failure)
          Right analysisValue -> do
            let expectedInfluenceGap =
                  Just
                    ( glRestrictionCount (taGrowth analysisValue)
                        - maybe 0 id (hlInfluenceEdgeCount (taHeuristic analysisValue))
                    )
            assertEqual
              "expected scheduler-aware termination heuristics to report the influence carrier"
              (Just 1, Just 1)
              ( hlInfluenceEdgeCount (taHeuristic analysisValue),
                hlInfluenceBoundedEdgeCount (taHeuristic analysisValue)
              )
            assertEqual
              "expected scheduler-aware termination heuristics to retain the static dynamic gap against influence edges"
              expectedInfluenceGap
              (hlStaticDynamicGap (taHeuristic analysisValue))
            assertEqual
              "expected cooldown pressure to reflect exact backoff parameters rather than a fabricated score"
              (Just (1.0 / 3.0))
              (hlInfluenceCooldownPressure (taHeuristic analysisValue))

testIntrospectionScheduling :: Assertion
testIntrospectionScheduling =
  let structuralProfile =
        structuralPriorityProfile defaultSchedulerConfig identifiedAcyclicChainSystem
      runtimeProfile =
        runtimePriorityProfile defaultSchedulerConfig acyclicTrace identifiedAcyclicChainSystem
      structuralConfig =
        schedulerConfigWithIntrospection
          StructuralTrace
          Nothing
          defaultSchedulerConfig
          identifiedAcyclicChainSystem
      runtimeConfig =
        schedulerConfigWithIntrospection
          (RewriteRuntimeTrace acyclicTrace)
          Nothing
          defaultSchedulerConfig
          identifiedAcyclicChainSystem
      lookupPriority ruleKey =
        lookupPriorityEvidence (RewriteRuleId ruleKey)
   in do
        assertEqual
          "expected structural scheduling to prioritize the upstream rule in the chain by outgoing influence"
          (priorityEvidence 1 0 0 nonCriticalPriorityRank)
          (lookupPriority 0 structuralProfile)
        assertEqual
          "expected the terminal chain rule to have no outgoing structural influence score"
          mempty
          (lookupPriority 1 structuralProfile)
        assertEqual
          "expected runtime scheduling to accumulate observed transition and scheduled counts for the fired upstream rule"
          (priorityEvidence 1 1 1 nonCriticalPriorityRank)
          (lookupPriority 0 runtimeProfile)
        assertEqual
          "expected runtime scheduling to retain observed scheduling on the downstream rule while preserving zero outgoing transitions"
          (priorityEvidence 0 0 1 nonCriticalPriorityRank)
          (lookupPriority 1 runtimeProfile)
        assertEqual
          "expected structural scheduler config decoration to retain the base deterministic order"
          ByRuleIdThenSubstitution
          (scOrder structuralConfig)
        assertBool
          "expected structural scheduler config decoration to install a non-empty introspection profile"
          (not (priorityProfileNull (scPriorityProfile structuralConfig)))
        assertBool
          "expected runtime scheduler config decoration to install a non-empty introspection profile"
          (not (priorityProfileNull (scPriorityProfile runtimeConfig)))

testSpectralSchedulingOverlay :: Assertion
testSpectralSchedulingOverlay =
  case buildResolutionBundle identifiedAcyclicChainSystem 2 of
    Left failure ->
      assertFailure (show failure)
    Right resolutionValue ->
      case raSpectralPages (rbAnalysis resolutionValue) of
        Left failure ->
          assertFailure ("spectral pages failed: " <> show failure)
        Right spectralPages ->
          let siteValue = mkGrothendieckSite identifiedAcyclicChainSystem 2
              influenceComplex = rewriteInfluenceComplex defaultSchedulerConfig identifiedAcyclicChainSystem
              obstructionLayers =
                [ [syntheticObstructionClass 1 [ruleCellForOrFail "ab" contextSpanAB siteValue] [2]],
                  [syntheticObstructionClass 2 [ruleCellForOrFail "ab" contextSpanAB siteValue, ruleCellForOrFail "bc" contextSpanBC siteValue] [3, 3]]
                ]
              overlayValue =
                spectralSchedulingOverlay
                  obstructionOverlay
                  spectralPages
                  obstructionLayers
                  influenceComplex
           in do
                assertEqual
                  "expected one graded cluster per active obstruction layer"
                  2
                  (length (ricGradedObstructionClusters overlayValue))
                assertEqual
                  "expected the first cluster to activate in pass 1"
                  (Just (HomologicalDegree 1))
                  (gocDegree <$> listToMaybe (ricGradedObstructionClusters overlayValue))

testSpectralSchedulingPriority :: Assertion
testSpectralSchedulingPriority =
  case buildResolutionBundle identifiedAcyclicChainSystem 2 of
    Left failure ->
      assertFailure (show failure)
    Right resolutionValue ->
      case raSpectralPages (rbAnalysis resolutionValue) of
        Left failure ->
          assertFailure ("spectral pages failed: " <> show failure)
        Right spectralPages ->
          let siteValue = mkGrothendieckSite identifiedAcyclicChainSystem 2
              overlayValue =
                spectralSchedulingOverlay
                  obstructionOverlay
                  spectralPages
                  [ [syntheticObstructionClass 1 [ruleCellForOrFail "ab" contextSpanAB siteValue] [2]],
                    [syntheticObstructionClass 2 [ruleCellForOrFail "ab" contextSpanAB siteValue, ruleCellForOrFail "bc" contextSpanBC siteValue] [1, 1]]
                  ]
                  (rewriteInfluenceComplex defaultSchedulerConfig identifiedAcyclicChainSystem)
              priorityMap =
                priorityMapFromProfile
                  (spectralSchedulingPriorityObservation runtimeRuleIdOf id overlayValue)
           in assertBool
                "expected the rule participating in the degree-1 cluster to outrank the rule only activated at degree 2"
                ( case
                    ( Map.lookup (RewriteRuleId 0) priorityMap,
                      Map.lookup (RewriteRuleId 1) priorityMap
                    ) of
                    (Just lowerDegreeEvidence, Just higherDegreeEvidence) ->
                      comparePriorityEvidence lowerDegreeEvidence higherDegreeEvidence == LT
                    _ ->
                      False
                )

testTowerWarningClusters :: Assertion
testTowerWarningClusters =
  case buildResolutionBundle identifiedAcyclicChainSystem 2 of
    Left failure ->
      assertFailure (show failure)
    Right resolutionValue ->
      case raSpectralPages (rbAnalysis resolutionValue) of
        Left failure ->
          assertFailure ("spectral pages failed: " <> show failure)
        Right spectralPages ->
          let siteValue = mkGrothendieckSite identifiedAcyclicChainSystem 2
              overlayValue =
                spectralSchedulingOverlay
                  obstructionOverlay
                  spectralPages
                  [ [syntheticObstructionClass 1 [ruleCellForOrFail "ab" contextSpanAB siteValue] [1]],
                    [syntheticObstructionClass 2 [ruleCellForOrFail "bc" contextSpanBC siteValue] [4]]
                  ]
                  (rewriteInfluenceComplex defaultSchedulerConfig identifiedAcyclicChainSystem)
           in assertEqual
                "expected tower warning extraction to retain only the large-norm cluster"
                [HomologicalDegree 2]
                (fmap gocDegree (towerWarningClusters 3 overlayValue))

syntheticObstructionClass ::
  Int ->
  [GrothendieckCell (RewriteSystem ArithF)] ->
  [Rational] ->
  ObstructionClass (GrothendieckCell (RewriteSystem ArithF)) (CompositionWitness (RewriteTag ArithF))
syntheticObstructionClass degreeValue supportingCells coefficients =
  let evaluatedCells = zip supportingCells coefficients
   in ObstructionClass
        { ocDegree = HomologicalDegree degreeValue,
          ocCocycleRepresentative =
            RepresentativeChain
              { representativeDegree = HomologicalDegree degreeValue,
                representativeTerms = []
              },
          ocSupportingCells = supportingCells,
          ocDerivedProfile =
            ConstantDerivedProfile
              { cdpAmbientHypercohomology = IntMap.empty,
                cdpSupportHypercohomology = IntMap.empty,
                cdpExtendedSupportHypercohomology = IntMap.empty
              },
          ocInterpretation =
            ObstructionInterpretation
              { oiCellEvaluations = evaluatedCells,
                oiWitnessEvidence = [],
                oiObstructedCells = supportingCells,
                oiComposedCells = [],
                oiHarmonicLoops = [],
                oiHarmonicFailure = Nothing
              }
        }

ruleCellForOrFail ::
  String ->
  RewriteMorphism ArithF ->
  GrothendieckSite (RewriteSystem ArithF) ->
  GrothendieckCell (RewriteSystem ArithF)
ruleCellForOrFail labelValue spanValue siteValue =
  fromMaybe
    (error ("missing Grothendieck cell for rule " <> labelValue))
    (ruleCellFor spanValue siteValue)

ruleCellFor ::
  RewriteMorphism ArithF ->
  GrothendieckSite (RewriteSystem ArithF) ->
  Maybe (GrothendieckCell (RewriteSystem ArithF))
ruleCellFor spanValue siteValue =
  grothendieckSiteCells siteValue
    & filter ((== 1) . grothendieckCellDimension)
    & find
      ( \cellValue ->
          maybe False (samePatternRewriteShape spanValue) (grothendieckCellSingleMorphism cellValue)
      )

testSupportRuntimeOverlay :: Assertion
testSupportRuntimeOverlay =
  let moduleSupport = principalSupport ModuleTwistCtx
      overlay = rewriteSupportRuntimeOverlayFromTrace sampleSupportTrace
      profile = supportRuntimeRulePriorityProfile rewriteRuleKey overlay
      findSupportStat ruleIdValue supportValue =
        sroSupportStats overlay
          & filter
            ( \supportStat ->
                srkRuleId (srssKey supportStat) == ruleIdValue
                  && srkSupport (srssKey supportStat) == supportValue
            )
          & listToMaybe
      findRuleStat ruleIdValue =
        sroRuleStats overlay
          & filter ((== ruleIdValue) . srrsRuleId)
          & listToMaybe
      priorityMap =
        priorityMapFromProfile profile
   in do
        assertEqual
          "expected the overlay to keep one support-local aggregate per (rule, support) pair"
          3
          (length (sroSupportStats overlay))
        assertEqual
          "expected the overlay to expose one rule aggregate per observed rule"
          2
          (supportRuntimeObservedRuleCount overlay)
        assertEqual
          "expected the overlay to retain suppressed-rule pressure"
          1
          (supportRuntimeSuppressedRuleCount overlay)
        assertEqual
          "expected the overlay to retain cooldown pressure"
          1
          (supportRuntimeCooldownRuleCount overlay)
        case findSupportStat (RewriteRuleId 0) moduleSupport of
          Nothing ->
            assertFailure "expected a module-support aggregate for rule 0"
          Just supportStat ->
            do
              assertEqual
                "expected module support aggregation to count both rounds"
                2
                (observedRoundCount (srssCounts supportStat))
              assertEqual
                "expected module support aggregation to add matched counts"
                (workCountExact 3)
                (suppressionMatchedCount (srssCounts supportStat))
              assertEqual
                "expected module support aggregation to add scheduled counts"
                2
                (suppressionScheduledCount (srssCounts supportStat))
              assertEqual
                "expected module support aggregation to retain suppression pressure"
                (workCountExact 1)
                (suppressionSuppressedCount (srssCounts supportStat))
              assertEqual
                "expected module support aggregation to retain cooldown-suppressed rounds"
                1
                (cooldownSuppressedRoundCount (srssCounts supportStat))
        case findRuleStat (RewriteRuleId 0) of
          Nothing ->
            assertFailure "expected a rule aggregate for rule 0"
          Just ruleStat ->
            do
              assertEqual
                "expected rule aggregation to retain both support-local stats"
                2
                (length (srrsSupportStats ruleStat))
              assertEqual
                "expected rule aggregation to union observed rounds"
                2
                (observedRoundCount (srrsCounts ruleStat))
              assertEqual
                "expected rule aggregation to add matched counts across supports"
                (workCountExact 4)
                (suppressionMatchedCount (srrsCounts ruleStat))
              assertEqual
                "expected rule aggregation to add scheduled counts across supports"
                2
                (suppressionScheduledCount (srrsCounts ruleStat))
              assertEqual
                "expected rule aggregation to add suppressed counts across supports"
                (workCountExact 2)
                (suppressionSuppressedCount (srrsCounts ruleStat))
              assertEqual
                "expected rule aggregation to retain cooldown pressure"
                1
                (cooldownSuppressedRoundCount (srrsCounts ruleStat))
        assertEqual
          "expected the derived scheduler profile to mark suppressed rules as critical"
          (Just (priorityEvidence 0 0 2 criticalPriorityRank))
          (Map.lookup 0 priorityMap)
        assertEqual
          "expected the derived scheduler profile to keep unsuppressed rules non-critical"
          (Just (priorityEvidence 0 0 3 nonCriticalPriorityRank))
          (Map.lookup 1 priorityMap)

testAdaptiveIntrospectionSaturation :: Assertion
testAdaptiveIntrospectionSaturation =
  withAdaptiveInitialGraph $ \graph2 ->
    let saturationConfig ::
          PlanSpec
            (EGraphU UnitContextSiteOwner ScopeCtx ArithF () TrivialContext)
            (SatGraph (EGraphU UnitContextSiteOwner ScopeCtx ArithF () TrivialContext))
            RewriteRuleId
        saturationConfig =
          withSchedulerConfig
            (traceAllSchedulerConfig deterministicSchedulerConfig)
            (planSpec (SaturationBudget 4 32) GenericJoinMatching emptyRewriteRuntimeCapabilities)
   in case
        runIntrospectedSaturation
          RuntimeBetweenRounds
          (rewriteSystemIntrospectionSubject adaptiveRewriteSystem)
          (PlainIntrospectedSaturation saturationConfig)
          [adaptiveEngineRuleAB, adaptiveEngineRuleBC]
          graph2
        of
        Left saturationError ->
          assertFailure ("expected introspection-aware saturation to succeed, got " <> show saturationError)
        Right saturationReport ->
          case ContextReport.reportDiagnosticTrace id saturationReport of
            PaleSaturation.SaturationTrace (firstIteration : _) ->
              do
                assertEqual
                  "expected the engine to honor the structural introspection overlay on the first round"
                  [RewriteRuleId 1, RewriteRuleId 0]
                  (fmap rtRuleId (PaleSaturation.sitRuleTraces firstIteration))
                assertBool
                  "expected adaptive introspection saturation to advance through the engine rather than short-circuiting"
                  (rsrIterations (reportSummary saturationReport) >= 1)
            PaleSaturation.SaturationTrace [] ->
              assertFailure "expected introspection-aware saturation to record at least one traced iteration"

testAdaptiveIntrospectionStrategySaturation :: Assertion
testAdaptiveIntrospectionStrategySaturation =
  withAdaptiveInitialGraph $ \graph2 ->
    let saturationConfig ::
          PlanSpec
            (EGraphU UnitContextSiteOwner ScopeCtx ArithF () TrivialContext)
            (SatGraph (EGraphU UnitContextSiteOwner ScopeCtx ArithF () TrivialContext))
            RewriteRuleId
        saturationConfig =
          withSchedulerConfig
            (traceAllSchedulerConfig deterministicSchedulerConfig)
            (planSpec (SaturationBudget 4 32) GenericJoinMatching emptyRewriteRuntimeCapabilities)
        guideStrategy ::
          Program () (EGraphStrategyPhase UnitContextSiteOwner ScopeCtx ArithF () TrivialContext)
        guideStrategy =
          phase
            (EGraphStrategyPhase "adaptive-phase" saturationConfig noGate)
   in case
        runIntrospectedSaturation
          RuntimeBetweenRounds
          (derivedIntrospectionSubject [adaptiveRuleAB, adaptiveRuleBC])
          (StrategyIntrospectedSaturation guideStrategy)
          [adaptiveEngineRuleAB, adaptiveEngineRuleBC]
          graph2
        of
        Left saturationError ->
          assertFailure ("expected introspection-aware strategy saturation to succeed, got " <> show saturationError)
        Right strategyReport ->
          case maybe (PaleSaturation.SaturationTrace []) (ContextReport.reportDiagnosticTrace id) (reportLastPhase strategyReport) of
            PaleSaturation.SaturationTrace (firstIteration : _) ->
              assertEqual
                "expected the strategy runner to route through the adaptive scheduler hook"
                [RewriteRuleId 1, RewriteRuleId 0]
                (fmap rtRuleId (PaleSaturation.sitRuleTraces firstIteration))
            PaleSaturation.SaturationTrace [] ->
              assertFailure "expected introspection-aware strategy saturation to record at least one traced iteration"

adaptiveInitialGraph :: Either UnionFindAllocationError (EGraph ArithF ())
adaptiveInitialGraph =
  fmap snd (addTerm (arithNumTerm 1) (emptyEGraph adaptiveAnalysisSpec))
    >>= fmap snd . addTerm (arithNumTerm 2)

withAdaptiveInitialGraph :: (EGraph ArithF () -> Assertion) -> Assertion
withAdaptiveInitialGraph assertions =
  either
    (assertFailure . ("adaptive graph allocation failed: " <>) . show)
    assertions
    adaptiveInitialGraph
