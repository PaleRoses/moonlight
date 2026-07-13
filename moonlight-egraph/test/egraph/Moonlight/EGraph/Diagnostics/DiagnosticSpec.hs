{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Moonlight.EGraph.Diagnostics.DiagnosticSpec
  ( tests,
  )
where

import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Data.List (isInfixOf)
import Moonlight.Control.Schedule (backoffConfig)
import Moonlight.Core (emptySubstitution)
import Moonlight.EGraph.Pure.Context (activateContext, contextMerge, emptyContextEGraph)
import Moonlight.EGraph.Pure.Context.Core (cegBase)
import Moonlight.EGraph.Pure.Context.Proof (emptyProofEGraph, recordProofStepWith, summarizeProofLog)
import Moonlight.EGraph.Pure.Introspection.Diagnostic
  ( ContextDiagnostic (cdCachedContexts, cdPropagationConverged, cdRestrictionCount),
    EGraphDiagnostic (egdClassCount, egdNodeCount),
    EGraphSnapshot (egsClasses, egsPendingMerges),
    contextDiagnostic,
    graphDiagnostic,
    renderEGraphDot,
    renderEGraphSummary,
    snapshotEGraph,
  )
import Moonlight.EGraph.Pure.Saturation.Front
  ( EGraphFront,
    EGraphFrontReport (..),
    FrontPhase (Authored),
    SaturationBudget (..),
    def,
    done,
    egraph,
    runEGraphFront,
  )
import Moonlight.EGraph.Pure.Saturation.Front.PackedNode (PackedNode)
import Moonlight.EGraph.Pure.Saturation.Matching (MatchingStrategy (GenericJoinMatching))
import Moonlight.EGraph.Pure.Types (EGraph, RewriteRuleId (RewriteRuleId), canonicalizeClassId)
import Moonlight.EGraph.Saturation.Context.State (sceContextGraph)
import Moonlight.EGraph.Test.Arith.Core (ArithF, NodeCount)
import Moonlight.EGraph.Test.Arith.Fixture (classOfArith, one, onePlusZero, onePlusZeroPlusZero, seedArithTerms, zero)
import Moonlight.EGraph.Test.Arith.Rules (addZeroRightRule)
import Moonlight.EGraph.Test.Assertions (expectSaturation)
import Moonlight.EGraph.Test.Case (HUnitCase (..), hunitCases)
import Moonlight.EGraph.Test.Context.ThreeLevel (Scope (..))
import Moonlight.EGraph.Test.Front.Tiny qualified as FrontTiny
import Data.Fix (Fix)
import Moonlight.EGraph.Test.Saturation
  ( EGraphSaturationConfig,
    EGraphSaturationReport,
    data SaturationConfig,
    backoffSchedulerConfig,
    saturateWith,
    scBudget,
    scMatchingStrategy,
    scSchedulerConfig,
    srTrace,
    traceAllSchedulerConfig,
  )
import Moonlight.Rewrite.ProofContext
  ( ProofCompressionSummary (..),
    defaultProofStepInput,
  )
import Moonlight.Pale.Diagnostic.Derived.Rewrite
  ( RewriteOutcomeSummary (..),
    rtrsTotalTransitions,
    rtrsTransitions,
    summarizeRewriteTransitions,
    summarizeSaturationTrace,
  )
import Moonlight.Pale.Diagnostic.Section.Rewrite (rtsFromRule, rtsToRule)
import Moonlight.Pale.Test.Site.Assertion (expectRight, withResult)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertEqual, assertFailure)
import Moonlight.FiniteLattice
  ( ContextLattice,
    latticeContext
  )

tests :: TestTree
tests =
  testGroup "diagnostic" . hunitCases $
    [ HUnitCase "snapshotEGraph reports class and pending merge structure" $ do
        graphSnapshot <- snapshotEGraph <$> frontSeedGraph frontThreeSeedProgram
        length (egsClasses graphSnapshot) @?= 3
        egsPendingMerges graphSnapshot @?= [],
      HUnitCase "graphDiagnostic reports structural counts" $ do
        diagnostic <- graphDiagnostic <$> frontSeedGraph frontThreeSeedProgram
        egdClassCount diagnostic @?= 3
        egdNodeCount diagnostic @?= 3,
      HUnitCase "renderEGraphDot is deterministic" $ do
        graph <- frontSeedGraph frontOneSumSeedProgram
        let firstRender = renderEGraphDot graph
            secondRender = renderEGraphDot graph
        firstRender @?= secondRender
        assertBool "expected a graphviz header" ("digraph egraph" `isInfixOf` firstRender),
      HUnitCase "renderEGraphSummary includes class summaries" $ do
        summary <- renderEGraphSummary <$> frontSeedGraph frontOneSeedProgram
        assertBool "expected the rendered summary to mention the class payload" ("Class 0" `isInfixOf` summary),
      HUnitCase "summarizeSaturationTrace aggregates scheduled rewrites" $ do
        saturationReport <- tracedArithSaturation onePlusZero diagnosticTraceBudget
        assertBool "expected scheduled rewrites in trace summary" (rosTotalScheduled (summarizeSaturationTrace (srTrace saturationReport)) > 0),
      HUnitCase "summarizeRewriteTransitions reports rewrite trajectories" $ do
        saturationReport <- tracedArithSaturation onePlusZeroPlusZero rewriteTransitionBudget
        let transitionSummary = summarizeRewriteTransitions (srTrace saturationReport)
        assertEqual "one self-transition (Rule 0 -> Rule 0)" 1 (rtrsTotalTransitions transitionSummary)
        case rtrsTransitions transitionSummary of
          [transitionStat] -> do
            rtsFromRule transitionStat @?= RewriteRuleId 0
            rtsToRule transitionStat @?= RewriteRuleId 0
          _ -> assertFailure "expected exactly one rewrite transition stat referencing add-zero-right",
      HUnitCase "summarizeProofLog measures provenance compression" $ do
        graph <- expectRight (seedArithTerms [one, zero])
        oneClassId <- expectRight (classOfArith one graph)
        zeroClassId <- expectRight (classOfArith zero graph)
        let contextGraph = emptyContextEGraph diagnosticLattice graph
            canonicalize = canonicalizeClassId (cegBase contextGraph)
            proofGraph1 = recordProofStepWith canonicalize (defaultProofStepInput (RewriteRuleId 0) oneClassId zeroClassId emptySubstitution ()) (emptyProofEGraph contextGraph)
            proofGraph2 = recordProofStepWith canonicalize (defaultProofStepInput (RewriteRuleId 1) oneClassId zeroClassId emptySubstitution ()) proofGraph1
            compressionSummary = summarizeProofLog proofGraph2
        pcsTotalSteps compressionSummary @?= 2
        pcsUniqueClassPairs compressionSummary @?= 1
        pcsCompressionSavings compressionSummary @?= 1,
      HUnitCase "contextDiagnostic reports sheaf propagation state" $ do
        graph <- expectRight (seedArithTerms [one, zero, onePlusZero])
        oneClassId <- expectRight (classOfArith one graph)
        sumClassId <- expectRight (classOfArith onePlusZero graph)
        withResult (contextMerge ModuleCtx sumClassId oneClassId (emptyContextEGraph diagnosticLattice graph)) $ \mergedGraph ->
          withResult (activateContext ModuleCtx mergedGraph) $ \moduleActiveGraph ->
            withResult (activateContext LocalCtx moduleActiveGraph) $ \contextGraph -> do
              let diagnostic = contextDiagnostic contextGraph
              assertEqual
                "cached contexts are the explicit analysis frontier"
                [ModuleCtx, LocalCtx]
                (cdCachedContexts diagnostic)
              assertBool "expected at least one restriction edge" (cdRestrictionCount diagnostic > 0)
              cdPropagationConverged diagnostic @?= True
    ]

diagnosticLattice :: ContextLattice Scope
diagnosticLattice =
  case latticeContext of
    Right latticeValue -> latticeValue
    Left compileError ->
      error ("invalid diagnostic Scope lattice fixture: " <> show compileError)

frontSeedGraph :: FrontDiagnosticProgram () -> IO FrontDiagnosticGraph
frontSeedGraph program = do
  report <- FrontTiny.expectFront (runEGraphFront program FrontTiny.emptyFrontGraph)
  pure (cegBase (sceContextGraph (efrFinalGraph report)))

tracedArithSaturation :: Fix ArithF -> SaturationBudget -> IO (EGraphSaturationReport SurfaceKind ArithF NodeCount)
tracedArithSaturation term budget = do
  graph <- expectRight (seedArithTerms [term])
  expectSaturation (saturateWith (tracedArithConfig budget) [addZeroRightRule] graph)

tracedArithConfig :: SaturationBudget -> EGraphSaturationConfig SurfaceKind ArithF NodeCount ()
tracedArithConfig budget =
  SaturationConfig
    { scBudget = budget,
      scMatchingStrategy = GenericJoinMatching,
      scSchedulerConfig = traceAllSchedulerConfig (backoffSchedulerConfig (backoffConfig 1000 10))
    }

frontOneSeedProgram :: FrontDiagnosticProgram ()
frontOneSeedProgram =
  egraph $ do
    _ <- def @"one" FrontTiny.one
    pure done

frontOneSumSeedProgram :: FrontDiagnosticProgram ()
frontOneSumSeedProgram =
  egraph $ do
    _ <- def @"one" FrontTiny.one
    _ <- def @"sum" (FrontTiny.add FrontTiny.one FrontTiny.zero)
    pure done

frontThreeSeedProgram :: FrontDiagnosticProgram ()
frontThreeSeedProgram =
  egraph $ do
    _ <- def @"one" FrontTiny.one
    _ <- def @"zero" FrontTiny.zero
    _ <- def @"sum" (FrontTiny.add FrontTiny.one FrontTiny.zero)
    pure done

diagnosticTraceBudget :: SaturationBudget
diagnosticTraceBudget =
  SaturationBudget 100 100000

rewriteTransitionBudget :: SaturationBudget
rewriteTransitionBudget =
  SaturationBudget 4 32

type FrontDiagnosticProgram result =
  EGraphFront 'Authored FrontTiny.FrontTinySig FrontTiny.NodeCount FrontTiny.FrontTinyContext result

type FrontDiagnosticGraph =
  EGraph (PackedNode FrontTiny.FrontTinySig) FrontTiny.NodeCount
