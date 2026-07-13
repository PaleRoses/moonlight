module Moonlight.EGraph.Introspection.NerveSpec.Gluing.Relative
  ( tests,
  )
where

import Moonlight.EGraph.Introspection.NerveSpec.Gluing.Prelude
import Moonlight.EGraph.Introspection.NerveSpec.Fixture
import Moonlight.EGraph.Introspection.Core.Rewrite.Successor qualified as RewriteSuccessor
import Moonlight.Pale.Diagnostic.Section.Rewrite (rosScheduledCount, rtsCount)
import Data.List.NonEmpty qualified as NonEmpty

tests :: TestTree
tests =
  testGroup
    "relative"
    [ testCase "identified rewrite systems expose native rule ids for runtime overlays" testIdentifiedRewriteSystem,
      testCase "ambiguous structural spans surface ambiguous runtime identities rather than fabricating one" testAmbiguousRuntimeRuleIdentity,
      testCase "unidentified rewrite systems keep successor identity annotations empty" testUnidentifiedSuccessorNodeIdentity,
      testCase "runtime successor overlay reuses saturation diagnostics rather than inventing weights" testRuntimeWeightedSuccessor,
      testCase "relative diagnostics separate absolute cross-context structure from grounded rule influence" testRelativeDiagnostics,
      testCase "runtime relative diagnostics separate observed grounded influence from merely structural grounding" testRuntimeRelativeDiagnostics,
      testCase "support runtime relative diagnostics retain support-local runtime pressure without fabricating successor observations" testSupportRuntimeRelativeDiagnostics,
      testCase "Grothendieck consistency profile reports Grothendieck Tarski coverage without flattening the site" testGrothendieckConsistencyProfile
    ]

testIdentifiedRewriteSystem :: Assertion
testIdentifiedRewriteSystem =
  let successorComplex = RewriteSuccessor.rewriteSuccessorComplex identifiedAcyclicChainSystem
   in do
    assertEqual
      "expected identified rewrite systems to expose the rule ids carried at construction time"
      [Just (RewriteRuleId 0), Just (RewriteRuleId 1)]
      (fmap (rewriteRuleIdOf identifiedAcyclicChainSystem) [contextSpanAB, contextSpanBC])
    assertEqual
      "expected the native runtime identity resolver to distinguish unique identities explicitly"
      [ UniqueRuntimeRuleIdentity (RewriteRuleId 0),
        UniqueRuntimeRuleIdentity (RewriteRuleId 1)
      ]
      (fmap (resolveRuntimeRuleIdentity identifiedAcyclicChainSystem) [contextSpanAB, contextSpanBC])
    assertEqual
      "expected successor nodes to carry the same native rule identities as annotations"
      [ UniqueRuntimeRuleIdentity (RewriteRuleId 0),
        UniqueRuntimeRuleIdentity (RewriteRuleId 1)
      ]
      (fmap snRuntimeRuleIdentity (rscNodes successorComplex))

testAmbiguousRuntimeRuleIdentity :: Assertion
testAmbiguousRuntimeRuleIdentity =
  let ambiguousIdentity = AmbiguousRuntimeRuleIdentity ((RewriteRuleId 0) :| [RewriteRuleId 7])
      successorComplex = RewriteSuccessor.rewriteSuccessorComplex ambiguousIdentifiedSystem
   in do
        assertEqual
          "expected duplicate structural spans with different rule ids to surface an ambiguous runtime identity"
          ambiguousIdentity
          (resolveRuntimeRuleIdentity ambiguousIdentifiedSystem contextSpanAB)
        assertEqual
          "expected successor nodes to preserve ambiguous native identities rather than discarding them"
          [ambiguousIdentity]
          (fmap snRuntimeRuleIdentity (rscNodes successorComplex))

testUnidentifiedSuccessorNodeIdentity :: Assertion
testUnidentifiedSuccessorNodeIdentity =
  assertEqual
    "expected span-only rewrite systems to carry no native runtime identity annotation on successor nodes"
    [NoRuntimeRuleIdentity, NoRuntimeRuleIdentity]
    (fmap snRuntimeRuleIdentity (rscNodes (RewriteSuccessor.rewriteSuccessorComplex acyclicChainSystem)))

testRuntimeWeightedSuccessor :: Assertion
testRuntimeWeightedSuccessor =
  let runtimeOverlay =
        RewriteSuccessor.runtimeWeightedSuccessorComplexFromTrace acyclicTrace identifiedAcyclicChainSystem
   in do
        assertEqual
          "expected the runtime overlay to observe the unique structural successor edge"
          1
          (RewriteSuccessor.runtimeWeightedEdgeCount runtimeOverlay)
        assertEqual
          "expected the structural successor edge to be fully observed in the trace"
          0
          (RewriteSuccessor.unobservedStructuralEdgeCount runtimeOverlay)
        assertEqual
          "expected all visible successor nodes to map to runtime rule ids"
          0
          (length (RewriteSuccessor.rwscUnmappedNodes runtimeOverlay))
        assertEqual
          "expected native runtime identity resolution to report no ambiguity for the identified chain"
          0
          (length (RewriteSuccessor.rwscAmbiguousNodes runtimeOverlay))
        case RewriteSuccessor.rwscWeightedEdges runtimeOverlay of
          [weightedEdge] -> do
            assertEqual
              "expected the observed transition count to come from rewrite transition diagnostics"
              [1]
              (fmap rtsCount (NonEmpty.toList (rieTransitions (rweEvidence weightedEdge))))
            assertEqual
              "expected the source outcome stat to come from saturation outcome diagnostics"
              [1]
              (maybe [] (fmap rosScheduledCount . NonEmpty.toList) (rieSourceOutcomes (rweEvidence weightedEdge)))
            assertEqual
              "expected the target outcome stat to come from saturation outcome diagnostics"
              [1]
              (maybe [] (fmap rosScheduledCount . NonEmpty.toList) (rieTargetOutcomes (rweEvidence weightedEdge)))
          _ ->
            assertFailure "expected exactly one weighted successor edge"

testRelativeDiagnostics :: Assertion
testRelativeDiagnostics =
  case multiContextSystemResult of
    Left failure ->
      assertFailure (show failure)
    Right rewriteSystem ->
      case relativeDiagnostics rewriteSystem 2 of
        Left failure ->
          assertFailure (show failure)
        Right diagnosticsValue ->
          let absoluteValue = rdAbsolute diagnosticsValue
              grothendieckSummary = adGrothendieckSummary absoluteValue
              successorComplex = RewriteSuccessor.rewriteSuccessorComplex rewriteSystem
           in do
                assertBool
                  "expected absolute diagnostics to retain cross-context structure"
                  (gssCrossContextMorphismCount grothendieckSummary > 0)
                assertEqual
                  "expected the absolute layer to expose the vertical restriction loss explicitly"
                  2
                  (gssVerticalMorphismCount grothendieckSummary)
                assertBool
                  "expected the absolute layer to include Grothendieck-site cells"
                  (gssCellCount grothendieckSummary > 0)
                assertBool
                  "expected the absolute layer to include Grothendieck-site faces"
                  (gssFaceCount grothendieckSummary > 0)
                assertEqual
                  "expected multi-context absolute diagnostics to carry verified multi-context nilpotence evidence"
                  MultiContextNilpotent
                  (gssCoboundaryNilpotenceEvidence (adGrothendieckSummary absoluteValue))
                assertEqual
                  "expected the Grothendieck homotopy profile to preserve connectedness in the multi-context chain"
                  1
                  (nhpConnectedComponents (gssHomotopyProfile grothendieckSummary))
                assertEqual
                  "expected rule grounding to lose exactly the vertical structure"
                  (gssVerticalMorphismCount grothendieckSummary)
                  (rdVerticalLoss diagnosticsValue)
                assertEqual
                  "expected grounded node coverage to coincide with the visible rule-successor carrier"
                  (successorNodeCount successorComplex)
                  (rdGroundedNodeCoverage diagnosticsValue)
                assertEqual
                  "expected diagonal and horizontal rule morphisms to compress onto the grounded rule carrier"
                  1
                  (rdStructuralCompressionGap diagnosticsValue)
                assertEqual
                  "expected the visible static rule chain to ground to one scheduler-successor edge"
                  1
                  (rdGroundedChainCount diagnosticsValue)

testRuntimeRelativeDiagnostics :: Assertion
testRuntimeRelativeDiagnostics =
  case identifiedMultiContextSystemResult of
    Left failure ->
      assertFailure (show failure)
    Right rewriteSystem ->
      case runtimeRelativeDiagnostics acyclicTrace rewriteSystem 2 of
        Left failure ->
          assertFailure (show failure)
        Right diagnosticsValue -> do
          let baseValue = rrdBase diagnosticsValue
          assertEqual
            "expected runtime-relative diagnostics to retain the structural relative base"
            1
            (rdGroundedChainCount baseValue)
          assertEqual
            "expected every grounded morphism in the identified fixture to be observed in the runtime outcome summary"
            (rdGroundedMorphismCount baseValue)
            (rrdObservedGroundedMorphismCount diagnosticsValue)
          assertEqual
            "expected runtime-relative node coverage to match the grounded node coverage on the fully observed fixture"
            (rdGroundedNodeCoverage baseValue)
            (rrdObservedGroundedNodeCoverage diagnosticsValue)
          assertEqual
            "expected the grounded rule chain to be observed in the runtime transition summary"
            1
            (rrdObservedGroundedChainCount diagnosticsValue)
          assertEqual
            "expected no grounded chains to remain structurally unobserved on the fully observed fixture"
            0
            (rrdUnobservedGroundedChainCount diagnosticsValue)
          assertEqual
            "expected native identified rewrite systems to leave no grounded nodes unmapped at runtime"
            0
            (rrdUnmappedGroundedNodeCount diagnosticsValue)
          assertEqual
            "expected the identified fixture to avoid ambiguous grounded runtime identities"
            0
            (rrdAmbiguousGroundedNodeCount diagnosticsValue)

testSupportRuntimeRelativeDiagnostics :: Assertion
testSupportRuntimeRelativeDiagnostics =
  case identifiedMultiContextSystemResult of
    Left failure ->
      assertFailure (show failure)
    Right rewriteSystem ->
      case supportRuntimeRelativeDiagnostics sampleSupportTrace rewriteSystem 2 of
        Left failure ->
          assertFailure (show failure)
        Right diagnosticsValue -> do
          let baseValue = rrdBase diagnosticsValue
          assertEqual
            "expected support-runtime relative diagnostics to retain the structural relative base"
            1
            (rdGroundedChainCount baseValue)
          assertEqual
            "expected support-runtime relative diagnostics not to fabricate successor-runtime grounded observations"
            (0, 0, 0)
            ( rrdObservedGroundedMorphismCount diagnosticsValue,
              rrdObservedGroundedNodeCoverage diagnosticsValue,
              rrdObservedGroundedChainCount diagnosticsValue
            )
          assertEqual
            "expected support-runtime relative diagnostics to surface observed support rule count from the overlay"
            (Just 2)
            (fmap srcObservedRuleCount (rrdSupportRuntimeCounts diagnosticsValue))
          assertEqual
            "expected support-runtime relative diagnostics to surface suppressed support-rule pressure"
            (Just 1)
            (fmap srcSuppressedRuleCount (rrdSupportRuntimeCounts diagnosticsValue))
          assertEqual
            "expected support-runtime relative diagnostics to surface cooldown pressure"
            (Just 1)
            (fmap srcCooldownRuleCount (rrdSupportRuntimeCounts diagnosticsValue))

testGrothendieckConsistencyProfile :: Assertion
testGrothendieckConsistencyProfile =
  case multiContextSystemResult of
    Left failure ->
      assertFailure (show failure)
    Right rewriteSystem ->
      case grothendieckConsistencyProfile rewriteSystem 2 of
        Left failure ->
          assertFailure (show failure)
        Right profileValue -> do
          assertBool
            "expected the Grothendieck Tarski profile to converge on the compatible multi-context chain"
            (gcpConverged profileValue)
          assertEqual
            "expected the compatible multi-context chain to report no restriction mismatches"
            0
            (gcpMismatchCount profileValue)
          assertEqual
            "expected the Grothendieck Tarski profile to retain full consistent-cell coverage on the compatible chain"
            (Just 1.0)
            (fmap probValue (gcpConsistencyRatio profileValue))
