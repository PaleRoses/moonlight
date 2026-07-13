{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.EGraph.Extraction.GuideSpec
  ( tests,
  )
where

import Data.IntSet qualified as IntSet
import Moonlight.EGraph.Effect.CoveringSurface
  ( SurfaceKind,
  )
import Moonlight.Control.Gate
  ( GuideEvidence (geCheckpointHits),
    GuideRoundTrace (grtRetainedCount, grtSelection),
    GuideSelection (GuidePreferred),
    noGate,
  )
import Moonlight.Control.Weight
  ( nonCriticalPriorityRank,
    priorityEvidence,
    priorityProfileFromList,
  )
import Moonlight.Control.Schedule (withPriorityProfile)
import Moonlight.Control.Class
  ( phase,
  )
import Moonlight.Control.Trace
  ( PhaseSummary (spsMatchesApplied),
    Report (reportLastPhase, reportTrace),
    phaseSummaries,
    totalMatchesApplied,
  )
import Moonlight.Core (Language, UnionFindAllocationError)
import Moonlight.Delta.Scope qualified as Delta
import Moonlight.EGraph.Pure.Change (EGraphMutationResult (..))
import Moonlight.EGraph.Pure.Context (emptyContextEGraph)
import Moonlight.EGraph.Pure.Context.Proof (serializeProofLog)
import Moonlight.EGraph.Pure.Rebuild (rebuildWithDelta)
import Moonlight.EGraph.Pure.Relational (EGraphRelationalMatchObstruction, wcojMatchCompiledWithRoots)
import Moonlight.EGraph.Pure.Rewrite.Env (EGraphRewriteEnv (ereFactStore), emptyEGraphRewriteEnv)
import Moonlight.EGraph.Pure.Rewrite.Guard (acceptRewriteCondition)
import Moonlight.EGraph.Pure.Rewrite.Program (runExecutableRewriteMatchEGraphCommitted)
import Moonlight.EGraph.Pure.Saturation.Front
  ( EGraphFront,
    EGraphFrontReport (efrResult, efrScheduleReports),
    FrontPhase (Authored),
  )
import Moonlight.EGraph.Pure.Saturation.Front qualified as Front
import Moonlight.EGraph.Pure.Saturation.Guidance (GuidanceRound (grMatches, grTrace), applyGuidance)
import Moonlight.EGraph.Pure.Saturation.Logic.Run (EGraphLogicReport (elrSaturation))
import Moonlight.EGraph.Pure.Saturation.Matching (matchingDeltaFromRebuild, matchingFrontierFromDelta)
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingProofEGraph,
    emptySaturatingProofEGraph,
  )
import Moonlight.EGraph.Pure.Types (ClassId, EGraph, RewriteRuleId (..), classIdKey)
import Moonlight.EGraph.Test.Arith.Core (ArithF, NodeCount)
import Moonlight.EGraph.Test.Arith.Fixture (commutedAddGuidance, onePlusZero, seedArith)
import Moonlight.EGraph.Test.Arith.Rules (addCommuteRule, addZeroRightRule)
import Moonlight.EGraph.Test.Assertions (expectCompile, expectSaturation)
import Moonlight.EGraph.Test.Case (HUnitCase (..), hunitCases)
import Moonlight.EGraph.Test.Config (testConfig, tracingTestConfig)
import Moonlight.EGraph.Test.Front.Tiny qualified as Tiny
import Moonlight.EGraph.Test.Saturation
  ( EGraphSaturationConfig,
    EGraphStrategyPhase (..),
    ProofSaturationSpec (..),
    psrProofGraph,
    runProofSaturationSpec,
    saturateByStrategy,
    scSchedulerConfig,
    srGuideTrace,
    withSchedulerConfig,
    srTrace,
  )
import Moonlight.Rewrite.ProofContext (ProofAnnotationBuilder (..), ProofAnnotationInput (..), ProofStep (psAnnotation))
import Moonlight.Rewrite.Runtime (ExecutableRewriteMatch (..))
import Moonlight.Rewrite.Runtime (RulePlan (..))
import Moonlight.Rewrite.System
  ( CompiledGuard,
    GuardEvidence,
    RewriteCondition,
    emptyGuardCapabilityResolver,
  )
import Moonlight.Rewrite.System (emptyFactStore)
import Moonlight.Rewrite.System (RawRewriteRule (rrId))
import Moonlight.Saturation.Substrate (compileRewriteRules)
import Moonlight.Saturation.Context.Runtime.Report (reportMatchesApplied)
import Moonlight.Pale.Diagnostic.Section.Rewrite (rtRuleId)
import Moonlight.Pale.Diagnostic.Section.Saturation (sitRuleTraces)
import Moonlight.Pale.Diagnostic.Section.Saturation qualified as PaleSaturation
import Moonlight.Pale.Test.Site.Core (canonicalTestBudget)
import Moonlight.Pale.Test.Site.Assertion (expectRight)
import Moonlight.Saturation.Substrate (TrivialContext, trivialLattice)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=), assertBool, assertFailure)

data GuideNote = GuideNote
  { gnRewriteRuleId :: RewriteRuleId,
    gnGuideHitCount :: Int
  }
  deriving stock (Eq, Show)

tests :: TestTree
tests =
  testGroup "guide" . hunitCases $
    [ HUnitCase "applyGuidance retains only matches that preview into preferred checkpoint classes" $ do
        graph <- expectRight onePlusZeroGraph
        eligibleMatches <- compiledArithMatches [addZeroRightRule, addCommuteRule] graph
        let guidanceRoundValue = applyGuidance commutedAddGuidance 0 emptyFactStore graph eligibleMatches
        length eligibleMatches @?= 2
        assertPreferredRound guidanceRoundValue
        guidedMatch <- requireSingleton "expected exactly one guided rewrite match" (grMatches guidanceRoundValue)
        rpId (ermRule guidedMatch) @?= RewriteRuleId 1
        assertBool "expected guided match evidence" (hasGuideHit guidedMatch),
      HUnitCase "runProofSaturationSpec threads guide evidence into proof annotations" $ do
        graph <- expectRight onePlusZeroGraph
        proofReport <- expectSaturation (runProofSaturationSpec guideProofSpec [addZeroRightRule, addCommuteRule] (guideProofGraph graph))
        firstProofStep <- requireFirst "expected at least one proof step" (serializeProofLog (psrProofGraph proofReport))
        firstGuideTrace <- requireFirst "expected guide trace entries" (srGuideTrace proofReport)
        psAnnotation firstProofStep @?= GuideNote (RewriteRuleId 1) 1
        assertPreferredTrace firstGuideTrace,
      HUnitCase "strategy phase priority profile threads into phase execution" $ do
        graph <- expectRight onePlusZeroGraph
        strategyReport <-
          expectSaturation $
            saturateByStrategy
              (phase (EGraphStrategyPhase "adaptive-phase" priorityConfig noGate))
              []
              [addZeroRightRule, addCommuteRule]
              graph
        case maybe (PaleSaturation.SaturationTrace []) srTrace (reportLastPhase strategyReport) of
          PaleSaturation.SaturationTrace (firstIteration : _) ->
            fmap rtRuleId (sitRuleTraces firstIteration) @?= [rrId addCommuteRule, rrId addZeroRightRule]
          PaleSaturation.SaturationTrace [] ->
            assertFailure "expected an adaptive strategy trace",
      HUnitCase "saturateByStrategy does not reapply a single collapse rewrite across iterations" $ do
        graph <- expectRight onePlusZeroGraph
        strategyReport <-
          expectSaturation $
            saturateByStrategy
              (phase (EGraphStrategyPhase "single-collapse" tracingConfig noGate))
              []
              [addZeroRightRule]
              graph
        totalMatchesApplied strategyReport @?= 1
        fmap spsMatchesApplied (phaseSummaries (reportTrace strategyReport)) @?= [1],
      HUnitCase "front-authored companion reaches saturation through public EDSL" $ do
        frontReport <-
          Tiny.expectFront (Front.runEGraphFront frontAuthoredGuidanceCompanion Tiny.emptyFrontGraph)
        efrResult frontReport @?= True
        assertBool
          "front-authored guidance companion did not apply any public-EDSL rewrite"
          (frontMatchesApplied frontReport > 0),
      HUnitCase "collapsed e-class still re-matches the original add-zero pattern after rebuild" $ do
        graph <- expectRight onePlusZeroGraph
        initialMatch <- requireFirst "expected initial add-zero match" =<< compiledArithMatches [addZeroRightRule] graph
        rewriteCommit <-
          expectSaturation $
            runExecutableRewriteMatchEGraphCommitted
              emptyEGraphRewriteEnv {ereFactStore = emptyFactStore}
              initialMatch
              graph
        let (rebuildDelta, _, rebuiltGraph) = rebuildWithDelta (emrGraph rewriteCommit)
            impactedKeys = Delta.scopeKeys (matchingFrontierFromDelta (matchingDeltaFromRebuild rebuildDelta))
        postRewriteMatches <- compiledArithMatches [addZeroRightRule] rebuiltGraph
        assertBool
          ("post-collapse rebuild delta stays non-stable (root=" <> show (classIdKey (ermRootClass initialMatch)) <> ", impacted=" <> show impactedKeys <> ")")
          (maybe False (not . IntSet.null) impactedKeys)
        length postRewriteMatches @?= 1
    ]

frontAuthoredGuidanceCompanion ::
  EGraphFront 'Authored Tiny.FrontTinySig Tiny.NodeCount Tiny.FrontTinyContext Bool
frontAuthoredGuidanceCompanion =
  Front.egraph $ do
    simplificationRules <-
      Front.rulesetNamed "front-guidance-companion" Tiny.simpleArithmeticRules
    startTerm <-
      Front.defNamed "front-guidance-start" (Tiny.add (Tiny.sym "x") Tiny.zero)
    Front.run (Front.runFor Tiny.defaultBudget simplificationRules)
    Front.check @"front-guidance-simplifies" ((Front.===) startTerm (Tiny.sym "x"))

frontMatchesApplied ::
  EGraphFrontReport Tiny.FrontTinySig Tiny.NodeCount Tiny.FrontTinyContext result ->
  Int
frontMatchesApplied =
  sum . fmap (reportMatchesApplied . elrSaturation) . efrScheduleReports

rewriteMatches ::
  (Language f, Show (f ())) =>
  [RulePlan (CompiledGuard SurfaceKind f) f] ->
  EGraph f a ->
  Either EGraphRelationalMatchObstruction [ExecutableRewriteMatch (CompiledGuard SurfaceKind f) GuardEvidence (GuideEvidence ClassId) f]
rewriteMatches preparedRewrites graph =
  fmap concat $
    traverse
      (\preparedRewrite -> fmap (foldMap (acceptedRewriteMatch preparedRewrite)) (wcojMatchCompiledWithRoots (rpQuery preparedRewrite) graph))
      preparedRewrites
  where
    acceptedRewriteMatch preparedRewrite (rootClassId, substitution) =
      let rewriteMatch = ExecutableRewriteMatch preparedRewrite rootClassId Nothing Nothing substitution
       in either
            (const [])
            (\guardEvidence -> [rewriteMatch {ermGuardEvidence = guardEvidence}])
            (acceptRewriteCondition emptyFactStore emptyGuardCapabilityResolver rewriteMatch graph)

compiledArithMatches ::
  [RawRewriteRule (RewriteCondition SurfaceKind ArithF) ArithF] ->
  EGraph ArithF NodeCount ->
  IO [ExecutableRewriteMatch (CompiledGuard SurfaceKind ArithF) GuardEvidence (GuideEvidence ClassId) ArithF]
compiledArithMatches rules graph = do
  compiledRewrites <- expectCompile (compileRewriteRules @(EGraphU SurfaceKind ArithF NodeCount ()) rules)
  expectCompile (rewriteMatches compiledRewrites graph)

assertPreferredRound :: GuidanceRound SurfaceKind ArithF -> Assertion
assertPreferredRound guidanceRoundValue =
  assertPreferredTrace (grTrace guidanceRoundValue)

assertPreferredTrace :: GuideRoundTrace -> Assertion
assertPreferredTrace guideTraceValue = do
  grtSelection guideTraceValue @?= GuidePreferred
  grtRetainedCount guideTraceValue @?= 1

hasGuideHit :: ExecutableRewriteMatch guard evidence (GuideEvidence ClassId) ArithF -> Bool
hasGuideHit =
  maybe False (not . null . geCheckpointHits) . ermGuideEvidence

requireSingleton :: String -> [value] -> IO value
requireSingleton _ [value] =
  pure value
requireSingleton failureMessage _ =
  assertFailure failureMessage

requireFirst :: String -> [value] -> IO value
requireFirst _ (value : _) =
  pure value
requireFirst failureMessage [] =
  assertFailure failureMessage

guideNoteBuilder :: ProofAnnotationBuilder TrivialContext GuideNote
guideNoteBuilder =
  ProofAnnotationBuilder $
    \proofAnnotationInput ->
      GuideNote
        { gnRewriteRuleId = paiRewriteRuleId proofAnnotationInput,
          gnGuideHitCount = maybe 0 (length . geCheckpointHits) (paiGuideEvidence proofAnnotationInput)
        }

guideProofSpec :: ProofSaturationSpec SurfaceKind ArithF NodeCount TrivialContext GuideNote
guideProofSpec =
  ProofSaturationSpec
    { pssSaturation = testConfig canonicalTestBudget,
      pssGuidance = Just commutedAddGuidance,
      pssProofBuilder = guideNoteBuilder,
      pssActiveContext = Nothing
    }

guideProofGraph :: EGraph ArithF NodeCount -> SaturatingProofEGraph SurfaceKind ArithF NodeCount TrivialContext GuideNote
guideProofGraph =
  emptySaturatingProofEGraph . emptyContextEGraph trivialLattice

priorityConfig :: EGraphSaturationConfig SurfaceKind ArithF NodeCount ()
priorityConfig =
  withSchedulerConfig
    ( withPriorityProfile
        ( priorityProfileFromList
            [ (rrId addZeroRightRule, priorityEvidence 0 0 0 nonCriticalPriorityRank),
              (rrId addCommuteRule, priorityEvidence 1 0 0 nonCriticalPriorityRank)
            ]
        )
        (scSchedulerConfig tracingConfig)
    )
    tracingConfig

tracingConfig :: EGraphSaturationConfig SurfaceKind ArithF NodeCount ()
tracingConfig =
  tracingTestConfig canonicalTestBudget

onePlusZeroGraph :: Either UnionFindAllocationError (EGraph ArithF NodeCount)
onePlusZeroGraph =
  snd <$> seedArith onePlusZero
