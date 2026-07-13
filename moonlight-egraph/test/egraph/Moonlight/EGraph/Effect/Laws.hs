{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}

module Moonlight.EGraph.Effect.Laws
  ( tests,
  )
where

import Control.Monad (filterM)
import Data.Bifunctor ( first )
import Moonlight.EGraph.Effect.CoveringSurface
  ( SurfaceKind,
  )
import Moonlight.EGraph.Effect.Harness
    ( analysisJoinAssociative,
      analysisJoinCommutative,
      antiUnifyGeneralizes,
      antiUnifyLeast,
      contextGlobalSection,
      contextGlobalSectionInvariantLaw,
      contextMergeMonotone,
      contextMorphismAssociativeLaw,
      contextMorphismLeftIdentityLaw,
      contextMorphismRightIdentityLaw,
      contextRestrictionComposition,
      contextRestrictionFunctorialActionLaw,
      contextRestrictionIdentityLaw,
      extractDeterministic,
      extractInClass,
      extractOptimal,
      findIdempotent,
      hashConsIdempotent,
      mergeCommutative,
      obstructionComplete,
      proofContextConsistency,
      proofSoundness,
      rebuildIdempotent,
      saturationBounded )
import Moonlight.EGraph.Effect.LawNames
    ( EGraphLawName(AnalysisJoinAssociative, FindIdempotent,
                    MergeCommutative, HashConsIdempotent, RebuildIdempotent,
                    SaturationBounded, ExtractInClass, ExtractOptimal,
                    ExtractDeterministic, ContextRestrictionComposition,
                    ContextRestrictionIdentity, ContextMorphismLeftIdentity,
                    ContextMorphismRightIdentity, ContextMorphismAssociative,
                    ContextRestrictionFunctorialAction, ContextGlobalSection,
                    ContextGlobalSectionInvariant, ProofSoundness,
                    ProofContextConsistency, AntiUnifyGeneralizes, AntiUnifyLeast,
                    ObstructionComplete, SupportUnionIdempotent,
                    SupportUnionCommutative, SupportUnionAssociative,
                    SupportMeetIntersection, SupportRestrictionDistributive,
                    SupportSaturationOrderInvariant, AnalysisJoinCommutative) )
import Moonlight.EGraph.Pure.Context
    ( contextMerge, emptyContextEGraph, globalMerge, ContextEGraph, ContextDeltaError )
import Moonlight.EGraph.Pure.Context.Core (cegBase)
import Moonlight.EGraph.Pure.Extraction
    ( ExtractionFixpointBudget(..), ExtractionResult(erTerm) )
import Moonlight.EGraph.Pure.Kernel.HashCons ( addTerm )
import Moonlight.EGraph.Pure.Context.Proof
    ( ProofEGraph,
      ProofGraph(pgGraph),
      emptyProofEGraph,
      recordProofStepWith,
      summarizeProofLog )
import Moonlight.EGraph.Pure.Rebuild ( merge, rebuild )
import Moonlight.EGraph.Pure.Saturation.Matching
    ( MatchingStrategy(GenericJoinMatching) )
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.EGraph.Saturation.Context.State
    ( SaturatingProofEGraph,
      emptySaturatingProofEGraph,
      sceContextGraph )
import Moonlight.EGraph.Pure.Types
    ( ClassId, RewriteRuleId(..), EGraph, canonicalizeClassId, emptyEGraph )
import Moonlight.EGraph.Test.Arith.Core
    ( ArithF(..), NodeCount(..), addTermNode, analysisSpec, numTerm )
import Moonlight.EGraph.Test.Arith.Cost ( arithCost )
import Moonlight.EGraph.Test.Arith.Rules
    ( ArithRewriteFixture(CommuteAddFixture),
      addZeroRightRule,
      arithRewriteFixture )
import Moonlight.EGraph.Test.Context.ThreeLevel ( Scope(..) )
import Moonlight.EGraph.Test.Saturation
    ( SaturationConfig, data SaturationConfig, scBudget, scMatchingStrategy, scSchedulerConfig,
      SaturationBudget(..),
      SaturationTermination,
      backoffSchedulerConfig,
      deterministicSchedulerConfig,
      srIterations,
      srMatchesApplied,
      srCarrier,
      srResult,
      emptyRewriteRuntimeCapabilities,
      prepareEGraphSupportPlan,
      runEGraphSupportPlan,
      traceAllSchedulerConfig
      )
import Moonlight.EGraph.Test.Saturation.Diagnostics
    ( SupportFamilyDiagnostics(..), supportFamilyDiagnostics )
import Data.Fix ( Fix(..) )
import Moonlight.Core (UnionFindAllocationError, emptySubstitution)
import Moonlight.Rewrite.System
  ( RawRewriteRule
  )
import Moonlight.Rewrite.System (RewriteCondition)
import Moonlight.Rewrite.ProofContext
    ( ProofCompressionSummary,
      defaultProofAnnotationBuilder,
      defaultProofStepInput )
import Moonlight.Rewrite.ProofContext
    ( SupportBasis,
      principalSupport,
      supportBasis,
      supportContains,
      supportGenerators,
      supportMeet,
      supportReachableContexts,
      supportUnion )
import Moonlight.Sheaf.Context.Site
    ( fromFiniteLattice )
import Moonlight.Control.Schedule
    ( ScheduleGroup(..),
      SchedulerConfig,
      backoffConfig )
import Moonlight.Control.Schedule.Round
    ( ScheduleTrace(..) )
import Moonlight.Control.Count
    ( naturalToBoundedInt,
      workCountLowerBoundToBoundedInt )
import Moonlight.Control.Scheduling.Support
    ( SupportTraceView(..),
      scheduleTraceSupportView )
import Moonlight.Saturation.Support.Core
    ( SupportSaturationReportFor,
      SupportScheduleGroup,
      supportReportScheduleTrace )
import Moonlight.Saturation.Context.Driver (crrResult)
import Moonlight.Saturation.Context.Error (SaturationError)
import Moonlight.Saturation.Context.Program.Spec (staticRewriteContextSnapshot)
import Moonlight.Pale.Test.LawSuite
    ( QuickCheckLawBundle,
      lawSuiteGroup,
      quickCheckLawBundle,
      quickCheckLawBundleGroup,
      quickCheckLawDefinition )
import Moonlight.Pale.Test.Site.Core ( TestBudget(..), canonicalTestBudget )
import Test.Tasty ( TestTree, localOption, testGroup )
import Test.Tasty.HUnit
    ( Assertion, assertBool, assertEqual, assertFailure, testCase )
import qualified Test.Tasty.QuickCheck as QC
    ( Property,
      (===),
      counterexample,
      QuickCheckTests(QuickCheckTests) )
import Moonlight.EGraph.Pure.Saturation.Extraction qualified as Saturation
    ( ContextScope (CachedObjects),
      ContextualExtractionObstruction,
      contextualExtractionPartitionsBounded )
import Data.Set qualified as Set
    ( Set, fromList, intersection, toList )
import Moonlight.Sheaf.Twist.SupportedRuleSpec qualified as SheafTwist
import Moonlight.Sheaf.Twist.Extraction qualified as SheafTwist
    ( ContextualExtractionPartition(cepContexts, cepResult) )
import Moonlight.FiniteLattice
  ( ContextLattice (clBottom, clTop),
    ContextLatticeLookupError,
    contextLatticeElements,
    latticeContext
  )

effectLawBundle :: QuickCheckLawBundle String EGraphLawName
effectLawBundle =
  quickCheckLawBundle
    "effect-laws"
    [ quickCheckLawDefinition FindIdempotent
        (withBaseFixture $ \(oneClassId, _, graph) -> findIdempotent oneClassId graph),
      quickCheckLawDefinition MergeCommutative
        (withBaseFixture $ \(oneClassId, sumClassId, graph) -> mergeCommutative oneClassId sumClassId graph),
      quickCheckLawDefinition HashConsIdempotent
        (withBaseFixture $ \(_, _, graph) -> either (const False) id (hashConsIdempotent (addTermNode (numTerm 1) (numTerm 0)) graph)),
      quickCheckLawDefinition RebuildIdempotent
        (withBaseFixture $ \(_, _, graph) -> rebuildIdempotent graph),
      quickCheckLawDefinition SaturationBounded
        (withBaseFixture $ \(_, _, graph) -> saturationBounded defaultBudget [addZeroRightRule] graph),
      quickCheckLawDefinition ExtractInClass
        (withBaseFixture $ \(_, sumClassId, graph) -> extractInClass arithCost sumClassId graph),
      quickCheckLawDefinition ExtractOptimal
        (withBaseFixture $ \(_, sumClassId, graph) -> extractOptimal arithCost sumClassId graph),
      quickCheckLawDefinition ExtractDeterministic
        (withBaseFixture $ \(_, sumClassId, graph) -> extractDeterministic arithCost sumClassId graph),
      quickCheckLawDefinition ContextRestrictionComposition
        ( withContextFixture $ \(sumClassId, oneClassId, contextGraph) ->
            contextRestrictionComposition LocalCtx ModuleCtx GlobalCtx contextGraph && contextMergeMonotone ModuleCtx sumClassId oneClassId contextGraph
        ),
      quickCheckLawDefinition ContextRestrictionIdentity
        (withContextFixture $ \(_, _, contextGraph) -> contextRestrictionIdentityLaw ModuleCtx contextGraph),
      quickCheckLawDefinition ContextMorphismLeftIdentity
        (withContextFixture $ \(_, _, contextGraph) -> contextMorphismLeftIdentityLaw ModuleCtx contextGraph),
      quickCheckLawDefinition ContextMorphismRightIdentity
        (withContextFixture $ \(_, _, contextGraph) -> contextMorphismRightIdentityLaw ModuleCtx contextGraph),
      quickCheckLawDefinition ContextMorphismAssociative
        (withContextFixture $ \(_, _, contextGraph) -> contextMorphismAssociativeLaw LocalCtx ModuleCtx GlobalCtx GlobalCtx contextGraph),
      quickCheckLawDefinition ContextRestrictionFunctorialAction
        (withContextFixture $ \(_, _, contextGraph) -> contextRestrictionFunctorialActionLaw LocalCtx ModuleCtx GlobalCtx contextGraph),
      quickCheckLawDefinition ContextGlobalSection
        (withContextFixture $ \(sumClassId, oneClassId, contextGraph) -> contextGlobalSection sumClassId oneClassId contextGraph),
      quickCheckLawDefinition ContextGlobalSectionInvariant
        (withGlobalContextFixture $ \contextGraph -> contextGlobalSectionInvariantLaw LocalCtx GlobalCtx contextGraph),
      quickCheckLawDefinition ProofSoundness
        (either (const False) (proofSoundness cegBase) proofFixture),
      quickCheckLawDefinition ProofContextConsistency
        (either (const False) (proofContextConsistency id) proofFixture),
      quickCheckLawDefinition AntiUnifyGeneralizes
        (withBaseFixture $ \(oneClassId, sumClassId, graph) -> antiUnifyGeneralizes oneClassId sumClassId (rebuild (merge oneClassId sumClassId graph))),
      quickCheckLawDefinition AntiUnifyLeast
        (withBaseFixture $ \(oneClassId, sumClassId, graph) -> antiUnifyLeast oneClassId sumClassId (rebuild (merge oneClassId sumClassId graph))),
      quickCheckLawDefinition ObstructionComplete
        (withContextFixture $ \(sumClassId, oneClassId, contextGraph) -> obstructionComplete sumClassId oneClassId GlobalCtx contextGraph),
      quickCheckLawDefinition SupportUnionIdempotent supportUnionIdempotent,
      quickCheckLawDefinition SupportUnionCommutative supportUnionCommutative,
      quickCheckLawDefinition SupportUnionAssociative supportUnionAssociative,
      quickCheckLawDefinition SupportMeetIntersection supportMeetIntersection,
      quickCheckLawDefinition SupportRestrictionDistributive supportRestrictionDistributive,
      quickCheckLawDefinition SupportSaturationOrderInvariant supportSaturationOrderInvariant,
      quickCheckLawDefinition AnalysisJoinCommutative
        (\leftValue rightValue -> analysisJoinCommutative leftValue rightValue analysisSpec),
      quickCheckLawDefinition AnalysisJoinAssociative
        (\firstValue secondValue thirdValue -> analysisJoinAssociative firstValue secondValue thirdValue analysisSpec)
    ]

tests :: TestTree
tests =
  testGroup
    "egraph-effect-laws"
    [ localOption
        (QC.QuickCheckTests 20)
        (lawSuiteGroup "egraph-effect-laws" [quickCheckLawBundleGroup "egraph" id [effectLawBundle]]),
      supportDiagnosticsTests
    ]

supportDiagnosticsTests :: TestTree
supportDiagnosticsTests =
  testGroup
    "support-diagnostics"
    [ testCase "support family diagnostics expose globalized rule coverage" testSupportFamilyDiagnosticsExposeGlobalCoverage,
      testCase "support backoff scheduler suppresses duplicate matches" testSupportBackoffSchedulerSuppressesDuplicateMatches
    ]

scopeLattice :: ContextLattice Scope
scopeLattice =
  case latticeContext of
    Right latticeValue -> latticeValue
    Left compileError ->
      error ("invalid effect-law Scope lattice fixture: " <> show compileError)

testSupportFamilyDiagnosticsExposeGlobalCoverage :: Assertion
testSupportFamilyDiagnosticsExposeGlobalCoverage = do
  let contexts = [LocalCtx, ModuleCtx, GlobalCtx]
      site = fromFiniteLattice scopeLattice
  supportFamilyValue <-
    either
      (assertFailure . ("invalid support family diagnostics fixture: " <>) . show)
      pure
      ( SheafTwist.supportedRuleBook
          site
          [ SheafTwist.SupportedRuleSpec
              { SheafTwist.srsSupport = principalSupport (clBottom scopeLattice),
                SheafTwist.srsRule = addZeroRightRule
              },
            SheafTwist.SupportedRuleSpec
              { SheafTwist.srsSupport = principalSupport (clTop scopeLattice),
                SheafTwist.srsRule = commuteAddRule
              }
          ]
      )
  let diagnostics =
        supportFamilyDiagnostics site contexts supportFamilyValue
  assertEqual
    "diagnostics should distinguish bottom-global support from top-local support"
    SupportFamilyDiagnostics
      { sfdCachedContextCount = 3,
        sfdSupportedRuleCount = 2,
        sfdCompiledRuleEntryCount = 4,
        sfdMaxRuleContextWidth = 3,
        sfdGlobalSupportedRuleCount = 1
      }
    diagnostics

testSupportBackoffSchedulerSuppressesDuplicateMatches :: Assertion
testSupportBackoffSchedulerSuppressesDuplicateMatches = do
  case
    ( supportTraceSummaryForScheduler deterministicSchedulerConfig,
      supportTraceSummaryForScheduler (traceAllSchedulerConfig (backoffSchedulerConfig (backoffConfig 1 2)))
    ) of
    (Left deterministicFailure, _) ->
      assertFailure deterministicFailure
    (_, Left backoffFailure) ->
      assertFailure backoffFailure
    (Right deterministicTrace, Right backoffTrace) -> do
      assertEqual
        "deterministic scheduling should not suppress supported matches"
        0
        (supportSuppressedCount deterministicTrace)
      assertBool
        ( "backoff scheduling should suppress at least one supported match"
            <> " (trace="
            <> show backoffTrace
            <> ")"
        )
        (supportSuppressedCount backoffTrace > 0)

baseFixture :: Either UnionFindAllocationError (ClassId, ClassId, EGraph ArithF NodeCount)
baseFixture =
  addTerm (numTerm 1) (emptyEGraph analysisSpec) >>= \(oneClassId, graph1) ->
    addTerm (numTerm 0) graph1 >>= \(_, graph2) ->
      fmap
        (\(sumClassId, graph3) -> (oneClassId, sumClassId, graph3))
        (addTerm (addTermNode (numTerm 1) (numTerm 0)) graph2)

data EffectFixtureError
  = EffectFixtureAllocationFailed UnionFindAllocationError
  | EffectFixtureContextFailed (ContextDeltaError ArithF Scope)
  deriving stock (Show)

withBaseFixture :: ((ClassId, ClassId, EGraph ArithF NodeCount) -> Bool) -> Bool
withBaseFixture check =
  either (const False) check baseFixture

withBaseFixtureProperty :: ((ClassId, ClassId, EGraph ArithF NodeCount) -> QC.Property) -> QC.Property
withBaseFixtureProperty check =
  either
    (\allocationError -> QC.counterexample ("base fixture allocation failed: " <> show allocationError) False)
    check
    baseFixture

contextFixture :: Either EffectFixtureError (ClassId, ClassId, ContextEGraph ArithF NodeCount Scope)
contextFixture =
  first EffectFixtureAllocationFailed baseFixture >>= \(oneClassId, sumClassId, graph) ->
    fmap
      (\contextGraph -> (sumClassId, oneClassId, contextGraph))
      (first EffectFixtureContextFailed (contextMerge ModuleCtx sumClassId oneClassId (emptyContextEGraph scopeLattice graph)))

globalContextFixture :: Either EffectFixtureError (ContextEGraph ArithF NodeCount Scope)
globalContextFixture =
  contextFixture >>= \(sumClassId, oneClassId, contextGraph) ->
    first EffectFixtureContextFailed (globalMerge sumClassId oneClassId contextGraph)

proofFixture :: Either EffectFixtureError (ProofEGraph ArithF NodeCount Scope ())
proofFixture =
  contextFixture >>= \(sumClassId, oneClassId, contextGraph) ->
    fmap
      ( \mergedContextGraph ->
          recordProofStepWith
            (canonicalizeClassId (cegBase mergedContextGraph))
            (defaultProofStepInput (RewriteRuleId 0) sumClassId oneClassId emptySubstitution ())
            (emptyProofEGraph mergedContextGraph)
      )
      (first EffectFixtureContextFailed (globalMerge sumClassId oneClassId contextGraph))

withContextFixture :: ((ClassId, ClassId, ContextEGraph ArithF NodeCount Scope) -> Bool) -> Bool
withContextFixture check =
  either (const False) check contextFixture

withGlobalContextFixture :: (ContextEGraph ArithF NodeCount Scope -> Bool) -> Bool
withGlobalContextFixture check =
  either (const False) check globalContextFixture

defaultBudget :: SaturationBudget
defaultBudget =
  SaturationBudget
    { sbMaxIterations = testBudgetMaxIterations canonicalTestBudget,
      sbMaxNodes = testBudgetMaxNodes canonicalTestBudget
    }

commuteAddRule :: RawRewriteRule (RewriteCondition SurfaceKind ArithF) ArithF
commuteAddRule =
  arithRewriteFixture (RewriteRuleId 1) CommuteAddFixture

supportBasisFromScopes :: [Scope] -> Either (ContextLatticeLookupError Scope) (SupportBasis Scope)
supportBasisFromScopes =
  supportBasis scopeLattice

supportContextSet :: SupportBasis Scope -> Either (ContextLatticeLookupError Scope) (Set.Set Scope)
supportContextSet =
  fmap Set.fromList . supportReachableContexts scopeLattice (contextLatticeElements scopeLattice)

restrictedSupportContextSet :: Scope -> SupportBasis Scope -> Either (ContextLatticeLookupError Scope) (Set.Set Scope)
restrictedSupportContextSet contextValue supportValue =
  supportMeet scopeLattice supportValue (principalSupport contextValue)
    >>= supportContextSet

checkedSupportLaw :: Either (ContextLatticeLookupError Scope) Bool -> Bool
checkedSupportLaw =
  either (const False) id

supportUnionIdempotent :: [Scope] -> Bool
supportUnionIdempotent rawSupport =
  checkedSupportLaw $ do
    supportValue <- supportBasisFromScopes rawSupport
    unionSupport <- supportUnion scopeLattice supportValue supportValue
    pure (unionSupport == supportValue)

supportUnionCommutative :: [Scope] -> [Scope] -> Bool
supportUnionCommutative leftRawSupport rightRawSupport =
  checkedSupportLaw $ do
    leftSupport <- supportBasisFromScopes leftRawSupport
    rightSupport <- supportBasisFromScopes rightRawSupport
    leftUnion <- supportUnion scopeLattice leftSupport rightSupport
    rightUnion <- supportUnion scopeLattice rightSupport leftSupport
    pure (leftUnion == rightUnion)

supportUnionAssociative :: [Scope] -> [Scope] -> [Scope] -> Bool
supportUnionAssociative firstRawSupport secondRawSupport thirdRawSupport =
  checkedSupportLaw $ do
    firstSupport <- supportBasisFromScopes firstRawSupport
    secondSupport <- supportBasisFromScopes secondRawSupport
    thirdSupport <- supportBasisFromScopes thirdRawSupport
    secondThirdSupport <- supportUnion scopeLattice secondSupport thirdSupport
    firstSecondSupport <- supportUnion scopeLattice firstSupport secondSupport
    leftAssociated <- supportUnion scopeLattice firstSupport secondThirdSupport
    rightAssociated <- supportUnion scopeLattice firstSecondSupport thirdSupport
    pure (leftAssociated == rightAssociated)

supportMeetIntersection :: [Scope] -> [Scope] -> Bool
supportMeetIntersection leftRawSupport rightRawSupport =
  checkedSupportLaw $ do
    leftSupport <- supportBasisFromScopes leftRawSupport
    rightSupport <- supportBasisFromScopes rightRawSupport
    meetSupport <- supportMeet scopeLattice leftSupport rightSupport
    meetContexts <- supportContextSet meetSupport
    leftContexts <- supportContextSet leftSupport
    rightContexts <- supportContextSet rightSupport
    pure (meetContexts == Set.intersection leftContexts rightContexts)

supportRestrictionDistributive :: Scope -> [Scope] -> Bool
supportRestrictionDistributive contextValue rawSupport =
  checkedSupportLaw $ do
    supportValue <- supportBasisFromScopes rawSupport
    restrictedContexts <- restrictedSupportContextSet contextValue supportValue
    supportContexts <- supportContextSet supportValue
    filteredContexts <-
      fmap Set.fromList $
        filterM
          (supportContains scopeLattice (principalSupport contextValue))
          (Set.toList supportContexts)
    pure (restrictedContexts == filteredContexts)

supportSaturationOrderInvariant :: [Scope] -> [Scope] -> QC.Property
supportSaturationOrderInvariant zeroRawSupport commuteRawSupport =
  case (supportBasisFromScopes zeroRawSupport, supportBasisFromScopes commuteRawSupport) of
    (Right zeroSupport, Right commuteSupport) ->
      withBaseFixtureProperty $ \(_, targetClass, graph) ->
      let
          proofGraph0 :: SaturatingProofEGraph SurfaceKind ArithF NodeCount Scope ()
          proofGraph0 = emptySaturatingProofEGraph (emptyContextEGraph scopeLattice graph)
          site = fromFiniteLattice scopeLattice
          saturationConfig :: SaturationConfig (EGraphU SurfaceKind ArithF NodeCount Scope) RewriteRuleId
          saturationConfig =
            SaturationConfig
              { scBudget = defaultBudget,
                scMatchingStrategy = GenericJoinMatching,
                scSchedulerConfig = traceAllSchedulerConfig deterministicSchedulerConfig
              }
          forwardFamily =
            either
              (error . ("invalid forward support family fixture: " <>) . show)
              id
              ( SheafTwist.supportedRuleBook
                  site
                  [ SheafTwist.SupportedRuleSpec {SheafTwist.srsSupport = zeroSupport, SheafTwist.srsRule = addZeroRightRule},
                    SheafTwist.SupportedRuleSpec {SheafTwist.srsSupport = commuteSupport, SheafTwist.srsRule = commuteAddRule}
                  ]
              )
          reverseFamily =
            either
              (error . ("invalid reverse support family fixture: " <>) . show)
              id
              ( SheafTwist.supportedRuleBook
                  site
                  [ SheafTwist.SupportedRuleSpec {SheafTwist.srsSupport = commuteSupport, SheafTwist.srsRule = commuteAddRule},
                    SheafTwist.SupportedRuleSpec {SheafTwist.srsSupport = zeroSupport, SheafTwist.srsRule = addZeroRightRule}
                  ]
              )
          forwardResult =
            first show
              (runArithSupportSaturation saturationConfig forwardFamily proofGraph0)
              >>= first show . supportOutcomeSummary targetClass
          reverseResult =
            first show
              (runArithSupportSaturation saturationConfig reverseFamily proofGraph0)
              >>= first show . supportOutcomeSummary targetClass
       in QC.counterexample
            ("forward=" <> show forwardResult <> "\nreverse=" <> show reverseResult)
            (forwardResult QC.=== reverseResult)
    (leftSupportResult, rightSupportResult) ->
      QC.counterexample
        ( "invalid support fixtures: "
            <> show leftSupportResult
            <> " / "
            <> show rightSupportResult
        )
        False

runArithSupportSaturation ::
  SaturationConfig (EGraphU SurfaceKind ArithF NodeCount Scope) RewriteRuleId ->
  SheafTwist.SupportedRuleBook Scope (RawRewriteRule (RewriteCondition SurfaceKind ArithF) ArithF) ->
  SaturatingProofEGraph SurfaceKind ArithF NodeCount Scope () ->
  Either
    (SaturationError (EGraphU SurfaceKind ArithF NodeCount Scope) (SupportScheduleGroup (EGraphU SurfaceKind ArithF NodeCount Scope)))
    (SupportSaturationReportFor (EGraphU SurfaceKind ArithF NodeCount Scope) (SaturatingProofEGraph SurfaceKind ArithF NodeCount Scope ()))
runArithSupportSaturation saturationConfig supportFamilyValue initialProofGraph = do
  planValue <-
    prepareEGraphSupportPlan
      Nothing
      (const (staticRewriteContextSnapshot emptyRewriteRuntimeCapabilities))
      saturationConfig
      supportFamilyValue
      mempty
      initialProofGraph
  crrResult
    <$> runEGraphSupportPlan
      defaultProofAnnotationBuilder
      mempty
      planValue
      initialProofGraph

supportOutcomeSummary ::
  ClassId ->
  SupportSaturationReportFor
    (EGraphU SurfaceKind ArithF NodeCount Scope)
    (SaturatingProofEGraph SurfaceKind ArithF NodeCount Scope ()) ->
  Either
    (Saturation.ContextualExtractionObstruction Scope)
    ( SaturationTermination,
      Int,
      Int,
      [(RewriteRuleId, [Scope], Int, Int, Int, Bool)],
      [(String, Set.Set Scope)],
      ProofCompressionSummary
    )
supportOutcomeSummary targetClass supportReport = do
  partitions <-
    Saturation.contextualExtractionPartitionsBounded
      supportExtractionBudget
      Saturation.CachedObjects
      mempty
      arithCost
      targetClass
      (sceContextGraph (pgGraph proofGraph))
  pure
    ( srResult supportReport,
      srIterations supportReport,
      srMatchesApplied supportReport,
      fmap supportTraceEntrySummary (supportReportScheduleTrace supportReport),
      fmap
        ( \partitionValue ->
            ( renderTerm (SheafTwist.cepResult partitionValue),
              SheafTwist.cepContexts partitionValue
            )
        )
        partitions,
      summarizeProofLog proofGraph
    )
  where
    proofGraph =
      srCarrier supportReport

supportExtractionBudget :: ExtractionFixpointBudget
supportExtractionBudget = ExtractionFixpointBudget 4096

supportTraceEntrySummary ::
  ScheduleTrace (ScheduleGroup RewriteRuleId (SupportBasis Scope)) ->
  (RewriteRuleId, [Scope], Int, Int, Int, Bool)
supportTraceEntrySummary traceEntry =
  ( stvRuleId scheduleTraceSupportView traceEntry,
    maybe [] supportGenerators (stvSupport scheduleTraceSupportView traceEntry),
    workCountLowerBoundToBoundedInt (stvMatchedCount scheduleTraceSupportView traceEntry),
    naturalToBoundedInt (stvScheduledCount scheduleTraceSupportView traceEntry),
    workCountLowerBoundToBoundedInt (stvSuppressedCount scheduleTraceSupportView traceEntry),
    stvSuppressedByCooldown scheduleTraceSupportView traceEntry
  )

supportTraceSummaryForScheduler ::
  SchedulerConfig RewriteRuleId ->
  Either String [(RewriteRuleId, [Scope], Int, Int, Int, Bool)]
supportTraceSummaryForScheduler schedulerConfig = do
  supportValue <- first show (supportBasisFromScopes [LocalCtx, ModuleCtx, GlobalCtx])
  (_, graph1) <- first show (addTerm (numTerm 1) (emptyEGraph analysisSpec))
  (_, graph2) <- first show (addTerm (numTerm 2) graph1)
  (_, graph3) <- first show (addTerm (numTerm 0) graph2)
  (_, graph4) <- first show (addTerm (addTermNode (numTerm 1) (numTerm 0)) graph3)
  (targetClass, graph5) <- first show (addTerm (addTermNode (numTerm 2) (numTerm 0)) graph4)
  let proofGraph0 :: SaturatingProofEGraph SurfaceKind ArithF NodeCount Scope ()
      proofGraph0 = emptySaturatingProofEGraph (emptyContextEGraph scopeLattice graph5)
      site = fromFiniteLattice scopeLattice
  supportFamilyValue <-
    first
      (("invalid scheduler support family fixture: " <>) . show)
      ( SheafTwist.supportedRuleBook
          site
          [ SheafTwist.SupportedRuleSpec
              { SheafTwist.srsSupport = supportValue,
                SheafTwist.srsRule = addZeroRightRule
              }
          ]
      )
  let saturationConfig =
        SaturationConfig
          { scBudget = defaultBudget,
            scMatchingStrategy = GenericJoinMatching,
            scSchedulerConfig = schedulerConfig
          }
  supportReport <-
    first show
      (runArithSupportSaturation saturationConfig supportFamilyValue proofGraph0)
  (_, _, _, traceEntries, _, _) <-
    first show (supportOutcomeSummary targetClass supportReport)
  pure traceEntries

supportSuppressedCount :: [(RewriteRuleId, [Scope], Int, Int, Int, Bool)] -> Int
supportSuppressedCount =
  sum . fmap (\(_, _, _, _, suppressedCount, _) -> suppressedCount)

renderTerm :: ExtractionResult ArithF cost -> String
renderTerm =
  renderFix . erTerm

renderFix :: Fix ArithF -> String
renderFix term =
  case term of
    Fix (Num number) -> show number
    Fix (Var index) -> "x" <> show index
    Fix (Add leftTerm rightTerm) -> "(" <> renderFix leftTerm <> "+" <> renderFix rightTerm <> ")"
    Fix (Mul leftTerm rightTerm) -> "(" <> renderFix leftTerm <> "*" <> renderFix rightTerm <> ")"
    Fix (Neg childTerm) -> "(-" <> renderFix childTerm <> ")"
