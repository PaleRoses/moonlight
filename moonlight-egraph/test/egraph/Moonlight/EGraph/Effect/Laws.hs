{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Moonlight.EGraph.Effect.Laws
  ( tests,
  )
where

import Control.Monad (filterM)
import Data.Bifunctor ( first )
import Data.Maybe (isJust)
import Moonlight.EGraph.Effect.CoveringSurface
  ( SurfaceKind,
  )
import Moonlight.EGraph.Effect.Harness
    ( analysisJoinAssociative,
      analysisJoinCommutative,
      contextGlobalSection,
      contextGlobalSectionInvariantLaw,
      contextMergeMonotone,
      contextMorphismAssociativeLaw,
      contextMorphismLeftIdentityLaw,
      contextMorphismRightIdentityLaw,
      contextRestrictionComposition,
      contextRestrictionFunctorialActionLaw,
      contextRestrictionIdentityLaw,
      extractInClass,
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
                    ContextGlobalSectionInvariant, ContextMergeMonotone,
                    ProofSoundness,
                    ProofContextConsistency, AntiUnifyGeneralizes, AntiUnifyLeast,
                    ObstructionComplete, SupportUnionIdempotent,
                    SupportUnionCommutative, SupportUnionAssociative,
                    SupportMeetIntersection, SupportRestrictionDistributive,
                    SupportSaturationOrderInvariant, AnalysisJoinCommutative) )
import Moonlight.EGraph.Effect.LawNames (eGraphLawName)
import Moonlight.EGraph.Pure.Context
    ( contextMerge, withEmptyContextEGraph, globalMerge, ContextEGraph )
import Moonlight.EGraph.Pure.Context (cegBase, cegSite)
import Moonlight.EGraph.Pure.Extraction
    ( ExtractionResult(..), ExtractionWorkBudget(..), depthCost, extract,
      stableExtractionSnapshotFromEGraph )
import Moonlight.EGraph.Pure.AntiUnify
    ( BinaryLGGResult(binaryLggPattern), antiUnify )
import Moonlight.EGraph.Pure.Change (EGraphMutationResult(emrGraph, emrResult))
import Moonlight.EGraph.Pure.Kernel.HashCons ( addTerm, insertTermsTracked )
import Moonlight.EGraph.Pure.Context.Proof
    ( ProofEGraph,
      ProofGraph(pgGraph),
      emptyProofEGraph,
      recordProofStepWith,
      summarizeProofLog )
import Moonlight.EGraph.Pure.Rebuild
    ( equateClassesTracked, rebuildTracked )
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
    ( ArithF(..), NodeCount(..), addTermNode, analysisSpec, negTermNode,
      numTerm )
import Moonlight.EGraph.Test.Arith.Cost ( arithCost )
import Moonlight.EGraph.Test.Arith.Fixture (seedArithPair)
import Moonlight.EGraph.Test.Arith.Matcher ()
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
import Moonlight.Core
    ( Pattern(..), UnionFindAllocationError, emptySubstitution, mkPatternVar )
import Moonlight.Core.Pattern.Automata
    ( compilePatternAutomaton, matchesPatternAutomaton )
import Moonlight.Rewrite.Algebra (matchPattern)
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
    ( withPreparedContextSiteFromFiniteLattice )
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
import Test.Tasty.QuickCheck qualified as QC
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

generatedEffectLawBundle :: QuickCheckLawBundle String EGraphLawName
generatedEffectLawBundle =
  quickCheckLawBundle
    "generated-effect-laws"
    [ quickCheckLawDefinition ExtractDeterministic extractionOrderDeterministic,
      quickCheckLawDefinition AntiUnifyGeneralizes generatedAntiUnifyGeneralizes,
      quickCheckLawDefinition AntiUnifyLeast generatedAntiUnifyLeast,
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
    [ fixedEffectLawTests,
      localOption
        (QC.QuickCheckTests 20)
        (lawSuiteGroup "egraph-effect-laws" [quickCheckLawBundleGroup "egraph" id [generatedEffectLawBundle]]),
      supportDiagnosticsTests
    ]

fixedEffectLawTests :: TestTree
fixedEffectLawTests =
  testGroup
    "fixed-effect-laws"
    [ fixedLawCase FindIdempotent
        (withBaseFixture $ \(oneClassId, _, graph) -> findIdempotent oneClassId graph),
      fixedLawCase MergeCommutative
        (withBaseFixture $ \(oneClassId, sumClassId, graph) -> mergeCommutative oneClassId sumClassId graph),
      fixedLawCase HashConsIdempotent
        (withBaseFixture $ \(_, _, graph) -> either (const False) id (hashConsIdempotent (addTermNode (numTerm 1) (numTerm 0)) graph)),
      fixedLawCase RebuildIdempotent
        (withBaseFixture $ \(_, _, graph) -> rebuildIdempotent graph),
      fixedLawCase SaturationBounded
        (withBaseFixture $ \(_, _, graph) -> saturationBounded defaultBudget [addZeroRightRule] graph),
      fixedLawCase ExtractInClass
        (withBaseFixture $ \(_, sumClassId, graph) -> extractInClass arithCost sumClassId graph),
      testCase (eGraphLawName ExtractOptimal) testExactExtractionOptimum,
      fixedLawCase ContextRestrictionComposition
        (withContextFixture $ \(_, _, contextGraph) -> contextRestrictionComposition LocalCtx ModuleCtx GlobalCtx contextGraph),
      fixedLawCase ContextMergeMonotone
        (withContextFixture $ \(sumClassId, oneClassId, contextGraph) -> contextMergeMonotone ModuleCtx sumClassId oneClassId contextGraph),
      fixedLawCase ContextRestrictionIdentity
        (withContextFixture $ \(_, _, contextGraph) -> contextRestrictionIdentityLaw ModuleCtx contextGraph),
      fixedLawCase ContextMorphismLeftIdentity
        (withContextFixture $ \(_, _, contextGraph) -> contextMorphismLeftIdentityLaw ModuleCtx contextGraph),
      fixedLawCase ContextMorphismRightIdentity
        (withContextFixture $ \(_, _, contextGraph) -> contextMorphismRightIdentityLaw ModuleCtx contextGraph),
      fixedLawCase ContextMorphismAssociative
        (withContextFixture $ \(_, _, contextGraph) -> contextMorphismAssociativeLaw LocalCtx ModuleCtx GlobalCtx GlobalCtx contextGraph),
      fixedLawCase ContextRestrictionFunctorialAction
        (withContextFixture $ \(_, _, contextGraph) -> contextRestrictionFunctorialActionLaw LocalCtx ModuleCtx GlobalCtx contextGraph),
      fixedLawCase ContextGlobalSection
        (withContextFixture $ \(sumClassId, oneClassId, contextGraph) -> contextGlobalSection sumClassId oneClassId contextGraph),
      fixedLawCase ContextGlobalSectionInvariant
        (withGlobalContextFixture $ \contextGraph -> contextGlobalSectionInvariantLaw LocalCtx GlobalCtx contextGraph),
      fixedLawCase ProofSoundness
        (withProofFixture (proofSoundness cegBase)),
      fixedLawCase ProofContextConsistency
        (withProofFixture (proofContextConsistency id)),
      fixedLawCase ObstructionComplete
        (withContextFixture $ \(sumClassId, oneClassId, contextGraph) -> obstructionComplete sumClassId oneClassId GlobalCtx contextGraph)
    ]

fixedLawCase :: EGraphLawName -> Bool -> TestTree
fixedLawCase lawNameValue lawHolds =
  testCase (eGraphLawName lawNameValue) $
    assertBool (eGraphLawName lawNameValue <> " failed") lawHolds

testExactExtractionOptimum :: Assertion
testExactExtractionOptimum =
  case exactExtractionFixture of
    Left allocationError ->
      assertFailure ("exact extraction fixture allocation failed: " <> show allocationError)
    Right (targetClass, graph) ->
      case stableExtractionSnapshotFromEGraph graph >>= extract arithCost targetClass of
        Nothing ->
          assertFailure "expected the stable target class to be extractable"
        Just extractionResult -> do
          assertEqual "the independent optimum is the one-node literal" (numTerm 1) (erTerm extractionResult)
          assertEqual "the independent optimum has cost one" 1 (erCost extractionResult)

exactExtractionFixture :: Either UnionFindAllocationError (ClassId, EGraph ArithF NodeCount)
exactExtractionFixture = do
  (oneClass, sumClass, graph) <- baseFixture
  let mergedGraph = emrGraph (equateClassesTracked oneClass sumClass graph)
  pure (oneClass, emrGraph (rebuildTracked mergedGraph))

data ExtractionOrderOutcome = ExtractionOrderOutcome
  { eooTerm :: !(Fix ArithF),
    eooCost :: !Int,
    eooClassIsCanonicalTarget :: !Bool
  }
  deriving stock (Eq, Show)

extractionOrderDeterministic :: Int -> Int -> QC.Property
extractionOrderDeterministic rawSeed rawAlternativeCount =
  let seed = rawSeed `mod` 17
      alternativeCount = 2 + rawAlternativeCount `mod` 5
      alternatives = orderedAlternativeTerms seed alternativeCount
      forwardResult = buildExtractionOrderOutcome False alternatives
      reverseResult = buildExtractionOrderOutcome True (reverse alternatives)
   in case (forwardResult, reverseResult) of
        (Right (Just forwardOutcome), Right (Just reverseOutcome)) ->
          QC.counterexample
            ( "forward="
                <> show forwardOutcome
                <> "\nreverse="
                <> show reverseOutcome
            )
            ( QC.conjoin
                [ forwardOutcome QC.=== reverseOutcome,
                  eooClassIsCanonicalTarget forwardOutcome QC.=== True,
                  eooClassIsCanonicalTarget reverseOutcome QC.=== True
                ]
            )
        _ ->
          QC.counterexample
            ( "forward="
                <> show forwardResult
                <> "\nreverse="
                <> show reverseResult
            )
            False

orderedAlternativeTerms :: Int -> Int -> [Fix ArithF]
orderedAlternativeTerms seed alternativeCount =
  zipWith
    (\depth literalValue -> applyNegations depth (numTerm literalValue))
    [0 .. alternativeCount - 1]
    [seed ..]

applyNegations :: Int -> Fix ArithF -> Fix ArithF
applyNegations depth termValue =
  foldl' (\currentTerm _ -> negTermNode currentTerm) termValue [1 .. depth]

buildExtractionOrderOutcome ::
  Bool ->
  [Fix ArithF] ->
  Either UnionFindAllocationError (Maybe ExtractionOrderOutcome)
buildExtractionOrderOutcome rebuildAfterEachMerge constructionTerms = do
  insertion <- insertTermsTracked constructionTerms (emptyEGraph analysisSpec)
  let classIds = emrResult insertion
      insertedGraph = emrGraph insertion
  case constructionTerms of
    [] ->
      pure Nothing
    targetTerm : _ -> do
      (targetClass, graphWithTarget) <- addTerm targetTerm insertedGraph
      let mergeStep graph classId =
            let mergedGraph = emrGraph (equateClassesTracked targetClass classId graph)
             in if rebuildAfterEachMerge
                  then emrGraph (rebuildTracked mergedGraph)
                  else mergedGraph
          stableGraph =
            emrGraph
              ( rebuildTracked
                  (foldl' mergeStep graphWithTarget (filter (/= targetClass) classIds))
              )
      pure $ do
        snapshot <- stableExtractionSnapshotFromEGraph stableGraph
        extractionResult <- extract arithCost targetClass snapshot
        pure
          ExtractionOrderOutcome
            { eooTerm = erTerm extractionResult,
              eooCost = erCost extractionResult,
              eooClassIsCanonicalTarget =
                erClass extractionResult == canonicalizeClassId stableGraph targetClass
            }

generatedAntiUnifyGeneralizes :: QC.Property
generatedAntiUnifyGeneralizes =
  withGeneratedAntiUnify $ \leftTerm rightTerm lggPattern ->
    QC.counterexample
      ("lgg=" <> show lggPattern <> "\nleft=" <> show leftTerm <> "\nright=" <> show rightTerm)
      ( patternAcceptsTerm lggPattern leftTerm
          QC..&&. patternAcceptsTerm lggPattern rightTerm
      )

generatedAntiUnifyLeast :: QC.Property
generatedAntiUnifyLeast =
  withGeneratedAntiUnify $ \leftTerm rightTerm lggPattern ->
    QC.forAllBlind (genCommonGeneralizer leftTerm rightTerm) $ \candidatePattern ->
      let candidateIsCommon =
            patternAcceptsTerm candidatePattern leftTerm
              && patternAcceptsTerm candidatePattern rightTerm
          lggIsInstanceOfCandidate =
            isJust (matchPattern candidatePattern lggPattern)
       in QC.counterexample
            ( "candidate="
                <> show candidatePattern
                <> "\nlgg="
                <> show lggPattern
                <> "\nleft="
                <> show leftTerm
                <> "\nright="
                <> show rightTerm
            )
            (candidateIsCommon QC..&&. lggIsInstanceOfCandidate)

withGeneratedAntiUnify :: (Fix ArithF -> Fix ArithF -> Pattern ArithF -> QC.Property) -> QC.Property
withGeneratedAntiUnify check =
  QC.forAllBlind genDistinctArithTerms $ \(leftTerm, rightTerm) ->
    case seedArithPair leftTerm rightTerm of
      Left allocationError ->
        QC.counterexample ("anti-unification fixture allocation failed: " <> show allocationError) False
      Right (leftClass, rightClass, graph) ->
        case antiUnify depthCost leftClass rightClass graph of
          Left obstruction ->
            QC.counterexample ("anti-unification failed: " <> show obstruction) False
          Right lggResult ->
            check leftTerm rightTerm (binaryLggPattern lggResult)

patternAcceptsTerm :: Pattern ArithF -> Fix ArithF -> Bool
patternAcceptsTerm patternValue termValue =
  matchesPatternAutomaton (compilePatternAutomaton patternValue) termValue

genDistinctArithTerms :: QC.Gen (Fix ArithF, Fix ArithF)
genDistinctArithTerms = do
  leftTerm <- QC.resize 8 QC.arbitrary
  rightTerm <- QC.resize 8 (QC.arbitrary `QC.suchThat` (/= leftTerm))
  pure (leftTerm, rightTerm)

genCommonGeneralizer :: Fix ArithF -> Fix ArithF -> QC.Gen (Pattern ArithF)
genCommonGeneralizer =
  genCommonGeneralizerAt 0

genCommonGeneralizerAt :: Int -> Fix ArithF -> Fix ArithF -> QC.Gen (Pattern ArithF)
genCommonGeneralizerAt variableKey (Fix leftNode) (Fix rightNode) =
  let abstractHere =
        pure (PatternVar (mkPatternVar variableKey))
      preserve nodeGenerator =
        QC.frequency [(1, abstractHere), (3, PatternNode <$> nodeGenerator)]
      leftChildKey =
        2 * variableKey + 1
      rightChildKey =
        2 * variableKey + 2
   in case (leftNode, rightNode) of
        (Num leftValue, Num rightValue)
          | leftValue == rightValue ->
              preserve (pure (Num leftValue))
        (Var leftIndex, Var rightIndex)
          | leftIndex == rightIndex ->
              preserve (pure (Var leftIndex))
        (Add leftA leftB, Add rightA rightB) ->
          preserve
            ( Add
                <$> genCommonGeneralizerAt leftChildKey leftA rightA
                <*> genCommonGeneralizerAt rightChildKey leftB rightB
            )
        (Mul leftA leftB, Mul rightA rightB) ->
          preserve
            ( Mul
                <$> genCommonGeneralizerAt leftChildKey leftA rightA
                <*> genCommonGeneralizerAt rightChildKey leftB rightB
            )
        (Neg leftChild, Neg rightChild) ->
          preserve (Neg <$> genCommonGeneralizerAt leftChildKey leftChild rightChild)
        _ ->
          abstractHere

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
  withPreparedContextSiteFromFiniteLattice scopeLattice $ \site -> do
    let contexts = [LocalCtx, ModuleCtx, GlobalCtx]
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

withBaseFixture :: ((ClassId, ClassId, EGraph ArithF NodeCount) -> Bool) -> Bool
withBaseFixture check =
  either (const False) check baseFixture

withBaseFixtureProperty :: ((ClassId, ClassId, EGraph ArithF NodeCount) -> QC.Property) -> QC.Property
withBaseFixtureProperty check =
  either
    (\allocationError -> QC.counterexample ("base fixture allocation failed: " <> show allocationError) False)
    check
    baseFixture

withContextFixture ::
  (forall owner. (ClassId, ClassId, ContextEGraph owner ArithF NodeCount Scope) -> Bool) ->
  Bool
withContextFixture check =
  either
    (const False)
    ( \(oneClassId, sumClassId, graph) ->
        withEmptyContextEGraph scopeLattice graph $ \emptyContextGraph ->
          either
            (const False)
            (\contextGraph -> check (sumClassId, oneClassId, contextGraph))
            (contextMerge ModuleCtx sumClassId oneClassId emptyContextGraph)
    )
    baseFixture

withGlobalContextFixture ::
  (forall owner. ContextEGraph owner ArithF NodeCount Scope -> Bool) ->
  Bool
withGlobalContextFixture check =
  withContextFixture $ \(sumClassId, oneClassId, contextGraph) ->
    either
      (const False)
      check
      (globalMerge sumClassId oneClassId contextGraph)

withProofFixture ::
  (forall owner. ProofEGraph owner ArithF NodeCount Scope () -> Bool) ->
  Bool
withProofFixture check =
  withContextFixture $ \(sumClassId, oneClassId, contextGraph) ->
    either
      (const False)
      ( \mergedContextGraph ->
          check
            ( recordProofStepWith
                (canonicalizeClassId (cegBase mergedContextGraph))
                (defaultProofStepInput (RewriteRuleId 0) sumClassId oneClassId emptySubstitution ())
                (emptyProofEGraph mergedContextGraph)
            )
      )
      (globalMerge sumClassId oneClassId contextGraph)

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
        withEmptyContextEGraph scopeLattice graph $ \contextGraph ->
          let
            proofGraph0 = emptySaturatingProofEGraph contextGraph
            site = cegSite contextGraph
            saturationConfig =
              SaturationConfig
                { scBudget = defaultBudget,
                  scMatchingStrategy = GenericJoinMatching,
                  scSchedulerConfig = traceAllSchedulerConfig deterministicSchedulerConfig
                }
            forwardFamilyResult =
              first
                (("invalid forward support family fixture: " <>) . show)
                ( SheafTwist.supportedRuleBook
                    site
                    [ SheafTwist.SupportedRuleSpec {SheafTwist.srsSupport = zeroSupport, SheafTwist.srsRule = addZeroRightRule},
                      SheafTwist.SupportedRuleSpec {SheafTwist.srsSupport = commuteSupport, SheafTwist.srsRule = commuteAddRule}
                    ]
                )
            reverseFamilyResult =
              first
                (("invalid reverse support family fixture: " <>) . show)
                ( SheafTwist.supportedRuleBook
                    site
                    [ SheafTwist.SupportedRuleSpec {SheafTwist.srsSupport = commuteSupport, SheafTwist.srsRule = commuteAddRule},
                      SheafTwist.SupportedRuleSpec {SheafTwist.srsSupport = zeroSupport, SheafTwist.srsRule = addZeroRightRule}
                    ]
                )
            runFamily supportFamilyValue =
              first show
                (runArithSupportSaturation saturationConfig supportFamilyValue proofGraph0)
                >>= first show . supportOutcomeSummary targetClass
            forwardResult = forwardFamilyResult >>= runFamily
            reverseResult = reverseFamilyResult >>= runFamily
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
  forall owner.
  SaturationConfig (EGraphU owner SurfaceKind ArithF NodeCount Scope) RewriteRuleId ->
  SheafTwist.SupportedRuleBook owner Scope (RawRewriteRule (RewriteCondition SurfaceKind ArithF) ArithF) ->
  SaturatingProofEGraph owner SurfaceKind ArithF NodeCount Scope () ->
  Either
    (SaturationError (EGraphU owner SurfaceKind ArithF NodeCount Scope) (SupportScheduleGroup (EGraphU owner SurfaceKind ArithF NodeCount Scope)))
    (SupportSaturationReportFor (EGraphU owner SurfaceKind ArithF NodeCount Scope) (SaturatingProofEGraph owner SurfaceKind ArithF NodeCount Scope ()))
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
  forall owner.
  ClassId ->
  SupportSaturationReportFor
    (EGraphU owner SurfaceKind ArithF NodeCount Scope)
    (SaturatingProofEGraph owner SurfaceKind ArithF NodeCount Scope ()) ->
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

supportExtractionBudget :: ExtractionWorkBudget
supportExtractionBudget = ExtractionWorkBudget 4096

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
  withEmptyContextEGraph scopeLattice graph5 $ \contextGraph -> do
    let proofGraph0 = emptySaturatingProofEGraph contextGraph
        site = cegSite contextGraph
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
  show . erTerm
