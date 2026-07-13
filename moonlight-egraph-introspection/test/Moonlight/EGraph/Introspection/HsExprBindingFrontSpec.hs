{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.EGraph.Introspection.HsExprBindingFrontSpec
  ( tests,
  )
where

import Data.Foldable (toList)
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Set qualified as Set
import GHC.Types.Name.Occurrence (mkVarOcc, occNameString)
import GHC.Types.Name.Reader (mkRdrUnqual, rdrNameOcc)
import Moonlight.Algebra (JoinSemilattice (..))
import Moonlight.Control.Schedule (identitySchedulerRefinement)
import Moonlight.Core (BinderId (..), Pattern (..), RewriteRuleId (..), binderIdKey)
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( BinderAnn (..),
    ConvertedModule (..),
    GuardedAltF (..),
    HsExprBindingCorpus (..),
    HsExprBindingFactRule,
    HsExprBindingRule,
    HsExprBindingRuleMetrics (..),
    HsExprF (..),
    HsGuardStmtF (..),
    HsPatF (..),
    HsStmtF (..),
    HsVarRef (..),
    ScopeCtx (ActualScope),
    ScopeIndex,
    ScopedExpr (..),
    SurfaceName (..),
    TopLevelBinding (..),
    convertHaskellSource,
    convertedModuleContextLattice,
    hsExprBindingCorpus,
    hsExprBindingRuleIdBase,
    hsExprChildBinderEdges,
    hsExprRuntimeCapabilitiesForContextGraph,
    hsExprCapabilityGenerationForContextGraph,
    hsExprSiteLawFamily,
    hsExprSubstitutionAllowedFactId,
    identityInsertionSeeding,
    insertConvertedModuleWithMetrics,
    matchesHsExprPattern,
    patBinders,
  )
import Moonlight.EGraph.Introspection.Core.HsExpr.FreeScope
  ( FreeScopeWitness,
    HasFreeScopeWitness (..),
    hsExprFreeScopeWitness,
  )
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec, semilatticeAnalysis)
import Moonlight.EGraph.Pure.Context (ContextEGraph, cegSite, emptyContextEGraph)
import Moonlight.EGraph.Pure.Context.Proof (ProofGraph (pgGraph))
import Moonlight.EGraph.Pure.Extraction
  ( CostAlgebra (..),
    ExtractionFixpointBudget (..),
    ExtractionResult (..),
  )
import Moonlight.EGraph.Pure.Saturation.Extraction (contextualExtractBounded)
import Moonlight.EGraph.Pure.Saturation.Guidance (egraphSupportGuidance)
import Moonlight.EGraph.Pure.Saturation.Matching (MatchingStrategy (GenericJoinMatching))
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingProofEGraph,
    emptySaturatingProofEGraph,
    sceContextGraph,
  )
import Moonlight.EGraph.Pure.Types (ClassId, emptyEGraph)
import Data.Fix (Fix (..))
import Moonlight.Rewrite.ProofContext (ProofAnnotationBuilder, defaultProofAnnotationBuilder)
import Moonlight.Rewrite.System (LawBook (..), lawRule)
import Moonlight.Rewrite.System (RawFactRule (..))
import Moonlight.Rewrite.System (RawRewriteRule (..))
import Moonlight.Saturation.Context.Program.Spec
  ( PlanSpec,
    RewriteContextSnapshot (..),
    deterministicSchedulerConfig,
    planSpec,
    withGuidance,
    withRewriteContext,
    withSchedulerConfig,
  )
import Moonlight.Saturation.Context.Driver
  ( carrierGoal,
    contextExecutionSpec,
    crrResult,
  )
import Moonlight.Saturation.Context.Runtime.Report (ReportSummary (..), reportSummary, srCarrier)
import Moonlight.Saturation.Core (SaturationBudget (..), SaturationTermination (..))
import Moonlight.Saturation.Support.Algebra (supportRuntimePolicy)
import Moonlight.Saturation.Support.Compile (compileSupportProgram)
import Moonlight.Saturation.Support.Core (SupportScheduleGroup)
import Moonlight.Saturation.Support.Driver (prepareSupportPlan, runSupportPlan)
import Moonlight.Sheaf.Context.Site (PreparedContextSite, fromFiniteLattice)
import Moonlight.Sheaf.Twist.SupportedRuleSpec
  ( SupportedFactBook,
    SupportedFactSpec (..),
    SupportedRuleBook,
    SupportedRuleSpec (..),
    supportedFactSpecs,
    supportedRuleBook,
    supportedRules,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "hsexpr-binding-front"
    [ plainRedexCase,
      captureCase,
      guardPatternCaptureCase,
      multiIfPatternCaptureCase,
      localPatternCaptureCase,
      clausePatternCaptureCase,
      clauseBinderEdgeCase,
      disjointIdentifierCase,
      unionContractionCase,
      freshenedRuntimeCase
    ]

plainSource :: String
plainSource =
  unlines
    [ "module Nebula.Front.Plain where",
      "plain = (\\x -> wrap x) alpha"
    ]

captureSource :: String
captureSource =
  unlines
    [ "module Nebula.Front.Capture where",
      "capture = (\\x -> \\w -> use x) w"
    ]

guardPatternCaptureSource :: String
guardPatternCaptureSource =
  unlines
    [ "module Nebula.Front.GuardCapture where",
      "guardCapture = (\\x -> case source of { Just z | Just y <- lookupThing z -> use x y; _ -> fallback x }) y"
    ]

multiIfPatternCaptureSource :: String
multiIfPatternCaptureSource =
  unlines
    [ "{-# LANGUAGE MultiWayIf #-}",
      "module Nebula.Front.MultiIfCapture where",
      "multiIfCapture = (\\x -> if | Just y <- lookupThing source -> use x y | otherwise -> fallback x) y"
    ]

localPatternCaptureSource :: String
localPatternCaptureSource =
  unlines
    [ "module Nebula.Front.LocalPatternCapture where",
      "localPatternCapture = (\\x -> let (y, kept) = pairSource in use x y) y"
    ]

clausePatternCaptureSource :: String
clausePatternCaptureSource =
  unlines
    [ "{-# LANGUAGE LambdaCase #-}",
      "module Nebula.Front.ClauseCapture where",
      "clauseCapture = (\\x -> \\case { Just y -> use x y; Nothing -> fallback x }) y"
    ]

shadowSource :: String
shadowSource =
  unlines
    [ "module Nebula.Front.Shadow where",
      "shadow = let g = \\x -> use x x in g alpha"
    ]

plainRedexCase :: TestTree
plainRedexCase =
  testCase "a plain redex yields one guarded ground beta and one substitution fact" $ do
    convertedModule <- parsedFixture "plain" plainSource
    corpus <- expectRight "plain corpus" (bindingCorpus convertedModule)
    hbcMetrics corpus
      @?= HsExprBindingRuleMetrics
        { hbrmRedexSiteCount = 1,
          hbrmAllowedCount = 1,
          hbrmFresheningCount = 0,
          hbrmObstructionCount = 0,
          hbrmGeneratedRuleCount = 1,
          hbrmFactRuleCount = 1
        }
    betaRule <- case supportedRules (hbcRules corpus) of
      [ruleSpec] ->
        pure (srsRule ruleSpec)
      ruleSpecs ->
        assertFailure ("expected exactly one generated rule, saw " <> show (length ruleSpecs))
    rrId betaRule @?= RewriteRuleId hsExprBindingRuleIdBase
    assertBool "beta rule is fact-guarded" (isJust (rrCondition betaRule))
    assertBool "beta rule carries no post-substitution" (isNothing (rrPostSubst betaRule))
    bindingTerm <- singleBindingTerm convertedModule
    assertBool "beta lhs is the redex itself" (matchesHsExprPattern (rrLhs betaRule) bindingTerm)
    assertBool
      "beta rhs is the statically contracted application"
      (matchesHsExprPattern (rrRhs betaRule) (globalApplication "wrap" "alpha"))
    factRule <- singleFactRule corpus
    frFactId factRule @?= hsExprSubstitutionAllowedFactId
    length (frProjection factRule) @?= 1
    assertBool "fact rule is unconditional" (isNothing (frCondition factRule))
    assertBool "fact pattern is the redex itself" (matchesHsExprPattern (frPattern factRule) bindingTerm)

captureCase :: TestTree
captureCase =
  testCase "a capture-threatened redex freshens the binder and guards the freshened beta" $ do
    convertedModule <- parsedFixture "capture" captureSource
    corpus <- expectRight "capture corpus" (bindingCorpus convertedModule)
    hbcMetrics corpus
      @?= HsExprBindingRuleMetrics
        { hbrmRedexSiteCount = 1,
          hbrmAllowedCount = 0,
          hbrmFresheningCount = 1,
          hbrmObstructionCount = 1,
          hbrmGeneratedRuleCount = 2,
          hbrmFactRuleCount = 1
        }
    (alphaRule, betaRule) <-
      case partition (isNothing . rrCondition . srsRule) (supportedRules (hbcRules corpus)) of
        ([alphaSpec], [betaSpec]) ->
          pure (srsRule alphaSpec, srsRule betaSpec)
        _ ->
          assertFailure "expected exactly one unguarded alpha rule and one guarded beta rule"
    Set.fromList [rrId alphaRule, rrId betaRule]
      @?= Set.fromList
        [ RewriteRuleId hsExprBindingRuleIdBase,
          RewriteRuleId (hsExprBindingRuleIdBase + 1)
        ]
    bindingTerm <- singleBindingTerm convertedModule
    assertBool "alpha lhs is the original redex" (matchesHsExprPattern (rrLhs alphaRule) bindingTerm)
    assertBool
      "beta lhs is the freshened redex the alpha rule materializes"
      (matchesHsExprPattern (rrLhs betaRule) (rrRhs alphaRule))
    case rrRhs betaRule of
      PatternNode (LamF freshAnn (PatternNode (AppF (PatternNode (VarF (GlobalName functionName))) (PatternNode (VarF (GlobalName argumentName)))))) -> do
        occNameString (rdrNameOcc (baName freshAnn)) @?= "w0"
        occNameString (rdrNameOcc functionName) @?= "use"
        occNameString (rdrNameOcc argumentName) @?= "w"
        assertBool
          "freshened binder mints an identifier unseen in the original redex"
          (binderIdKey (baId freshAnn) `notElem` patternLamBinderKeys (rrLhs alphaRule))
      _ ->
        assertFailure "freshened beta rhs is not a lambda over the contracted body"
    factRule <- singleFactRule corpus
    frFactId factRule @?= hsExprSubstitutionAllowedFactId
    assertBool
      "fact pattern is the original redex, derivable before the alpha rule fires"
      (matchesHsExprPattern (frPattern factRule) bindingTerm)

guardPatternCaptureCase :: TestTree
guardPatternCaptureCase =
  testCase "a pattern guard that would capture the beta argument forces alpha then guarded beta" $ do
    convertedModule <- parsedFixture "guard-capture" guardPatternCaptureSource
    corpus <- expectRight "guard-capture corpus" (bindingCorpus convertedModule)
    assertBool
      "guard pattern capture must force freshening"
      (hbrmFresheningCount (hbcMetrics corpus) > 0)
    assertBool
      "guard pattern capture must prevent direct substitution"
      (hbrmAllowedCount (hbcMetrics corpus) == 0)
    (alphaRule, betaRule) <-
      case partition (isNothing . rrCondition . srsRule) (supportedRules (hbcRules corpus)) of
        ([alphaSpec], [betaSpec]) ->
          pure (srsRule alphaSpec, srsRule betaSpec)
        _ ->
          assertFailure "expected exactly one unguarded alpha rule and one guarded beta rule"
    bindingTerm <- singleBindingTerm convertedModule
    assertBool "alpha lhs is the original guarded redex" (matchesHsExprPattern (rrLhs alphaRule) bindingTerm)
    assertBool
      "beta lhs is the freshened guarded redex the alpha rule materializes"
      (matchesHsExprPattern (rrLhs betaRule) (rrRhs alphaRule))
    let guardBinderNames = patternGuardBinderNames (rrRhs betaRule)
    assertBool
      "the guard pattern binder is alpha-renamed away from the beta argument"
      (not (null guardBinderNames) && all (/= "y") guardBinderNames)
    assertBool
      "the beta argument remains the free y occurrence after contraction"
      ("y" `elem` patternGlobalNames (rrRhs betaRule))

multiIfPatternCaptureCase :: TestTree
multiIfPatternCaptureCase =
  testCase "a multi-way if pattern guard that would capture the beta argument forces alpha then guarded beta" $ do
    convertedModule <- parsedFixture "multi-if-capture" multiIfPatternCaptureSource
    corpus <- expectRight "multi-if-capture corpus" (bindingCorpus convertedModule)
    assertBool
      "multi-way if guard pattern capture must force freshening"
      (hbrmFresheningCount (hbcMetrics corpus) > 0)
    assertBool
      "multi-way if guard pattern capture must prevent direct substitution"
      (hbrmAllowedCount (hbcMetrics corpus) == 0)
    (alphaRule, betaRule) <-
      case partition (isNothing . rrCondition . srsRule) (supportedRules (hbcRules corpus)) of
        ([alphaSpec], [betaSpec]) ->
          pure (srsRule alphaSpec, srsRule betaSpec)
        _ ->
          assertFailure "expected exactly one unguarded alpha rule and one guarded beta rule"
    bindingTerm <- singleBindingTerm convertedModule
    assertBool "alpha lhs is the original multi-way if redex" (matchesHsExprPattern (rrLhs alphaRule) bindingTerm)
    assertBool
      "beta lhs is the freshened multi-way if redex the alpha rule materializes"
      (matchesHsExprPattern (rrLhs betaRule) (rrRhs alphaRule))
    let guardBinderNames = patternGuardBinderNames (rrRhs betaRule)
    assertBool
      "the multi-way if guard pattern binder is alpha-renamed away from the beta argument"
      (not (null guardBinderNames) && all (/= "y") guardBinderNames)
    assertBool
      "the beta argument remains the free y occurrence after contraction"
      ("y" `elem` patternGlobalNames (rrRhs betaRule))

localPatternCaptureCase :: TestTree
localPatternCaptureCase =
  testCase "a local pattern bind that would capture the beta argument forces alpha then guarded beta" $ do
    convertedModule <- parsedFixture "local-pattern-capture" localPatternCaptureSource
    corpus <- expectRight "local-pattern-capture corpus" (bindingCorpus convertedModule)
    assertBool
      "local pattern bind capture must force freshening"
      (hbrmFresheningCount (hbcMetrics corpus) > 0)
    assertBool
      "local pattern bind capture must prevent direct substitution"
      (hbrmAllowedCount (hbcMetrics corpus) == 0)
    (alphaRule, betaRule) <-
      case partition (isNothing . rrCondition . srsRule) (supportedRules (hbcRules corpus)) of
        ([alphaSpec], [betaSpec]) ->
          pure (srsRule alphaSpec, srsRule betaSpec)
        _ ->
          assertFailure "expected exactly one unguarded alpha rule and one guarded beta rule"
    bindingTerm <- singleBindingTerm convertedModule
    assertBool "alpha lhs is the original local-pattern redex" (matchesHsExprPattern (rrLhs alphaRule) bindingTerm)
    assertBool
      "beta lhs is the freshened local-pattern redex the alpha rule materializes"
      (matchesHsExprPattern (rrLhs betaRule) (rrRhs alphaRule))
    let localBinderNames = patternLocalBindBinderNames (rrRhs betaRule)
    assertBool
      "the local pattern binder is alpha-renamed away from the beta argument"
      (not (null localBinderNames) && all (/= "y") localBinderNames)
    assertBool
      "the beta argument remains the free y occurrence after contraction"
      ("y" `elem` patternGlobalNames (rrRhs betaRule))

clausePatternCaptureCase :: TestTree
clausePatternCaptureCase =
  testCase "a clause argument pattern that would capture the beta argument forces alpha then guarded beta" $ do
    convertedModule <- parsedFixture "clause-capture" clausePatternCaptureSource
    corpus <- expectRight "clause-capture corpus" (bindingCorpus convertedModule)
    assertBool
      "clause pattern capture must force freshening"
      (hbrmFresheningCount (hbcMetrics corpus) > 0)
    assertBool
      "clause pattern capture must prevent direct substitution"
      (hbrmAllowedCount (hbcMetrics corpus) == 0)
    (alphaRule, betaRule) <-
      case partition (isNothing . rrCondition . srsRule) (supportedRules (hbcRules corpus)) of
        ([alphaSpec], [betaSpec]) ->
          pure (srsRule alphaSpec, srsRule betaSpec)
        _ ->
          assertFailure "expected exactly one unguarded alpha rule and one guarded beta rule"
    bindingTerm <- singleBindingTerm convertedModule
    assertBool "alpha lhs is the original clause redex" (matchesHsExprPattern (rrLhs alphaRule) bindingTerm)
    assertBool
      "beta lhs is the freshened clause redex the alpha rule materializes"
      (matchesHsExprPattern (rrLhs betaRule) (rrRhs alphaRule))
    let clauseBinderNames = patternClauseBinderNames (rrRhs betaRule)
    assertBool
      "the clause pattern binder is alpha-renamed away from the beta argument"
      (not (null clauseBinderNames) && all (/= "y") clauseBinderNames)
    assertBool
      "the beta argument remains the free y occurrence after contraction"
      ("y" `elem` patternGlobalNames (rrRhs betaRule))

clauseBinderEdgeCase :: TestTree
clauseBinderEdgeCase =
  testCase "clause pattern binders enter only their clause body segment" $ do
    let firstBinder = testBinder 1 "left"
        secondBinder = testBinder 2 "pair"
        thirdBinder = testBinder 3 "right"
        fourthBinder = testBinder 4 "inner"
        nodeValue =
          ClausesF
            [ ([PVarP firstBinder, PTupleP [PVarP secondBinder, PWildP]], Fix (VarF (LocalName firstBinder))),
              ([PConP (mkRdrUnqual (mkVarOcc "Just")) [PVarP thirdBinder, PVarP fourthBinder]], Fix (VarF (LocalName thirdBinder)))
            ]
    hsExprChildBinderEdges nodeValue
      @?= Map.fromList
        [ ("clause-0-body", Set.fromList (fmap SurfaceName ["left", "pair"])),
          ("clause-1-body", Set.fromList (fmap SurfaceName ["right", "inner"]))
        ]

disjointIdentifierCase :: TestTree
disjointIdentifierCase =
  testCase "bridge rule identifiers are disjoint from the site family" $ do
    convertedModule <- parsedFixture "shadow" shadowSource
    corpus <- expectRight "shadow corpus" (bindingCorpus convertedModule)
    siteBook <- expectRight "site family" (siteRuleBook convertedModule)
    let siteIdentifiers = ruleIdentifiers siteBook
        bridgeIdentifiers = ruleIdentifiers (hbcRules corpus)
    assertBool "site family is non-empty" (not (Set.null siteIdentifiers))
    assertBool "bridge corpus is non-empty" (not (Set.null bridgeIdentifiers))
    assertBool
      "rule identifier ranges are disjoint"
      (Set.null (Set.intersection siteIdentifiers bridgeIdentifiers))
    assertBool
      "every bridge identifier sits at or above the reserved base"
      (all (\(RewriteRuleId ruleKey) -> ruleKey >= hsExprBindingRuleIdBase) (Set.toList bridgeIdentifiers))
    assertBool
      "every site identifier sits below the reserved base"
      (all (\(RewriteRuleId ruleKey) -> ruleKey < hsExprBindingRuleIdBase) (Set.toList siteIdentifiers))

unionContractionCase :: TestTree
unionContractionCase =
  testCase "the union corpus contracts the shadow binding past what the site family reaches alone" $ do
    convertedModule <- parsedFixture "shadow" shadowSource
    corpus <- expectRight "shadow corpus" (bindingCorpus convertedModule)
    hbcMetrics corpus
      @?= HsExprBindingRuleMetrics
        { hbrmRedexSiteCount = 2,
          hbrmAllowedCount = 2,
          hbrmFresheningCount = 0,
          hbrmObstructionCount = 0,
          hbrmGeneratedRuleCount = 2,
          hbrmFactRuleCount = 2
        }
    siteBook <- expectRight "site family" (siteRuleBook convertedModule)
    latticeValue <- expectRight "shadow lattice" (convertedModuleContextLattice convertedModule)
    let contextGraph0 = emptyContextEGraph latticeValue (emptyEGraph (specAnalysisSpec (cmScopeIndex convertedModule)))
    (seedClasses, _, _, contextGraph1) <-
      expectRight "shadow insertion" (insertConvertedModuleWithMetrics identityInsertionSeeding convertedModule contextGraph0)
    (bindingContext, seedClass) <- case (cmBindings convertedModule, seedClasses) of
      ([bindingValue], [seedValue]) ->
        pure (ActualScope (seOccScope (tlbScopedTerm bindingValue)), seedValue)
      _ ->
        assertFailure "expected exactly one binding and one seed class"
    (siteResult, siteMatches) <-
      saturateAndExtract "site-only" contextGraph1 siteBook mempty bindingContext seedClass
    (unionResult, unionMatches) <-
      saturateAndExtract
        "union"
        contextGraph1
        (siteBook <> hbcRules corpus)
        (hbcFacts corpus)
        bindingContext
        seedClass
    case erTerm unionResult of
      Fix (AppF (Fix (AppF (Fix (VarF (GlobalName functionName))) (Fix (VarF (GlobalName firstArgument))))) (Fix (VarF (GlobalName secondArgument)))) -> do
        occNameString (rdrNameOcc functionName) @?= "use"
        occNameString (rdrNameOcc firstArgument) @?= "alpha"
        occNameString (rdrNameOcc secondArgument) @?= "alpha"
      _ ->
        assertFailure
          ( "union extraction did not contract to the fully applied body"
              <> "; site cost "
              <> show (erCost siteResult)
              <> " with (matches, fact rounds) "
              <> show siteMatches
              <> ", union cost "
              <> show (erCost unionResult)
              <> " with (matches, fact rounds) "
              <> show unionMatches
          )
    assertBool
      "union extraction is strictly cheaper than the site family alone"
      (erCost unionResult < erCost siteResult)

freshenedRuntimeCase :: TestTree
freshenedRuntimeCase =
  testCase "the alpha-then-guarded-beta chain fires on the runtime for a freshened redex" $ do
    convertedModule <- parsedFixture "capture" captureSource
    corpus <- expectRight "capture corpus" (bindingCorpus convertedModule)
    siteBook <- expectRight "site family" (siteRuleBook convertedModule)
    latticeValue <- expectRight "capture lattice" (convertedModuleContextLattice convertedModule)
    let contextGraph0 = emptyContextEGraph latticeValue (emptyEGraph (specAnalysisSpec (cmScopeIndex convertedModule)))
    (seedClasses, _, _, contextGraph1) <-
      expectRight "capture insertion" (insertConvertedModuleWithMetrics identityInsertionSeeding convertedModule contextGraph0)
    (bindingContext, seedClass) <- case (cmBindings convertedModule, seedClasses) of
      ([bindingValue], [seedValue]) ->
        pure (ActualScope (seOccScope (tlbScopedTerm bindingValue)), seedValue)
      _ ->
        assertFailure "expected exactly one binding and one seed class"
    bindingTerm <- singleBindingTerm convertedModule
    let originalSize = patternNodeCount bindingTerm
    (bridgeOnlyResult, _) <-
      saturateAndExtract
        "capture bridge-only"
        contextGraph1
        (hbcRules corpus)
        (hbcFacts corpus)
        bindingContext
        seedClass
    assertBool
      "the bridge chain alone strictly improves the capture binding"
      (erCost bridgeOnlyResult < originalSize)
    (unionResult, _) <-
      saturateAndExtract
        "capture union"
        contextGraph1
        (siteBook <> hbcRules corpus)
        (hbcFacts corpus)
        bindingContext
        seedClass
    assertBool
      "the freshened contraction strictly improves the capture binding"
      (erCost unionResult < originalSize)
    assertBool
      "interleaving the site family never degrades the bridge chain"
      (erCost unionResult <= erCost bridgeOnlyResult)

patternNodeCount :: Pattern HsExprF -> Int
patternNodeCount = \case
  PatternVar {} ->
    1
  PatternNode layer ->
    1 + sum (fmap patternNodeCount layer)

type SpecAnalysis :: Type
data SpecAnalysis = SpecAnalysis
  { saCount :: !Int,
    saScope :: !FreeScopeWitness
  }
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice SpecAnalysis where
  join left right =
    SpecAnalysis
      { saCount = max (saCount left) (saCount right),
        saScope = join (saScope left) (saScope right)
      }

instance HasFreeScopeWitness SpecAnalysis where
  freeScopeWitness = saScope

specAnalysisSpec :: ScopeIndex -> AnalysisSpec HsExprF SpecAnalysis
specAnalysisSpec scopeIndex =
  semilatticeAnalysis
    ( \nodeValue ->
        SpecAnalysis
          { saCount = 1 + foldr (\childValue total -> saCount childValue + total) 0 nodeValue,
            saScope = hsExprFreeScopeWitness scopeIndex (fmap saScope nodeValue)
          }
    )

type SpecUniverse :: Type
type SpecUniverse = EGraphU ScopeCtx HsExprF SpecAnalysis ScopeCtx

specSaturationBudget :: SaturationBudget
specSaturationBudget =
  SaturationBudget
    { sbMaxIterations = 8,
      sbMaxNodes = 2000
    }

bindingFrontPlanSpec ::
  ContextEGraph HsExprF SpecAnalysis ScopeCtx ->
  PlanSpec
    SpecUniverse
    (SaturatingProofEGraph ScopeCtx HsExprF SpecAnalysis ScopeCtx ())
    (SupportScheduleGroup SpecUniverse)
bindingFrontPlanSpec contextGraph =
  withGuidance
    ( egraphSupportGuidance
        (hsExprRuntimeCapabilitiesForContextGraph contextGraph)
        Nothing
    )
    ( withRewriteContext
        ( \proofGraph ->
            RewriteContextSnapshot
              { rcsCapabilityGeneration =
                  hsExprCapabilityGenerationForContextGraph (sceContextGraph (pgGraph proofGraph)),
                rcsRewriteContext = hsExprRuntimeCapabilitiesForContextGraph (sceContextGraph (pgGraph proofGraph))
              }
        )
        ( withSchedulerConfig
            deterministicSchedulerConfig
            ( planSpec
                specSaturationBudget
                GenericJoinMatching
                (hsExprRuntimeCapabilitiesForContextGraph contextGraph)
            )
        )
    )

saturateAndExtract ::
  String ->
  ContextEGraph HsExprF SpecAnalysis ScopeCtx ->
  SupportedRuleBook ScopeCtx HsExprBindingRule ->
  SupportedFactBook ScopeCtx HsExprBindingFactRule ->
  ScopeCtx ->
  ClassId ->
  IO (ExtractionResult HsExprF Int, (Int, Int))
saturateAndExtract label contextGraph ruleBook factBook bindingContext seedClass = do
  let proofGraph = emptySaturatingProofEGraph contextGraph
      proofBuilder :: ProofAnnotationBuilder ScopeCtx ()
      proofBuilder = defaultProofAnnotationBuilder
  compiledProgram <-
    expectRight
      (label <> " support compilation")
      (compileSupportProgram @SpecUniverse (cegSite contextGraph) ruleBook factBook)
  supportPlan <-
    expectRight
      (label <> " support planning")
      (prepareSupportPlan (bindingFrontPlanSpec contextGraph) compiledProgram)
  supportRun <-
    expectRight
      (label <> " saturation")
      ( runSupportPlan
          ( contextExecutionSpec
              (supportRuntimePolicy identitySchedulerRefinement proofBuilder)
              (carrierGoal mempty)
          )
          supportPlan
          proofGraph
      )
  let supportReport = crrResult supportRun
  let summary = reportSummary supportReport
  rsrResult summary @?= ReachedFixedPoint
  let saturatedGraph = sceContextGraph (pgGraph (srCarrier supportReport))
  extractionOutcome <-
    expectRight
      (label <> " extraction")
      ( contextualExtractBounded
          (ExtractionFixpointBudget 512)
          bindingContext
          mempty
          sizeCostAlgebra
          seedClass
          saturatedGraph
      )
  winner <- maybe (assertFailure (label <> " extraction produced no winner")) pure extractionOutcome
  pure (winner, (rsrMatchesApplied summary, rsrFactRoundCount summary))

sizeCostAlgebra :: CostAlgebra HsExprF Int
sizeCostAlgebra =
  CostAlgebra ((+ 1) . sum)

convertedModuleSite :: ConvertedModule -> Either String (PreparedContextSite ScopeCtx)
convertedModuleSite convertedModule =
  fromFiniteLattice <$> first show (convertedModuleContextLattice convertedModule)

bindingCorpus :: ConvertedModule -> Either String HsExprBindingCorpus
bindingCorpus convertedModule = do
  site <- convertedModuleSite convertedModule
  first show (hsExprBindingCorpus site convertedModule)

siteRuleBook :: ConvertedModule -> Either String (SupportedRuleBook ScopeCtx HsExprBindingRule)
siteRuleBook convertedModule = do
  site <- convertedModuleSite convertedModule
  lawBook <- first show (hsExprSiteLawFamily convertedModule)
  first show (supportedRuleBook site (fmap lawRule (lawBookEntries lawBook)))

ruleIdentifiers :: SupportedRuleBook ScopeCtx HsExprBindingRule -> Set.Set RewriteRuleId
ruleIdentifiers =
  Set.fromList . fmap (rrId . srsRule) . supportedRules

patternLamBinderKeys :: Pattern HsExprF -> [Int]
patternLamBinderKeys = \case
  PatternVar {} ->
    []
  PatternNode layer ->
    let nestedKeys = concatMap patternLamBinderKeys (toList layer)
     in case layer of
          LamF binderAnn _ ->
            binderIdKey (baId binderAnn) : nestedKeys
          _ ->
            nestedKeys

patternGuardBinderNames :: Pattern HsExprF -> [String]
patternGuardBinderNames = \case
  PatternVar {} ->
    []
  PatternNode layer ->
    ownGuardBinderNames layer <> concatMap patternGuardBinderNames (toList layer)
  where
    ownGuardBinderNames :: HsExprF r -> [String]
    ownGuardBinderNames = \case
      GuardedF alternatives ->
        [ occNameString (rdrNameOcc (baName binderAnn))
        | GuardedAltF guards _ <- alternatives,
          guardStmt <- guards,
          binderAnn <- guardStmtBinders guardStmt
        ]
      MultiIfF alternatives ->
        [ occNameString (rdrNameOcc (baName binderAnn))
        | GuardedAltF guards _ <- alternatives,
          guardStmt <- guards,
          binderAnn <- guardStmtBinders guardStmt
        ]
      _ ->
        []

    guardStmtBinders :: HsGuardStmtF r -> [BinderAnn]
    guardStmtBinders = \case
      GuardBoolF _ -> []
      GuardPatF guardPattern _ -> patBinders guardPattern
      GuardLetF _ bindings -> foldMap (patBinders . fst) bindings

patternLocalBindBinderNames :: Pattern HsExprF -> [String]
patternLocalBindBinderNames = \case
  PatternVar {} ->
    []
  PatternNode layer ->
    ownLocalBindBinderNames layer <> concatMap patternLocalBindBinderNames (toList layer)
  where
    ownLocalBindBinderNames :: HsExprF r -> [String]
    ownLocalBindBinderNames = \case
      LetF _ bindings _ ->
        rowBinderNames bindings
      DoF statements ->
        foldMap statementLocalBindBinderNames statements
      GuardedF alternatives ->
        foldMap guardedAltLocalBindBinderNames alternatives
      MultiIfF alternatives ->
        foldMap guardedAltLocalBindBinderNames alternatives
      _ ->
        []

    rowBinderNames :: [(HsPatF, r)] -> [String]
    rowBinderNames bindings =
      [ occNameString (rdrNameOcc (baName binderAnn))
      | (rowPattern, _) <- bindings,
        binderAnn <- patBinders rowPattern
      ]

    statementLocalBindBinderNames :: HsStmtF r -> [String]
    statementLocalBindBinderNames = \case
      BindStmtF {} -> []
      BodyStmtF {} -> []
      LetStmtF _ bindings -> rowBinderNames bindings

    guardedAltLocalBindBinderNames :: GuardedAltF r -> [String]
    guardedAltLocalBindBinderNames (GuardedAltF guards _) =
      foldMap guardLocalBindBinderNames guards

    guardLocalBindBinderNames :: HsGuardStmtF r -> [String]
    guardLocalBindBinderNames = \case
      GuardBoolF {} -> []
      GuardPatF {} -> []
      GuardLetF _ bindings -> rowBinderNames bindings

patternClauseBinderNames :: Pattern HsExprF -> [String]
patternClauseBinderNames = \case
  PatternVar {} ->
    []
  PatternNode layer ->
    ownClauseBinderNames layer <> concatMap patternClauseBinderNames (toList layer)
  where
    ownClauseBinderNames :: HsExprF r -> [String]
    ownClauseBinderNames = \case
      ClausesF clauses ->
        [ occNameString (rdrNameOcc (baName binderAnn))
        | (clausePatterns, _) <- clauses,
          clausePattern <- clausePatterns,
          binderAnn <- patBinders clausePattern
        ]
      _ ->
        []

patternGlobalNames :: Pattern HsExprF -> [String]
patternGlobalNames = \case
  PatternVar {} ->
    []
  PatternNode layer ->
    let nestedNames = concatMap patternGlobalNames (toList layer)
     in case layer of
          VarF (GlobalName rdrName) ->
            occNameString (rdrNameOcc rdrName) : nestedNames
          _ ->
            nestedNames

globalVariable :: String -> Pattern HsExprF
globalVariable name =
  PatternNode (VarF (GlobalName (mkRdrUnqual (mkVarOcc name))))

globalApplication :: String -> String -> Pattern HsExprF
globalApplication functionName argumentName =
  PatternNode (AppF (globalVariable functionName) (globalVariable argumentName))

testBinder :: Int -> String -> BinderAnn
testBinder binderKey name =
  BinderAnn (BinderId binderKey) (mkRdrUnqual (mkVarOcc name))

parsedFixture :: String -> String -> IO ConvertedModule
parsedFixture label source =
  either (\err -> assertFailure (label <> ": " <> show err)) pure (convertHaskellSource (label <> ".hs") source)

singleBindingTerm :: ConvertedModule -> IO (Pattern HsExprF)
singleBindingTerm convertedModule =
  case cmBindings convertedModule of
    [bindingValue] ->
      pure (tlbTerm bindingValue)
    bindings ->
      assertFailure ("expected exactly one binding, saw " <> show (length bindings))

singleFactRule :: HsExprBindingCorpus -> IO HsExprBindingFactRule
singleFactRule corpus =
  case supportedFactSpecs (hbcFacts corpus) of
    [factSpec] ->
      pure (sfsRule factSpec)
    factSpecs ->
      assertFailure ("expected exactly one fact rule, saw " <> show (length factSpecs))

expectRight :: Show err => String -> Either err a -> IO a
expectRight label =
  either (\err -> assertFailure (label <> ": " <> show err)) pure
