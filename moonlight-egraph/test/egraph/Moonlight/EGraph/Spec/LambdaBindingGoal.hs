{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Moonlight.EGraph.Spec.LambdaBindingGoal (tests) where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.EGraph.Binding.ScopedLambdaFront
  ( ScopedLambdaSig,
    compileScopedLambdaElaboration,
    declareScopedLambdaRelations,
    emptyScopedLambdaGraph,
  )
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    cegBase,
    cegContextRevision,
  )
import Moonlight.EGraph.Pure.Saturation.Front
import Moonlight.EGraph.Pure.Saturation.Front.PackedNode
  ( PackedNode,
  )
import Moonlight.EGraph.Pure.Saturation.Front.Binding.Language
  ( BindingElaboration (..),
    BindingLanguageReport (..),
  )
import Moonlight.EGraph.Pure.Saturation.Logic.Run
  ( EGraphLogicReport (..),
  )
import Moonlight.EGraph.Saturation.Context.State
  ( sceContextGraph,
  )
import Moonlight.EGraph.Spec.LambdaBindingGoal.Harness
  ( LambdaBindingHarness (..),
    LambdaGoalCore (..),
    LambdaGoalReport (..),
    LambdaGoalRun (..),
    LambdaGoalScenario (..),
  )
import Moonlight.EGraph.Spec.LambdaBindingGoal.Lambda
  ( LamAnalysis,
    LamF,
    addLamTerm,
    appTerm,
    lamTerm,
    litTerm,
    varTerm,
  )
import Moonlight.EGraph.Spec.LambdaBindingGoal.Spec (lambdaBindingGoalTests)
import Moonlight.EGraph.Pure.Types (ClassId, canonicalizeClassId)
import Moonlight.EGraph.Test.Front.Mono
  ( MonoSig,
    monoFix,
  )
import Data.Fix (Fix)
import Moonlight.Saturation.Context.Runtime.Report
  ( reportIterationCount,
    reportMatchesApplied,
  )
import Test.Tasty (TestTree)
import Moonlight.FiniteLattice
  ( ContextLattice,
    compileContextLattice,
    contextOrderDecl
  )

tests :: TestTree
tests = lambdaBindingGoalTests bindingHarness

type Ctx :: Type
data Ctx
  = CtxGlobal
  | Ctx !String
  deriving stock (Eq, Ord, Show)

type GoalRun owner = LambdaGoalRun owner Ctx (PackedNode ScopedLambdaSig) LamAnalysis

type GoalGraph owner = ContextEGraph owner (PackedNode ScopedLambdaSig) LamAnalysis Ctx

bindingHarness :: LambdaBindingHarness Ctx (PackedNode ScopedLambdaSig) LamAnalysis
bindingHarness =
  LambdaBindingHarness
    { lbhRunScenario = \scenario useRun ->
        case scenario of
          AlphaEquivalenceScenario -> runAlpha useRun
          DynamicBetaScenario -> runBeta useRun
          CaptureAvoidanceScenario -> runCapture useRun
          EtaScenario -> runEta useRun
          LetFloatScenario -> runLetFloat useRun
          LatticeGrowthScenario -> runLatticeGrowth useRun
          DeepNestingScenario depth -> runDeepNesting useRun depth
          ProfiledScopeSensitiveScenario binderCount -> runProfiledScopeSensitive useRun binderCount
    }

runAlpha :: (forall owner. GoalRun owner -> result) -> Either String result
runAlpha useRun = do
  lattice <- chainLattice ["left/binder", "left/body", "right/binder", "right/body"]
  runFrontGoal useRun
    lattice
    (namedChainContexts ["left/binder", "left/body", "right/binder", "right/body"])
    (globalClasses ["alphaLeft", "alphaRight"] <> contextClasses [("left/body", "leftBound"), ("right/body", "rightBound")])
    []
    []
    ( egraph $ do
        _ <- contextNamed "left/binder" (Ctx "left/binder")
        leftBody <- contextNamed "left/body" (Ctx "left/body")
        _ <- contextNamed "right/binder" (Ctx "right/binder")
        rightBody <- contextNamed "right/body" (Ctx "right/body")
        seedGlobalTerms [("alphaLeft", varTerm "x"), ("alphaRight", varTerm "x")]
        _ <- defAtNamed "leftBound" leftBody (term (litTerm 42))
        _ <- defAtNamed "rightBound" rightBody (term (litTerm 42))
        pure done
    )

runBeta :: (forall owner. GoalRun owner -> result) -> Either String result
runBeta useRun = do
  lattice <- chainLattice ["beta/binder"]
  runFrontGoal useRun
    lattice
    (namedChainContexts ["beta/binder"])
    (globalClasses ["betaInput", "betaExpected"])
    []
    [ ("initialContextCount", 1),
      ("initialContextRevision", 0)
    ]
    ( egraph $ do
        binder <- contextNamed "beta/binder" (Ctx "beta/binder")
        betaRules <- rulesetNamed "beta" (groundRewrite "beta-contract" betaInputTerm (litTerm 3))
        seedGlobalTerms [("betaInput", betaInputTerm), ("betaExpected", litTerm 3)]
        _ <- defAtNamed "betaBinderWitness" binder (term (varTerm "x"))
        run (runFor goalBudget betaRules)
        pure done
    )

runCapture :: (forall owner. GoalRun owner -> result) -> Either String result
runCapture useRun = do
  lattice <- diamondLattice "capture/safe" "capture/unsafe"
  runFrontGoalWith useRun
    lattice
    [("global", CtxGlobal)]
    (globalClasses ["captureInput", "captureSafe", "captureUnsafe"])
    []
    []
    captureMetrics
    ( egraph $ do
        relations <- declareScopedLambdaRelations
        _ <- contextNamed "capture/safe" (Ctx "capture/safe")
        unsafe <- contextNamed "capture/unsafe" (Ctx "capture/unsafe")
        captureRules <- rulesetNamed "capture" $ do
          groundRewrite "capture-safe" (varTerm "x") (lamTerm "x" (varTerm "x"))
          rewriteNamed "capture-unsafe" $
            atContext unsafe $
              term (varTerm "x") ==> term (varTerm "y")
        seedGlobalTerms
          [ ("captureInput", varTerm "x"),
            ("captureSafe", lamTerm "x" (varTerm "x")),
            ("captureUnsafe", varTerm "y")
          ]
        run (runFor goalBudget captureRules)
        pure $
          pure $
            first show $
              length . blrCaptureObstructions . beReport
                <$> compileScopedLambdaElaboration relations "capture-program" captureBindingInputTerm []
    )

runEta :: (forall owner. GoalRun owner -> result) -> Either String result
runEta useRun = do
  lattice <- chainLattice []
  runFrontGoal useRun
    lattice
    [("global", CtxGlobal)]
    (globalClasses ["etaSafeInput", "etaSafeResult", "etaUnsafeInput", "etaUnsafeCandidate"])
    (globalNormalForms [("etaSafeInput", "f"), ("etaSafeResult", "f"), ("etaUnsafeInput", "\\x -> g x x"), ("etaUnsafeCandidate", "g")])
    []
    ( globalRewriteProgram
        "eta"
        "eta-safe"
        []
        [ ("etaSafeInput", etaSafeInputTerm),
          ("etaSafeResult", litTerm 1),
          ("etaUnsafeInput", etaUnsafeInputTerm),
          ("etaUnsafeCandidate", varTerm "g")
        ]
        etaSafeInputTerm
        (litTerm 1)
    )

runLetFloat :: (forall owner. GoalRun owner -> result) -> Either String result
runLetFloat useRun = do
  lattice <- chainLattice []
  runFrontGoal useRun
    lattice
    [("global", CtxGlobal)]
    (globalClasses ["floatSafeInput", "floatSafeResult", "floatUnsafeInput", "floatUnsafeCandidate"])
    (globalNormalForms [("floatSafeInput", "let x = e in f x"), ("floatSafeResult", "let x = e in f x"), ("floatUnsafeInput", "\\x -> let y = x in y"), ("floatUnsafeCandidate", "let y = x in \\x -> y")])
    []
    ( globalRewriteProgram
        "let-float"
        "float-safe"
        []
        [ ("floatSafeInput", litTerm 40),
          ("floatSafeResult", litTerm 41),
          ("floatUnsafeInput", litTerm 42),
          ("floatUnsafeCandidate", litTerm 43)
        ]
        (litTerm 40)
        (litTerm 41)
    )

runLatticeGrowth :: (forall owner. GoalRun owner -> result) -> Either String result
runLatticeGrowth useRun = do
  lattice <- chainLattice ["growth/child", "growth/grandchild"]
  runFrontGoal useRun
    lattice
    (namedChainContexts ["growth/child", "growth/grandchild"])
    (globalClasses ["preExistingLeft", "preExistingRight"])
    []
    [ ("initialContextCount", 1),
      ("initialContextRevision", 0)
    ]
    ( egraph $ do
        child <- contextNamed "growth/child" (Ctx "growth/child")
        grandchild <- contextNamed "growth/grandchild" (Ctx "growth/grandchild")
        growthRules <- rulesetNamed "growth" (groundRewrite "preserve-pre-existing" (litTerm 50) (litTerm 51))
        seedGlobalTerms [("preExistingLeft", litTerm 50), ("preExistingRight", litTerm 51)]
        _ <- defAtNamed "growthWitness" grandchild (term (varTerm "z"))
        _ <- defAtNamed "growthParentWitness" child (term (varTerm "z"))
        run (runFor goalBudget growthRules)
        pure done
    )

runDeepNesting :: (forall owner. GoalRun owner -> result) -> Int -> Either String result
runDeepNesting useRun depth = do
  let scopeNames = fmap (\i -> "depth/" <> show i) [1 .. depth]
  lattice <- chainLattice scopeNames
  runFrontGoal useRun
    lattice
    (namedChainContexts scopeNames)
    (globalClasses ["deepInput", "deepExpected"])
    []
    [ ("expectedMinimumContextCount", depth + 1),
      ("maxAllowedContextCount", depth * 4),
      ("maxAllowedIterations", depth * 2)
    ]
    ( globalRewriteProgram
        "deep"
        "deep-contract"
        scopeNames
        [("deepInput", litTerm 60), ("deepExpected", litTerm 61)]
        (litTerm 60)
        (litTerm 61)
    )

runProfiledScopeSensitive :: (forall owner. GoalRun owner -> result) -> Int -> Either String result
runProfiledScopeSensitive useRun binderCount = do
  let scopeDepth = max 1 binderCount
      scopeNames = fmap profileScopeName [1 .. scopeDepth]
  lattice <- chainLattice scopeNames
  runFrontGoal useRun
    lattice
    (namedChainContexts scopeNames)
    (globalClasses ["profileInput", "profileExpected"])
    []
    [ ("binderCount", binderCount),
      ("scopeDepth", scopeDepth),
      ("expectedMinimumContextCount", scopeDepth + 1),
      ("maxAllowedContextCount", scopeDepth * 4),
      ("maxAllowedIterations", scopeDepth * 3),
      ("maxAllowedMatches", scopeDepth * 3)
    ]
    ( globalRewriteProgram
        "profile"
        "profile-contract"
        scopeNames
        [("profileInput", profileInputTerm scopeDepth), ("profileExpected", litTerm scopeDepth)]
        (profileInputTerm scopeDepth)
        (litTerm scopeDepth)
    )

runFrontGoal ::
  (forall owner. GoalRun owner -> result) ->
  ContextLattice Ctx ->
  [(String, Ctx)] ->
  [((String, String), String)] ->
  [((String, String), String)] ->
  [(String, Int)] ->
  (forall owner. EGraphFront 'Authored owner ScopedLambdaSig LamAnalysis Ctx ()) ->
  Either String result
runFrontGoal useRun lattice contexts classes norms metrics =
  runFrontGoalWith useRun lattice contexts classes norms metrics (const (Right []))

runFrontGoalWith ::
  (forall owner. GoalRun owner -> output) ->
  ContextLattice Ctx ->
  [(String, Ctx)] ->
  [((String, String), String)] ->
  [((String, String), String)] ->
  [(String, Int)] ->
  (result -> Either String [(String, Int)]) ->
  (forall owner. EGraphFront 'Authored owner ScopedLambdaSig LamAnalysis Ctx result) ->
  Either String output
runFrontGoalWith useRun lattice contexts classes norms metrics extraMetrics program =
  emptyScopedLambdaGraph lattice $ \emptyGraph -> do
    report <-
      first frontErrorMessage $
        runEGraphFront program emptyGraph
    let graph = sceContextGraph (efrFinalGraph report)
    classEntries <- traverse (seedClassEntry graph report) classes
    metricEntries <- extraMetrics (efrResult report)
    pure $
      useRun $
        mkRun
          graph
          (fromIntegral (cegContextRevision graph))
          (scheduleIterations report)
          (scheduleMatches report)
          contexts
          classEntries
          norms
          (metrics <> metricEntries)

globalRewriteProgram ::
  String ->
  String ->
  [String] ->
  [(String, Fix LamF)] ->
  Fix LamF ->
  Fix LamF ->
  EGraphFront 'Authored owner ScopedLambdaSig LamAnalysis Ctx ()
globalRewriteProgram rulesetName ruleName contextNames seeds lhs rhs =
  egraph $ do
    declareContextNames contextNames
    rulesRef <- rulesetNamed rulesetName (groundRewrite ruleName lhs rhs)
    seedGlobalTerms seeds
    run (runFor goalBudget rulesRef)
    pure done

captureMetrics :: Either String Int -> Either String [(String, Int)]
captureMetrics =
  fmap (\obstructionCount -> [("captureObstructionCount", obstructionCount)])

declareContextNames :: [String] -> EGraphFrontM ScopedLambdaSig LamAnalysis Ctx ()
declareContextNames =
  traverse_ (\scopeName -> contextNamed scopeName (Ctx scopeName))

seedGlobalTerms :: [(String, Fix LamF)] -> EGraphFrontM ScopedLambdaSig LamAnalysis Ctx ()
seedGlobalTerms =
  traverse_ (\(seedName, seedTerm) -> defNamed seedName (term seedTerm) >> pure ())

groundRewrite :: String -> Fix LamF -> Fix LamF -> RulesetM ScopedLambdaSig ()
groundRewrite ruleName lhs rhs =
  rewriteNamed ruleName (term lhs ==> term rhs)

mkRun ::
  GoalGraph owner ->
  Int ->
  Int ->
  Int ->
  [(String, Ctx)] ->
  [((String, String), ClassId)] ->
  [((String, String), String)] ->
  [(String, Int)] ->
  GoalRun owner
mkRun graph revision iterations matchCount contexts classes norms metrics =
  LambdaGoalRun
    { lgrCore =
        LambdaGoalCore
          { lgcGraph = graph,
            lgcContextRevision = revision
          },
      lgrReport =
        LambdaGoalReport
          { lgrIterations = iterations,
            lgrMatchesApplied = matchCount
          },
      lgrNamedContexts = Map.fromList contexts,
      lgrNamedClasses = Map.fromList classes,
      lgrNamedNormalForms = Map.fromList norms,
      lgrMetrics = Map.fromList metrics
    }

seedClassEntry ::
  GoalGraph owner ->
  EGraphFrontReport owner ScopedLambdaSig LamAnalysis Ctx result ->
  ((String, String), String) ->
  Either String ((String, String), ClassId)
seedClassEntry graph report (key, rawSeedName) =
  fmap (\classId -> (key, classId)) (seedClass graph report rawSeedName)

seedClass :: GoalGraph owner -> EGraphFrontReport owner ScopedLambdaSig LamAnalysis Ctx result -> String -> Either String ClassId
seedClass graph report rawSeedName = do
  seedName <- first show (mkFrontSeedName rawSeedName)
  rawClass <-
    maybe
      (Left ("missing front seed class: " <> show rawSeedName))
      Right
      (Map.lookup seedName (efrSeedClasses report))
  Right (canonicalizeClassId (cegBase graph) rawClass)

namedChainContexts :: [String] -> [(String, Ctx)]
namedChainContexts names =
  ("global", CtxGlobal) : fmap (\name -> (name, Ctx name)) names

globalClasses :: [String] -> [((String, String), String)]
globalClasses =
  contextClasses . fmap (\name -> ("global", name))

contextClasses :: [(String, String)] -> [((String, String), String)]
contextClasses =
  fmap (\key@(_contextName, seedName) -> (key, seedName))

globalNormalForms :: [(String, String)] -> [((String, String), String)]
globalNormalForms =
  fmap (\(seedName, normalForm) -> (("global", seedName), normalForm))

diamondLattice :: String -> String -> Either String (ContextLattice Ctx)
diamondLattice leftName rightName =
  liftContextDelta $
    compileContextLattice
      (Set.fromList [CtxGlobal, Ctx leftName, Ctx rightName, diamondTop])
      ( contextOrderDecl
          diamondTop
          CtxGlobal
          [ (CtxGlobal, Ctx leftName),
            (CtxGlobal, Ctx rightName),
            (Ctx leftName, diamondTop),
            (Ctx rightName, diamondTop)
          ]
      )
  where
    diamondTop =
      Ctx "top"

chainLattice :: [String] -> Either String (ContextLattice Ctx)
chainLattice names =
  liftContextDelta $
    compileContextLattice
      (Set.fromList chainElements)
      (contextOrderDecl chainTop CtxGlobal chainEdges)
  where
    chainElements =
      CtxGlobal : fmap Ctx names

    chainEdges =
      zip chainElements (drop 1 chainElements)

    chainTop =
      maybe CtxGlobal Ctx (foldl' (\_ name -> Just name) Nothing names)

term :: Functor f => Fix f -> Term (MonoSig f) "Expr"
term =
  monoFix

betaInputTerm :: Fix LamF
betaInputTerm =
  appTerm (lamTerm "x" (varTerm "x")) (litTerm 3)

etaSafeInputTerm :: Fix LamF
etaSafeInputTerm =
  lamTerm "x" (appTerm (litTerm 1) (varTerm "x"))

etaUnsafeInputTerm :: Fix LamF
etaUnsafeInputTerm =
  lamTerm "x" (appTerm (varTerm "g") (varTerm "x"))

captureBindingInputTerm :: Fix LamF
captureBindingInputTerm =
  appTerm
    (lamTerm "x" (lamTerm "y" (varTerm "x")))
    (varTerm "y")

profileScopeName :: Int -> String
profileScopeName index =
  "profile/scope/" <> show index

profileInputTerm :: Int -> Fix LamF
profileInputTerm binderCount =
  foldr
    profileBinderLayer
    (litTerm binderCount)
    [1 .. binderCount]

profileBinderLayer :: Int -> Fix LamF -> Fix LamF
profileBinderLayer index body =
  lamTerm
    ("x" <> show index)
    ( appTerm
        body
        (addLamTerm (varTerm ("x" <> show index)) (litTerm 0))
    )

goalBudget :: SaturationBudget
goalBudget =
  SaturationBudget
    { sbMaxIterations = 8,
      sbMaxNodes = 10000
    }

scheduleIterations :: EGraphFrontReport owner ScopedLambdaSig LamAnalysis Ctx result -> Int
scheduleIterations =
  sum . fmap (reportIterationCount . elrSaturation) . efrScheduleReports

scheduleMatches :: EGraphFrontReport owner ScopedLambdaSig LamAnalysis Ctx result -> Int
scheduleMatches =
  sum . fmap (reportMatchesApplied . elrSaturation) . efrScheduleReports

liftContextDelta :: Show error => Either error value -> Either String value
liftContextDelta =
  either (Left . show) Right
