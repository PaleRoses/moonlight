{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Binding.ScopedLambdaSaturationSpec
  ( tests,
  )
where

import Data.Bifunctor (first)
import Moonlight.EGraph.Binding.ScopedLambdaFront
  ( ScopedLambdaContext (..),
    ScopedLambdaExtraTerm (..),
    ScopedLambdaSig,
    compileScopedLambdaElaboration,
    compileScopedLambdaShapePlan,
    declareScopedLambdaRelations,
    scopedLambdaGraph,
  )
import Moonlight.EGraph.Pure.Context (contextPreparedObjects)
import Moonlight.EGraph.Pure.Extraction
  ( AnalysisCostAlgebra,
    CostAlgebra (..),
    ExtractionResult (..),
  )
import Moonlight.EGraph.Pure.Saturation.Front
import Moonlight.EGraph.Pure.Saturation.Front.Binding
import Moonlight.EGraph.Pure.Saturation.Front.Binding.Language
  ( BindingLanguageError,
    BindingLanguageIngestion (..),
    blrCaptureObstructions,
    blrSubstitutionOutcomes,
    emitBindingElaboration,
  )
import Moonlight.EGraph.Pure.Saturation.Logic.Run (EGraphLogicReport (..))
import Moonlight.EGraph.Saturation.Context.State
  ( sceContextGraph,
  )
import Moonlight.EGraph.Spec.LambdaBindingGoal.Lambda
  ( LamAnalysis,
    LamF (..),
    Name,
    addLamTerm,
    appTerm,
    lamTerm,
    litTerm,
    varTerm,
  )
import Moonlight.EGraph.Test.Case (HUnitCase (..), hunitCases)
import Moonlight.EGraph.Test.Front.Mono
  ( MonoSig,
    monoCostAlgebra,
    monoExtractTerm,
    monoFix,
    monoNode,
  )
import Data.Fix (Fix (..))
import Moonlight.Rewrite.DSL (Node)
import Moonlight.Saturation.Context.Runtime.Report
  ( reportIterationCount,
    reportMatchesApplied,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    (@?=),
  )

data ScopedLambdaProgramError
  = ScopedLambdaBindingFailed !(BindingLanguageError Name)
  | ScopedLambdaEmitFailed !BindingIngestError
  deriving stock (Eq, Ord, Show)

data ScopedLambdaResult = ScopedLambdaResult
  { slrCaptureAlpha :: !Bool,
    slrCaptureForbidden :: !Bool,
    slrGuardedRedexBeta :: !Bool,
    slrGuardedBeta :: !Bool,
    slrBlockedSeeded :: !Bool,
    slrBlockedAlpha :: !Bool,
    slrBlockedFreshened :: !Bool,
    slrBlockedUnsafe :: !Bool,
    slrBaseDidNotReduce :: !Bool,
    slrFresheningOutcomeCount :: !Int,
    slrCaptureObstructionCount :: !Int,
    slrCaptureExtraction :: !(Maybe (Extracted ScopedLambdaSig "Expr" Int))
  }

tests :: TestTree
tests =
  testGroup "scoped-lambda-saturation" . hunitCases $
    [ HUnitCase
        "derived binding ingestion authors guards, contexts, and extraction"
        scopedLambdaGoldenPath
    ]

scopedLambdaGoldenPath :: Assertion
scopedLambdaGoldenPath = do
  bindingPlan <- expectRight "scoped lambda binding plan" scopedLambdaShapePlan
  fmap bindingPathName (bindingPlanPaths bindingPlan)
    @?= [ "capture-program",
          "capture-program/function",
          "capture-program/function/body",
          "capture-program/function/body/body",
          "capture-program/argument",
          "capture-program/body",
          "capture-program/body/left",
          "capture-program/body/left/function",
          "capture-program/body/left/function/body",
          "capture-program/body/left/argument",
          "capture-program/body/right",
          "capture-program/blocked",
          "capture-program/blocked/function",
          "capture-program/blocked/function/body",
          "capture-program/blocked/function/body/body",
          "capture-program/blocked/argument"
        ]
  bindingPlanContexts bindingPlan @?= [ScopedBinder, ScopedBody]

  graph <- expectRight "scoped lambda graph" (scopedLambdaGraph bindingPlan)
  report <- expectFrontReport (runEGraphFront scopedLambdaProgram graph)
  result <- expectRight "scoped lambda program" (efrResult report)
  let iterations = scheduleIterations report
      matches = scheduleMatches report
      flagSummary =
        "captureAlpha="
          <> show (slrCaptureAlpha result)
          <> " captureForbidden="
          <> show (slrCaptureForbidden result)
          <> " guardedRedexBeta="
          <> show (slrGuardedRedexBeta result)
          <> " guardedBeta="
          <> show (slrGuardedBeta result)
          <> " blockedSeeded="
          <> show (slrBlockedSeeded result)
          <> " blockedAlpha="
          <> show (slrBlockedAlpha result)
          <> " blockedFreshened="
          <> show (slrBlockedFreshened result)
          <> " blockedUnsafe="
          <> show (slrBlockedUnsafe result)
          <> " outcomes="
          <> show (slrFresheningOutcomeCount result)
          <> " obstructions="
          <> show (slrCaptureObstructionCount result)
          <> " matches="
          <> show matches
          <> " iterations="
          <> show iterations

  slrCaptureAlpha result @?= True
  slrCaptureForbidden result @?= False
  assertBool flagSummary (slrGuardedRedexBeta result)
  assertBool flagSummary (slrGuardedBeta result)
  assertBool flagSummary (slrBlockedSeeded result)
  assertBool flagSummary (slrBlockedAlpha result)
  slrBlockedFreshened result @?= True
  slrBlockedUnsafe result @?= False
  slrBaseDidNotReduce result @?= False
  slrFresheningOutcomeCount result @?= 3
  slrCaptureObstructionCount result @?= 1

  case slrCaptureExtraction result of
    Nothing -> assertFailure "expected capture-safe extraction"
    Just extraction ->
      renderLamTerm (monoExtractTerm (erTerm extraction)) @?= renderLamTerm alphaSafeTerm

  assertBool
    ("expected real rewrite matches, saw " <> show matches)
    (matches > 0)
  assertBool
    ("scoped lambda should stay inside the tiny golden-path iteration budget, saw " <> show iterations)
    (iterations <= 8)
  assertBool
    "front-authored run should expose the lexical contexts through the prepared site"
    (all (`elem` contextSiteWitness report) [ScopedBinder, ScopedBody])

scopedLambdaProgram :: EGraphFront 'Authored ScopedLambdaSig LamAnalysis ScopedLambdaContext (Either ScopedLambdaProgramError ScopedLambdaResult)
scopedLambdaProgram =
  egraph $ do
    relations <- declareScopedLambdaRelations
    case compileScopedLambdaElaboration relations "capture-program" captureInputTerm scopedLambdaExtraTerms of
      Left err ->
        pure (pure (Left (ScopedLambdaBindingFailed err)))
      Right elaboration -> do
        elaborationResult <- emitBindingElaboration elaboration
        case elaborationResult of
          Left err ->
            pure (pure (Left (ScopedLambdaBindingFailed err)))
          Right (languageIngestion, bindingRules) ->
            case scopedLambdaRefs (bliBindingIngestion languageIngestion) of
              Left err ->
                pure (pure (Left err))
              Right refs -> do
                lambdaRules <- scopedLambdaRules refs

                run (runFor scopedLambdaBudget bindingRules)
                run (runFor scopedLambdaBudget lambdaRules)

                captureAlpha <- checkAt @"capture-alpha" (slrsBinder refs) (slrsCapture refs === term alphaSafeTerm)
                captureForbidden <- checkAt @"capture-forbidden" (slrsBinder refs) (slrsCapture refs === term forbiddenCaptureTerm)
                guardedRedexBeta <- checkAt @"guarded-redex-beta" (slrsBody refs) (slrsGuardedRedex refs === term yTerm)
                guardedBeta <- checkAt @"guarded-beta" (slrsBody refs) (slrsGuarded refs === term yTerm)
                blockedSeeded <- checkAt @"blocked-seeded" (slrsBody refs) (slrsBlocked refs === term blockedBodyFix)
                blockedAlpha <- checkAt @"blocked-alpha" (slrsBody refs) (slrsBlocked refs === term blockedFreshenedRedex)
                blockedFreshened <- checkAt @"blocked-freshened" (slrsBody refs) (slrsBlocked refs === term blockedFreshenedResult)
                blockedUnsafe <- checkAt @"blocked-unsafe" (slrsBody refs) (slrsBlocked refs === term blockedUnsafeResult)
                baseReduced <- check @"base-did-not-reduce" (slrsCapture refs === term alphaSafeTerm)
                captureExtraction <- extractAt @"capture-extract" (slrsBinder refs) scopedLambdaCost (slrsCapture refs)
                let languageReport =
                      bliReport languageIngestion

                pure $
                  fmap Right $
                    ScopedLambdaResult
                      <$> captureAlpha
                      <*> captureForbidden
                      <*> guardedRedexBeta
                      <*> guardedBeta
                      <*> blockedSeeded
                      <*> blockedAlpha
                      <*> blockedFreshened
                      <*> blockedUnsafe
                      <*> baseReduced
                      <*> pure (length (blrSubstitutionOutcomes languageReport))
                      <*> pure (length (blrCaptureObstructions languageReport))
                      <*> captureExtraction

scopedLambdaRules ::
  ScopedLambdaRefs ->
  EGraphFrontM ScopedLambdaSig LamAnalysis ScopedLambdaContext RulesetRef
scopedLambdaRules refs =
  rulesetNamed "lambda" $ do
    rewriteNamed "body-add-zero" $
      atContext (slrsBody refs) $
        add #body zero ==> #body

data ScopedLambdaRefs = ScopedLambdaRefs
  { slrsBinder :: !(ContextRef ScopedLambdaContext),
    slrsBody :: !(ContextRef ScopedLambdaContext),
    slrsCapture :: !(TermRef ScopedLambdaSig "Expr"),
    slrsGuardedRedex :: !(TermRef ScopedLambdaSig "Expr"),
    slrsGuarded :: !(TermRef ScopedLambdaSig "Expr"),
    slrsBlocked :: !(TermRef ScopedLambdaSig "Expr")
  }

scopedLambdaRefs :: BindingIngestion ScopedLambdaSig ScopedLambdaContext -> Either ScopedLambdaProgramError ScopedLambdaRefs
scopedLambdaRefs ingestion = do
  capturePathValue <- pathOrBindingError capturePath
  guardedRedexPathValue <- pathOrBindingError guardedRedexPath
  guardedBodyPathValue <- pathOrBindingError guardedBodyPath
  blockedBodyPathValue <- pathOrBindingError blockedBodyPath
  binder <- ingestOrProgramError (bindingIngestionScopeAt capturePathValue ingestion)
  body <- ingestOrProgramError (bindingIngestionScopeAt guardedBodyPathValue ingestion)
  guardedRedex <- ingestOrProgramError (bindingIngestionTermAt guardedRedexPathValue ingestion)
  guarded <- ingestOrProgramError (bindingIngestionTermAt guardedBodyPathValue ingestion)
  blocked <- ingestOrProgramError (bindingIngestionTermAt blockedBodyPathValue ingestion)
  pure
    ScopedLambdaRefs
      { slrsBinder = binder,
        slrsBody = body,
        slrsCapture = biRootTerm ingestion,
        slrsGuardedRedex = guardedRedex,
        slrsGuarded = guarded,
        slrsBlocked = blocked
      }

pathOrBindingError :: Either BindingIngestError BindingPath -> Either ScopedLambdaProgramError BindingPath
pathOrBindingError =
  first ScopedLambdaEmitFailed

ingestOrProgramError :: Either BindingIngestError value -> Either ScopedLambdaProgramError value
ingestOrProgramError =
  first ScopedLambdaEmitFailed

scopedLambdaShapePlan :: Either BindingIngestError (BindingPlan ScopedLambdaSig ScopedLambdaContext)
scopedLambdaShapePlan =
  compileScopedLambdaShapePlan "capture-program" captureInputTerm scopedLambdaExtraTerms

scopedLambdaExtraTerms :: [ScopedLambdaExtraTerm]
scopedLambdaExtraTerms =
  [ ScopedLambdaExtraTerm "body" ScopedBody guardedBodyFix,
    ScopedLambdaExtraTerm "blocked" ScopedBody blockedBodyFix
  ]

capturePath :: Either BindingIngestError BindingPath
capturePath =
  bindingPathFromSegmentsNamed "capture-program" []

guardedBodyPath :: Either BindingIngestError BindingPath
guardedBodyPath =
  bindingPathFromSegmentsNamed "capture-program" ["body"]

guardedRedexPath :: Either BindingIngestError BindingPath
guardedRedexPath =
  bindingPathFromSegmentsNamed "capture-program" ["body", "left"]

blockedBodyPath :: Either BindingIngestError BindingPath
blockedBodyPath =
  bindingPathFromSegmentsNamed "capture-program" ["blocked"]

scopedLambdaBudget :: SaturationBudget
scopedLambdaBudget =
  SaturationBudget
    { sbMaxIterations = 20,
      sbMaxNodes = 10000
    }

term :: Functor f => Fix f -> Term (MonoSig f) "Expr"
term =
  monoFix

add :: Term ScopedLambdaSig "Expr" -> Term ScopedLambdaSig "Expr" -> Term ScopedLambdaSig "Expr"
add left right =
  monoNode (LAdd left right)

zero :: Term ScopedLambdaSig "Expr"
zero =
  term (litTerm 0)

yTerm :: Fix LamF
yTerm =
  varTerm "y"

zTerm :: Fix LamF
zTerm =
  varTerm "z"

identityTerm :: Fix LamF
identityTerm =
  lamTerm "x" (varTerm "x")

captureInputTerm :: Fix LamF
captureInputTerm =
  appTerm
    (lamTerm "x" (lamTerm "w" (varTerm "x")))
    yTerm

alphaSafeTerm :: Fix LamF
alphaSafeTerm =
  lamTerm "w" yTerm

forbiddenCaptureTerm :: Fix LamF
forbiddenCaptureTerm =
  lamTerm "w" (varTerm "w")

guardedBodyFix :: Fix LamF
guardedBodyFix =
  addLamTerm
    (appTerm identityTerm yTerm)
    (litTerm 0)

blockedBodyFix :: Fix LamF
blockedBodyFix =
  appTerm
    (lamTerm "x" (lamTerm "z" (varTerm "x")))
    zTerm

blockedUnsafeResult :: Fix LamF
blockedUnsafeResult =
  lamTerm "z" zTerm

blockedFreshenedRedex :: Fix LamF
blockedFreshenedRedex =
  appTerm
    (lamTerm "x" (lamTerm "z0" (varTerm "x")))
    zTerm

blockedFreshenedResult :: Fix LamF
blockedFreshenedResult =
  lamTerm "z0" zTerm

renderLamTerm :: Fix LamF -> String
renderLamTerm (Fix layer) =
  case layer of
    LVar name -> show name
    LLit value -> show value
    LLam name body -> "(lam " <> show name <> " " <> renderLamTerm body <> ")"
    LApp function argument -> "(app " <> renderLamTerm function <> " " <> renderLamTerm argument <> ")"
    LAdd left right -> "(add " <> renderLamTerm left <> " " <> renderLamTerm right <> ")"

scopedLambdaCost :: AnalysisCostAlgebra (Node ScopedLambdaSig) LamAnalysis Int
scopedLambdaCost =
  monoCostAlgebra $
    CostAlgebra $
      \case
        LVar {} -> 1
        LLit {} -> 1
        LLam _ body -> body + 1
        LApp function argument -> function + argument + 1
        LAdd left right -> left + right + 1

scheduleIterations :: EGraphFrontReport ScopedLambdaSig LamAnalysis ScopedLambdaContext result -> Int
scheduleIterations =
  sum . fmap (reportIterationCount . elrSaturation) . efrScheduleReports

scheduleMatches :: EGraphFrontReport ScopedLambdaSig LamAnalysis ScopedLambdaContext result -> Int
scheduleMatches =
  sum . fmap (reportMatchesApplied . elrSaturation) . efrScheduleReports

contextSiteWitness :: EGraphFrontReport ScopedLambdaSig LamAnalysis ScopedLambdaContext result -> [ScopedLambdaContext]
contextSiteWitness =
  contextPreparedObjects . sceContextGraph . efrFinalGraph

expectRight :: (Show errorValue) => String -> Either errorValue value -> IO value
expectRight label =
  either
    (\err -> assertFailure (label <> ": " <> show err) >> fail (label <> ": " <> show err))
    pure

expectFrontReport :: Either (EGraphFrontError ScopedLambdaSig LamAnalysis ScopedLambdaContext) value -> IO value
expectFrontReport =
  either
    (\err -> assertFailure (frontErrorMessage err) >> fail (frontErrorMessage err))
    pure
