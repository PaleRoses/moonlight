{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Generate the paper's capture-avoidance obstruction figure.
module Main (main) where

import Data.Bifunctor (first)
import Data.Fix (Fix)
import Data.Foldable (toList, traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (intercalate, isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Void (Void, absurd)
import Moonlight.Core (ClassId, classIdKey)
import Moonlight.EGraph.Binding.ScopedLambdaFront
  ( ScopedLambdaSig,
    compileScopedLambdaElaboration,
    declareScopedLambdaRelations,
    emptyScopedLambdaGraph,
  )
import Moonlight.EGraph.Pure.Context (ContextEGraph, cegBase)
import Moonlight.EGraph.Pure.Saturation.Front
import Moonlight.EGraph.Pure.Saturation.Front.Binding.Language
  ( BindingElaboration (..),
    BindingLanguageReport (..),
    BindingSubstitutionDecision (..),
  )
import Moonlight.EGraph.Pure.Saturation.Front.PackedNode
  ( PackedNode,
    packedTag,
  )
import Moonlight.EGraph.Pure.Types
  ( ENode (..),
    canonicalizeClassId,
  )
import Moonlight.EGraph.Saturation.Context.State (sceContextGraph)
import Moonlight.EGraph.Spec.LambdaBindingGoal.Lambda
  ( LamAnalysis,
    LamF (..),
    Name,
    appTerm,
    lamTerm,
    nameString,
    varTerm,
  )
import Moonlight.EGraph.Test.Front.Mono (MonoSig, monoFix)
import Moonlight.FiniteLattice
  ( ContextLattice,
    compileContextLattice,
    contextOrderDecl,
  )
import Moonlight.Sheaf.Context.Core
  ( AnalysisRestrictionMismatch (..),
    ContextRestrictionMismatch (..),
    SectionMismatch (..),
  )
import Moonlight.Sheaf.Descent.Context
  ( QuotientDescentObstruction (..),
    descentAt,
  )
import Moonlight.Sheaf.Obstruction
  ( ConditionFailureData (..),
    ContextBarrierData (..),
    EquivalenceLookupFailureData (..),
    Obstruction (..),
    PropagationBarrierData (..),
    RestrictionBarrierData (..),
    RestrictionLookupFailureData (..),
    StructuralMismatchData (..),
    obstructionReport,
  )
import Moonlight.Sheaf.Verdict
  ( SearchVerdict (..),
  )
import System.Directory (createDirectoryIfMissing)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

main :: IO ()
main =
  createDirectoryIfMissing True figureDirectory
    *> defaultMain (testGroup "egraph-obstruction-figures" (fmap figureCase figures))

figures :: [(FilePath, String)]
figures =
  [("capture-avoidance.txt", captureAvoidanceFigure)]

figureCase :: (FilePath, String) -> TestTree
figureCase (fileName, rendered) =
  testCase fileName $
    writeFile (figureDirectory <> "/" <> fileName) rendered
      *> assertBool
        (fileName <> ": runtime fixture failed to produce its object")
        (not ("runtime fixture failed" `isInfixOf` rendered))

figureDirectory :: FilePath
figureDirectory =
  "artifacts/paper/figures"

captureAvoidanceFigure :: String
captureAvoidanceFigure =
  renderEither "capture-avoidance" renderCaptureRun captureRun

renderEither :: String -> (a -> String) -> Either String a -> String
renderEither title renderValue =
  either
    (renderBlock title . (["status: runtime fixture failed"] <>) . singletonLine "failure")
    renderValue

singletonLine :: String -> String -> [String]
singletonLine label value =
  [label <> ": " <> value]

renderBlock :: String -> [String] -> String
renderBlock title linesValue =
  unlines (("== " <> title <> " ==") : linesValue)

type CaptureGraph =
  ContextEGraph (PackedNode ScopedLambdaSig) LamAnalysis CaptureCtx

data CaptureCtx
  = CaptureGlobal
  | CaptureCtx String
  deriving stock (Eq, Ord, Show)

data CaptureRun = CaptureRun
  { crGraph :: CaptureGraph,
    crClasses :: Map.Map String ClassId,
    crCaptureObstructions :: [BindingSubstitutionDecision Name]
  }

captureRun :: Either String CaptureRun
captureRun = do
  lattice <- captureLattice "capture/safe" "capture/unsafe"
  report <-
    first frontErrorMessage $
      runEGraphFront captureProgram (emptyScopedLambdaGraph lattice)
  captureObstructions <- efrResult report
  graphValue <-
    pure (sceContextGraph (efrFinalGraph report))
  classesValue <-
    traverse (seedClass graphValue report) ["captureInput", "captureSafe", "captureUnsafe"]
  pure
    CaptureRun
      { crGraph = graphValue,
        crClasses = Map.fromList (zip ["captureInput", "captureSafe", "captureUnsafe"] classesValue),
        crCaptureObstructions = captureObstructions
      }

captureProgram ::
  EGraphFront 'Authored ScopedLambdaSig LamAnalysis CaptureCtx (Either String [BindingSubstitutionDecision Name])
captureProgram =
  egraph $ do
    relations <- declareScopedLambdaRelations
    unsafe <- contextNamed "capture/unsafe" (CaptureCtx "capture/unsafe")
    _safe <- contextNamed "capture/safe" (CaptureCtx "capture/safe")
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
    run (runFor figureBudget captureRules)
    pure $
      pure $
        first show $
          blrCaptureObstructions . beReport
            <$> compileScopedLambdaElaboration relations "capture-program" captureBindingInputTerm []

captureBindingInputTerm :: Fix LamF
captureBindingInputTerm =
  appTerm
    (lamTerm "x" (lamTerm "y" (varTerm "x")))
    (varTerm "y")

captureLattice :: String -> String -> Either String (ContextLattice CaptureCtx)
captureLattice leftName rightName =
  first show $
    compileContextLattice
      (Set.fromList [CaptureGlobal, CaptureCtx leftName, CaptureCtx rightName, CaptureCtx "top"])
      ( contextOrderDecl
          (CaptureCtx "top")
          CaptureGlobal
          [ (CaptureGlobal, CaptureCtx leftName),
            (CaptureGlobal, CaptureCtx rightName),
            (CaptureCtx leftName, CaptureCtx "top"),
            (CaptureCtx rightName, CaptureCtx "top")
          ]
      )

seedGlobalTerms :: [(String, Fix LamF)] -> EGraphFrontM ScopedLambdaSig LamAnalysis context ()
seedGlobalTerms =
  traverse_ (\(seedName, seedTerm) -> defNamed seedName (term seedTerm) *> pure ())

groundRewrite :: String -> Fix LamF -> Fix LamF -> RulesetM ScopedLambdaSig ()
groundRewrite ruleName lhs rhs =
  rewriteNamed ruleName (term lhs ==> term rhs)

term :: Functor f => Fix f -> Term (MonoSig f) "Expr"
term =
  monoFix

figureBudget :: SaturationBudget
figureBudget =
  SaturationBudget
    { sbMaxIterations = 8,
      sbMaxNodes = 10000
    }

seedClass ::
  CaptureGraph ->
  EGraphFrontReport ScopedLambdaSig LamAnalysis CaptureCtx result ->
  String ->
  Either String ClassId
seedClass graphValue report rawSeedName = do
  seedName <- first show (mkFrontSeedName rawSeedName)
  rawClass <-
    maybe
      (Left ("missing front seed class: " <> show rawSeedName))
      Right
      (Map.lookup seedName (efrSeedClasses report))
  Right (canonicalizeClassId (cegBase graphValue) rawClass)

renderCaptureRun :: CaptureRun -> String
renderCaptureRun capture =
  let graphValue = crGraph capture
      captureSafe = classNamed "captureSafe" capture
      captureUnsafe = classNamed "captureUnsafe" capture
      descentVerdict = descentAt CaptureGlobal graphValue
      pairObstructionLines =
        either
          (const [])
          ( \(leftClass, rightClass) ->
              fmap
                ( prettyObstruction
                    prettyClassId
                    prettyPackedENode
                    prettyOpaque
                    prettyCaptureCtx
                    prettyOpaque
                    (prettySectionMismatch prettyClassId prettyLamAnalysis)
                    prettyOpaque
                )
                (obstructionReport leftClass rightClass CaptureGlobal graphValue)
          )
          ((,) <$> captureSafe <*> captureUnsafe)
   in renderBlock
        "capture-avoidance"
        ( [ "runtime objects:",
            "  blrCaptureObstructions :: [BindingSubstitutionDecision Name]",
            "  obstructionReport    :: ClassId -> ClassId -> CaptureCtx -> CaptureGraph -> [Obstruction ClassId (ENode (PackedNode ScopedLambdaSig)) rule CaptureCtx subst stat failure]",
            "",
            "capture-language refusals:"
          ]
            <> indentLines 2 (renderList renderBindingDecision (crCaptureObstructions capture))
            <> [ "",
                 "descentAt CaptureGlobal:"
               ]
            <> indentLines 2 (renderSearchVerdict prettyVoid (prettyQuotientObstruction prettyCaptureCtx prettyClassId) descentVerdict)
            <> [ "",
                 "pair-typed obstructionReport(captureSafe,captureUnsafe,CaptureGlobal):"
               ]
            <> indentLines
              2
              (renderList id pairObstructionLines)
        )

classNamed :: String -> CaptureRun -> Either String ClassId
classNamed name capture =
  maybe
    (Left ("missing class: " <> name))
    Right
    (Map.lookup name (crClasses capture))

renderSearchVerdict ::
  (refusal -> String) ->
  (obstruction -> String) ->
  SearchVerdict refusal obstruction ->
  [String]
renderSearchVerdict prettyRefusal prettyObstructionValue =
  \case
    SearchAccepted ->
      ["SearchAccepted"]
    SearchRejected obstructions ->
      "SearchRejected:" : indentLines 2 (renderList prettyObstructionValue (toList obstructions))
    SearchUndecided refusals partials ->
      [ "SearchUndecided:",
        "  refusals:"
      ]
        <> indentLines 4 (renderList prettyRefusal (toList refusals))
        <> [ "  partial obstructions:" ]
        <> indentLines 4 (renderList prettyObstructionValue partials)

prettyQuotientObstruction ::
  (context -> String) ->
  (rep -> String) ->
  QuotientDescentObstruction context rep ->
  String
prettyQuotientObstruction prettyContext prettyRep =
  \case
    QuotientDescentObstruction parent cover tuples ->
      intercalate
        "\n"
        ( [ "QuotientDescentObstruction",
            "  parent = " <> prettyContext parent,
            "  cover  = " <> bracketed prettyContext cover,
            "  tuples:"
          ]
            <> indentLines 4 (renderList (prettyIntMap prettyRep) tuples)
        )
    DescentCoverLookupObstruction parent _failure ->
      intercalate
        "\n"
        [ "DescentCoverLookupObstruction",
          "  parent  = " <> prettyContext parent,
          "  failure = <context-lattice-lookup>"
        ]
    DescentClassSectionLookupObstruction contextValue _failure ->
      intercalate
        "\n"
        [ "DescentClassSectionLookupObstruction",
          "  context = " <> prettyContext contextValue,
          "  failure = <prepared-context-support-lookup>"
        ]
    DescentMeetLookupObstruction parent cover left right _failure ->
      intercalate
        "\n"
        [ "DescentMeetLookupObstruction",
          "  parent  = " <> prettyContext parent,
          "  cover   = " <> bracketed prettyContext cover,
          "  left    = " <> prettyContext left,
          "  right   = " <> prettyContext right,
          "  failure = <context-lattice-lookup>"
        ]
    DescentSupportLookupObstruction parent cover representative _failure ->
      intercalate
        "\n"
        [ "DescentSupportLookupObstruction",
          "  parent         = " <> prettyContext parent,
          "  cover          = " <> bracketed prettyContext cover,
          "  representative = " <> prettyRep representative,
          "  failure        = <prepared-context-support-lookup>"
        ]
    DescentJoinLookupObstruction left right _failure ->
      intercalate
        "\n"
        [ "DescentJoinLookupObstruction",
          "  left    = " <> prettyContext left,
          "  right   = " <> prettyContext right,
          "  failure = <context-lattice-lookup>"
        ]
    DescentVacuousCoverObstruction parent cover coordinates ->
      intercalate
        "\n"
        [ "DescentVacuousCoverObstruction",
          "  parent      = " <> prettyContext parent,
          "  cover       = " <> bracketed prettyContext cover,
          "  coordinates = " <> bracketed prettyInt (toList coordinates)
        ]
    DescentMonotonicityObstruction parent cover representative restricted coordinates ->
      intercalate
        "\n"
        [ "DescentMonotonicityObstruction",
          "  parent         = " <> prettyContext parent,
          "  cover          = " <> prettyContext cover,
          "  representative = " <> prettyRep representative,
          "  restricted     = " <> bracketed prettyRep restricted,
          "  coordinates    = " <> bracketed prettyInt coordinates
        ]

prettyObstruction ::
  (eq -> String) ->
  (node -> String) ->
  (rule -> String) ->
  (context -> String) ->
  (subst -> String) ->
  (stat -> String) ->
  (failure -> String) ->
  Obstruction eq node rule context subst stat failure ->
  String
prettyObstruction prettyEq prettyNode prettyRule prettyContext prettySubst prettyStat prettyFailure =
  \case
    StructuralMismatch dataValue ->
      intercalate
        "\n"
        ( [ "StructuralMismatch",
            "  left    = " <> prettyEq (smdLeft dataValue),
            "  right   = " <> prettyEq (smdRight dataValue),
            "  context = " <> prettyContext (smdContext dataValue),
            "  pairs:"
          ]
            <> indentLines 4 (renderList (prettyPair prettyNode prettyNode) (smdMismatchedNodes dataValue))
        )
    ConditionFailure dataValue ->
      intercalate
        "\n"
        [ "ConditionFailure",
          "  rule         = " <> prettyRule (cfdRule dataValue),
          "  context      = " <> prettyContext (cfdContext dataValue),
          "  substitution = " <> prettySubst (cfdSubstitution dataValue)
        ]
    EquivalenceLookupFailure dataValue ->
      intercalate
        "\n"
        [ "EquivalenceLookupFailure",
          "  left    = " <> prettyEq (elfdLeft dataValue),
          "  right   = " <> prettyEq (elfdRight dataValue),
          "  context = " <> prettyContext (elfdContext dataValue),
          "  failure = " <> prettyFailure (elfdFailure dataValue)
        ]
    ContextBarrier dataValue ->
      intercalate
        "\n"
        [ "ContextBarrier",
          "  left     = " <> prettyEq (cbdLeft dataValue),
          "  right    = " <> prettyEq (cbdRight dataValue),
          "  valid    = " <> bracketed prettyContext (cbdValidContexts dataValue),
          "  invalid  = " <> bracketed prettyContext (cbdInvalidContexts dataValue)
        ]
    RestrictionBarrier dataValue ->
      intercalate
        "\n"
        ( [ "RestrictionBarrier",
            "  left    = " <> prettyEq (rbdLeft dataValue),
            "  right   = " <> prettyEq (rbdRight dataValue),
            "  context = " <> prettyContext (rbdContext dataValue),
            "  holds-strictly-below diagnostics:"
          ]
            <> indentLines 4 (renderList prettyStat (rbdStats dataValue))
        )
    RestrictionLookupFailure dataValue ->
      intercalate
        "\n"
        [ "RestrictionLookupFailure",
          "  left    = " <> prettyEq (rlfdLeft dataValue),
          "  right   = " <> prettyEq (rlfdRight dataValue),
          "  context = " <> prettyContext (rlfdContext dataValue),
          "  failure = " <> prettyFailure (rlfdFailure dataValue)
        ]
    PropagationBarrier dataValue ->
      intercalate
        "\n"
        [ "PropagationBarrier",
          "  left    = " <> prettyEq (pbdLeft dataValue),
          "  right   = " <> prettyEq (pbdRight dataValue),
          "  context = " <> prettyContext (pbdContext dataValue),
          "  failure = " <> prettyFailure (pbdFailure dataValue)
        ]

renderBindingDecision :: BindingSubstitutionDecision Name -> String
renderBindingDecision decision =
  intercalate
    "\n"
    [ "BindingSubstitutionDecision",
      "  redex-path             = " <> prettyOpaque (bsdRedexPath decision),
      "  binder                 = " <> prettyName (bsdBinder decision),
      "  body-path              = " <> prettyOpaque (bsdBodyPath decision),
      "  argument-path          = " <> prettyOpaque (bsdArgumentPath decision),
      "  argument-free-binders  = " <> bracketed prettyName (toList (bsdArgumentFreeBinders decision)),
      "  body-capturing-binders = " <> bracketed prettyName (toList (bsdBodyCapturingBinders decision)),
      "  allowed                = " <> prettyBool (bsdAllowed decision)
    ]

prettySectionMismatch ::
  (classId -> String) ->
  (analysis -> String) ->
  SectionMismatch classId analysis ->
  String
prettySectionMismatch prettyClass prettyAnalysis =
  \case
    OnlyClass mismatch ->
      intercalate
        "\n"
        [ "OnlyClass",
          "  " <> prettyContextRestrictionMismatch prettyClass mismatch
        ]
    OnlyAnalysis mismatch ->
      intercalate
        "\n"
        [ "OnlyAnalysis",
          "  " <> prettyAnalysisRestrictionMismatch prettyAnalysis mismatch
        ]
    BothMismatch classMismatch analysisMismatch ->
      intercalate
        "\n"
        [ "BothMismatch",
          "  " <> prettyContextRestrictionMismatch prettyClass classMismatch,
          "  " <> prettyAnalysisRestrictionMismatch prettyAnalysis analysisMismatch
        ]

prettyContextRestrictionMismatch :: (classId -> String) -> ContextRestrictionMismatch classId -> String
prettyContextRestrictionMismatch prettyClass mismatch =
  "class-key="
    <> prettyInt (crmClassKey mismatch)
    <> "; expected="
    <> prettyMaybe prettyClass (crmExpectedRepresentative mismatch)
    <> "; actual="
    <> prettyMaybe prettyClass (crmActualRepresentative mismatch)

prettyAnalysisRestrictionMismatch :: (analysis -> String) -> AnalysisRestrictionMismatch analysis -> String
prettyAnalysisRestrictionMismatch prettyAnalysis mismatch =
  "class-key="
    <> prettyInt (armClassKey mismatch)
    <> "; expected="
    <> prettyMaybe prettyAnalysis (armExpectedAnalysis mismatch)
    <> "; actual="
    <> prettyMaybe prettyAnalysis (armActualAnalysis mismatch)

prettyPackedENode :: ENode (PackedNode ScopedLambdaSig) -> String
prettyPackedENode (ENode packed) =
  "Packed(" <> prettyLamLayerUnit (packedTag packed) <> "; children=" <> bracketed prettyClassId (toList packed) <> ")"

prettyLamLayerUnit :: LamF () -> String
prettyLamLayerUnit =
  \case
    LVar name ->
      "Var(" <> prettyName name <> ")"
    LLam name () ->
      "Lam(" <> prettyName name <> ")"
    LApp () () ->
      "App"
    LLit value ->
      "Lit(" <> prettyInt value <> ")"
    LAdd () () ->
      "Add"

prettyLamAnalysis :: LamAnalysis -> String
prettyLamAnalysis () =
  "()"

prettyClassId :: ClassId -> String
prettyClassId classId =
  "c" <> prettyInt (classIdKey classId)

prettyName :: Name -> String
prettyName name =
  nameString name

prettyCaptureCtx :: CaptureCtx -> String
prettyCaptureCtx =
  \case
    CaptureGlobal ->
      "CaptureGlobal"
    CaptureCtx rawName ->
      rawName

prettyVoid :: Void -> String
prettyVoid =
  absurd

prettyOpaque :: value -> String
prettyOpaque _ =
  "<opaque>"

prettyBool :: Bool -> String
prettyBool =
  \case
    True -> "true"
    False -> "false"

prettyInt :: Int -> String
prettyInt =
  show

prettyMaybe :: (value -> String) -> Maybe value -> String
prettyMaybe prettyValue =
  \case
    Nothing ->
      "none"
    Just value ->
      prettyValue value

prettyPair :: (left -> String) -> (right -> String) -> (left, right) -> String
prettyPair prettyLeft prettyRight (left, right) =
  "(" <> prettyLeft left <> ", " <> prettyRight right <> ")"

prettyIntMap :: (value -> String) -> IntMap value -> String
prettyIntMap prettyValue intMap =
  "{" <> intercalate ", " (fmap prettyEntry (IntMap.toAscList intMap)) <> "}"
  where
    prettyEntry (key, value) =
      prettyInt key <> " -> " <> prettyValue value

bracketed :: (value -> String) -> [value] -> String
bracketed prettyValue values =
  "[" <> intercalate ", " (fmap prettyValue values) <> "]"

renderList :: (value -> String) -> [value] -> [String]
renderList prettyValue values =
  case values of
    [] ->
      ["<none>"]
    _ ->
      zipWith renderItem [1 :: Int ..] values
  where
    renderItem index value =
      intercalate "\n" (("#" <> prettyInt index <> ":") : indentLines 2 (lines (prettyValue value)))

indentLines :: Int -> [String] -> [String]
indentLines width =
  fmap ((replicate width ' ') <>)
