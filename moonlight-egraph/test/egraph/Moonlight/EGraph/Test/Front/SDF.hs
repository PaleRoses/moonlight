{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.EGraph.Test.Front.SDF
  ( SDFSig,
    emptySDFGraph,
    sdfBudget,
    sdfCost,
    sphere,
    capsule,
    box,
    sdfUnion,
    sdfIntersect,
    sdfComplement,
    smoothUnion,
    sdfEmpty,
    sdfFull,
    latticeRules,
    complementRules,
    commutativityRules,
    smoothBlendRules,
    coarseApproximationRule,
    coarseApproximationRules,
    allRules,
    assertSDFEquivalent,
    assertSDFNotEquivalent,
    assertSDFExtractCost,
    runSDFExtractCost,
    runSDFSaturates,
  )
where

import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Moonlight.Core (emptyTheorySpec)
import Moonlight.EGraph.Pure.Context (emptyContextEGraphFromSite)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Moonlight.EGraph.Pure.Extraction (AnalysisCostAlgebra, ExtractionResult (..))
import Moonlight.EGraph.Pure.Saturation.Front
import Moonlight.EGraph.Pure.Saturation.Front.PackedNode
  ( PackedNode,
    packAnalysisSpec,
  )
import Moonlight.EGraph.Pure.Types (emptyEGraphWithTheory)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingContextEGraph,
    emptySaturatingContextEGraph,
  )
import Moonlight.EGraph.Test.Front.Mono
import Moonlight.EGraph.Test.SDF.Core qualified as RawSDF
import Moonlight.EGraph.Test.SDF.Core (Depth, SDFF (..))
import Moonlight.Rewrite.DSL (Node)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))
import Moonlight.Sheaf.Context.Site (UnitContextSiteOwner, unitPreparedContextSite)

type SDFSig = MonoSig SDFF

emptySDFGraph :: SaturatingContextEGraph UnitContextSiteOwner SurfaceKind (PackedNode SDFSig) Depth ()
emptySDFGraph =
  emptySaturatingContextEGraph $
    emptyContextEGraphFromSite unitPreparedContextSite $
      emptyEGraphWithTheory (packAnalysisSpec (monoAnalysisSpec RawSDF.depthAnalysis)) emptyTheorySpec

sdfBudget :: SaturationBudget
sdfBudget =
  SaturationBudget
    { sbMaxIterations = 10,
      sbMaxNodes = 100
    }

sdfCost :: AnalysisCostAlgebra (Node SDFSig) Depth Int
sdfCost =
  monoCostAlgebra RawSDF.sdfCost

sphere :: Double -> Term SDFSig "Expr"
sphere radius =
  monoNode (Sphere radius)

capsule :: Double -> Double -> Term SDFSig "Expr"
capsule radius height =
  monoNode (Capsule radius height)

box :: Double -> Double -> Double -> Term SDFSig "Expr"
box width height depth =
  monoNode (Box width height depth)

sdfUnion :: Term SDFSig "Expr" -> Term SDFSig "Expr" -> Term SDFSig "Expr"
sdfUnion left right =
  monoNode (SDFUnion left right)

sdfIntersect :: Term SDFSig "Expr" -> Term SDFSig "Expr" -> Term SDFSig "Expr"
sdfIntersect left right =
  monoNode (SDFIntersect left right)

sdfComplement :: Term SDFSig "Expr" -> Term SDFSig "Expr"
sdfComplement term =
  monoNode (Complement term)

smoothUnion :: Double -> Term SDFSig "Expr" -> Term SDFSig "Expr" -> Term SDFSig "Expr"
smoothUnion blend left right =
  monoNode (SmoothUnion blend left right)

sdfEmpty :: Term SDFSig "Expr"
sdfEmpty =
  monoNode SDFEmpty

sdfFull :: Term SDFSig "Expr"
sdfFull =
  monoNode SDFFull

latticeRules :: RulesetM SDFSig ()
latticeRules =
  sdfRules RawSDF.sdfLatticeLaws

complementRules :: RulesetM SDFSig ()
complementRules =
  sdfRules RawSDF.sdfComplementLaws

commutativityRules :: RulesetM SDFSig ()
commutativityRules =
  sdfRules RawSDF.sdfCommutativityLaws

smoothBlendRules :: RulesetM SDFSig ()
smoothBlendRules =
  sdfRules RawSDF.sdfSmoothBlendLaws

coarseApproximationRules :: RulesetM SDFSig ()
coarseApproximationRules =
  factRuleNamed
    (RawSDF.sdfFactLawName RawSDF.nonDegenerateRadiusFactLaw)
    nonDegenerateRadiusRelation
    (sdfLawTerm (RawSDF.sdfFactLawTerm RawSDF.nonDegenerateRadiusFactLaw))
    *> coarseApproximationRule

coarseApproximationRule :: RulesetM SDFSig ()
coarseApproximationRule =
  emitSDFLaw RawSDF.sdfCoarseApproximationLaw

allRules :: RulesetM SDFSig ()
allRules =
  sdfRules RawSDF.sdfGlobalLaws

sdfRules :: [RawSDF.SDFLaw] -> RulesetM SDFSig ()
sdfRules =
  traverse_ emitSDFLaw

emitSDFLaw :: RawSDF.SDFLaw -> RulesetM SDFSig ()
emitSDFLaw law =
  let lhs = sdfLawTerm (RawSDF.sdfLawLhs law)
      rhs = sdfLawTerm (RawSDF.sdfLawRhs law)
      body = lhs ==> rhs
      guardedBody =
        case RawSDF.sdfLawRequirement law of
          RawSDF.UnconditionalSDFLaw -> body
          RawSDF.RequiresNonDegenerateRadius ->
            body `when_` has nonDegenerateRadiusRelation lhs
   in rewriteNamed (RawSDF.sdfLawName law) guardedBody

sdfLawTerm :: RawSDF.SDFLawTerm -> Term SDFSig "Expr"
sdfLawTerm =
  RawSDF.foldSDFLawTerm sdfLawVariable monoNode

sdfLawVariable :: RawSDF.SDFLawVariable -> Term SDFSig "Expr"
sdfLawVariable =
  \case
    RawSDF.SDFLawX -> #x
    RawSDF.SDFLawY -> #y

nonDegenerateRadiusRelation :: RelationRef '["Expr"]
nonDegenerateRadiusRelation =
  relationRefWithFactId
    "non-degenerate-radius"
    RawSDF.nonDegenerateRadiusFactId

assertSDFEquivalent :: RulesetM SDFSig () -> Term SDFSig "Expr" -> Term SDFSig "Expr" -> Assertion
assertSDFEquivalent selectedRules left right = do
  report <- expectFront (runEGraphFront (equivalenceProgram selectedRules left right) emptySDFGraph)
  efrResult report @?= True

assertSDFNotEquivalent :: RulesetM SDFSig () -> Term SDFSig "Expr" -> Term SDFSig "Expr" -> Assertion
assertSDFNotEquivalent selectedRules left right = do
  report <- expectFront (runEGraphFront (equivalenceProgram selectedRules left right) emptySDFGraph)
  efrResult report @?= False

assertSDFExtractCost :: String -> Term SDFSig "Expr" -> (Int -> Assertion) -> Assertion
assertSDFExtractCost label term checkCost = do
  report <- expectFront (runEGraphFront (extractionProgram term) emptySDFGraph)
  case efrResult report of
    Just extractionResult -> checkCost (erCost extractionResult)
    Nothing -> assertFailure ("expected extraction result for " <> label)

runSDFExtractCost :: Term SDFSig "Expr" -> Either String (Maybe Int)
runSDFExtractCost term =
  fmap (fmap erCost . efrResult) $
    first frontErrorMessage $
      runEGraphFront (extractionProgram term) emptySDFGraph

runSDFSaturates :: RulesetM SDFSig () -> Term SDFSig "Expr" -> Either String ()
runSDFSaturates selectedRules term =
  fmap (const ()) $
    first frontErrorMessage $
      runEGraphFront (saturationProgram selectedRules term) emptySDFGraph

saturationProgram :: RulesetM SDFSig () -> Term SDFSig "Expr" -> EGraphFront 'Authored UnitContextSiteOwner SDFSig Depth () ()
saturationProgram selectedRules term =
  egraph $ do
    rules <- ruleset @"sdf" selectedRules
    _ <- def @"start" term

    run $
      runFor sdfBudget rules

    pure done

equivalenceProgram :: RulesetM SDFSig () -> Term SDFSig "Expr" -> Term SDFSig "Expr" -> EGraphFront 'Authored UnitContextSiteOwner SDFSig Depth () Bool
equivalenceProgram selectedRules left right =
  egraph $ do
    rules <- ruleset @"sdf" selectedRules
    lhs <- def @"lhs" left

    run $
      runUntil (lhs === right) $
        runFor sdfBudget rules

    check @"equivalent" (lhs === right)

extractionProgram :: Term SDFSig "Expr" -> EGraphFront 'Authored UnitContextSiteOwner SDFSig Depth () (Maybe (ExtractionResult (Node SDFSig) Int))
extractionProgram term =
  egraph $ do
    rules <- ruleset @"sdf" allRules
    start <- def @"start" term

    run $
      runFor sdfBudget rules

    extract @"best" sdfCost start

expectFront :: Either (EGraphFrontError UnitContextSiteOwner SDFSig Depth ()) value -> IO value
expectFront =
  \case
    Right value -> pure value
    Left err -> assertFailure (frontErrorMessage err)
