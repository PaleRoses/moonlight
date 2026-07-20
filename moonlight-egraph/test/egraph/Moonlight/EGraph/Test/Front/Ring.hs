{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.EGraph.Test.Front.Ring
  ( RingSig,
    emptyRingGraph,
    frontRingCost,
    viewFrontRingTerm,
    rVar,
    rAdd,
    rMul,
    rNeg,
    rZero,
    rOne,
    ringIdentityRules,
    ringAnnihilationRules,
    ringNegationRules,
    ringDistributionRules,
    ringExplosionRules,
    ringSaturationRules,
    RingExtraction,
    RingExtractionRun (..),
    RingSaturationReport,
    runRingExtraction,
    runRingSaturation,
    expectFront,
  )
where

import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Moonlight.Core (emptyTheorySpec)
import Moonlight.EGraph.Pure.Context (emptyContextEGraphFromSite)
import Moonlight.EGraph.Pure.Context (cegBase)
import Moonlight.EGraph.Pure.Extraction
  ( AnalysisCostAlgebra,
    ExtractionWorkBudget (..),
    ExtractionResult,
    extractWithAnalysisBounded,
    stableExtractionSnapshotFromEGraph,
  )
import Moonlight.EGraph.Pure.Saturation.Front
  ( EGraphFront,
    EGraphFrontError,
    EGraphFrontReport (..),
    FrontPhase (Authored),
    RulesetM,
    SaturationBudget,
    Term,
    def,
    done,
    egraph,
    frontErrorMessage,
    rewrite,
    ruleset,
    run,
    runEGraphFront,
    runFor,
    (==>),
  )
import Moonlight.EGraph.Pure.Saturation.Front.PackedNode
  ( PackedNode,
    packAnalysisCostAlgebra,
    packAnalysisSpec,
    unpackExtractionResult,
  )
import Moonlight.EGraph.Pure.Saturation.Logic.Run (EGraphLogicReport (elrSaturation))
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.EGraph.Pure.Types (emptyEGraphWithTheory)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingContextEGraph,
    emptySaturatingContextEGraph,
    sceContextGraph,
  )
import Moonlight.EGraph.Test.Front.Mono
  ( MonoSig,
    monoAnalysisSpec,
    monoCostAlgebra,
    monoExtractTerm,
    monoNode,
  )
import Moonlight.EGraph.Test.Ring.Core
  ( NodeCount,
    RingF,
    RingTermView,
  )
import Moonlight.EGraph.Test.Ring.Core qualified as RawRing
import Moonlight.EGraph.Test.Saturation (SaturationReport)
import Data.Fix (Fix)
import Moonlight.Rewrite.DSL (Node)
import Test.Tasty.HUnit (assertFailure)
import Data.Map.Strict qualified as Map
import Moonlight.Sheaf.Context.Site (UnitContextSiteOwner, unitPreparedContextSite)

type RingSig =
  MonoSig RingF

emptyRingGraph :: SaturatingContextEGraph UnitContextSiteOwner SurfaceKind (PackedNode RingSig) NodeCount ()
emptyRingGraph =
  emptySaturatingContextEGraph $
    emptyContextEGraphFromSite unitPreparedContextSite $
      emptyEGraphWithTheory (packAnalysisSpec (monoAnalysisSpec RawRing.ringAnalysis)) emptyTheorySpec

frontRingCost :: AnalysisCostAlgebra (Node RingSig) NodeCount Int
frontRingCost =
  monoCostAlgebra RawRing.ringCost

viewFrontRingTerm :: Fix (Node RingSig) -> RingTermView
viewFrontRingTerm =
  RawRing.viewRingTerm . monoExtractTerm

rVar :: String -> Term RingSig "Expr"
rVar name =
  monoNode (RawRing.Var name)

rAdd :: Term RingSig "Expr" -> Term RingSig "Expr" -> Term RingSig "Expr"
rAdd left right =
  monoNode (RawRing.Add left right)

rMul :: Term RingSig "Expr" -> Term RingSig "Expr" -> Term RingSig "Expr"
rMul left right =
  monoNode (RawRing.Mul left right)

rNeg :: Term RingSig "Expr" -> Term RingSig "Expr"
rNeg term =
  monoNode (RawRing.Neg term)

rZero :: Term RingSig "Expr"
rZero =
  monoNode RawRing.RZero

rOne :: Term RingSig "Expr"
rOne =
  monoNode RawRing.ROne

ringIdentityRules :: RulesetM RingSig ()
ringIdentityRules = do
  rewrite @"add-zero-right" $
    rAdd #x rZero ==> #x
  rewrite @"add-zero-left" $
    rAdd rZero #x ==> #x
  rewrite @"mul-one-right" $
    rMul #x rOne ==> #x
  rewrite @"mul-one-left" $
    rMul rOne #x ==> #x

ringAnnihilationRules :: RulesetM RingSig ()
ringAnnihilationRules = do
  rewrite @"mul-zero-right" $
    rMul #x rZero ==> rZero
  rewrite @"mul-zero-left" $
    rMul rZero #x ==> rZero

ringNegationRules :: RulesetM RingSig ()
ringNegationRules = do
  rewrite @"double-neg" $
    rNeg (rNeg #x) ==> #x
  rewrite @"add-neg-self" $
    rAdd #x (rNeg #x) ==> rZero

ringDistributionRules :: RulesetM RingSig ()
ringDistributionRules = do
  rewrite @"distribute-left" $
    rMul #a (rAdd #b #c) ==> rAdd (rMul #a #b) (rMul #a #c)
  rewrite @"distribute-right" $
    rMul (rAdd #a #b) #c ==> rAdd (rMul #a #c) (rMul #b #c)

ringCommutativityRules :: RulesetM RingSig ()
ringCommutativityRules = do
  rewrite @"add-commute" $
    rAdd #a #b ==> rAdd #b #a
  rewrite @"mul-commute" $
    rMul #a #b ==> rMul #b #a

ringExplosionRules :: RulesetM RingSig ()
ringExplosionRules = do
  ringCommutativityRules
  ringDistributionRules

ringSaturationRules :: RulesetM RingSig ()
ringSaturationRules = do
  ringDistributionRules
  ringIdentityRules
  ringAnnihilationRules
  ringNegationRules

type RingExtraction =
  ExtractionResult (Node RingSig) Int

data RingExtractionRun = RingExtractionRun
  { rerExtraction :: !(Maybe RingExtraction),
    rerSaturation :: !RingSaturationReport
  }

type RingSaturationReport =
  SaturationReport (EGraphU UnitContextSiteOwner SurfaceKind (PackedNode RingSig) NodeCount ())

runRingSaturation :: SaturationBudget -> RulesetM RingSig () -> Term RingSig "Expr" -> IO RingSaturationReport
runRingSaturation budget selectedRules term = do
  frontReport <- expectFront (runEGraphFront (saturationProgram budget selectedRules term) emptyRingGraph)
  singleSaturationReport frontReport

runRingExtraction :: SaturationBudget -> RulesetM RingSig () -> Term RingSig "Expr" -> IO RingExtractionRun
runRingExtraction budget selectedRules term = do
  frontReport <- expectFront (runEGraphFront (saturationProgram budget selectedRules term) emptyRingGraph)
  RingExtractionRun
    <$> boundedRingExtraction frontReport
    <*> singleSaturationReport frontReport

saturationProgram :: SaturationBudget -> RulesetM RingSig () -> Term RingSig "Expr" -> RingFrontProgram ()
saturationProgram budget selectedRules term =
  egraph $ do
    selected <- ruleset @"ring" selectedRules
    _ <- def @"start" term
    run (runFor budget selected)
    pure done

boundedRingExtraction :: EGraphFrontReport UnitContextSiteOwner RingSig NodeCount () () -> IO (Maybe RingExtraction)
boundedRingExtraction frontReport =
  case Map.elems (efrSeedClasses frontReport) of
    [startClass] ->
      case stableExtractionSnapshotFromEGraph (cegBase (sceContextGraph (efrFinalGraph frontReport))) of
        Nothing ->
          assertFailure "expected stable ring extraction snapshot"
        Just snapshot ->
          case extractWithAnalysisBounded ringExtractionBudget (packAnalysisCostAlgebra frontRingCost) startClass snapshot of
            Left convergenceReport ->
              assertFailure ("bounded ring extraction did not converge: " <> show convergenceReport)
            Right extractionResult ->
              pure (fmap unpackExtractionResult extractionResult)
    seedClasses ->
      assertFailure ("expected exactly one ring seed class, got " <> show (length seedClasses))

ringExtractionBudget :: ExtractionWorkBudget
ringExtractionBudget =
  ExtractionWorkBudget 4096

singleSaturationReport :: EGraphFrontReport UnitContextSiteOwner RingSig NodeCount () result -> IO RingSaturationReport
singleSaturationReport frontReport =
  case efrScheduleReports frontReport of
    [logicReport] -> pure (elrSaturation logicReport)
    reports -> assertFailure ("expected exactly one ring schedule report, got " <> show (length reports))

type RingFrontProgram result =
  EGraphFront 'Authored UnitContextSiteOwner RingSig NodeCount () result

expectFront :: Either (EGraphFrontError UnitContextSiteOwner RingSig NodeCount ()) value -> IO value
expectFront =
  \case
    Right value -> pure value
    Left err -> assertFailure (frontErrorMessage err)
