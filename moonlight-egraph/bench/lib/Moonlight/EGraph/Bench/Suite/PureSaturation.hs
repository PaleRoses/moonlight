{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Bench.Suite.PureSaturation
  ( pureSaturationBenchmarks,
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( ClassId,
    Pattern (..),
    RewriteRuleId (..),
    Substitution,
    emptyTheorySpec,
  )
import Moonlight.Core qualified as EGraph
import Moonlight.Delta.Scope qualified as Delta
import Moonlight.EGraph.Bench.Corpus
  ( BenchRingSig,
    activateContextMerges,
    arithAddXXPattern,
    arithAddXYPattern,
    benchMonoAnalysisSpec,
    benchMonoNode,
    buildArithGraph,
    caseLabel,
    mergeableAnatomyRegions,
    requireMergePairs,
  )
import Moonlight.EGraph.Bench.Harness.Digest (contextGraphDigest)
import Moonlight.EGraph.Bench.Harness.Run (requireRight)
import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    emptyContextEGraphFromSite,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Saturation.Front
  ( CompiledEGraphFront,
    EGraphFront,
    EGraphFrontReport (..),
    FrontPhase (Authored),
    RulesetM,
    SaturationBudget (..),
    Term,
    compileEGraphFront,
    def,
    done,
    egraph,
    frontSeedTermNamed,
    rewrite,
    ruleset,
    run,
    runCompiledEGraphFront,
    runEGraphFront,
    runFor,
    (==>),
  )
import Moonlight.EGraph.Pure.Saturation.Front.PackedNode
  ( PackedNode,
    packAnalysisSpec,
  )
import Moonlight.EGraph.Pure.Saturation.Logic.Run (EGraphLogicReport (..))
import Moonlight.EGraph.Pure.Saturation.Matching (MatchingStrategy (GenericJoinMatching))
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.EGraph.Pure.Types (emptyEGraphWithTheory)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingContextEGraph,
    emptySaturatingContextEGraph,
    sceContextGraph,
  )
import Moonlight.EGraph.Test.Arith.Core qualified as Arith
import Moonlight.EGraph.Test.Context.Anatomy
  ( AnatomyRegion (..),
    coarseAnatomyLattice,
  )
import Moonlight.EGraph.Test.Ring.Core qualified as Ring
import Moonlight.EGraph.Test.Saturation (srMatchesApplied)
import Moonlight.FiniteLattice
  ( supportGenerators,
  )
import Moonlight.Rewrite.ProofContext (SupportedRewriteMatch (..))
import Moonlight.Rewrite.Runtime (emptyRewriteRuntimeCapabilities)
import Moonlight.Rewrite.Runtime (ExecutableRewriteMatch (..))
import Moonlight.Rewrite.Runtime (RulePlan, rpId)
import Moonlight.Rewrite.System
  ( CompiledGuard,
    GuardCapabilityResolver (..),
    RewriteCondition (..),
    guardTrue,
  )
import Moonlight.Rewrite.System
  ( FactDerivationIndex,
    emptyFactDerivationIndex,
  )
import Moonlight.Rewrite.System
  ( FactStore,
    emptyFactStore,
  )
import Moonlight.Rewrite.System (RawRewriteRule (..))
import Moonlight.Saturation.Substrate
  ( SatMatchState,
    compileRewriteRules,
    contextSupportedMatchesPrepared,
    initialMatchState,
    materializeRawMatchesAtContextView,
    mergeSupportedMatch,
    rawContextMatchesPrepared,
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    UnitContextSiteOwner,
    unitPreparedContextSite,
    withPreparedContextSiteFromFiniteLattice,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )

pureSaturationBenchmarks :: Benchmark
pureSaturationBenchmarks =
  bgroup
    "pure-saturation"
    [ bgroup "supported-emission" contextSupportedEmissionBenches,
      bgroup "front-ring" saturationBenches
    ]

type ArithEmissionRulePlan =
  RulePlan (CompiledGuard SurfaceKind Arith.ArithF) Arith.ArithF

type SupportedEmissionU owner =
  EGraphU owner SurfaceKind Arith.ArithF Arith.NodeCount AnatomyRegion

type SupportedEmissionMatchState owner =
  SatMatchState (SupportedEmissionU owner)

data SupportedEmissionFixture owner = SupportedEmissionFixture
  { sefContextGraph :: !(ContextEGraph owner Arith.ArithF Arith.NodeCount AnatomyRegion),
    sefActiveContexts :: ![AnatomyRegion],
    sefContextInputs :: !(Map.Map AnatomyRegion (FactStore, FactDerivationIndex, [ArithEmissionRulePlan])),
    sefRules :: ![ArithEmissionRulePlan],
    sefVariantRule :: !ArithEmissionRulePlan
  }

instance NFData (SupportedEmissionFixture owner) where
  rnf fixture =
    contextGraphDigest (sefContextGraph fixture)
      `seq` length (sefActiveContexts fixture)
      `seq` Map.size (sefContextInputs fixture)
      `seq` length (sefRules fixture)
      `seq` supportedEmissionRegionNativeDigest fixture
      `seq` supportedEmissionPerContextOracleDigest fixture
      `seq` ()

contextSupportedEmissionBenches :: [Benchmark]
contextSupportedEmissionBenches =
  concatMap
    ( \gridPoint ->
        [ supportedEmissionBench "region-native" supportedEmissionRegionNativeDigest gridPoint,
          supportedEmissionBench "per-context-oracle" supportedEmissionPerContextOracleDigest gridPoint
        ]
    )
    ((,) <$> [1, 2, 4, 8] <*> [1000, 4000])

supportedEmissionBench ::
  String ->
  (forall owner. SupportedEmissionFixture owner -> Int) ->
  (Int, Int) ->
  Benchmark
supportedEmissionBench label digest (contextCount, termCount) =
  withPreparedContextSiteFromFiniteLattice coarseAnatomyLattice $ \site ->
    env (prepareSupportedEmissionFixture site contextCount termCount) $ \fixture ->
      bench
        (label <> "/" <> caseLabel [("K", contextCount), ("N", termCount)])
        (nf digest fixture)

prepareSupportedEmissionFixture ::
  PreparedContextSite owner AnatomyRegion ->
  Int ->
  Int ->
  IO (SupportedEmissionFixture owner)
prepareSupportedEmissionFixture site contextCount termCount = do
  (baseGraph, _classIds) <- requireRight "supported emission graph allocation" (buildArithGraph termCount)
  let activeContexts =
        take contextCount mergeableAnatomyRegions
  childClassIds <-
    requireRight "supported emission class allocation" $
      traverse
        (\index -> fst <$> addTerm (Arith.numTerm index) baseGraph)
        [0 .. 2 * contextCount - 1]
  mergePairs <- requireMergePairs "supported emission" contextCount childClassIds
  contextGraph <-
    requireRight
      "supported emission activation"
      (activateContextMerges mergePairs (emptyContextEGraphFromSite site baseGraph))
  (rules, variantRule) <- compileSupportedEmissionRules
  contextInputs <- supportedEmissionContextInputs activeContexts rules
  let fixture =
        SupportedEmissionFixture
          { sefContextGraph = contextGraph,
            sefActiveContexts = activeContexts,
            sefContextInputs = contextInputs,
            sefRules = rules,
            sefVariantRule = variantRule
          }
  totalRawCounts <- supportedEmissionRawCounts (sefRules fixture) fixture
  variantRawCounts <- supportedEmissionRawCounts [sefVariantRule fixture] fixture
  putStrLn
    ( "context-supported-emission K="
        <> show contextCount
        <> " N="
        <> show termCount
        <> ": active contexts "
        <> show activeContexts
        <> ", witness contexts "
        <> show (Map.keys contextInputs)
        <> ", raw matches per active context "
        <> show totalRawCounts
        <> ", same-child variant matches per active context "
        <> show variantRawCounts
        <> ", region-native digest "
        <> show (supportedEmissionRegionNativeDigest fixture)
        <> ", per-context oracle digest "
        <> show (supportedEmissionPerContextOracleDigest fixture)
    )
  evaluate
    ( supportedEmissionRegionNativeDigest fixture
        + supportedEmissionPerContextOracleDigest fixture
    )
    *> pure fixture

supportedEmissionContextInputs ::
  [AnatomyRegion] ->
  [ArithEmissionRulePlan] ->
  IO (Map.Map AnatomyRegion (FactStore, FactDerivationIndex, [ArithEmissionRulePlan]))
supportedEmissionContextInputs activeContexts rules = do
  let activeInputs =
        Map.fromList
          [ (contextValue, (emptyFactStore, emptyFactDerivationIndex, rules))
          | contextValue <- activeContexts
          ]
      globalInput =
        Map.singleton Whole (emptyFactStore, emptyFactDerivationIndex, rules)
  pure (Map.union activeInputs globalInput)

compileSupportedEmissionRules :: IO ([ArithEmissionRulePlan], ArithEmissionRulePlan)
compileSupportedEmissionRules = do
  addAnyRule <- compileSupportedEmissionRule (RewriteRuleId 9401) arithAddXYPattern Nothing
  addSameChildRule <- compileSupportedEmissionRule (RewriteRuleId 9402) arithAddXXPattern Nothing
  guardedAddAnyRule <- compileSupportedEmissionRule (RewriteRuleId 9403) arithAddXYPattern (Just (RewriteCondition guardTrue))
  pure ([addAnyRule, addSameChildRule, guardedAddAnyRule], addSameChildRule)

compileSupportedEmissionRule ::
  RewriteRuleId ->
  Pattern Arith.ArithF ->
  Maybe (RewriteCondition SurfaceKind Arith.ArithF) ->
  IO ArithEmissionRulePlan
compileSupportedEmissionRule ruleId patternValue conditionValue =
  case compileRewriteRules @(EGraphU UnitContextSiteOwner SurfaceKind Arith.ArithF Arith.NodeCount AnatomyRegion) [rawRule] of
    Right [compiledRule] ->
      pure compiledRule
    Right compiledRules ->
      fail ("supported emission rule expected one compiled rule, got " <> show (length compiledRules))
    Left compileError ->
      fail ("supported emission rule failed to compile: " <> show compileError)
  where
    rawRule =
      RawRewriteRule
        { rrId = ruleId,
          rrLhs = patternValue,
          rrRhs = PatternVar (EGraph.mkPatternVar 0),
          rrCondition = conditionValue,
          rrApplicationCondition = Nothing,
          rrPostSubst = Nothing
        }

supportedEmissionCapabilityResolver :: GuardCapabilityResolver SurfaceKind
supportedEmissionCapabilityResolver =
  GuardCapabilityResolver (\_ _ -> True)

supportedEmissionStartingState :: forall owner. SupportedEmissionMatchState owner
supportedEmissionStartingState =
  initialMatchState @(SupportedEmissionU owner)
    GenericJoinMatching
    emptyRewriteRuntimeCapabilities

supportedEmissionGraph ::
  SupportedEmissionFixture owner ->
  SaturatingContextEGraph owner SurfaceKind Arith.ArithF Arith.NodeCount AnatomyRegion
supportedEmissionGraph =
  emptySaturatingContextEGraph . sefContextGraph

supportedEmissionRegionNativeDigest :: forall owner. SupportedEmissionFixture owner -> Int
supportedEmissionRegionNativeDigest fixture =
  either
    (const (-1))
    (supportedEmissionMatchesDigest . snd)
    ( contextSupportedMatchesPrepared @(SupportedEmissionU owner)
        emptyRewriteRuntimeCapabilities
        supportedEmissionCapabilityResolver
        0
        Delta.fullDelta
        (supportedEmissionGraph fixture)
        (sefContextInputs fixture)
        []
        supportedEmissionStartingState
    )

supportedEmissionPerContextOracleDigest :: SupportedEmissionFixture owner -> Int
supportedEmissionPerContextOracleDigest fixture =
  either
    (const (-1))
    (supportedEmissionMatchesDigest . Map.elems . snd)
    ( foldM
        (supportedEmissionOracleStep fixture)
        (supportedEmissionStartingState, Map.empty)
        (Map.toAscList (sefContextInputs fixture))
    )

supportedEmissionOracleStep ::
  forall owner.
  SupportedEmissionFixture owner ->
  ( SupportedEmissionMatchState owner,
    Map.Map (RewriteRuleId, ClassId, Substitution) (SupportedRewriteMatch AnatomyRegion SurfaceKind Arith.ArithF)
  ) ->
  (AnatomyRegion, (FactStore, FactDerivationIndex, [ArithEmissionRulePlan])) ->
  Either
    ()
    ( SupportedEmissionMatchState owner,
      Map.Map (RewriteRuleId, ClassId, Substitution) (SupportedRewriteMatch AnatomyRegion SurfaceKind Arith.ArithF)
    )
supportedEmissionOracleStep fixture (matchState, supportedMap) (contextValue, (factStore, factIndex, rules)) = do
  (nextState, rawMatches) <-
    first
      (const ())
      ( rawContextMatchesPrepared @(SupportedEmissionU owner)
          emptyRewriteRuntimeCapabilities
          contextValue
          0
          Delta.fullDelta
          (supportedEmissionGraph fixture)
          factStore
          factIndex
          rules
          matchState
      )
  supportedMatches <-
    first
      (const ())
      ( materializeRawMatchesAtContextView @(SupportedEmissionU owner)
          emptyRewriteRuntimeCapabilities
          supportedEmissionCapabilityResolver
          contextValue
          factStore
          factIndex
          (supportedEmissionGraph fixture)
          rawMatches
      )
  nextMap <-
    foldM
      (supportedEmissionInsertOracleMatch fixture)
      supportedMap
      supportedMatches
  pure (nextState, nextMap)

supportedEmissionInsertOracleMatch ::
  SupportedEmissionFixture owner ->
  Map.Map (RewriteRuleId, ClassId, Substitution) (SupportedRewriteMatch AnatomyRegion SurfaceKind Arith.ArithF) ->
  SupportedRewriteMatch AnatomyRegion SurfaceKind Arith.ArithF ->
  Either () (Map.Map (RewriteRuleId, ClassId, Substitution) (SupportedRewriteMatch AnatomyRegion SurfaceKind Arith.ArithF))
supportedEmissionInsertOracleMatch fixture supportedMap supportedMatch =
  Map.alterF
    (supportedEmissionMergeOracleMatch fixture supportedMatch)
    (supportedEmissionMatchKey supportedMatch)
    supportedMap

supportedEmissionMergeOracleMatch ::
  forall owner.
  SupportedEmissionFixture owner ->
  SupportedRewriteMatch AnatomyRegion SurfaceKind Arith.ArithF ->
  Maybe (SupportedRewriteMatch AnatomyRegion SurfaceKind Arith.ArithF) ->
  Either () (Maybe (SupportedRewriteMatch AnatomyRegion SurfaceKind Arith.ArithF))
supportedEmissionMergeOracleMatch fixture candidate existing =
  case existing of
    Nothing ->
      Right (Just candidate)
    Just held ->
      Just
        <$> first
          (const ())
          (mergeSupportedMatch @(SupportedEmissionU owner) (supportedEmissionGraph fixture) held candidate)

supportedEmissionMatchKey ::
  SupportedRewriteMatch context capability f ->
  (RewriteRuleId, ClassId, Substitution)
supportedEmissionMatchKey supportedMatch =
  let rewriteMatch =
        srmMatch supportedMatch
   in (rpId (ermRule rewriteMatch), ermRootClass rewriteMatch, ermSubstitution rewriteMatch)

supportedEmissionMatchesDigest ::
  [SupportedRewriteMatch AnatomyRegion SurfaceKind Arith.ArithF] ->
  Int
supportedEmissionMatchesDigest matches =
  length matches
    + foldl'
      (\total supportedMatch -> total + length (supportGenerators (srmSupport supportedMatch)))
      0
      matches

supportedEmissionRawCounts ::
  [ArithEmissionRulePlan] ->
  SupportedEmissionFixture owner ->
  IO [(AnatomyRegion, Int)]
supportedEmissionRawCounts rules fixture =
  traverse
    ( \contextValue ->
        fmap
          (\rawCount -> (contextValue, rawCount))
          (supportedEmissionRawCountAt rules fixture contextValue)
    )
    (sefActiveContexts fixture)

supportedEmissionRawCountAt ::
  forall owner.
  [ArithEmissionRulePlan] ->
  SupportedEmissionFixture owner ->
  AnatomyRegion ->
  IO Int
supportedEmissionRawCountAt rules fixture contextValue =
  fmap (length . snd) $
    requireRight
      ("supported emission raw count at " <> show contextValue)
      ( rawContextMatchesPrepared @(SupportedEmissionU owner)
          emptyRewriteRuntimeCapabilities
          contextValue
          0
          Delta.fullDelta
          (supportedEmissionGraph fixture)
          emptyFactStore
          emptyFactDerivationIndex
          rules
          supportedEmissionStartingState
      )

saturationBenches :: [Benchmark]
saturationBenches =
  saturationCases >>= \benchCase -> [saturationBench benchCase, saturationCompiledBench benchCase]

saturationCases :: [(String, SaturationBudget, RulesetM BenchRingSig (), Term BenchRingSig "Expr")]
saturationCases =
  [ ("identity-nested", SaturationBudget 8 160, ringIdentityRules, nestedIdentityTerm),
    ("distribution-2x2", SaturationBudget 10 260, ringSaturationRules, abcdProduct),
    ("distribution-3x2", SaturationBudget 12 520, ringSaturationRules, tripleProduct)
  ]

saturationBench ::
  (String, SaturationBudget, RulesetM BenchRingSig (), Term BenchRingSig "Expr") ->
  Benchmark
saturationBench (label, budget, selectedRules, term) =
  bench (label <> "/authored-cold") (nf (runRingSaturationDigest budget selectedRules) term)

saturationCompiledBench ::
  (String, SaturationBudget, RulesetM BenchRingSig (), Term BenchRingSig "Expr") ->
  Benchmark
saturationCompiledBench (label, budget, selectedRules, term) =
  env (prepareCompiledRingFrontFixture budget selectedRules term) $ \fixture ->
    bench (label <> "/compiled-warm") (nf runCompiledRingSaturationDigest fixture)


runRingSaturationDigest ::
  SaturationBudget ->
  RulesetM BenchRingSig () ->
  Term BenchRingSig "Expr" ->
  Int
runRingSaturationDigest budget selectedRules term =
  either
    (const (-1))
    ringFrontReportDigest
    (runEGraphFront (ringSaturationProgram budget selectedRules term) emptyRingFrontGraph)

data CompiledRingFrontFixture = CompiledRingFrontFixture
  { crfCompiled :: !(CompiledEGraphFront UnitContextSiteOwner BenchRingSig Ring.NodeCount () ()),
    crfTerm :: !(Term BenchRingSig "Expr")
  }

instance NFData CompiledRingFrontFixture where
  rnf fixture =
    runCompiledRingSaturationDigest fixture `seq` ()

prepareCompiledRingFrontFixture ::
  SaturationBudget ->
  RulesetM BenchRingSig () ->
  Term BenchRingSig "Expr" ->
  IO CompiledRingFrontFixture
prepareCompiledRingFrontFixture budget selectedRules term =
  either
    (const (fail "compiled ring front fixture failed to compile"))
    (\compiled -> pure (CompiledRingFrontFixture compiled term))
    (compileEGraphFront (ringSaturationProgram budget selectedRules term))

runCompiledRingSaturationDigest :: CompiledRingFrontFixture -> Int
runCompiledRingSaturationDigest fixture =
  either
    (const (-1))
    ringFrontReportDigest
    ( runCompiledEGraphFront
        (crfCompiled fixture)
        emptyRingFrontGraph
        [frontSeedTermNamed "start" (crfTerm fixture)]
    )

ringSaturationProgram ::
  SaturationBudget ->
  RulesetM BenchRingSig () ->
  Term BenchRingSig "Expr" ->
  EGraphFront 'Authored UnitContextSiteOwner BenchRingSig Ring.NodeCount () ()
ringSaturationProgram budget selectedRules term =
  egraph $ do
    selected <- ruleset @"ring" selectedRules
    _ <- def @"start" term
    run (runFor budget selected)
    pure done

ringFrontReportDigest ::
  EGraphFrontReport UnitContextSiteOwner BenchRingSig Ring.NodeCount () () ->
  Int
ringFrontReportDigest report =
  contextGraphDigest (sceContextGraph (efrFinalGraph report))
    + foldl' (\count logicReport -> count + srMatchesApplied (elrSaturation logicReport)) 0 (efrScheduleReports report)

emptyRingFrontGraph :: SaturatingContextEGraph UnitContextSiteOwner SurfaceKind (PackedNode BenchRingSig) Ring.NodeCount ()
emptyRingFrontGraph =
  emptySaturatingContextEGraph $
    emptyContextEGraphFromSite unitPreparedContextSite $
      emptyEGraphWithTheory (packAnalysisSpec (benchMonoAnalysisSpec Ring.ringAnalysis)) emptyTheorySpec

ringIdentityRules :: RulesetM BenchRingSig ()
ringIdentityRules = do
  rewrite @"add-zero-right" $
    rAdd #x rZero ==> #x
  rewrite @"add-zero-left" $
    rAdd rZero #x ==> #x
  rewrite @"mul-one-right" $
    rMul #x rOne ==> #x
  rewrite @"mul-one-left" $
    rMul rOne #x ==> #x

ringAnnihilationRules :: RulesetM BenchRingSig ()
ringAnnihilationRules = do
  rewrite @"mul-zero-right" $
    rMul #x rZero ==> rZero
  rewrite @"mul-zero-left" $
    rMul rZero #x ==> rZero

ringNegationRules :: RulesetM BenchRingSig ()
ringNegationRules = do
  rewrite @"double-neg" $
    rNeg (rNeg #x) ==> #x
  rewrite @"add-neg-self" $
    rAdd #x (rNeg #x) ==> rZero

ringDistributionRules :: RulesetM BenchRingSig ()
ringDistributionRules = do
  rewrite @"distribute-left" $
    rMul #a (rAdd #b #c) ==> rAdd (rMul #a #b) (rMul #a #c)
  rewrite @"distribute-right" $
    rMul (rAdd #a #b) #c ==> rAdd (rMul #a #c) (rMul #b #c)

ringSaturationRules :: RulesetM BenchRingSig ()
ringSaturationRules = do
  ringDistributionRules
  ringIdentityRules
  ringAnnihilationRules
  ringNegationRules

nestedIdentityTerm :: Term BenchRingSig "Expr"
nestedIdentityTerm =
  rMul
    (rMul (rAdd (rVar "a") rZero) (rMul rOne (rVar "b")))
    (rAdd (rVar "c") (rVar "d"))

abcdProduct :: Term BenchRingSig "Expr"
abcdProduct =
  rMul
    (rAdd (rVar "a") (rVar "b"))
    (rAdd (rVar "c") (rVar "d"))

tripleProduct :: Term BenchRingSig "Expr"
tripleProduct =
  rMul abcdProduct (rAdd (rVar "e") (rVar "f"))

rVar :: String -> Term BenchRingSig "Expr"
rVar =
  benchMonoNode . Ring.Var

rAdd :: Term BenchRingSig "Expr" -> Term BenchRingSig "Expr" -> Term BenchRingSig "Expr"
rAdd leftTerm rightTerm =
  benchMonoNode (Ring.Add leftTerm rightTerm)

rMul :: Term BenchRingSig "Expr" -> Term BenchRingSig "Expr" -> Term BenchRingSig "Expr"
rMul leftTerm rightTerm =
  benchMonoNode (Ring.Mul leftTerm rightTerm)

rNeg :: Term BenchRingSig "Expr" -> Term BenchRingSig "Expr"
rNeg =
  benchMonoNode . Ring.Neg

rZero :: Term BenchRingSig "Expr"
rZero =
  benchMonoNode Ring.RZero

rOne :: Term BenchRingSig "Expr"
rOne =
  benchMonoNode Ring.ROne
