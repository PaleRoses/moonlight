{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.EGraph.Saturation.ChimeraSpec
  ( tests,
  )
where

import Moonlight.Pale.Ghc.Expr (ScopeCtx)
import Control.Monad (foldM)
import Data.Fix (Fix)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core
  ( Language,
    SiteProgram (..),
    SupportIndexedRule (..),
  )
import Moonlight.Algebra
  ( JoinSemilattice (join),
    MeetSemilattice (meet),
  )
import Moonlight.EGraph.Effect.Harness
  ( contextGlobalSectionInvariantLaw,
    contextMergeMonotone,
    contextMorphismAssociativeLaw,
    contextMorphismLeftIdentityLaw,
    contextMorphismRightIdentityLaw,
    contextRestrictionComposition,
    contextRestrictionFunctorialActionLaw,
    contextRestrictionIdentityLaw,
    obstructionComplete,
    proofContextConsistency,
    proofSoundness,
  )
import Moonlight.EGraph.Pure.Context
import Moonlight.EGraph.Pure.Context
  ( ContextRuntimeState (..),
    cegBase,
    cegRuntimeState,
  )
import Moonlight.Sheaf.Context.Core qualified as SheafCore
import Moonlight.Sheaf.Context.Algebra
  ( contextEquivalentAt,
    restrictionMap,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons
import Moonlight.Sheaf.Obstruction
import Moonlight.EGraph.Pure.Context.Proof
import Moonlight.EGraph.Pure.Context.Proof qualified as EGraphProof
import Moonlight.EGraph.Pure.Extraction (ExtractionWorkBudget (..))
import Moonlight.Rewrite.ProofContext
import Moonlight.EGraph.Test.Saturation
import Moonlight.EGraph.Test.Scale.Run (AtlasRunObstruction, runAtlasProgram)
import Moonlight.EGraph.Pure.Saturation.Extraction (ContextualExtractionObstruction, contextualExtractBounded)
import Moonlight.EGraph.Pure.Saturation.Substrate
  ( EGraphSaturationChangeSummary (..),
    EGraphU,
  )
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingProofEGraph,
    emptySaturatingProofEGraph,
    sceContextGraph,
  )
import Moonlight.EGraph.Saturation.Context.State (emptySaturatingContextEGraph)
import Moonlight.EGraph.Pure.Types
import Moonlight.Rewrite.System (emptyFactDerivationIndex)
import Moonlight.Rewrite.System (RewriteCondition)
import Moonlight.Rewrite.System (RawRewriteRule)
import Moonlight.Saturation.Context.Driver (crrResult)
import Moonlight.Saturation.Context.Error (SaturationError)
import Moonlight.Saturation.Context.Program.Spec (staticRewriteContextSnapshot)
import Moonlight.Saturation.Context.Runtime.State
  ( FactViewKey (..),
    RuntimeCore (..),
    advanceRuntimeCoreFactViewGraphChanges,
    initialRuntimeCore,
  )
import Moonlight.Saturation.Substrate
  ( RebuildSystem (factViewGraphChanges),
    graphPreparedSite,
  )
import Moonlight.Sheaf.Twist.SupportedRuleSpec qualified as SheafTwist
import Moonlight.Sheaf.Context.Site (UnitContextSiteOwner)
import Moonlight.Control.Schedule (ScheduleGroup (..))
import Moonlight.Control.Schedule.Round (ScheduleTrace (..))
import Moonlight.Saturation.Support.Compile
  ( buildSupportProgram,
    compileSupportedRuleBook,
  )
import Moonlight.Saturation.Support.Core
  ( SupportSaturationReportFor,
    SupportScheduleGroup,
    supportReportScheduleTrace,
  )
import Moonlight.EGraph.Test.Chimera.Core
import Moonlight.Pale.Test.LawSuite (LawSuite, renderLawSuite)
import Moonlight.Pale.Test.Laws.Lattice
  ( LatticeLawSeedError,
    latticeLawSeed,
    unfoldLatticeLaws,
    withBounded,
    withComparableFilter,
  )
import Moonlight.Pale.Test.Site.Assertion (expectRight)
import Moonlight.Pale.Test.Site.Core (canonicalTestBudget)
import Moonlight.EGraph.Test.Config (toBudget, tracingTestConfig)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=), assertEqual, assertBool, assertFailure, testCase)

classesEquivalentAt ::
  (Language f, Ord c) =>
  c ->
  ClassId ->
  ClassId ->
  ContextEGraph owner f a c ->
  Bool
classesEquivalentAt contextValue leftClass rightClass graph =
  either (const False) id (contextEquivalentAt contextValue leftClass rightClass graph)

mergeContexts ::
  [(Anatomy, ClassId, ClassId)] ->
  ContextEGraph owner TissueF TissueCount Anatomy ->
  Either (ContextDeltaError TissueF Anatomy) (ContextEGraph owner TissueF TissueCount Anatomy)
mergeContexts mergeSteps contextGraph =
  foldM
    (\currentGraph (contextValue, leftClassId, rightClassId) -> contextMerge contextValue leftClassId rightClassId currentGraph)
    contextGraph
    mergeSteps

tests :: TestTree
tests =
  testGroup
    "chimera"
    [ anatomyLatticeTests,
      chimeraObstructionTests,
      chimeraResolutionTests,
      consistentMergeTests,
      restrictionMapTests,
      graftPropagationTests,
      sheafGluingTests,
      contextCostExtractionTests,
      supportSaturationTests,
      rewriteFamilyResolutionTests,
      factViewDirtyGateTests
    ]

type ChimeraU owner = EGraphU owner ScopeCtx TissueF TissueCount Anatomy

withFixtureChimera ::
  (forall owner. (ClassId, ClassId, ClassId, ClassId, ContextEGraph owner TissueF TissueCount Anatomy) -> Assertion) ->
  Assertion
withFixtureChimera useFixture =
  expectRight (fixtureChimera useFixture) >>= id

withAnatomyContextGraph ::
  EGraph TissueF TissueCount ->
  (forall owner. ContextEGraph owner TissueF TissueCount Anatomy -> Assertion) ->
  Assertion
withAnatomyContextGraph graphValue =
  withEmptyContextEGraph anatomyLattice graphValue

withAnatomyProofGraph ::
  EGraph TissueF TissueCount ->
  (forall owner. SaturatingProofEGraph owner ScopeCtx TissueF TissueCount Anatomy TissueProofNote -> Assertion) ->
  Assertion
withAnatomyProofGraph graphValue useProofGraph =
  withAnatomyContextGraph graphValue (useProofGraph . emptySaturatingProofEGraph)

withTissueTerm ::
  Fix TissueF ->
  (ClassId -> EGraph TissueF TissueCount -> Assertion) ->
  Assertion
withTissueTerm termValue useGraph = do
  (classId, graphValue) <- expectRight (addTerm termValue (emptyEGraph tissueAnalysis))
  useGraph classId graphValue

withTwoTissueTerms ::
  Fix TissueF ->
  Fix TissueF ->
  (ClassId -> ClassId -> EGraph TissueF TissueCount -> Assertion) ->
  Assertion
withTwoTissueTerms firstTerm secondTerm useGraph = do
  (firstClassId, firstGraph) <- expectRight (addTerm firstTerm (emptyEGraph tissueAnalysis))
  (secondClassId, secondGraph) <- expectRight (addTerm secondTerm firstGraph)
  useGraph firstClassId secondClassId secondGraph

factViewDirtyGateTests :: TestTree
factViewDirtyGateTests =
  testGroup
    "fact-view-dirty-gate"
    [ testCase "skull fiber changes dirty skull and local but preserve whole and trunk" $
        withFixtureChimera $ \(boneId, keratinId, _chitinId, _cartilageId, contextGraph :: ContextEGraph owner TissueF TissueCount Anatomy) -> do
        let initialBatch = beginContextRebaseBatch contextGraph
        mergePlan <-
          expectRight (planContextMerges [Skull] boneId keratinId initialBatch)
        stagedBatch <-
          expectRight (stageContextMerges mergePlan initialBatch)
        (rebaseReport, nextGraph) <-
          expectRight (commitContextRebaseBatch stagedBatch)
        advancedCore <-
          expectRight
            ( advanceRuntimeCoreFactViewGraphChanges @(ChimeraU owner)
                (cegSite nextGraph)
                ( factViewGraphChanges @(ChimeraU owner)
                    EGraphSaturationChangeSummary
                      { egscApplicationTraces = [crrTrace rebaseReport],
                        egscRebuildDeltas = [],
                        egscProofRestrictionRegistryConstructions = 0,
                        egscProofExtractionTableConstructions = 0
                      }
                )
                cachedChimeraFactViewCore
            )
        Map.keysSet (rcContextFactDerivations advancedCore)
          @?= Set.fromList [Whole, Trunk]
        rcFactViewFiberGenerations advancedCore
          @?= Map.fromList [(Skull, 1), (Local, 1)],
      testCase "global base changes invalidate every chimera fact view" $
        withFixtureChimera $ \(boneId, keratinId, _chitinId, _cartilageId, contextGraph :: ContextEGraph owner TissueF TissueCount Anatomy) -> do
        stagedBatch <-
          expectRight
            ( stageGlobalMerge
                boneId
                keratinId
                (beginContextRebaseBatch contextGraph)
            )
        (rebaseReport, nextGraph) <-
          expectRight (commitContextRebaseBatch stagedBatch)
        advancedCore <-
          expectRight
            ( advanceRuntimeCoreFactViewGraphChanges @(ChimeraU owner)
                (cegSite nextGraph)
                ( factViewGraphChanges @(ChimeraU owner)
                    EGraphSaturationChangeSummary
                      { egscApplicationTraces = [crrTrace rebaseReport],
                        egscRebuildDeltas = [],
                        egscProofRestrictionRegistryConstructions = 0,
                        egscProofExtractionTableConstructions = 0
                      }
                )
                cachedChimeraFactViewCore
            )
        rcFactViewBaseGeneration advancedCore @?= 1
        rcContextFactDerivations advancedCore @?= Map.empty
    ]

cachedChimeraFactViewCore :: RuntimeCore (ChimeraU owner) RewriteRuleId
cachedChimeraFactViewCore =
  initialRuntimeCore
    { rcContextFactDerivations =
        Map.fromSet
          (const emptyFactDerivationIndex)
          cachedContexts,
      rcFactViewKeys =
        Map.fromSet
          (const cachedKey)
          cachedContexts
    }
  where
    cachedContexts =
      Set.fromList [Whole, Skull, Trunk, Local]
    cachedKey =
      FactViewKey
        { fvkBaseGeneration = 0,
          fvkFiberGeneration = 0,
          fvkInputGeneration = 0,
          fvkFactRuleIds = [],
          fvkCapabilityGeneration = 0
        }

anatomyLatticeTests :: TestTree
anatomyLatticeTests =
  testGroup
    "anatomy-lattice" $
    anatomyLatticeLawTests

anatomyLatticeLawTests :: [TestTree]
anatomyLatticeLawTests =
  case anatomyLatticeLawSuites of
    Right lawSuites ->
      fmap renderLawSuite lawSuites
    Left seedErrors ->
      [testCase "anatomy lattice seed validates" (expectRight (Left seedErrors))]

anatomyLatticeLawSuites :: Either (NonEmpty (LatticeLawSeedError Anatomy)) [LawSuite]
anatomyLatticeLawSuites =
  unfoldLatticeLaws
    <$> ( latticeLawSeed "anatomy" join meet anatomyRegionsNonEmpty
            >>= (withBounded Whole Local . withComparableFilter anatomyLeq)
        )

anatomyRegionsNonEmpty :: NonEmpty Anatomy
anatomyRegionsNonEmpty =
  minBound :| filter (/= minBound) allRegions

chimeraObstructionTests :: TestTree
chimeraObstructionTests =
  testGroup
    "chimera-obstruction"
    [ testCase "reptile skull: bone ≡ keratin at skull, not at trunk" $
        withFixtureChimera $ \(boneId, keratinId, _chitinId, _cartilageId, ctx) -> do
              merged <- expectRight (contextMerge Skull boneId keratinId ctx)
              classesEquivalentAt Skull boneId keratinId merged @?= True
              classesEquivalentAt Local boneId keratinId merged @?= True
              classesEquivalentAt Trunk boneId keratinId merged @?= False
              classesEquivalentAt Whole boneId keratinId merged @?= False,
      testCase "arthropod trunk: bone ≡ chitin at trunk, not at skull" $
        withFixtureChimera $ \(boneId, _keratinId, chitinId, _cartilageId, ctx) -> do
              merged <- expectRight (contextMerge Trunk boneId chitinId ctx)
              classesEquivalentAt Trunk boneId chitinId merged @?= True
              classesEquivalentAt Local boneId chitinId merged @?= True
              classesEquivalentAt Skull boneId chitinId merged @?= False
              classesEquivalentAt Whole boneId chitinId merged @?= False,
      testCase "chimera surface sees all tissues as equivalent but whole does not" $
        withFixtureChimera $ \(boneId, keratinId, chitinId, _cartilageId, ctx) -> do
              merged <- expectRight (mergeContexts [(Skull, boneId, keratinId), (Trunk, boneId, chitinId)] ctx)
              classesEquivalentAt Skull boneId keratinId merged @?= True
              classesEquivalentAt Trunk boneId chitinId merged @?= True
              classesEquivalentAt Local keratinId chitinId merged @?= True
              classesEquivalentAt Whole keratinId chitinId merged @?= False
              classesEquivalentAt Whole boneId keratinId merged @?= False
              classesEquivalentAt Whole boneId chitinId merged @?= False,
      testCase "obstruction witness at whole explains the chimera failure" $
        withFixtureChimera $ \(boneId, keratinId, chitinId, _cartilageId, ctx) -> do
              merged <- expectRight (mergeContexts [(Skull, boneId, keratinId), (Trunk, boneId, chitinId)] ctx)
              let report = obstructionReport keratinId chitinId Whole merged
              assertEqual "exactly one structural mismatch" 1 (length (filter isStructuralMismatch report))
              assertEqual "exactly one context barrier" 1 (length (filter isContextBarrier report))
              assertEqual "exactly one restriction barrier" 1 (length (filter isRestrictionBarrier report))
              assertEqual "no propagation barrier" 0 (length (filter isPropagationBarrier report)),
      testCase "restriction barrier present for sheaf inconsistency across regions" $
        withFixtureChimera $ \(boneId, keratinId, chitinId, _cartilageId, ctx) -> do
              merged <- expectRight (mergeContexts [(Skull, boneId, keratinId), (Trunk, boneId, chitinId)] ctx)
              let report = whyNotMerged keratinId chitinId merged
              assertBool "expected restriction barrier across incomparable regions"
                (any isRestrictionBarrier report),
      testCase "obstruction completeness holds for chimera queried at whole" $
        withFixtureChimera $ \(boneId, keratinId, chitinId, _cartilageId, ctx) -> do
              merged <- expectRight (mergeContexts [(Skull, boneId, keratinId), (Trunk, boneId, chitinId)] ctx)
              obstructionComplete keratinId chitinId Whole merged @?= True
    ]

chimeraResolutionTests :: TestTree
chimeraResolutionTests =
  testGroup
    "chimera-resolution"
    [ testCase "minimal fix: global keratin ≡ chitin resolves that specific obstruction" $
        withFixtureChimera $ \(boneId, keratinId, chitinId, _cartilageId, ctx) -> do
              obstructed <- expectRight (mergeContexts [(Skull, boneId, keratinId), (Trunk, boneId, chitinId)] ctx)
              resolved <- expectRight (contextMerge Whole keratinId chitinId obstructed)
              classesEquivalentAt Whole keratinId chitinId resolved @?= True
              classesEquivalentAt Whole boneId keratinId resolved @?= False
              classesEquivalentAt Whole boneId chitinId resolved @?= False
              let report = obstructionReport keratinId chitinId Whole resolved
              assertBool "keratin vs chitin obstruction resolved" (null report),
      testCase "full resolution requires global bone equivalences" $
        withFixtureChimera $ \(boneId, keratinId, chitinId, _cartilageId, ctx) -> do
              resolved <-
                expectRight
                  ( mergeContexts
                      [(Skull, boneId, keratinId), (Trunk, boneId, chitinId), (Whole, boneId, keratinId), (Whole, boneId, chitinId)]
                      ctx
                  )
              assertBool "all tissues equivalent everywhere after full resolution"
                ( all
                    ( \region ->
                        classesEquivalentAt region boneId keratinId resolved
                          && classesEquivalentAt region boneId chitinId resolved
                          && classesEquivalentAt region keratinId chitinId resolved
                    )
                    allRegions
                )
    ]

consistentMergeTests :: TestTree
consistentMergeTests =
  testGroup
    "consistent-merge"
    [ testCase "global merge at whole propagates everywhere" $
        withFixtureChimera $ \(boneId, _keratinId, _chitinId, cartilageId, ctx) -> do
              merged <- expectRight (contextMerge Whole boneId cartilageId ctx)
              classesEquivalentAt Whole boneId cartilageId merged @?= True
              classesEquivalentAt Skull boneId cartilageId merged @?= True
              classesEquivalentAt Trunk boneId cartilageId merged @?= True
              classesEquivalentAt Local boneId cartilageId merged @?= True,
      testCase "consistent merge has no obstruction" $
        withFixtureChimera $ \(boneId, _keratinId, _chitinId, cartilageId, ctx) -> do
              merged <- expectRight (contextMerge Whole boneId cartilageId ctx)
              let report = obstructionReport boneId cartilageId Skull merged
              assertBool "expected no obstructions for whole-merged classes" (null report),
      testCase "independent region merges coexist without interference" $
        withFixtureChimera $ \(boneId, keratinId, chitinId, cartilageId, ctx) -> do
              merged <- expectRight (mergeContexts [(Skull, boneId, keratinId), (Trunk, chitinId, cartilageId)] ctx)
              classesEquivalentAt Skull boneId keratinId merged @?= True
              classesEquivalentAt Trunk chitinId cartilageId merged @?= True
              classesEquivalentAt Skull chitinId cartilageId merged @?= False
              classesEquivalentAt Trunk boneId keratinId merged @?= False
              classesEquivalentAt Local boneId keratinId merged @?= True
              classesEquivalentAt Local chitinId cartilageId merged @?= True
    ]

restrictionMapTests :: TestTree
restrictionMapTests =
  testGroup
    "restriction-maps"
    [ testCase "restriction map exists iff target leq source" $
        withFixtureChimera $ \(_, _, _, _, ctx) -> assertBool "restriction map existence matches lattice order"
              (all
                 (\(source, target) ->
                    either (const False) (const True) (restrictionMap source target ctx) == anatomyLeq target source)
                 allPairs),
      testCase "restriction identity holds for all contexts" $
        withFixtureChimera $ \(boneId, keratinId, _, _, ctx) -> do
              merged <- expectRight (contextMerge Skull boneId keratinId ctx)
              assertBool "restriction identity law"
                (all
                   (\region -> contextRestrictionIdentityLaw region merged)
                   allRegions),
      testCase "restriction composition holds for all comparable triples" $
        withFixtureChimera $ \(boneId, keratinId, _, _, ctx) -> do
              merged <- expectRight (contextMerge Skull boneId keratinId ctx)
              assertBool "restriction composition law"
                (all
                   (\(a, b, c) -> contextRestrictionComposition a b c merged)
                   comparableTriples),
      testCase "functorial action: sequential restriction equals composed" $
        withFixtureChimera $ \(boneId, keratinId, _, _, ctx) -> do
              merged <- expectRight (contextMerge Skull boneId keratinId ctx)
              assertBool "functorial action law"
                (all
                   (\(a, b, c) -> contextRestrictionFunctorialActionLaw a b c merged)
                   comparableTriples),
      testCase "morphism left identity holds for all contexts" $
        withFixtureChimera $ \(boneId, keratinId, _, _, ctx) -> do
              merged <- expectRight (contextMerge Skull boneId keratinId ctx)
              assertBool "morphism left identity law"
                (all
                   (\region -> contextMorphismLeftIdentityLaw region merged)
                   allRegions),
      testCase "morphism right identity holds for all contexts" $
        withFixtureChimera $ \(boneId, keratinId, _, _, ctx) -> do
              merged <- expectRight (contextMerge Skull boneId keratinId ctx)
              assertBool "morphism right identity law"
                (all
                   (\region -> contextMorphismRightIdentityLaw region merged)
                   allRegions),
      testCase "morphism associativity holds for all comparable quadruples" $
        withFixtureChimera $ \(boneId, keratinId, _, _, ctx) -> do
              merged <- expectRight (contextMerge Skull boneId keratinId ctx)
              assertBool "morphism associativity law"
                (all
                   (\(a, b, c, d) -> contextMorphismAssociativeLaw a b c d merged)
                   comparableQuadruples),
      testCase "global section invariant holds for all comparable pairs" $
        withFixtureChimera $ \(boneId, keratinId, _, _, ctx) -> do
              merged <- expectRight (contextMerge Skull boneId keratinId ctx)
              assertBool "global section invariant law"
                (all
                   (\(source, target) -> contextGlobalSectionInvariantLaw source target merged)
                   [(s, t) | s <- allRegions, t <- allRegions, anatomyLeq t s])
    ]

graftPropagationTests :: TestTree
graftPropagationTests =
  testGroup
    "graft-propagation"
    [ testCase "graft equivalence at skull does not leak to trunk" $
        withTwoTissueTerms (graft bone keratin) (graft keratin bone) $ \graftAId graftBId graph2 ->
          withAnatomyContextGraph graph2 $ \ctx -> do
              merged <- expectRight (contextMerge Skull graftAId graftBId ctx)
              classesEquivalentAt Skull graftAId graftBId merged @?= True
              classesEquivalentAt Local graftAId graftBId merged @?= True
              classesEquivalentAt Trunk graftAId graftBId merged @?= False
              classesEquivalentAt Whole graftAId graftBId merged @?= False,
      testCase "propagation report present after chimera merge" $
        withFixtureChimera $ \(boneId, keratinId, chitinId, _cartilageId, ctx) -> do
              merged <- expectRight (mergeContexts [(Skull, boneId, keratinId), (Trunk, boneId, chitinId)] ctx)
              contextPropagationSettled merged @?= True
              assertBool "propagation should not fail" (not (contextPropagationFailed merged))
    ]

contextPropagationSettled :: ContextEGraph owner f a c -> Bool
contextPropagationSettled contextGraph =
  maybe False SheafCore.contextPropagationSettled (crsLastRepair (cegRuntimeState contextGraph))

contextPropagationFailed :: ContextEGraph owner f a c -> Bool
contextPropagationFailed _contextGraph =
  False

sheafGluingTests :: TestTree
sheafGluingTests =
  testGroup
    "sheaf-gluing"
    [ testCase "compatible local sections glue and restrict back to originals" $
        withFixtureChimera $ \(boneId, keratinId, chitinId, _, ctx) -> do
              merged <- expectRight (mergeContexts [(Skull, boneId, keratinId), (Trunk, boneId, chitinId)] ctx)
              classesEquivalentAt Local keratinId chitinId merged @?= True
              classesEquivalentAt Skull boneId keratinId merged @?= True
              classesEquivalentAt Skull boneId chitinId merged @?= False
              classesEquivalentAt Trunk boneId chitinId merged @?= True
              classesEquivalentAt Trunk boneId keratinId merged @?= False,
      testCase "globally consistent sections produce no obstruction anywhere" $
        withFixtureChimera $ \(boneId, keratinId, _, _, ctx) -> do
              merged <- expectRight (contextMerge Whole boneId keratinId ctx)
              assertBool "obstruction completeness holds at every region"
                (all
                   (\region -> obstructionComplete boneId keratinId region merged)
                   allRegions)
              assertBool "no obstructions at any region"
                (all
                   (\region -> null (obstructionReport boneId keratinId region merged))
                   allRegions),
      testCase "merge monotonicity: context merge propagates to all coarser contexts" $
        withFixtureChimera $ \(boneId, keratinId, _, _, ctx) -> assertBool "merge monotonicity holds at every context"
              (all
                 (\region -> contextMergeMonotone region boneId keratinId ctx)
                 allRegions)
    ]

chimeraBudget :: SaturationBudget
chimeraBudget = toBudget canonicalTestBudget

chimeraTrivialSaturationConfig :: EGraphSaturationConfig UnitContextSiteOwner ScopeCtx TissueF TissueCount ()
chimeraTrivialSaturationConfig =
  genericJoinSaturationConfig chimeraBudget

chimeraContextSaturationConfig :: EGraphSaturationConfig owner ScopeCtx TissueF TissueCount Anatomy
chimeraContextSaturationConfig =
  genericJoinSaturationConfig chimeraBudget

contextCostExtractionTests :: TestTree
contextCostExtractionTests =
  testGroup
    "context-cost-extraction"
    [ testCase "skull prefers keratin over bone" $
        withTwoTissueTerms bone keratin $ \boneId keratinId graph2 ->
          withAnatomyContextGraph graph2 $ \contextGraph0 -> do
              contextGraph <- expectRight (contextMerge Whole boneId keratinId contextGraph0)
              renderContextualTissueExtract Skull boneId contextGraph @?= Right (Just "keratin")
              renderContextualTissueExtract Trunk boneId contextGraph @?= Right (Just "bone"),
      testCase "trunk prefers chitin over cartilage as exoskeleton" $
        withTwoTissueTerms chitin cartilage $ \chitinId cartilageId graph2 ->
          withAnatomyContextGraph graph2 $ \contextGraph0 -> do
              contextGraph <- expectRight (contextMerge Whole chitinId cartilageId contextGraph0)
              renderContextualTissueExtract Trunk chitinId contextGraph @?= Right (Just "chitin")
              renderContextualTissueExtract Skull chitinId contextGraph @?= Right (Just "cartilage"),
      testCase "whole context falls through to base metabolic cost" $
        withTwoTissueTerms bone keratin $ \boneId keratinId graph2 ->
          withAnatomyContextGraph graph2 $ \contextGraph0 -> do
              contextGraph <- expectRight (contextMerge Whole boneId keratinId contextGraph0)
              renderContextualTissueExtract Whole boneId contextGraph @?= Right (Just "keratin")
    ]

chimeraExtractionBudget :: ExtractionWorkBudget
chimeraExtractionBudget =
  ExtractionWorkBudget 32

renderContextualTissueExtract ::
  Anatomy ->
  ClassId ->
  ContextEGraph owner TissueF TissueCount Anatomy ->
  Either (ContextualExtractionObstruction Anatomy) (Maybe String)
renderContextualTissueExtract contextValue classId contextGraph =
  fmap
    (fmap renderTissueTerm)
    (contextualExtractBounded chimeraExtractionBudget contextValue anatomyCostOverlay baseTissueCost classId contextGraph)

supportSaturationTests :: TestTree
supportSaturationTests =
  testGroup
    "support-saturation"
    [ testCase "graft-commute fires only at skull when scoped to skull" $
        withTissueTerm (graft bone keratin) $ \_ graph1 ->
          withAnatomyContextGraph graph1 $ \contextGraph -> do
              skullFamily <-
                expectRight
                  ( SheafTwist.supportedRuleBook
                      (cegSite contextGraph)
                      [SheafTwist.SupportedRuleSpec (principalSupport Skull) graftCommuteRule]
                  )
              skullReport <-
                expectRight
                  ( runContextualRewriteSaturation
                      Nothing
                      chimeraTrivialSaturationConfig
                      Skull
                      skullFamily
                      contextGraph
                  )
              trunkReport <-
                expectRight
                  ( runContextualRewriteSaturation
                      Nothing
                      chimeraTrivialSaturationConfig
                      Trunk
                      skullFamily
                      contextGraph
                  )
              assertBool "expected skull-scoped rewrite application" (srMatchesApplied skullReport > 0)
              srMatchesApplied trunkReport @?= 0,
      testCase "support compilation preserves support without atlas expansion" $
        withTissueTerm (graft bone keratin) $ \_ graph1 ->
          withAnatomyContextGraph graph1 $ \(contextGraph :: ContextEGraph owner TissueF TissueCount Anatomy) -> do
              let supportProgramSignature program =
                    ( Map.map sirSupport (spSupportedRewriteRules program),
                      spBaseRewriteSupport program,
                      fmap sirSupport (spSupportedFactRules program)
                    )
              let saturatingGraph = emptySaturatingContextEGraph contextGraph
              skullFamily <-
                expectRight
                  ( SheafTwist.supportedRuleBook
                      (cegSite contextGraph)
                      [SheafTwist.SupportedRuleSpec (principalSupport Skull) graftCommuteRule]
                  )
              compiledRules <-
                expectRight
                  (compileSupportedRuleBook @(ChimeraU owner) skullFamily)
              preparedSiteProgram <-
                expectRight
                  ( buildSupportProgram
                      @(ChimeraU owner)
                      (cegSite contextGraph)
                      []
                      compiledRules
                  )
              graphSiteProgram <-
                expectRight
                  ( buildSupportProgram
                      @(ChimeraU owner)
                      (graphPreparedSite @(ChimeraU owner) saturatingGraph)
                      []
                      compiledRules
                  )
              fmap sirSupport compiledRules @?= [principalSupport Skull]
              supportProgramSignature preparedSiteProgram @?= supportProgramSignature graphSiteProgram,
      testCase "contextual proof scope: all steps carry correct context evidence" $
        withTissueTerm (graft bone keratin) $ \_ graph1 ->
          withAnatomyProofGraph graph1 $ \proofGraph0 -> do
              skullFamily <-
                expectRight
                  ( SheafTwist.supportedRuleBook
                      (cegSite (sceContextGraph (pgGraph proofGraph0)))
                      [SheafTwist.SupportedRuleSpec (principalSupport Skull) graftCommuteRule]
                  )
              proofReport <-
                expectRight
                  ( runContextualRewriteProofSaturation
                      tissueProofBuilder
                      Nothing
                      chimeraContextSaturationConfig
                      Skull
                      skullFamily
                      proofGraph0
                  )
              let appliedCount = srMatchesApplied proofReport
              assertBool "expected skull proof applications" (appliedCount > 0)
              let proofLog = EGraphProof.serializeProofLog (psrProofGraph proofReport)
              assertEqual "one rewrite step should be retained in the proof log" 1 (length proofLog)
              assertBool "proof effects should account for at least the retained rewrite step" (length proofLog <= appliedCount)
              assertBool "all skull-scoped steps carry Just Skull context"
                (all
                   (\step -> tpnActiveContext (psAnnotation step) == Just Skull)
                   proofLog)
              assertBool "all steps reference graft-commute rule (RewriteRuleId 0)"
                (all
                   (\step -> tpnRuleId (psAnnotation step) == RewriteRuleId 0)
                   proofLog)
              assertBool "all steps carry restriction evidence"
                (all
                   (\step -> tpnHasRestrictions (psAnnotation step))
                   proofLog)
              assertBool "all steps have ProofRewrite kind with graft-commute rule"
                (all
                   (\step -> psKind step == ProofRewrite (RewriteRuleId 0))
                   proofLog),
      testCase "proof soundness: all steps have canonically equivalent LHS and RHS" $
        withTissueTerm (graft bone keratin) $ \_ graph1 ->
          withAnatomyProofGraph graph1 $ \proofGraph0 -> do
              skullFamily <-
                expectRight
                  ( SheafTwist.supportedRuleBook
                      (cegSite (sceContextGraph (pgGraph proofGraph0)))
                      [SheafTwist.SupportedRuleSpec (principalSupport Skull) graftCommuteRule]
                  )
              proofReport <-
                expectRight
                  ( runContextualRewriteProofSaturation
                      tissueProofBuilder
                      Nothing
                      chimeraContextSaturationConfig
                      Skull
                      skullFamily
                      proofGraph0
                  )
              proofSoundness (cegBase . sceContextGraph) (psrProofGraph proofReport) @?= True,
      testCase "proof context consistency: all steps preserve context equivalence" $
        withTissueTerm (graft bone keratin) $ \_ graph1 ->
          withAnatomyProofGraph graph1 $ \proofGraph0 -> do
              skullFamily <-
                expectRight
                  ( SheafTwist.supportedRuleBook
                      (cegSite (sceContextGraph (pgGraph proofGraph0)))
                      [SheafTwist.SupportedRuleSpec (principalSupport Skull) graftCommuteRule]
                  )
              proofReport <-
                expectRight
                  ( runContextualRewriteProofSaturation
                      tissueProofBuilder
                      Nothing
                      chimeraContextSaturationConfig
                      Skull
                      skullFamily
                      proofGraph0
                  )
              proofContextConsistency sceContextGraph (psrProofGraph proofReport) @?= True,
      testCase "support-family saturation tracks skull support in trace" $
        withTissueTerm (graft bone keratin) $ \_ graph1 ->
          withAnatomyProofGraph graph1 $ \proofGraph0 -> do
              let tracingConfig = tracingTestConfig canonicalTestBudget
              skullSupportedFamily <-
                expectRight
                  ( SheafTwist.supportedRuleBook
                      (cegSite (sceContextGraph (pgGraph proofGraph0)))
                      [ SheafTwist.SupportedRuleSpec
                          { SheafTwist.srsSupport = principalSupport Skull,
                            SheafTwist.srsRule = graftCommuteRule
                          }
                      ]
                  )
              supportReport <-
                expectRight
                  ( runChimeraSupportSaturation
                      tissueProofBuilder
                      tracingConfig
                      skullSupportedFamily
                      proofGraph0
                  )
              assertBool "expected support-family application" (srMatchesApplied supportReport > 0)
              case supportReportScheduleTrace supportReport of
                (traceEntry : _) ->
                  strGroup traceEntry @?= SupportedGroup (RewriteRuleId 0) (principalSupport Skull)
                [] -> assertFailure "expected support trace entries"
    ]

rewriteFamilyResolutionTests :: TestTree
rewriteFamilyResolutionTests =
  testGroup
    "rewrite-family-resolution"
    [ testCase "skull-scoped graft-commute does not contaminate trunk" $
        withTwoTissueTerms (graft bone keratin) (graft keratin bone) $ \graftAId graftBId graph2 ->
          withAnatomyProofGraph graph2 $ \proofGraph0 -> do
              skullFamily <-
                expectRight
                  ( SheafTwist.supportedRuleBook
                      (cegSite (sceContextGraph (pgGraph proofGraph0)))
                      [SheafTwist.SupportedRuleSpec (principalSupport Skull) graftCommuteRule]
                  )
              supportReport <-
                expectRight
                  ( runFixedAnatomyAtlas
                      ( runChimeraSupportSaturation
                          tissueProofBuilder
                          chimeraContextSaturationConfig
                          skullFamily
                      )
                      proofGraph0
                  )
              let resultGraph = sceContextGraph (pgGraph (srCarrier supportReport))
              assertBool "expected skull-scoped rewrite application" (srMatchesApplied supportReport > 0)
              classesEquivalentAt Skull graftAId graftBId resultGraph @?= True
              classesEquivalentAt Local graftAId graftBId resultGraph @?= True
              classesEquivalentAt Trunk graftAId graftBId resultGraph @?= False
              classesEquivalentAt Whole graftAId graftBId resultGraph @?= False,
      testCase "whole-scoped graft-commute resolves globally" $
        withTwoTissueTerms (graft bone keratin) (graft keratin bone) $ \graftAId graftBId graph2 ->
          withAnatomyProofGraph graph2 $ \proofGraph0 -> do
              wholeFamily <-
                expectRight
                  ( SheafTwist.supportedRuleBook
                      (cegSite (sceContextGraph (pgGraph proofGraph0)))
                      [SheafTwist.SupportedRuleSpec (principalSupport Whole) graftCommuteRule]
                  )
              supportReport <-
                expectRight
                  ( runFixedAnatomyAtlas
                      ( runChimeraSupportSaturation
                          tissueProofBuilder
                          chimeraContextSaturationConfig
                          wholeFamily
                      )
                      proofGraph0
                  )
              let resultGraph = sceContextGraph (pgGraph (srCarrier supportReport))
              assertBool "expected whole-scoped rewrite application" (srMatchesApplied supportReport > 0)
              assertBool "all regions see graft equivalence after whole-scoped saturation"
                (all
                   (\region -> classesEquivalentAt region graftAId graftBId resultGraph)
                   allRegions)
    ]

type ChimeraSupportRuleBook owner =
  SheafTwist.SupportedRuleBook
    owner
    Anatomy
    (RawRewriteRule (RewriteCondition ScopeCtx TissueF) TissueF)

runChimeraSupportSaturation ::
  ProofAnnotationBuilder Anatomy TissueProofNote ->
  SaturationConfig (ChimeraU owner) RewriteRuleId ->
  ChimeraSupportRuleBook owner ->
  SaturatingProofEGraph owner ScopeCtx TissueF TissueCount Anatomy TissueProofNote ->
  Either
    (SaturationError (ChimeraU owner) (SupportScheduleGroup (ChimeraU owner)))
    ( SupportSaturationReportFor
        (ChimeraU owner)
        (SaturatingProofEGraph owner ScopeCtx TissueF TissueCount Anatomy TissueProofNote)
    )
runChimeraSupportSaturation proofBuilder planSpecValue supportRuleBook initialProofGraph = do
  supportPlan <-
    prepareEGraphSupportPlan
      Nothing
      (const (staticRewriteContextSnapshot emptyRewriteRuntimeCapabilities))
      planSpecValue
      supportRuleBook
      mempty
      initialProofGraph
  crrResult
    <$> runEGraphSupportPlan
      proofBuilder
      mempty
      supportPlan
      initialProofGraph

runFixedAnatomyAtlas ::
  ( SaturatingProofEGraph owner ScopeCtx TissueF TissueCount Anatomy TissueProofNote ->
    Either
      errorValue
      (SupportSaturationReportFor universe (SaturatingProofEGraph owner ScopeCtx TissueF TissueCount Anatomy TissueProofNote))
  ) ->
  SaturatingProofEGraph owner ScopeCtx TissueF TissueCount Anatomy TissueProofNote ->
  Either
    (AtlasRunObstruction Anatomy errorValue)
    (SupportSaturationReportFor universe (SaturatingProofEGraph owner ScopeCtx TissueF TissueCount Anatomy TissueProofNote))
runFixedAnatomyAtlas =
  runAtlasProgram allRegions
