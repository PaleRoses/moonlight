{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Spike A of the Site Campaign: the virtual-fiber law.
--
-- The fiber view at a context c is semantically the congruence closure of
-- the base graph extended by the union of AUTHORED deltas over the down-set
-- of c — no eager up-set fan-out required. Phase 2 made this the storage
-- discipline: 'contextMerge' writes exactly one authored delta and every
-- reader derives the observable relation over the down-set. These tests pin
-- the machinery to the lazy formula as its specification, and pin the
-- authored-only storage invariant itself. Emergent closure at joins
-- (cross-delta transitivity AND congruence) falls out of the shared rebuild
-- kernel on both sides.
module Moonlight.EGraph.Context.VirtualFiberSpec
  ( tests,
  )
where

import Data.Foldable (foldlM)
import Data.Bifunctor (first)
import Data.Fix (Fix)
import Data.Map.Strict qualified as Map
import Moonlight.Constraint
  ( ConstraintExpr (..),
  )
import Moonlight.Core
  ( ClassId,
    Operator (..),
    Pattern (..),
    PatternVar,
    UnionFindAllocationError,
    classIdKey,
    mkPatternVar,
  )
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    beginContextRebaseBatch,
    commitContextRebaseBatch,
    contextMerge,
    contextRebaseBatchBaseGraph,
    contextRepresentativeAt,
    withEmptyContextEGraph,
    planContextMerges,
    stageContextMerges,
    stageSupportClass,
    stageTermGlobally,
    stageTermAtContext,
    stageTermWithSupport,
  )
import Moonlight.EGraph.Pure.Context
  ( cegBase,
    cegClassSupportIndex,
    cegContextFibers,
    cegSite,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons
  ( addTerm,
  )
import Moonlight.EGraph.Pure.Rebuild
  ( merge,
    rebuild,
  )
import Moonlight.EGraph.Pure.Relational.Source
  ( structuralRowsForOperator,
  )
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    canonicalizeClassId,
    eGraphClassCount,
    eGraphNodeCount,
    eGraphStore,
    emptyEGraph,
  )
import Moonlight.EGraph.Pure.Saturation.Substrate
  ( EGraphU,
  )
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
import Moonlight.EGraph.Test.Context.Diamond
  ( DiamondCtx (..),
  )
import Moonlight.EGraph.Test.Context.MaterializedOracle
  ( materializedContextGraphAt,
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    latticeContext,
    leqContext,
    principalSupport,
  )
import Moonlight.Sheaf.Context.Site
  ( classSupportExplicitCarrierForKey,
    contextObjectKeyFor,
    supportCarrierContainsKey,
  )
import Moonlight.Rewrite.System
  ( GuardAtom (..),
    GuardBase (..),
    GuardChildIndex,
    GuardPath (..),
    GuardRef (..),
    GuardTerm (..),
    guardChildIndex,
    RewriteCondition (..),
    emptyGuardCapabilityResolver,
  )
import Moonlight.Rewrite.System
  ( CompiledFactRule,
    FactRule,
    FactRuleId (..),
    RawFactRule (..),
    emptyFactDerivationIndex,
  )
import Moonlight.Rewrite.System
  ( FactId (..),
    FactTuple (..),
    emptyFactStore,
    hasFact,
  )
import Moonlight.Saturation.Substrate
  ( compileFactRules,
    deriveFactClosure,
    deriveFactClosureAtContext,
    deriveFactClosuresAtContexts,
    graphBase,
    graphBaseContext,
    graphExecutionContexts,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@?=),
  )
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "virtual fiber law (Site Campaign spike A)"
    [ testCase "lazy join view reproduces emergent transitivity at the join" testLazyJoinTransitivity,
      testCase "lazy join view reproduces emergent congruence at the join" testLazyJoinCongruence,
      testCase "lazy join view collapses duplicated rows at the join" testLazyJoinRowCollapse,
      testCase "lazy views agree with eager fibers across the anatomy lattice" testAnatomyAgreement,
      testCase "lazy views agree with eager fibers across the diamond lattice" testDiamondAgreement,
      testCase "single-authored-delta scripts agree at every context" testSingleDeltaScripts,
      testCase "layered four-delta script agrees at every context" testLayeredScript,
      testCase "storage holds authored deltas only; join fibers stay virtual" testAuthoredOnlyStorage,
      QC.testProperty "context fact closure equals restrict-then-close and batched closure equals independent closure" factClosureVirtualFiberOracleLaw,
      testCase "collapsed distinct-child adds refuse the projection guard at the author and derive at the descendant" testRecursiveGuardCollapseBoundary,
      testGroup
        "colored insertion (Site Campaign spike B)"
        [ testCase "context-staged terms live in the shared store with scoped support" testStagedTermSupportScope,
          testCase "restaging the same term at a sibling reuses the node and widens support" testStagedTermHashconsReuse,
          testCase "context insertion canonicalizes child classes before hashconsing" testContextInsertionCanonicalizesChildren,
          testCase "context insertion preserves the visible derived fiber relation" testContextInsertionDerivedViewOracle,
          testCase "global staging remains the base-store degeneracy" testGlobalStagingDegeneracy
        ]
    ]

type AuthoredScript c = [(c, (ClassId, ClassId))]

type ContextEGraphAnatomy owner =
  ContextEGraph owner Arith.ArithF Arith.NodeCount AnatomyRegion

type ArithEGraphU owner = EGraphU owner () Arith.ArithF Arith.NodeCount AnatomyRegion

data CacheLineageCheckpoint owner = CacheLineageCheckpoint
  { clcLabel :: String,
    clcGraph :: ContextEGraphAnatomy owner
  }

data ArithFixture = ArithFixture
  { afBase :: EGraph Arith.ArithF Arith.NodeCount,
    afNumA :: ClassId,
    afNumB :: ClassId,
    afNumC :: ClassId,
    afNumD :: ClassId,
    afAddA :: ClassId,
    afAddC :: ClassId
  }

arithFixture :: Either UnionFindAllocationError ArithFixture
arithFixture = do
  let g0 = emptyEGraph Arith.analysisSpec
  (numA, g1) <- addTerm (Arith.numTerm 1) g0
  (numB, g2) <- addTerm (Arith.numTerm 2) g1
  (numC, g3) <- addTerm (Arith.numTerm 3) g2
  (numD, g4) <- addTerm (Arith.numTerm 4) g3
  (addA, g5) <- addTerm (Arith.addTermNode (Arith.numTerm 1) (Arith.numTerm 1)) g4
  (addC, g6) <- addTerm (Arith.addTermNode (Arith.numTerm 3) (Arith.numTerm 3)) g5
  pure
    ArithFixture
      { afBase = g6,
        afNumA = numA,
        afNumB = numB,
        afNumC = numC,
        afNumD = numD,
        afAddA = addA,
        afAddC = addC
      }

expectArithFixture :: IO ArithFixture
expectArithFixture =
  either (assertFailure . show) pure arithFixture

cacheLineageCheckpoints ::
  ArithFixture ->
  Bool ->
  (forall owner. [CacheLineageCheckpoint owner] -> result) ->
  Either String result
cacheLineageCheckpoints fixture preferAddA useCheckpoints =
  withEmptyContextEGraph coarseAnatomyLattice (afBase fixture) $ \graph0 -> do
    let (firstCompound, secondCompound) =
          if preferAddA
            then (afAddA fixture, afAddC fixture)
            else (afAddC fixture, afAddA fixture)
    graph1 <-
      first show
        (contextMerge Local (afNumA fixture) (afNumB fixture) graph0)
    graph2 <-
      first show
        (contextMerge ArmLeft (afNumB fixture) (afNumC fixture) graph1)
    let firstBatch = beginContextRebaseBatch graph2
    firstMergePlan <-
      first show (planContextMerges [ArmLeft] firstCompound (afNumD fixture) firstBatch)
    firstContractionBatch <-
      first show (stageContextMerges firstMergePlan firstBatch)
    (_, graphAfterFirstContraction) <-
      first show (commitContextRebaseBatch firstContractionBatch)
    let secondBatch = beginContextRebaseBatch graphAfterFirstContraction
    secondMergePlan <-
      first show (planContextMerges [ArmLeft] secondCompound (afNumD fixture) secondBatch)
    secondContractionBatch <-
      first show (stageContextMerges secondMergePlan secondBatch)
    (_, graphAfterSecondContraction) <-
      first show (commitContextRebaseBatch secondContractionBatch)
    let graph3 = graphAfterSecondContraction
    pure $
      useCheckpoints
        [ CacheLineageCheckpoint "authored Local commit" graph1,
          CacheLineageCheckpoint "inherited ArmLeft commit" graph2,
          CacheLineageCheckpoint "inherited Local view" graph2,
          CacheLineageCheckpoint "first contraction author" graphAfterFirstContraction,
          CacheLineageCheckpoint "first contraction descendant" graphAfterFirstContraction,
          CacheLineageCheckpoint "second contraction author" graphAfterSecondContraction,
          CacheLineageCheckpoint "second contraction descendant" graphAfterSecondContraction,
          CacheLineageCheckpoint "contracted author commit" graph3,
          CacheLineageCheckpoint "contracted descendant commit" graph3
        ]

factClosureVirtualFiberOracleLaw :: Bool -> QC.Property
factClosureVirtualFiberOracleLaw preferAddA =
  case arithFixture of
    Left allocationError -> QC.counterexample (show allocationError) False
    Right fixture ->
      either
        (\failure -> QC.counterexample failure False)
        id
        ( cacheLineageCheckpoints fixture preferAddA $ \(checkpoints :: [CacheLineageCheckpoint owner]) ->
            case compileFactRules @(ArithEGraphU owner) factClosureOracleRules of
              Left compileFailure ->
                QC.counterexample ("fact closure oracle rule compilation failed: " <> show compileFailure) False
              Right compiledRules ->
                QC.conjoin (fmap (factClosureCheckpointLaw fixture compiledRules) checkpoints)
        )

factClosureCheckpointLaw ::
  forall owner.
  ArithFixture ->
  [CompiledFactRule () Arith.ArithF] ->
  CacheLineageCheckpoint owner ->
  QC.Property
factClosureCheckpointLaw fixture compiledRules checkpoint =
  let contextGraph = clcGraph checkpoint
      graph = emptySaturatingContextEGraph contextGraph
      baseContext = graphBaseContext @(ArithEGraphU owner) graph
      contextValues = filter (/= baseContext) (graphExecutionContexts @(ArithEGraphU owner) graph)
   in case
        traverse
          ( \contextValue ->
              fmap
                (\materializedView -> (contextValue, materializedView))
                (materializedContextGraphAt contextValue contextGraph)
          )
          contextValues
        of
          Left supportError ->
            QC.counterexample
              (clcLabel checkpoint <> ": materialized oracle failed: " <> show supportError)
              False
          Right materializedPairs ->
            factClosureCheckpointLawWithViews
              fixture
              compiledRules
              checkpoint
              graph
              contextValues
              (Map.fromList materializedPairs)

factClosureCheckpointLawWithViews ::
  forall owner.
  ArithFixture ->
  [CompiledFactRule () Arith.ArithF] ->
  CacheLineageCheckpoint owner ->
  SaturatingContextEGraph owner () Arith.ArithF Arith.NodeCount AnatomyRegion ->
  [AnatomyRegion] ->
  Map.Map AnatomyRegion (EGraph Arith.ArithF Arith.NodeCount) ->
  QC.Property
factClosureCheckpointLawWithViews fixture compiledRules checkpoint graph contextValues materializedViews =
  let
      perContextResults =
        Map.fromList
          [ ( contextValue,
              deriveFactClosureAtContext @(ArithEGraphU owner)
                emptyGuardCapabilityResolver
                emptyFactStore
                compiledRules
                graph
                contextValue
                emptyFactStore
                emptyFactDerivationIndex
            )
            | contextValue <- contextValues
          ]
      batchInputs =
        Map.fromList
          [ (contextValue, (emptyFactStore, compiledRules))
            | contextValue <- contextValues
          ]
      batchedResult =
        deriveFactClosuresAtContexts @(ArithEGraphU owner)
          emptyGuardCapabilityResolver
          graph
          batchInputs
      independentResult =
        sequenceA perContextResults
      contextLaws =
        [ factClosureContextLaw checkpoint graph compiledRules contextValue materializedView
          | contextValue <- contextValues,
            Just materializedView <- [Map.lookup contextValue materializedViews]
        ]
      localCanonicalFacts =
        [ hasFact
            contextLocalFactId
            (FactTuple [contextCanonicalClass])
            factStore
          | contextValue <- contextValues,
            Just materializedView <- [Map.lookup contextValue materializedViews],
            let baseCanonicalClass = canonicalizeClassId (graphBase @(ArithEGraphU owner) graph) (afNumB fixture)
                contextCanonicalClass = canonicalizeClassId materializedView (afNumB fixture),
            contextCanonicalClass /= baseCanonicalClass,
            Just (Right (factStore, _derivations, _rounds)) <- [Map.lookup contextValue perContextResults]
        ]
      recursiveGuardFacts =
        [ hasFact
            recursiveGuardFactId
            (FactTuple [canonicalizeClassId materializedView (afNumA fixture)])
            factStore
          | contextValue <- contextValues,
            Just materializedView <- [Map.lookup contextValue materializedViews],
            Just (Right (factStore, _derivations, _rounds)) <- [Map.lookup contextValue perContextResults]
        ]
   in QC.conjoin
        ( contextLaws
            <> [ QC.counterexample
                   (clcLabel checkpoint <> ": batched closure diverged from independently derived context closures")
                   (batchedResult == independentResult),
                 QC.counterexample
                   (clcLabel checkpoint <> ": no fact landed in a context-local canonical class")
                   (or localCanonicalFacts),
                 QC.counterexample
                   (clcLabel checkpoint <> ": recursive node/project guard derived nowhere")
                   (or recursiveGuardFacts)
               ]
        )

factClosureContextLaw ::
  forall owner.
  CacheLineageCheckpoint owner ->
  SaturatingContextEGraph owner () Arith.ArithF Arith.NodeCount AnatomyRegion ->
  [CompiledFactRule () Arith.ArithF] ->
  AnatomyRegion ->
  EGraph Arith.ArithF Arith.NodeCount ->
  QC.Property
factClosureContextLaw checkpoint graph compiledRules contextValue materializedView =
  let optimizedResult =
        deriveFactClosureAtContext @(ArithEGraphU owner)
          emptyGuardCapabilityResolver
          emptyFactStore
          compiledRules
          graph
          contextValue
          emptyFactStore
          emptyFactDerivationIndex
      defaultResult =
        deriveFactClosure @(ArithEGraphU owner)
          emptyGuardCapabilityResolver
          emptyFactStore
          compiledRules
          materializedView
          emptyFactStore
          emptyFactDerivationIndex
   in QC.counterexample
        (clcLabel checkpoint <> ": context fact closure diverged at " <> show contextValue)
        ( fmap (\(factStore, derivations, _rounds) -> (factStore, derivations)) optimizedResult
            == fmap (\(factStore, derivations, _rounds) -> (factStore, derivations)) defaultResult
        )

testRecursiveGuardCollapseBoundary :: Assertion
testRecursiveGuardCollapseBoundary = do
  fixture <- either (assertFailure . show) pure arithFixture
  mapM_ (boundaryScript fixture) [True, False]
  where
    boundaryScript fixture preferAddA =
      either assertFailure id $
        cacheLineageCheckpoints fixture preferAddA $ \(checkpoints :: [CacheLineageCheckpoint owner]) ->
          case compileFactRules @(ArithEGraphU owner) factClosureOracleRules of
            Left compileFailure ->
              assertFailure ("fact closure oracle rule compilation failed: " <> show compileFailure)
            Right compiledRules ->
              mapM_
                (boundaryAt fixture compiledRules)
                [ checkpoint
                  | checkpoint <- checkpoints,
                    clcLabel checkpoint
                      `elem` ["second contraction author", "contracted author commit"]
                ]
    boundaryAt :: forall owner. ArithFixture -> [CompiledFactRule () Arith.ArithF] -> CacheLineageCheckpoint owner -> Assertion
    boundaryAt fixture compiledRules checkpoint = do
      let graph = emptySaturatingContextEGraph (clcGraph checkpoint)
      authorDerived <- recursiveFactAt fixture compiledRules graph ArmLeft
      descendantDerived <- recursiveFactAt fixture compiledRules graph Local
      assertBool
        (clcLabel checkpoint <> ": ambiguous projection guard derived at the collapsing author")
        (not authorDerived)
      assertBool
        (clcLabel checkpoint <> ": congruence-collapsed descendant lost the recursive fact")
        descendantDerived
    recursiveFactAt :: forall owner. ArithFixture -> [CompiledFactRule () Arith.ArithF] -> SaturatingContextEGraph owner () Arith.ArithF Arith.NodeCount AnatomyRegion -> AnatomyRegion -> IO Bool
    recursiveFactAt fixture compiledRules graph contextValue =
      case deriveFactClosureAtContext @(ArithEGraphU owner)
        emptyGuardCapabilityResolver
        emptyFactStore
        compiledRules
        graph
        contextValue
        emptyFactStore
        emptyFactDerivationIndex of
        Left _obstruction ->
          assertFailure ("fact closure obstructed at " <> show contextValue)
        Right (factStore, _derivations, _rounds) ->
          case materializedContextGraphAt contextValue (sceContextGraph graph) of
            Left supportError ->
              assertFailure ("materialized fact oracle failed: " <> show supportError)
            Right materializedView ->
              pure
                ( hasFact
                    recursiveGuardFactId
                    ( FactTuple
                        [ canonicalizeClassId
                            materializedView
                            (afNumA fixture)
                        ]
                    )
                    factStore
                )

factClosureOracleRules :: [FactRule () Arith.ArithF]
factClosureOracleRules =
  [ FactRule
      { frId = FactRuleId 7100,
        frName = "context-local-canonical-fact",
        frPattern = PatternNode (Arith.Num 2),
        frProjection = [factClosureRootRef],
        frFactId = contextLocalFactId,
        frCondition = Nothing
      },
    FactRule
      { frId = FactRuleId 7101,
        frName = "annotated-recursive-guard-fact",
        frPattern = PatternNode (Arith.Add (PatternVar factClosureX) (PatternVar factClosureX)),
        frProjection = [factClosureXRef],
        frFactId = recursiveGuardFactId,
        frCondition =
          Just
            ( RewriteCondition
                ( And
                    [ Atom (ClassesEquivalent factClosureRebuiltAdd factClosureRootTerm),
                      Atom (ClassesEquivalent factClosureProjectedChild factClosureXTerm)
                    ]
                )
            )
      }
  ]

contextLocalFactId :: FactId
contextLocalFactId = FactId 7100

recursiveGuardFactId :: FactId
recursiveGuardFactId = FactId 7101

factClosureX :: PatternVar
factClosureX = mkPatternVar 0

factClosureRootRef :: GuardRef
factClosureRootRef = GuardRef (GuardFromRoot, GuardPath [])

factClosureXRef :: GuardRef
factClosureXRef = GuardRef (GuardFromVar factClosureX, GuardPath [])

factClosureRootTerm :: GuardTerm Arith.ArithF
factClosureRootTerm = GuardRefTerm factClosureRootRef

factClosureXTerm :: GuardTerm Arith.ArithF
factClosureXTerm = GuardRefTerm factClosureXRef

factClosureRebuiltAdd :: GuardTerm Arith.ArithF
factClosureRebuiltAdd = GuardNodeTerm (Arith.Add factClosureXTerm factClosureXTerm)

factClosureProjectedChild :: GuardTerm Arith.ArithF
factClosureProjectedChild = GuardProjectTerm factClosureRebuiltAdd (guardChildIndex 0)

diamondLattice :: ContextLattice DiamondCtx
diamondLattice =
  either
    (error . ("invalid diamond lattice: " <>) . show)
    id
    latticeContext

lazyContextView ::
  Ord c =>
  ContextLattice c ->
  c ->
  AuthoredScript c ->
  EGraph Arith.ArithF Arith.NodeCount ->
  EGraph Arith.ArithF Arith.NodeCount
lazyContextView lattice viewContext authoredScript baseGraph =
  rebuild
    ( foldl
        (\graphValue (leftClass, rightClass) -> merge leftClass rightClass graphValue)
        baseGraph
        [ mergePair
          | (authoredAt, mergePair) <- authoredScript,
            leqContext lattice authoredAt viewContext == Right True
        ]
    )

eagerContextView ::
  (Ord c, Show c) =>
  ContextLattice c ->
  c ->
  AuthoredScript c ->
  EGraph Arith.ArithF Arith.NodeCount ->
  Either String (EGraph Arith.ArithF Arith.NodeCount)
eagerContextView lattice viewContext authoredScript baseGraph = do
  withEmptyContextEGraph lattice baseGraph $ \emptyContextGraph -> do
    contextGraph <-
      foldlM
        ( \graphValue (authoredAt, (leftClass, rightClass)) ->
            either
              (Left . show)
              Right
              (contextMerge authoredAt leftClass rightClass graphValue)
        )
        emptyContextGraph
        authoredScript
    first show (materializedContextGraphAt viewContext contextGraph)

testAuthoredOnlyStorage :: Assertion
testAuthoredOnlyStorage = do
  fixture <- expectArithFixture
  withEmptyContextEGraph coarseAnatomyLattice (afBase fixture) $ \emptyContextGraph -> do
    contextGraph <-
      either
        (assertFailure . show)
        pure
        ( foldlM
            ( \graphValue (authoredAt, (leftClass, rightClass)) ->
                contextMerge authoredAt leftClass rightClass graphValue
            )
            emptyContextGraph
            (armScript fixture)
        )
    Map.keys (cegContextFibers contextGraph) @?= [ArmLeft, ArmRight]
    joinView <- expectStep (first show (materializedContextGraphAt Local contextGraph))
    assertBool
      "join view must still glue the cross-arm chain without a stored join fiber"
      (equivalentIn joinView (afNumA fixture) (afNumC fixture))

equivalentIn :: EGraph Arith.ArithF Arith.NodeCount -> ClassId -> ClassId -> Bool
equivalentIn graph leftClass rightClass =
  canonicalizeClassId graph leftClass == canonicalizeClassId graph rightClass

addRowCount :: EGraph Arith.ArithF Arith.NodeCount -> Int
addRowCount graph =
  length (structuralRowsForOperator (eGraphStore graph) (Operator (Arith.Add () ())))

assertViewsAgree ::
  (Ord c, Show c) =>
  ContextLattice c ->
  [c] ->
  AuthoredScript c ->
  [ClassId] ->
  EGraph Arith.ArithF Arith.NodeCount ->
  Assertion
assertViewsAgree lattice contexts authoredScript probeClasses baseGraph =
  sequence_
    [ case eagerContextView lattice viewContext authoredScript baseGraph of
        Left failure ->
          assertFailure ("eager oracle failed at " <> show viewContext <> ": " <> failure)
        Right eagerView -> do
          let lazyView = lazyContextView lattice viewContext authoredScript baseGraph
          sequence_
            [ assertBool
                ( "lazy/eager disagreement at "
                    <> show viewContext
                    <> " on "
                    <> show (leftClass, rightClass)
                )
                ( equivalentIn lazyView leftClass rightClass
                    == equivalentIn eagerView leftClass rightClass
                )
              | leftClass <- probeClasses,
                rightClass <- probeClasses
            ]
          eGraphClassCount lazyView @?= eGraphClassCount eagerView
          addRowCount lazyView @?= addRowCount eagerView
      | viewContext <- contexts
    ]

armScript :: ArithFixture -> AuthoredScript AnatomyRegion
armScript fixture =
  [ (ArmLeft, (afNumA fixture, afNumB fixture)),
    (ArmRight, (afNumB fixture, afNumC fixture))
  ]

anatomyContexts :: [AnatomyRegion]
anatomyContexts =
  [minBound .. maxBound]

probes :: ArithFixture -> [ClassId]
probes fixture =
  [ afNumA fixture,
    afNumB fixture,
    afNumC fixture,
    afNumD fixture,
    afAddA fixture,
    afAddC fixture
  ]

testLazyJoinTransitivity :: Assertion
testLazyJoinTransitivity = do
  fixture <- expectArithFixture
  let joinView = lazyContextView coarseAnatomyLattice Local (armScript fixture) (afBase fixture)
      upperView = lazyContextView coarseAnatomyLattice Upper (armScript fixture) (afBase fixture)
      leftView = lazyContextView coarseAnatomyLattice ArmLeft (armScript fixture) (afBase fixture)
  assertBool "a ~ c must emerge at the join" (equivalentIn joinView (afNumA fixture) (afNumC fixture))
  assertBool "a ~ b must hold in the authoring context" (equivalentIn leftView (afNumA fixture) (afNumB fixture))
  assertBool "b ~ c must not leak into the authoring context" (not (equivalentIn leftView (afNumB fixture) (afNumC fixture)))
  assertBool "nothing may glue below both authors" (not (equivalentIn upperView (afNumA fixture) (afNumB fixture)))

testLazyJoinCongruence :: Assertion
testLazyJoinCongruence = do
  fixture <- expectArithFixture
  let joinView = lazyContextView coarseAnatomyLattice Local (armScript fixture) (afBase fixture)
  assertBool
    "Add(a,a) ~ Add(c,c) must emerge at the join by congruence"
    (equivalentIn joinView (afAddA fixture) (afAddC fixture))

testLazyJoinRowCollapse :: Assertion
testLazyJoinRowCollapse = do
  fixture <- expectArithFixture
  let joinView = lazyContextView coarseAnatomyLattice Local (armScript fixture) (afBase fixture)
  addRowCount (afBase fixture) @?= 2
  addRowCount joinView @?= 1

testAnatomyAgreement :: Assertion
testAnatomyAgreement = do
  fixture <- expectArithFixture
  assertViewsAgree
    coarseAnatomyLattice
    anatomyContexts
    (armScript fixture)
    (probes fixture)
    (afBase fixture)

testDiamondAgreement :: Assertion
testDiamondAgreement = do
  fixture <- expectArithFixture
  let script =
        [ (DLeft, (afNumA fixture, afNumB fixture)),
          (DRight, (afNumA fixture, afNumC fixture))
        ]
  assertViewsAgree
    diamondLattice
    [DBottom, DLeft, DRight, DTop]
    script
    (probes fixture)
    (afBase fixture)
  let topView = lazyContextView diamondLattice DTop script (afBase fixture)
      bottomView = lazyContextView diamondLattice DBottom script (afBase fixture)
  assertBool "b ~ c must glue at the top" (equivalentIn topView (afNumB fixture) (afNumC fixture))
  assertBool "b ~ c must not glue at the bottom" (not (equivalentIn bottomView (afNumB fixture) (afNumC fixture)))

testSingleDeltaScripts :: Assertion
testSingleDeltaScripts = do
  fixture <- expectArithFixture
  sequence_
    [ assertViewsAgree
        coarseAnatomyLattice
        anatomyContexts
        [(authoredAt, (afNumA fixture, afNumB fixture))]
        (probes fixture)
        (afBase fixture)
      | authoredAt <- anatomyContexts
    ]

visibleAt ::
  ContextEGraphAnatomy owner ->
  ClassId ->
  AnatomyRegion ->
  Either String Bool
visibleAt contextGraph stagedClass region = do
  regionKey <-
    either (Left . show) Right (contextObjectKeyFor (cegSite contextGraph) region)
  case classSupportExplicitCarrierForKey (cegClassSupportIndex contextGraph) (classIdKey stagedClass) of
    Nothing ->
      pure True
    Just carrier ->
      pure (supportCarrierContainsKey (cegSite contextGraph) carrier regionKey)

stageProbeAt ::
  AnatomyRegion ->
  ContextEGraphAnatomy owner ->
  Either String (ClassId, ContextEGraphAnatomy owner)
stageProbeAt region contextGraph = do
  (stagedClass, batchValue) <-
    either
      (Left . show)
      Right
      (stageTermAtContext region probeTerm (beginContextRebaseBatch contextGraph))
  (_, committedGraph) <-
    either (Left . show) Right (commitContextRebaseBatch batchValue)
  pure (stagedClass, committedGraph)
  where
    probeTerm =
      Arith.mulTermNode (Arith.numTerm 1) (Arith.numTerm 2)

expectStep :: Either String r -> IO r
expectStep =
  either assertFailure pure

testStagedTermSupportScope :: Assertion
testStagedTermSupportScope = do
  fixture <- expectArithFixture
  withEmptyContextEGraph coarseAnatomyLattice (afBase fixture) $ \contextGraph -> do
    (stagedClass, stagedGraph) <- expectStep (stageProbeAt ArmLeft contextGraph)
    visibleArmLeft <- expectStep (visibleAt stagedGraph stagedClass ArmLeft)
    visibleLocal <- expectStep (visibleAt stagedGraph stagedClass Local)
    visibleWhole <- expectStep (visibleAt stagedGraph stagedClass Whole)
    visibleSibling <- expectStep (visibleAt stagedGraph stagedClass ArmRight)
    assertBool "staged class must be visible at its authoring context" visibleArmLeft
    assertBool "staged class must be visible above its authoring context" visibleLocal
    assertBool "staged class must not leak below its authoring context" (not visibleWhole)
    assertBool "staged class must not leak to a sibling context" (not visibleSibling)

testStagedTermHashconsReuse :: Assertion
testStagedTermHashconsReuse = do
  fixture <- expectArithFixture
  withEmptyContextEGraph coarseAnatomyLattice (afBase fixture) $ \contextGraph -> do
    (firstClass, firstGraph) <- expectStep (stageProbeAt ArmLeft contextGraph)
    (secondClass, secondGraph) <- expectStep (stageProbeAt ArmRight firstGraph)
    secondClass @?= firstClass
    eGraphNodeCount (cegBase secondGraph) @?= eGraphNodeCount (cegBase firstGraph)
    visibleSibling <- expectStep (visibleAt secondGraph firstClass ArmRight)
    visibleOriginal <- expectStep (visibleAt secondGraph firstClass ArmLeft)
    visibleBelow <- expectStep (visibleAt secondGraph firstClass Whole)
    assertBool "support must widen to the second authoring context" visibleSibling
    assertBool "support must retain the first authoring context" visibleOriginal
    assertBool "widened support must still not leak below both authors" (not visibleBelow)

testContextInsertionCanonicalizesChildren :: Assertion
testContextInsertionCanonicalizesChildren = do
  fixture <- expectArithFixture
  withEmptyContextEGraph coarseAnatomyLattice (afBase fixture) $ \contextGraph -> do
    mergedGraph <-
      expectStep
        (either (Left . show) Right (contextMerge ArmLeft (afNumA fixture) (afNumB fixture) contextGraph))
    (leftParent, graphAfterLeft) <-
      expectStep (stageAndCommit ArmLeft (Arith.addTermNode (Arith.numTerm 1) (Arith.numTerm 1)) mergedGraph)
    (rightParent, graphAfterRight) <-
      expectStep (stageAndCommit ArmLeft (Arith.addTermNode (Arith.numTerm 2) (Arith.numTerm 1)) graphAfterLeft)
    rightParent @?= leftParent
    eGraphNodeCount (cegBase graphAfterRight) @?= eGraphNodeCount (cegBase graphAfterLeft)
    addRowCount (cegBase graphAfterRight) @?= addRowCount (cegBase graphAfterLeft)
    armView <- expectStep (first show (materializedContextGraphAt ArmLeft graphAfterRight))
    assertBool
      "the staged parents must be a single ArmLeft class"
      (equivalentIn armView leftParent rightParent)

testContextInsertionDerivedViewOracle :: Assertion
testContextInsertionDerivedViewOracle = do
  fixture <- expectArithFixture
  let leftTerm = Arith.addTermNode (Arith.numTerm 1) (Arith.numTerm 1)
      rightTerm = Arith.addTermNode (Arith.numTerm 2) (Arith.numTerm 1)
  (oracleLeftParent, oracleBase1) <- expectStep (first show (addTerm leftTerm (afBase fixture)))
  (oracleRightParent, oracleBase2) <- expectStep (first show (addTerm rightTerm oracleBase1))
  withEmptyContextEGraph coarseAnatomyLattice (afBase fixture) $ \contextGraph -> do
    mergedGraph <-
      expectStep
        (either (Left . show) Right (contextMerge ArmLeft (afNumA fixture) (afNumB fixture) contextGraph))
    (realLeftParent, graphAfterLeft) <- expectStep (stageAndCommit ArmLeft leftTerm mergedGraph)
    (realRightParent, realGraph) <- expectStep (stageAndCommit ArmLeft rightTerm graphAfterLeft)
    withEmptyContextEGraph coarseAnatomyLattice oracleBase2 $ \emptyOracleGraph -> do
      oracleSupportedGraph <-
        expectStep
          ( stageSupportOracle
              ArmLeft
              [oracleLeftParent, oracleRightParent, afNumA fixture, afNumB fixture]
              emptyOracleGraph
          )
      oracleGraph <-
        expectStep
          (either (Left . show) Right (contextMerge ArmLeft (afNumA fixture) (afNumB fixture) oracleSupportedGraph))
      assertVisibleRelationAgrees
        realGraph
        oracleGraph
        [ (afNumA fixture, afNumA fixture),
          (afNumB fixture, afNumB fixture),
          (realLeftParent, oracleLeftParent),
          (realRightParent, oracleRightParent)
        ]

testGlobalStagingDegeneracy :: Assertion
testGlobalStagingDegeneracy = do
  fixture <- expectArithFixture
  let termValue = Arith.mulTermNode (Arith.addTermNode (Arith.numTerm 1) (Arith.numTerm 2)) (Arith.numTerm 3)
  withEmptyContextEGraph coarseAnatomyLattice (afBase fixture) $ \contextGraph -> do
    (globalClass, globalBatch) <-
      either
        (assertFailure . show)
        pure
        (stageTermGlobally termValue (beginContextRebaseBatch contextGraph))
    let globalBase = contextRebaseBatchBaseGraph globalBatch
    (supportClass, supportBatch) <-
      either
        (assertFailure . show)
        pure
        (stageTermWithSupport (principalSupport Whole) termValue (beginContextRebaseBatch contextGraph))
    supportClass @?= globalClass
    eGraphNodeCount (contextRebaseBatchBaseGraph supportBatch) @?= eGraphNodeCount globalBase
    eGraphClassCount (contextRebaseBatchBaseGraph supportBatch) @?= eGraphClassCount globalBase

stageAndCommit ::
  AnatomyRegion ->
  Fix Arith.ArithF ->
  ContextEGraphAnatomy owner ->
  Either String (ClassId, ContextEGraphAnatomy owner)
stageAndCommit contextValue termValue contextGraph = do
  (classId, batchValue) <-
    either
      (Left . show)
      Right
      (stageTermAtContext contextValue termValue (beginContextRebaseBatch contextGraph))
  (_, committedGraph) <-
    either (Left . show) Right (commitContextRebaseBatch batchValue)
  pure (classId, committedGraph)

stageSupportOracle ::
  AnatomyRegion ->
  [ClassId] ->
  ContextEGraphAnatomy owner ->
  Either String (ContextEGraphAnatomy owner)
stageSupportOracle contextValue classIds contextGraph = do
  supportedBatch <-
    foldlM
      ( \batchValue classId ->
          either
            (Left . show)
            Right
            (stageSupportClass (principalSupport contextValue) classId batchValue)
      )
      (beginContextRebaseBatch contextGraph)
      classIds
  (_, supportedGraph) <-
    either (Left . show) Right (commitContextRebaseBatch supportedBatch)
  pure supportedGraph

assertVisibleRelationAgrees ::
  ContextEGraphAnatomy realOwner ->
  ContextEGraphAnatomy oracleOwner ->
  [(ClassId, ClassId)] ->
  Assertion
assertVisibleRelationAgrees realGraph oracleGraph alignedClasses =
  sequence_
    [ do
        realLeftVisible <- expectStep (visibleAt realGraph realLeft contextValue)
        realRightVisible <- expectStep (visibleAt realGraph realRight contextValue)
        oracleLeftVisible <- expectStep (visibleAt oracleGraph oracleLeft contextValue)
        oracleRightVisible <- expectStep (visibleAt oracleGraph oracleRight contextValue)
        realLeftVisible @?= oracleLeftVisible
        realRightVisible @?= oracleRightVisible
        if realLeftVisible && realRightVisible
          then do
            realLeftRepresentative <- expectStep (first show (contextRepresentativeAt contextValue realLeft realGraph))
            realRightRepresentative <- expectStep (first show (contextRepresentativeAt contextValue realRight realGraph))
            oracleLeftRepresentative <- expectStep (first show (contextRepresentativeAt contextValue oracleLeft oracleGraph))
            oracleRightRepresentative <- expectStep (first show (contextRepresentativeAt contextValue oracleRight oracleGraph))
            (realLeftRepresentative == realRightRepresentative)
              @?= (oracleLeftRepresentative == oracleRightRepresentative)
          else pure ()
      | contextValue <- anatomyContexts,
        (realLeft, oracleLeft) <- alignedClasses,
        (realRight, oracleRight) <- alignedClasses
    ]

testLayeredScript :: Assertion
testLayeredScript = do
  fixture <- expectArithFixture
  let script =
        [ (ArmLeft, (afNumA fixture, afNumB fixture)),
          (ArmRight, (afNumB fixture, afNumC fixture)),
          (Torso, (afNumC fixture, afNumD fixture)),
          (Upper, (afNumA fixture, afAddA fixture))
        ]
  assertViewsAgree
    coarseAnatomyLattice
    anatomyContexts
    script
    (probes fixture)
    (afBase fixture)
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
