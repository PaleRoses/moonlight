{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Pure.Saturation.LogicSpec
  ( tests,
  )
where

import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Data.Foldable (traverse_)
import Data.List (isInfixOf)
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core
  ( ClassId (..),
    RewriteRuleId,
  )
import Moonlight.EGraph.Pure.Extraction (ExtractionResult)
import Moonlight.EGraph.Pure.Saturation.Front
import Moonlight.EGraph.Pure.Saturation.Front.PackedNode
  ( PackedNode,
  )
import Moonlight.EGraph.Pure.Saturation.Logic.Run
  ( EGraphLogic,
    EGraphLogicReport (..),
    logic,
    observeRun,
    runEGraphLogic,
    seedFacts,
  )
import Moonlight.EGraph.Pure.Saturation.Logic.RunObservation
  ( RunObservation (..),
    SomeRunObservationResult (..),
  )
import Moonlight.EGraph.Pure.Saturation.Logic.Seed
  ( appendSeedFacts,
    resolveSeedFacts,
    singletonSeedFacts,
  )
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchingStrategy (GenericJoinMatching),
  )
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.EGraph.Test.Case (HUnitCase (..), hunitCases)
import Moonlight.EGraph.Test.Front.Tiny
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingContextEGraph,
  )
import Moonlight.Rewrite.DSL (Node)
import Moonlight.Rewrite.System
  ( emptyFactDerivationIndex,
  )
import Moonlight.Rewrite.System
  ( FactId (..),
    FactStore,
    FactTuple (..),
    emptyFactStore,
    factsFor,
    insertFact,
  )
import Moonlight.Saturation.Context.Driver
  ( ContextRunSpec,
    plainContextRunSpec,
  )
import Moonlight.Saturation.Context.Program.Spec
  ( PlanSpec,
    defaultPlanSpec,
  )
import Moonlight.Saturation.Context.Runtime.Report
  ( SaturationReport,
    srCarrier,
  )
import Moonlight.Saturation.Context.Runtime.State
  ( rcContextFactDerivations,
    rcContextFacts,
    rsCore,
  )
import Moonlight.Saturation.Matching
  ( MatchSite (..),
  )
import Moonlight.Saturation.Substrate
  ( graphConvergenceStateEquals,
  )
import Moonlight.Pale.Test.Section.ResourcePath
  ( renderResourcePathError,
    resolvePackageFile,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    (@?=),
  )

type TestU owner = EGraphU owner SurfaceKind (PackedNode FrontTinySig) NodeCount FrontTinyContext

type TestGraph owner = SaturatingContextEGraph owner SurfaceKind (PackedNode FrontTinySig) NodeCount FrontTinyContext

type MaybeExtraction cost =
  Maybe (ExtractionResult (Node FrontTinySig) cost)

tests :: TestTree
tests =
  testGroup "egraph-logic" . hunitCases $
    [ HUnitCase "source declarations remain rewrite/fact/activation/support only" sourceBoundaryGuard,
      HUnitCase "logic submodules are exposed and run layer uses live context runtime names" apiNameGuard,
      HUnitCase "seed facts resolve base and context sites without becoming source declarations" seedResolutionGuard,
      HUnitCase "base and context seed facts survive first derivation without fake derivations" seedRuntimeGuard,
      HUnitCase "front seed-only observations commit before resolving" seedOnlyObservationGuard,
      HUnitCase "seed-only logic leaves graph mutation ownership untouched" mutationOwnerGuard,
      HUnitCase "run observations collect, run against the report, and a seed-only run fires/dirties nothing" runObservationPlumbingGuard
    ]

sourceBoundaryGuard :: Assertion
sourceBoundaryGuard = do
  builderContents <- readRepoFile "../moonlight-saturation/src-public/Moonlight/Saturation/Context/Program/Internal/Builder.hs"
  sourceContents <- readRepoFile "../moonlight-saturation/src-public/Moonlight/Saturation/Context/Program/Source.hs"
  traverse_
    (assertContains builderContents)
    [ "DeclareRewrite",
      "DeclareFact",
      "ActivateBaseRewrite",
      "DeclareBaseRewriteSupport"
    ]
  traverse_
    (assertLacks (builderContents <> sourceContents))
    [ "DeclareSchedule",
      "DeclareCheck",
      "DeclareExtract",
      "DeclareSeedFacts"
    ]

apiNameGuard :: Assertion
apiNameGuard = do
  cabalContents <- readRepoFile "moonlight-egraph.cabal"
  runContents <- readRepoFile "src-pure-saturation/Moonlight/EGraph/Pure/Saturation/Logic/Run.hs"
  traverse_
    (assertContains cabalContents)
    [ "Moonlight.EGraph.Pure.Saturation.Logic.Seed",
      "Moonlight.EGraph.Pure.Saturation.Logic.Observation",
      "Moonlight.EGraph.Pure.Saturation.Logic.RunObservation",
      "Moonlight.EGraph.Pure.Saturation.Logic.Run"
    ]
  assertLacks cabalContents "    Moonlight.EGraph.Pure.Saturation.Logic\n"
  traverse_
    (assertContains runContents)
    [ "ContextRunSpec",
      "runRuntime",
      "seedRuntimeStateFacts"
    ]
  assertRepoFileMissing "src-pure-saturation/Moonlight/EGraph/Pure/Saturation/Logic.hs"
  traverse_
    (assertLacks runContents)
    [ "saturateFragment",
      "runContextFragment",
      "runContextProgram"
    ]

seedResolutionGuard :: Assertion
seedResolutionGuard =
  withContextualFrontGraph $ \(contextualFrontGraph :: TestGraph owner) -> do
    let baseStore = factStoreFor 10
        rainStore = factStoreFor 20
        resolved =
          resolveSeedFacts @(TestU owner)
            contextualFrontGraph
            ( appendSeedFacts
                (singletonSeedFacts BaseSite baseStore)
                (singletonSeedFacts (ContextSite Rain) rainStore)
            )
    Map.lookup BaseOnly resolved @?= Just baseStore
    Map.lookup Rain resolved @?= Just rainStore

seedRuntimeGuard :: Assertion
seedRuntimeGuard =
  withContextualFrontGraph $ \contextualFrontGraph -> do
    report <- expectLogicReport contextualFrontGraph
    let core = rsCore (elrRuntimeState report)
        baseFacts = Map.findWithDefault emptyFactStore BaseOnly (rcContextFacts core)
        rainFacts = Map.findWithDefault emptyFactStore Rain (rcContextFacts core)
        baseDerivations = Map.findWithDefault emptyFactDerivationIndex BaseOnly (rcContextFactDerivations core)
        rainDerivations = Map.findWithDefault emptyFactDerivationIndex Rain (rcContextFactDerivations core)
    factsFor testFactId baseFacts @?= Set.singleton (testFactTuple 10)
    factsFor testFactId rainFacts @?= Set.singleton (testFactTuple 20)
    baseDerivations @?= emptyFactDerivationIndex
    rainDerivations @?= emptyFactDerivationIndex

mutationOwnerGuard :: Assertion
mutationOwnerGuard =
  withContextualFrontGraph $ \(contextualFrontGraph :: TestGraph owner) -> do
    report <- expectLogicReport contextualFrontGraph
    assertBool
      "seed facts must not mutate the graph carrier"
      (graphConvergenceStateEquals @(TestU owner) contextualFrontGraph (srCarrier (elrSaturation report)))

runObservationLogic :: EGraphLogic owner SurfaceKind (PackedNode FrontTinySig) NodeCount FrontTinyContext
runObservationLogic =
  logic $ do
    seedFacts BaseSite (factStoreFor 10)
    seedFacts (ContextSite Rain) (factStoreFor 20)
    observeRun ObserveFiredRules
    observeRun ObserveBlockedRules
    observeRun ObserveDirtyKeys
    observeRun ObserveDirtyContexts

runObservationPlumbingGuard :: Assertion
runObservationPlumbingGuard =
  withContextualFrontGraph $ \contextualFrontGraph -> do
    case runEGraphLogic testRunSpec runObservationLogic contextualFrontGraph of
      Left _ ->
        assertFailure "expected run-observation logic to succeed"
      Right report ->
        case elrRunObservations report of
          [ SomeFiredRulesResult fired,
            SomeBlockedRulesResult blocked,
            SomeDirtyKeysResult dirtyKeys,
            SomeDirtyContextsResult dirtyContexts
            ] -> do
              fired @?= Set.empty
              blocked @?= Set.empty
              dirtyKeys @?= IntSet.empty
              dirtyContexts @?= Set.empty
          other ->
            assertFailure
              ("unexpected run-observation result count: " <> show (length other))

seedOnlyObservationGuard :: Assertion
seedOnlyObservationGuard =
  withEmptyFrontGraph $ \emptyFrontGraph -> do
    case runEGraphFront seedOnlyObservationProgram emptyFrontGraph of
      Right report
        | Just _ <- efrResult report ->
            pure ()
      Left err ->
        assertFailure ("expected seed-only front observation success, got front error: " <> frontErrorMessage err)
      Right _ ->
        assertFailure "expected seed-only front extraction result"

expectLogicReport :: TestGraph owner -> IO (EGraphLogicReport owner SurfaceKind (PackedNode FrontTinySig) NodeCount FrontTinyContext)
expectLogicReport contextualFrontGraph =
  case runEGraphLogic testRunSpec testLogic contextualFrontGraph of
    Right report -> pure report
    Left _ -> assertFailure "expected seeded EGraphLogic run to succeed"

testLogic :: EGraphLogic owner SurfaceKind (PackedNode FrontTinySig) NodeCount FrontTinyContext
testLogic =
  logic $ do
    seedFacts BaseSite (factStoreFor 10)
    seedFacts (ContextSite Rain) (factStoreFor 20)

testRunSpec ::
  ContextRunSpec
    (TestU owner)
    (TestGraph owner)
    RewriteRuleId
    (SaturationReport (TestU owner))
testRunSpec =
  plainContextRunSpec testPlanSpec mempty

testPlanSpec :: PlanSpec (TestU owner) (TestGraph owner) RewriteRuleId
testPlanSpec =
  defaultPlanSpec defaultBudget GenericJoinMatching

seedOnlyObservationProgram :: EGraphFront 'Authored owner FrontTinySig NodeCount FrontTinyContext (MaybeExtraction Int)
seedOnlyObservationProgram =
  egraph $ do
    start <- def @"dirty-start" (sym "x")
    extract @"dirty-extract" termSize start

factStoreFor :: Int -> FactStore
factStoreFor classKey =
  insertFact testFactId (testFactTuple classKey) emptyFactStore

testFactId :: FactId
testFactId =
  FactId 0

testFactTuple :: Int -> FactTuple
testFactTuple classKey =
  FactTuple [ClassId classKey]

readRepoFile :: FilePath -> IO String
readRepoFile relativePath = do
  result <- resolvePackageFile packageMarker relativePath
  either (assertFailure . renderResourcePathError) readFile result

assertRepoFileMissing :: FilePath -> Assertion
assertRepoFileMissing relativePath = do
  result <- resolvePackageFile packageMarker relativePath
  case result of
    Left _missing ->
      pure ()
    Right path ->
      assertFailure ("expected deleted resource file: " <> path)

packageMarker :: FilePath
packageMarker =
  "foundation/moonlight-egraph/moonlight-egraph.cabal"

assertContains :: String -> String -> Assertion
assertContains haystack needle =
  assertBool
    ("expected source to contain " <> show needle)
    (needle `isInfixOf` haystack)

assertLacks :: String -> String -> Assertion
assertLacks haystack needle =
  assertBool
    ("expected source not to contain " <> show needle)
    (not (needle `isInfixOf` haystack))
