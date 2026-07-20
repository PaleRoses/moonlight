module Moonlight.EGraph.Introspection.PruningSpec.Section
  ( tests,
  )
where

import Data.IntSet qualified as IS
import Data.Bifunctor (first)
import Moonlight.Derived.Complex (derivedPoset)
import Moonlight.Derived.Site (DerivedPoset, mkLocalClosed)
import Moonlight.Derived.Pruning
  ( VerdierPreparation (..),
    prepareVerdierPruning,
    preparedVerdierDual,
    preparedVerdierPrimal,
    verdierLocalClosedGate
  )
import Moonlight.Derived.Triangulated qualified as Triangulated
import Moonlight.EGraph.Introspection.PruningSpec.CommonPrelude
import Moonlight.EGraph.Introspection.PruningSpec.Fixture

tests :: TestTree
tests =
  testGroup
    "section"
    [ testCase "prepareVerdierPruning accepts a Gorenstein* two-point boundary and produces dual complex" testPrepareVerdierPruningAcceptsSphere,
      testCase "prepareVerdierPruning rejects a singleton poset" testPrepareVerdierPruningRejectsSingleton,
      testCase "verdierLocalClosedGate keeps regions when preparation is unavailable" testVerdierLocalClosedGateFallback,
      testCase "verdierGate rejects a full region whose dual restriction vanishes" testVerdierGateRejectsPreparedSphere,
      testCase "verdier gate rejects a singleton region whose dual restriction vanishes" testVerdierGateRejectsSingleNode,
      testCase "verdier gate keeps empty region (conservative default)" testVerdierGatePassesEmptyRegion,
      testCase "Verdier preparation marks a chain non-applicable and the gate remains conservative" testVerdierGateChainPosetFallback
    ]

testPrepareVerdierPruningAcceptsSphere :: Assertion
testPrepareVerdierPruningAcceptsSphere =
  withPreparedSphereVerdier $ \preparedPruning -> do
    preparedVerdierPrimal preparedPruning @?= zeroDerived
    derivedPoset (preparedVerdierDual preparedPruning) @?= sphereLikePoset

testPrepareVerdierPruningRejectsSingleton :: Assertion
testPrepareVerdierPruningRejectsSingleton =
  prepareVerdierPruning (Triangulated.zeroDerived singletonPoset)
    @?= Right VerdierNotApplicable

testVerdierLocalClosedGateFallback :: Assertion
testVerdierLocalClosedGateFallback =
  verdierDecision sphereLikePoset VerdierNotApplicable (IS.singleton 0) @?= Right True

testVerdierGateRejectsPreparedSphere :: Assertion
testVerdierGateRejectsPreparedSphere =
  withPreparedSphereVerdier $ \preparedPruning ->
    verdierDecision sphereLikePoset (VerdierPrepared preparedPruning) (IS.fromList [0, 1])
      @?= Right False

testVerdierGateRejectsSingleNode :: Assertion
testVerdierGateRejectsSingleNode =
  withPreparedSphereVerdier $ \preparedPruning ->
    verdierDecision sphereLikePoset (VerdierPrepared preparedPruning) (IS.singleton 0)
      @?= Right False

testVerdierGatePassesEmptyRegion :: Assertion
testVerdierGatePassesEmptyRegion =
  withPreparedSphereVerdier $ \preparedPruning ->
    assertBool
      "empty region should pass (conservative default)"
      (verdierDecision sphereLikePoset (VerdierPrepared preparedPruning) IS.empty == Right True)

testVerdierGateChainPosetFallback :: Assertion
testVerdierGateChainPosetFallback =
  case prepareVerdierPruning incomingDerived of
    Left preparationFailure ->
      assertFailure ("unexpected Verdier preparation failure: " <> show preparationFailure)
    Right preparation -> do
      preparation @?= VerdierNotApplicable
      verdierDecision chainPoset preparation (IS.fromList [0, 1]) @?= Right True

verdierDecision :: DerivedPoset -> VerdierPreparation -> IS.IntSet -> Either String Bool
verdierDecision posetValue preparedPruning nodeKeys =
  first show (mkLocalClosed posetValue nodeKeys)
    >>= first show . verdierLocalClosedGate preparedPruning
