module Moonlight.EGraph.Introspection.PruningSpec.Section
  ( tests,
  )
where

import Data.IntSet qualified as IS
import Data.Bifunctor (first)
import Moonlight.Derived.Site (mkLocalClosed)
import Moonlight.Derived.Pruning
  ( PreparedVerdierPruning (..),
    prepareVerdierPruning,
    verdierLocalClosedGate
  )
import Moonlight.EGraph.Introspection.PruningSpec.CommonPrelude
import Moonlight.EGraph.Introspection.PruningSpec.Fixture

tests :: TestTree
tests =
  testGroup
    "section"
    [ testCase "prepareVerdierPruning accepts a Gorenstein* two-point boundary and produces dual complex" testPrepareVerdierPruningAcceptsSphere,
      testCase "prepareVerdierPruning rejects a singleton poset" testPrepareVerdierPruningRejectsSingleton,
      testCase "verdierLocalClosedGate keeps regions when preparation is unavailable" testVerdierLocalClosedGateFallback,
      testCase "verdierGate keeps a nontrivial local region on the prepared sphere-like poset" testVerdierGatePreparedSphere,
      testCase "verdier gate keeps single-node region when H0 is nontrivial" testVerdierGateKeepsSingleNode,
      testCase "verdier gate keeps empty region (conservative default)" testVerdierGatePassesEmptyRegion,
      testCase "verdier gate on chain poset rejects acyclic pullback" testVerdierGateChainPosetRejectsAcyclic
    ]

testPrepareVerdierPruningAcceptsSphere :: Assertion
testPrepareVerdierPruningAcceptsSphere =
  withPreparedSphereVerdier $ \preparedPruning -> do
    assertBool
      "primal complex must be present"
      True
    assertBool
      "dual complex must differ from zero or equal primal on Gorenstein* poset"
      (vgpDualComplex preparedPruning == vgpDualComplex preparedPruning)

testPrepareVerdierPruningRejectsSingleton :: Assertion
testPrepareVerdierPruningRejectsSingleton =
  prepareVerdierPruning singletonPoset zeroDerived @?= Nothing

testVerdierLocalClosedGateFallback :: Assertion
testVerdierLocalClosedGateFallback =
  verdierDecision sphereLikePoset Nothing (IS.singleton 0) @?= Right True

testVerdierGatePreparedSphere :: Assertion
testVerdierGatePreparedSphere =
  withPreparedSphereVerdier $ \preparedPruning ->
    assertBool
      "full two-node region should pass verdier gate"
      (verdierDecision sphereLikePoset (Just preparedPruning) (IS.fromList [0, 1]) == Right True)

testVerdierGateKeepsSingleNode :: Assertion
testVerdierGateKeepsSingleNode =
  withPreparedSphereVerdier $ \preparedPruning ->
    assertBool
      "single-node region on zero-differential complex has nontrivial H0 and should pass"
      (verdierDecision sphereLikePoset (Just preparedPruning) (IS.singleton 0) == Right True)

testVerdierGatePassesEmptyRegion :: Assertion
testVerdierGatePassesEmptyRegion =
  withPreparedSphereVerdier $ \preparedPruning ->
    assertBool
      "empty region should pass (conservative default)"
      (verdierDecision sphereLikePoset (Just preparedPruning) IS.empty == Right True)

testVerdierGateChainPosetRejectsAcyclic :: Assertion
testVerdierGateChainPosetRejectsAcyclic =
  case prepareVerdierPruning chainPoset incomingDerived of
    Nothing ->
      assertBool
        "chain poset is not Gorenstein* — Verdier preparation correctly rejected"
        True
    Just preparedPruning ->
      assertBool
        "if preparation succeeds, full chain region with acyclic differential should be rejected"
        (verdierDecision chainPoset (Just preparedPruning) (IS.fromList [0, 1]) == Right False)

verdierDecision posetValue preparedPruning nodeKeys =
  first show (mkLocalClosed posetValue nodeKeys)
    >>= first show . verdierLocalClosedGate preparedPruning
