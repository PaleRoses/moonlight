module Moonlight.EGraph.Introspection.PruningSpec.Global
  ( tests,
  )
where

import Moonlight.Derived.Pruning
  ( SpectralPruningFailure (..),
    iterativeSpectralPrune,
    spectralPruningGate
  )
import Moonlight.EGraph.Introspection.PruningSpec.CommonPrelude
import Moonlight.EGraph.Introspection.PruningSpec.Fixture

tests :: TestTree
tests =
  testGroup
    "global"
    [ testCase "spectralPruningGate rejects non-positive page indices" testSpectralPruningGatePageZero,
      testCase "spectralPruningGate prunes cells whose bidegree vanishes on a later page" testSpectralPruningGateLaterPage,
      testCase "spectralPruningGate reports unavailable pages as typed obstructions" testSpectralPruningGateUnavailablePage,
      testCase "iterativeSpectralPrune shrinks monotonically across pages" testIterativeSpectralPrune
    ]

testSpectralPruningGatePageZero :: Assertion
testSpectralPruningGatePageZero =
  spectralPruningGate spectralOracle 0 id prunedCell @?= Left (SpectralPageIndexNonPositive 0)

testSpectralPruningGateLaterPage :: Assertion
testSpectralPruningGateLaterPage = do
  spectralPruningGate spectralOracle 1 id keptCell @?= Right True
  spectralPruningGate spectralOracle 1 id prunedCell @?= Right False

testSpectralPruningGateUnavailablePage :: Assertion
testSpectralPruningGateUnavailablePage =
  spectralPruningGate spectralOracle 2 id prunedCell @?= Left (SpectralPageUnavailable 2)

testIterativeSpectralPrune :: Assertion
testIterativeSpectralPrune =
  iterativeSpectralPrune spectralOracle id [keptCell, prunedCell]
    @?= [(0, [keptCell, prunedCell]), (1, [keptCell])]
