module Moonlight.EGraph.Introspection.PruningSpec.Global
  ( tests,
  )
where

import Moonlight.Derived.Pruning
  ( iterativeSpectralPrune,
    spectralPruningGate
  )
import Moonlight.EGraph.Introspection.PruningSpec.CommonPrelude
import Moonlight.EGraph.Introspection.PruningSpec.Fixture

tests :: TestTree
tests =
  testGroup
    "global"
    [ testCase "spectralPruningGate keeps every seed at page zero" testSpectralPruningGatePageZero,
      testCase "spectralPruningGate prunes cells whose bidegree vanishes on a later page" testSpectralPruningGateLaterPage,
      testCase "spectralPruningGate keeps seeds when bidegree lookup fails" testSpectralPruningGateMissingBidegree,
      testCase "iterativeSpectralPrune shrinks monotonically across pages" testIterativeSpectralPrune
    ]

testSpectralPruningGatePageZero :: Assertion
testSpectralPruningGatePageZero =
  spectralPruningGate spectralOracle 0 id prunedCell @?= True

testSpectralPruningGateLaterPage :: Assertion
testSpectralPruningGateLaterPage = do
  spectralPruningGate spectralOracle 1 id keptCell @?= True
  spectralPruningGate spectralOracle 1 id prunedCell @?= False

testSpectralPruningGateMissingBidegree :: Assertion
testSpectralPruningGateMissingBidegree =
  spectralPruningGate conservativeOracle 2 id prunedCell @?= True

testIterativeSpectralPrune :: Assertion
testIterativeSpectralPrune =
  iterativeSpectralPrune spectralOracle id [keptCell, prunedCell]
    @?= [(0, [keptCell, prunedCell]), (1, [keptCell])]
