module Moonlight.EGraph.Introspection.PropertySpec.Gluing
  ( tests,
  )
where

import Moonlight.EGraph.Introspection.PropertySpec.CommonPrelude
import Moonlight.EGraph.Introspection.PropertySpec.Fixture
import Moonlight.EGraph.Introspection.Analysis.Obstruction (nerveObstructions)

tests :: TestTree
tests =
  testGroup
    "gluing"
    [ testProperty "obstruction representatives satisfy the cocycle condition" propObstructionCocycleCondition
    ]

propObstructionCocycleCondition :: GeneratedRewriteSystem -> Property
propObstructionCocycleCondition generatedRewriteSystem =
  generatedRewriteSystemProperty generatedRewriteSystem $ \rewriteSystem ->
    let siteValue = mkGrothendieckSite rewriteSystem (fromIntegral analysisDepth)
     in case (grothendieckChainComplexFromSite siteValue, nerveObstructions rewriteSystem (fromIntegral analysisDepth)) of
          (Left failure, _) ->
            Left ("Grothendieck chain complex failed: " <> show failure)
          (_, Left failure) ->
            Left ("obstruction extraction failed: " <> show failure)
          (Right chainComplexValue, Right obstructionClasses) ->
            Right (all (obstructionRepresentativeClosed chainComplexValue) obstructionClasses)
