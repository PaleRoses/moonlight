module Moonlight.EGraph.Introspection.PropertySpec.Section
  ( tests,
  )
where

import Moonlight.EGraph.Introspection.PropertySpec.CommonPrelude
import Moonlight.EGraph.Introspection.PropertySpec.Fixture

tests :: TestTree
tests =
  testGroup
    "section"
    [ testProperty "rewrite contexts satisfy lattice absorption" propLatticeAbsorption,
      testProperty "contextLeq derives from lattice meet" propContextLeqDerivable
    ]

propLatticeAbsorption :: GeneratedContextPair -> Property
propLatticeAbsorption generatedContextPair =
  generatedContextPairProperty generatedContextPair $ \contextPair ->
    let leftContext = gcpLeftContext contextPair
        rightContext = gcpRightContext contextPair
        leftAbsorption = join leftContext (meet leftContext rightContext) == leftContext
        rightAbsorption = meet leftContext (join leftContext rightContext) == leftContext
        dualLeftAbsorption = join rightContext (meet rightContext leftContext) == rightContext
        dualRightAbsorption = meet rightContext (join rightContext leftContext) == rightContext
     in leftAbsorption && rightAbsorption && dualLeftAbsorption && dualRightAbsorption

propContextLeqDerivable :: GeneratedContextPair -> Property
propContextLeqDerivable generatedContextPair =
  generatedContextPairProperty generatedContextPair $ \contextPair ->
    let rewriteSystem :: RewriteSystem ArithF
        rewriteSystem = mkRewriteSystem []
        leftContext = gcpLeftContext contextPair
        rightContext = gcpRightContext contextPair
     in contextLeq rewriteSystem leftContext rightContext == (meet leftContext rightContext == leftContext)
