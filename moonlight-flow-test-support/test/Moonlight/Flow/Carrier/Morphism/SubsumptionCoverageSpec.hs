module Moonlight.Flow.Carrier.Morphism.SubsumptionCoverageSpec
  ( spec,
  )
where

import Moonlight.Flow.Carrier.Core.Coverage
  ( ObstructionToken (..),
    CoverageFact (..),
    obstructedCoverage,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

spec :: TestTree
spec =
  testGroup
    "Containment coverage"
    [ testCase "accepts lower-bound containment material" $
        containmentCoverageReusable LowerBound @?= True,
      testCase "rejects obstructed containment material" $
        containmentCoverageReusable (obstructedCoverage (ObstructionToken 7)) @?= False,
      testCase "accepts exact containment material" $
        containmentCoverageReusable ExactLocal @?= True
    ]
