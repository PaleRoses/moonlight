module ConstraintProbabilityTests (tests) where

import Test.Tasty (TestTree, testGroup)
import WFCProbabilitySpec qualified

tests :: TestTree
tests =
  testGroup
    "moonlight-discrete:constraint-probability"
    [WFCProbabilitySpec.tests]
