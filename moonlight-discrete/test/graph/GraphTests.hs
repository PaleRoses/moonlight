module GraphTests (tests) where

import DeltaSpec qualified
import LocalTopologySpec qualified
import OpticsSpec qualified
import SelectorSpec qualified
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "moonlight-discrete:graph"
    [ DeltaSpec.tests,
      LocalTopologySpec.tests,
      SelectorSpec.tests,
      OpticsSpec.tests
    ]
