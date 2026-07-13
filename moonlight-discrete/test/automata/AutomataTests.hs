module AutomataTests (tests) where

import AutomataLaws qualified
import RepoDisciplineSpec qualified
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "moonlight-discrete:automata"
    [ AutomataLaws.tests,
      RepoDisciplineSpec.tests
    ]
