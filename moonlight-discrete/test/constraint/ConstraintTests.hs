module ConstraintTests (tests) where

import ArcConsistencySpec qualified
import CNFSpec qualified
import CSPSpec qualified
import CoFiniteTruthSpec qualified
import ConstraintLaws qualified
import DPLLSpec qualified
import EndoPatchSpec qualified
import EvaluateSpec qualified
import NormalizeSpec qualified
import Test.Tasty (TestTree, testGroup)
import WFCAutomataLawSpec qualified
import WFCSpec qualified

tests :: TestTree
tests =
  testGroup
    "moonlight-discrete:constraint"
    [ ConstraintLaws.tests,
      ArcConsistencySpec.tests,
      NormalizeSpec.tests,
      CNFSpec.tests,
      CSPSpec.tests,
      DPLLSpec.tests,
      EvaluateSpec.tests,
      CoFiniteTruthSpec.tests,
      EndoPatchSpec.tests,
      WFCAutomataLawSpec.tests,
      WFCSpec.tests
    ]
