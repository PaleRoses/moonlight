module Main (main) where

import AutomataTests qualified
import ConstraintProbabilityTests qualified
import ConstraintTests qualified
import GraphTests qualified
import Moonlight.Pale.Test.Runner (runTestTreeGroup)
import OpticsTests qualified

main :: IO ()
main =
  runTestTreeGroup
    "moonlight-discrete"
    [ AutomataTests.tests,
      ConstraintTests.tests,
      ConstraintProbabilityTests.tests,
      GraphTests.tests,
      OpticsTests.tests
    ]
