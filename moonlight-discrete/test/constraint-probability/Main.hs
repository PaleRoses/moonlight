module Main (main) where

import ConstraintProbabilityTests qualified
import Moonlight.Pale.Test.Runner (runTestTree)

main :: IO ()
main =
  runTestTree ConstraintProbabilityTests.tests
