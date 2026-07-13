module Main (main) where

import ConstraintTests qualified
import Moonlight.Pale.Test.Runner (runTestTree)

main :: IO ()
main =
  runTestTree ConstraintTests.tests
