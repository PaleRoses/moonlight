module Main (main) where

import FiniteLatticeTests qualified
import Moonlight.Pale.Test.Runner (runTestTree)

main :: IO ()
main =
  runTestTree FiniteLatticeTests.tests
