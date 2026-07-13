module Main (main) where

import AutomataTests qualified
import Moonlight.Pale.Test.Runner (runTestTree)

main :: IO ()
main =
  runTestTree AutomataTests.tests
