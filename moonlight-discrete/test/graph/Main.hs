module Main (main) where

import GraphTests qualified
import Moonlight.Pale.Test.Runner (runTestTree)

main :: IO ()
main =
  runTestTree GraphTests.tests
