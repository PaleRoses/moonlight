module Main (main) where

import Moonlight.Pale.Test.Runner (runTestTree)
import Moonlight.Saturation.ContextTests qualified as ContextTests

main :: IO ()
main =
  runTestTree ContextTests.tests
