module Main (main) where

import Moonlight.Pale.Test.Runner (runTestTree)
import Moonlight.Saturation.CoreTests qualified as CoreTests

main :: IO ()
main =
  runTestTree CoreTests.tests
