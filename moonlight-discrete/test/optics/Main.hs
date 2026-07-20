module Main (main) where

import Moonlight.Pale.Test.Runner (runTestTree)
import OpticsTests qualified

main :: IO ()
main =
  runTestTree OpticsTests.tests
