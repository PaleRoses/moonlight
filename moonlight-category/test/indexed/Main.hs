module Main (main) where

import qualified IndexedTests
import Moonlight.Pale.Test.Runner (runTestTree)

main :: IO ()
main =
  runTestTree IndexedTests.tests
