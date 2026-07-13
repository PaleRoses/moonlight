module Main (main) where

import qualified FiniteTests
import Moonlight.Pale.Test.Runner (runTestTree)

main :: IO ()
main =
  runTestTree FiniteTests.tests
