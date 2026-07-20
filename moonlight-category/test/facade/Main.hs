module Main (main) where

import qualified FacadeTests
import Moonlight.Pale.Test.Runner (runTestTree)

main :: IO ()
main =
  runTestTree FacadeTests.tests
