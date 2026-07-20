module Main (main) where

import qualified AbstractTests
import Moonlight.Pale.Test.Runner (runTestTree)

main :: IO ()
main =
  runTestTree AbstractTests.tests
