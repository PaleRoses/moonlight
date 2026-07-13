module Main (main) where

import AbstractTests qualified
import Moonlight.Pale.Test.Runner (runTestTree)

main :: IO ()
main =
  runTestTree AbstractTests.tests
