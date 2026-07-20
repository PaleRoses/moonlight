module Main (main) where

import Moonlight.Pale.Test.Runner (runTestTree)
import Moonlight.Saturation.ObstructionTests qualified as ObstructionTests

main :: IO ()
main =
  runTestTree ObstructionTests.tests
