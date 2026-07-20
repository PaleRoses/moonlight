module Main (main) where

import Moonlight.Pale.Test.Runner (runTestTree)
import Moonlight.Saturation.ProtocolTests qualified as ProtocolTests

main :: IO ()
main =
  runTestTree ProtocolTests.tests
