module Main (main) where

import Moonlight.Pale.Test.Runner (runTestTreeGroup)
import Moonlight.Saturation.ContextTests qualified as ContextTests
import Moonlight.Saturation.CoreTests qualified as CoreTests
import Moonlight.Saturation.ObstructionTests qualified as ObstructionTests
import Moonlight.Saturation.ProtocolTests qualified as ProtocolTests

main :: IO ()
main =
  runTestTreeGroup
    "moonlight-saturation"
    [ CoreTests.tests,
      ProtocolTests.tests,
      ContextTests.tests,
      ObstructionTests.tests
    ]
