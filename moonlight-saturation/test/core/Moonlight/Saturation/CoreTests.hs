module Moonlight.Saturation.CoreTests
  ( tests,
  )
where

import Moonlight.Saturation.SaturationSpec qualified as SaturationSpec
import Moonlight.Saturation.EngineSpec (engineTests)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "core"
    [ engineTests,
      SaturationSpec.tests
    ]
