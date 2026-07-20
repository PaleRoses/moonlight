module Moonlight.Saturation.ContextTests
  ( tests,
  )
where

import Moonlight.Saturation.ContextSpec (contextTests)
import Moonlight.Saturation.RuntimeCandidateSpec (runtimeCandidateTests)
import Moonlight.Saturation.SchedulerSpec (schedulerTests)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "context"
    [ contextTests,
      runtimeCandidateTests,
      schedulerTests
    ]
