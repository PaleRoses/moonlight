module Main (main) where

import Moonlight.Saturation.ContextSpec (contextTests)
import Moonlight.Saturation.RuntimeCandidateSpec (runtimeCandidateTests)
import Moonlight.Saturation.SchedulerSpec (schedulerTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-saturation"
        [ contextTests,
          runtimeCandidateTests,
          schedulerTests
        ]
    )
