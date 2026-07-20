module Main (main) where

import Moonlight.Flow.Plan.PlanSpec qualified as PlanSpec
import Moonlight.Flow.Plan.ResidualSpec qualified as ResidualSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-flow-plan"
        [ PlanSpec.tests,
          ResidualSpec.tests
        ]
    )
