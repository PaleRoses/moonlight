module Main (main) where

import Moonlight.Control.FeedbackSpec qualified as FeedbackSpec
import Moonlight.Control.NumericalSpec qualified as NumericalSpec
import Moonlight.Control.PerturbationSpec (perturbationTests)
import Moonlight.Control.SuccessorSpec (successorTests)
import Moonlight.Control.SupportSpec qualified as SupportSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-control"
        [ perturbationTests,
          NumericalSpec.tests,
          FeedbackSpec.tests,
          successorTests,
          SupportSpec.tests
        ]
    )
