module Main (main) where

import Moonlight.Flow.Execution.Dense.WCOJ.DeltaAgreementSpec qualified as DeltaAgreementSpec
import Moonlight.Flow.Execution.MatchingSpec qualified as MatchingSpec
import Moonlight.Flow.Execution.Prepared.MatchingSpec qualified as PreparedMatchingSpec
import Test.Tasty
  ( defaultMain,
    testGroup,
  )

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-flow-execution"
        [ MatchingSpec.tests,
          PreparedMatchingSpec.tests,
          DeltaAgreementSpec.tests
        ]
    )
