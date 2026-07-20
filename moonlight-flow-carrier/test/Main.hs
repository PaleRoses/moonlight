module Main (main) where

import Moonlight.Flow.Carrier.Morphism.SubsumptionProjectionSpec qualified as SubsumptionProjectionSpec
import Moonlight.Flow.Carrier.Store.TraceCharacterizationSpec qualified as TraceCharacterizationSpec
import Test.Tasty
  ( defaultMain,
    testGroup,
  )

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-flow-carrier-internal"
        [ SubsumptionProjectionSpec.spec,
          TraceCharacterizationSpec.spec
        ]
    )
