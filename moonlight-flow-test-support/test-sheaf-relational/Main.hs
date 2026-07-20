module Main (main) where

import Moonlight.Sheaf.Relational.Carrier.FactSpec qualified as CarrierFactSpec
import Test.Tasty
  ( defaultMain,
    testGroup,
  )

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-sheaf-relational"
        [ CarrierFactSpec.tests
        ]
    )
