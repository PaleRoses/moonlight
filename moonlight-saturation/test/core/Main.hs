module Main (main) where

import Moonlight.Saturation.SaturationSpec qualified as SaturationSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-saturation:core"
        [ SaturationSpec.tests
        ]
    )
