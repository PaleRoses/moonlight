module Main (main) where

import Moonlight.Saturation.AggregateSpec (aggregateTests)
import Moonlight.Saturation.ObstructionEffectSpec (obstructionEffectTests)
import Moonlight.Saturation.RegionSpec (regionTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-saturation-obstruction"
        [ aggregateTests,
          obstructionEffectTests,
          regionTests
        ]
    )
