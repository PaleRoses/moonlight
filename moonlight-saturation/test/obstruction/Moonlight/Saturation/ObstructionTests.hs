module Moonlight.Saturation.ObstructionTests
  ( tests,
  )
where

import Moonlight.Saturation.AggregateSpec (aggregateTests)
import Moonlight.Saturation.AlgebraLawsSpec (algebraLawsTests)
import Moonlight.Saturation.LivePruningSpec (livePruningTests)
import Moonlight.Saturation.ObstructionEffectSpec (obstructionEffectTests)
import Moonlight.Saturation.RegionSpec (regionTests)
import Moonlight.Saturation.SearchSpec (searchTests)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "obstruction"
    [ aggregateTests,
      algebraLawsTests,
      livePruningTests,
      obstructionEffectTests,
      regionTests,
      searchTests
    ]
