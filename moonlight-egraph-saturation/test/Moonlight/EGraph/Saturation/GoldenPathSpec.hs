module Moonlight.EGraph.Saturation.GoldenPathSpec
  ( goldenPathTests,
  )
where

import Test.Tasty (TestTree, testGroup)
import qualified Moonlight.EGraph.Saturation.GoldenPathSpec.Cohomological as Cohomological
import qualified Moonlight.EGraph.Saturation.GoldenPathSpec.Pruning as Pruning
import qualified Moonlight.EGraph.Saturation.GoldenPathSpec.Sheaf as Sheaf

goldenPathTests :: TestTree
goldenPathTests =
  testGroup
    "GoldenPath"
    [ Cohomological.tests,
      Pruning.tests,
      Sheaf.tests
    ]
