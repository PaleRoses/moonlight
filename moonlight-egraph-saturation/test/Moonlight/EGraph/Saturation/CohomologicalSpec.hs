module Moonlight.EGraph.Saturation.CohomologicalSpec
  ( cohomologicalTests
  )
where

import Test.Tasty (TestTree, testGroup)
import qualified Moonlight.EGraph.Saturation.CohomologicalSpec.Site as Site
import qualified Moonlight.EGraph.Saturation.CohomologicalSpec.Bench as Bench

cohomologicalTests :: TestTree
cohomologicalTests =
  testGroup
    "Cohomological"
    [ Site.tests
    , Bench.tests
    ]
