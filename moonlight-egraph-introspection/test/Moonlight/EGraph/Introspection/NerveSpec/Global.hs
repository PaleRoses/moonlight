module Moonlight.EGraph.Introspection.NerveSpec.Global
  ( tests,
  )
where

import Test.Tasty (TestTree, testGroup)
import qualified Moonlight.EGraph.Introspection.NerveSpec.Global.Homology as Homology
import qualified Moonlight.EGraph.Introspection.NerveSpec.Global.Persistence as Persistence

tests :: TestTree
tests =
  testGroup
    "global"
    [ Homology.tests,
      Persistence.tests
    ]
