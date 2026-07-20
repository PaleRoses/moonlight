module Moonlight.EGraph.Introspection.PruningSpec
  ( tests,
  )
where

import Test.Tasty (TestTree, testGroup)
import qualified Moonlight.EGraph.Introspection.PruningSpec.Section as Section
import qualified Moonlight.EGraph.Introspection.PruningSpec.Gluing as Gluing
import qualified Moonlight.EGraph.Introspection.PruningSpec.Global as Global

tests :: TestTree
tests =
  testGroup
    "Pruning"
    [ Section.tests,
      Gluing.tests,
      Global.tests
    ]
