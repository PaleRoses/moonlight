module Moonlight.EGraph.Introspection.NerveSpec
  ( tests,
  )
where

import Test.Tasty (TestTree, testGroup)
import qualified Moonlight.EGraph.Introspection.NerveSpec.Site as Site
import qualified Moonlight.EGraph.Introspection.NerveSpec.Section as Section
import qualified Moonlight.EGraph.Introspection.NerveSpec.Handlers as Handlers
import qualified Moonlight.EGraph.Introspection.NerveSpec.Gluing as Gluing
import qualified Moonlight.EGraph.Introspection.NerveSpec.Global as Global

tests :: TestTree
tests =
  testGroup
    "introspection"
    [ Site.tests,
      Section.tests,
      Handlers.tests,
      Gluing.tests,
      Global.tests
    ]
