module Moonlight.EGraph.Introspection.NerveSpec.Site
  ( tests,
  )
where

import Test.Tasty (TestTree, testGroup)
import qualified Moonlight.EGraph.Introspection.NerveSpec.Site.Core as Core
import qualified Moonlight.EGraph.Introspection.NerveSpec.Site.Context as Context

tests :: TestTree
tests =
  testGroup
    "site"
    [ Core.tests,
      Context.tests
    ]
