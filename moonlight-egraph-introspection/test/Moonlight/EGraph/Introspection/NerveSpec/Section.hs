module Moonlight.EGraph.Introspection.NerveSpec.Section
  ( tests,
  )
where

import Test.Tasty (TestTree, testGroup)
import qualified Moonlight.EGraph.Introspection.NerveSpec.Section.Scheduling as Scheduling

tests :: TestTree
tests =
  testGroup
    "section"
    [ Scheduling.tests
    ]
