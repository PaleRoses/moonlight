module Moonlight.EGraph.Introspection.NerveSpec.Gluing
  ( tests,
  )
where

import Test.Tasty (TestTree, testGroup)
import qualified Moonlight.EGraph.Introspection.NerveSpec.Gluing.Relative as Relative

tests :: TestTree
tests =
  testGroup
    "gluing"
    [ Relative.tests
    ]
