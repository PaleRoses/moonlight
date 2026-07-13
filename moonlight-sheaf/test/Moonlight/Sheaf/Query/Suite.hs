module Moonlight.Sheaf.Query.Suite
  ( tests,
  )
where

import Moonlight.Sheaf.Query.PresheafSpec qualified as QueryPresheafSpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "query"
    [ QueryPresheafSpec.tests
    ]
