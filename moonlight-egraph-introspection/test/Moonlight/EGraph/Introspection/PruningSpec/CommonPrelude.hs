module Moonlight.EGraph.Introspection.PruningSpec.CommonPrelude
  ( module Test.Tasty,
    module Test.Tasty.HUnit,
  )
where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    (@?=),
    assertBool,
    assertFailure,
    testCase,
  )
