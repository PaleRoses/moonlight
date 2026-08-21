module FacadeTests
  ( tests,
  )
where

import qualified NotationSpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "facade"
    [NotationSpec.tests]
