module TimeFrontierSpec
  ( tests,
  )
where

import Test.Tasty
  ( TestTree,
    testGroup,
  )

tests :: TestTree
tests =
  testGroup
    "time, frontier, and local fact laws"
    []
