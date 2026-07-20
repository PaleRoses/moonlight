module Moonlight.Pale.Test.Site.AssertionSpec
  ( tests,
  )
where

import Moonlight.Pale.Test.Site.Assertion (expectRight, expectRightWithLabel)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "Moonlight.Pale.Test.Site.Assertion"
    [ testCase "unwraps an unlabeled Right" $
        expectRight (Right "value" :: Either String String) >>= (@?= "value"),
      testCase "unwraps a labeled Right" $
        expectRightWithLabel "fixture" (Right "value" :: Either String String) >>= (@?= "value")
    ]
