module Moonlight.Geometry.Gluing.RewriteSpec (tests) where

import Moonlight.Geometry.Gluing.Rewrite
import Moonlight.Geometry.Site.Parameters (allNoiseKernels)
import Moonlight.Geometry.Site.Token (SDFTokenF)
import Moonlight.Rewrite.System qualified as Rewrite
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "Rewrite"
    [ testCase "base rules compile" $ do
        assertBool "expected planned rewrite catalog" (either (const False) (not . null . Rewrite.rulePlans) (compileSdfBaseRules :: Either (SdfRewriteError ()) (Rewrite.RulePlanSet () SDFTokenF))),
      testCase "neutral noise rules cover all kernels" $ do
        assertBool "expected rewrite catalog to track all kernels" (either (const False) ((== expectedRuleCount) . length) sdfBaseRules)
    ]

expectedRuleCount :: Int
expectedRuleCount = 16 + 2 * length allNoiseKernels
