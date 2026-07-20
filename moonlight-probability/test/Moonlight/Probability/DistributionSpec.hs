module Moonlight.Probability.DistributionSpec
  ( tests,
  )
where

import Moonlight.Probability.Distribution
import Moonlight.Probability.Distribution.Parametric
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

approxEq :: Double -> Double -> Double -> Bool
approxEq tolerance left right = abs (left - right) <= tolerance

tests :: TestTree
tests =
  testGroup
    "distribution"
    [ testCase "normal distribution exposes mean and cdf" $
        case mkNormalDistribution 0.0 1.0 of
          Left err -> assertBool (show err) False
          Right distribution -> do
            assertBool "mean" (approxEq 1.0e-12 (distributionMean distribution) 0.0)
            assertBool "cdf at zero" (approxEq 1.0e-10 (distributionCumulative distribution 0.0) 0.5),
      testCase "uniform distribution quantile inverts midpoint" $
        case mkUniformDistribution (-2.0) 2.0 of
          Left err -> assertBool (show err) False
          Right distribution ->
            assertBool "uniform median" (approxEq 1.0e-12 (distributionQuantile distribution 0.5) 0.0)
    ]
