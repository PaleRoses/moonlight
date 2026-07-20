module Moonlight.Probability.SampleSpec
  ( tests,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.Probability.Distribution.Categorical (mkCategorical)
import Moonlight.Probability.Sample
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, (@?=), testCase)

approxEq :: Double -> Double -> Double -> Bool
approxEq tolerance left right = abs (left - right) <= tolerance

tests :: TestTree
tests =
  testGroup
    "sample"
    [ testCase "nextDouble is deterministic for a fixed seed" $ do
        let left = evalSample 42 nextDouble
            right = evalSample 42 nextDouble
        left @?= right,
      testCase "sampleCategorical is deterministic for a fixed seed" $
        case mkCategorical (Map.fromList [('x', 1.0), ('y', 3.0)]) of
          Left err -> assertBool (show err) False
          Right categorical ->
            let left = evalSample 7 (sampleCategorical categorical)
                right = evalSample 7 (sampleCategorical categorical)
             in left @?= right,
      testCase "sampleGamma is deterministic for a fixed seed" $ do
        let left = evalSample 11 (sampleGamma 2.5)
            right = evalSample 11 (sampleGamma 2.5)
        left @?= right,
      testCase "sampleGamma produces a positive variate for positive shape" $
        assertBool
          "positive gamma sample"
          (evalSample 19 (sampleGamma 2.5) > 0.0),
      testCase "sampleDirichlet is deterministic for a fixed seed" $ do
        let left = evalSample 23 (sampleDirichlet [0.5, 1.5, 2.0])
            right = evalSample 23 (sampleDirichlet [0.5, 1.5, 2.0])
        left @?= right,
      testCase "sampleDirichlet produces nonnegative weights summing to one" $
        let weights = evalSample 29 (sampleDirichlet [0.5, 1.5, 2.0])
            totalWeight = sum weights
         in assertBool
              "normalized dirichlet sample"
              (all (>= 0.0) weights && approxEq 1.0e-10 totalWeight 1.0)
    ]
