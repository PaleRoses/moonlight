module Moonlight.Probability.CategoricalSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Data.Set qualified as Set
import Moonlight.Probability.Core (mkProb, positiveProbOne, positiveProbValue)
import Moonlight.Probability.Distribution.Categorical
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

approxEq :: Double -> Double -> Double -> Bool
approxEq tolerance left right = abs (left - right) <= tolerance

tests :: TestTree
tests =
  testGroup
    "categorical"
    [ testCase "normalizes input weights" $
        case mkCategorical (Map.fromList [('a', 2.0), ('b', 6.0)]) of
          Left err -> assertBool (show err) False
          Right categorical ->
            let totalProbability = getSum (categoricalFoldMap (Sum . positiveProbValue . snd) categorical)
             in assertBool "normalized" (approxEq 1.0e-12 totalProbability 1.0),
      testCase "restriction preserves remaining support" $
        case mkCategorical (Map.fromList [('a', 1.0), ('b', 1.0), ('c', 1.0)]) of
          Left err -> assertBool (show err) False
          Right categorical ->
            case categoricalRestrict (Set.fromList ['a', 'c']) categorical of
              Nothing -> assertBool "expected restricted categorical" False
              Just restricted ->
                assertBool "restricted support" (categoricalSupport restricted == Set.fromList ['a', 'c']),
      testCase "blendCategorical merges supports and remains normalized" $
        case
          ( mkCategorical (Map.fromList [('a', 1.0)]),
            mkCategorical (Map.fromList [('b', 3.0)])
          ) of
          (Right leftCategorical, Right rightCategorical) ->
            let blended =
                  blendCategorical
                    ((positiveProbOne, leftCategorical) :| [(positiveProbOne, rightCategorical)])
                totalProbability = getSum (categoricalFoldMap (Sum . positiveProbValue . snd) blended)
             in assertBool
                  "blended support and normalization"
                  (categoricalSupport blended == Set.fromList ['a', 'b'] && approxEq 1.0e-12 totalProbability 1.0)
          _ -> assertBool "expected valid categoricals" False,
      testCase "collapse samples from support" $
        case (mkCategorical (Map.fromList [('a', 1.0), ('b', 9.0)]), mkProb 0.95) of
          (Left err, _) -> assertBool (show err) False
          (_, Left err) -> assertBool (show err) False
          (Right categorical, Right threshold) ->
            assertBool "collapse picks supported value" (categoricalCollapseAt threshold categorical == 'b')
    ]

