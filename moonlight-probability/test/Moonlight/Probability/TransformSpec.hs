module Moonlight.Probability.TransformSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Data.Set qualified as Set
import Moonlight.Probability.Core (positiveProbValue)
import Moonlight.Probability.Distribution
  ( distributionCumulative,
    distributionDensity,
    distributionQuantile,
  )
import Moonlight.Probability.Distribution.Categorical
  ( Categorical,
    categoricalFoldMap,
    categoricalSupport,
    mkCategorical,
  )
import Moonlight.Probability.Distribution.Parametric (mkNormalDistribution)
import Moonlight.Probability.Distribution.Transform
  ( conditionCategorical,
    mixtureCategorical,
    mixtureComponents,
    mkMixtureDistribution,
    mkTruncatedDistribution,
    truncatedBounds,
  )
import Moonlight.Probability.TestSupport.Generators
  ( PositiveWeightSample,
    withCategoricalFromPositiveWeights,
    withDisjointCategoricalPairFromPositiveWeights,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck
  ( NonEmptyList (..),
    Positive (..),
    Property,
    counterexample,
    testProperty,
  )

approxEq :: Double -> Double -> Double -> Bool
approxEq tolerance left right = abs (left - right) <= tolerance

tests :: TestTree
tests =
  testGroup
    "transform"
    [ testProperty "mixture distribution normalizes positive weights" propMixtureDistributionNormalizesWeights,
      testProperty "mixture categorical preserves unioned support and normalization" propMixtureCategoricalPreservesUnionedSupportAndNormalization,
      testProperty "conditioning preserves filtered support and normalization" propConditionCategoricalPreservesFilteredSupportAndNormalization,
      testProperty "conditioning rejects empty retained support" propConditionCategoricalRejectsEmptySupport,
      testProperty "truncation normalizes bounds and external density" propTruncatedDistributionNormalizesBoundsAndExternalDensity,
      testCase "mixture distribution normalizes positive weights (example)" $
        case mkMixtureDistribution ((2.0, 1.0 :: Double) :| [(6.0, 3.0)]) of
          Left err -> assertBool (show err) False
          Right mixture ->
            let totalWeight = sum (fmap (positiveProbValue . fst) (mixtureComponents mixture))
             in assertBool "normalized weights" (approxEq 1.0e-12 totalWeight 1.0),
      testCase "mixture categorical merges supports" $
        case
          ( mkCategorical (Map.singleton 'a' 1.0),
            mkCategorical (Map.singleton 'b' 1.0)
          ) of
          (Right leftCategorical, Right rightCategorical) ->
            case mixtureCategorical ((1.0, leftCategorical) :| [(3.0, rightCategorical)]) of
              Left err -> assertBool (show err) False
              Right blended ->
                assertBool "combined support" (categoricalSupport blended == Set.fromList ['a', 'b'])
          _ -> assertBool "expected valid categoricals" False,
      testCase "truncated distribution preserves interval and finite mean" $
        case mkNormalDistribution 0.0 1.0 of
          Left err -> assertBool (show err) False
          Right normal ->
            case mkTruncatedDistribution (-1.0, 1.0) normal of
              Left err -> assertBool (show err) False
              Right truncated ->
                assertBool
                  "bounded truncation"
                  (truncatedBounds truncated == (-1.0, 1.0) && approxEq 1.0e-12 (distributionCumulative truncated (-1.0)) 0.0 && approxEq 1.0e-12 (distributionCumulative truncated 1.0) 1.0)
    ]

propMixtureDistributionNormalizesWeights :: PositiveWeightSample -> Property
propMixtureDistributionNormalizesWeights positiveWeights =
  case mkMixtureDistribution (weightedValues positiveWeights) of
    Left err -> counterexample (show err) False
    Right mixture ->
      let totalWeight = sum (fmap (positiveProbValue . fst) (mixtureComponents mixture))
       in counterexample
            ("total mixture weight=" <> show totalWeight)
            (approxEq 1.0e-12 totalWeight 1.0)

propMixtureCategoricalPreservesUnionedSupportAndNormalization ::
  PositiveWeightSample ->
  PositiveWeightSample ->
  Positive Int ->
  Positive Int ->
  Property
propMixtureCategoricalPreservesUnionedSupportAndNormalization leftWeights rightWeights leftSeed rightSeed =
  withDisjointCategoricalPairFromPositiveWeights
    leftWeights
    rightWeights
    (\leftCategorical rightCategorical ->
       let expectedSupport = Set.union (categoricalSupport leftCategorical) (categoricalSupport rightCategorical)
           leftWeight = fromIntegral (getPositive leftSeed)
           rightWeight = fromIntegral (getPositive rightSeed)
        in case mixtureCategorical ((leftWeight, leftCategorical) :| [(rightWeight, rightCategorical)]) of
             Left err -> counterexample (show err) False
             Right blended ->
               let totalProbability = totalCategoricalProbability blended
                in counterexample
                     ( "expectedSupport="
                         <> show expectedSupport
                         <> ", blendedSupport="
                         <> show (categoricalSupport blended)
                         <> ", totalProbability="
                         <> show totalProbability
                     )
                     (categoricalSupport blended == expectedSupport && approxEq 1.0e-12 totalProbability 1.0)
    )

propConditionCategoricalPreservesFilteredSupportAndNormalization :: PositiveWeightSample -> Property
propConditionCategoricalPreservesFilteredSupportAndNormalization weightSample =
  withCategoricalFromPositiveWeights
    weightSample
    (\categorical ->
       let expectedSupport = Set.filter even (categoricalSupport categorical)
        in case conditionCategorical even categorical of
             Nothing ->
               counterexample
                 ("expected conditioned categorical for support=" <> show (categoricalSupport categorical))
                 False
             Just conditioned ->
               let totalProbability = totalCategoricalProbability conditioned
                in counterexample
                     ( "expectedSupport="
                         <> show expectedSupport
                         <> ", conditionedSupport="
                         <> show (categoricalSupport conditioned)
                         <> ", totalProbability="
                         <> show totalProbability
                     )
                     (categoricalSupport conditioned == expectedSupport && approxEq 1.0e-12 totalProbability 1.0)
    )

propConditionCategoricalRejectsEmptySupport :: PositiveWeightSample -> Property
propConditionCategoricalRejectsEmptySupport weightSample =
  withCategoricalFromPositiveWeights
    weightSample
    (\categorical ->
       let support = categoricalSupport categorical
           absentOutcome = Set.size support
           conditioned = conditionCategorical (> absentOutcome) categorical
        in counterexample
             ("support=" <> show support <> ", conditioned=" <> show conditioned)
             (conditioned == Nothing)
    )

propTruncatedDistributionNormalizesBoundsAndExternalDensity :: Positive Int -> Property
propTruncatedDistributionNormalizesBoundsAndExternalDensity widthSeed =
  case mkNormalDistribution 0.0 1.0 of
    Left err -> counterexample (show err) False
    Right normal ->
      let halfWidth = fromIntegral (getPositive widthSeed `mod` 12 + 1) / 4.0
          lowerBound = negate halfWidth
          upperBound = halfWidth
       in case mkTruncatedDistribution (lowerBound, upperBound) normal of
            Left err -> counterexample (show err) False
            Right truncated ->
              let lowerCdf = distributionCumulative truncated lowerBound
                  upperCdf = distributionCumulative truncated upperBound
                  leftDensity = distributionDensity truncated (lowerBound - 1.0)
                  rightDensity = distributionDensity truncated (upperBound + 1.0)
                  leftQuantile = distributionQuantile truncated 0.0
                  rightQuantile = distributionQuantile truncated 1.0
               in counterexample
                    ( "bounds="
                        <> show (truncatedBounds truncated)
                        <> ", lowerCdf="
                        <> show lowerCdf
                        <> ", upperCdf="
                        <> show upperCdf
                        <> ", leftDensity="
                        <> show leftDensity
                        <> ", rightDensity="
                        <> show rightDensity
                        <> ", leftQuantile="
                        <> show leftQuantile
                        <> ", rightQuantile="
                        <> show rightQuantile
                    )
                    ( truncatedBounds truncated == (lowerBound, upperBound)
                        && approxEq 1.0e-12 lowerCdf 0.0
                        && approxEq 1.0e-12 upperCdf 1.0
                        && approxEq 1.0e-12 leftDensity 0.0
                        && approxEq 1.0e-12 rightDensity 0.0
                        && leftQuantile >= lowerBound - 1.0e-12
                        && rightQuantile <= upperBound + 1.0e-12
                    )

totalCategoricalProbability :: Categorical Int -> Double
totalCategoricalProbability categorical =
  getSum (categoricalFoldMap (Sum . positiveProbValue . snd) categorical)

weightedValues :: PositiveWeightSample -> NonEmpty (Double, Double)
weightedValues (NonEmpty positiveWeights) =
  case positiveWeights of
    positiveWeight : remainingWeights ->
      let firstWeight = fromIntegral (getPositive positiveWeight)
          otherWeights = fmap (fromIntegral . getPositive) remainingWeights
       in (firstWeight, 0.0) :| zip otherWeights (fmap fromIntegral [1 :: Int ..])
    [] -> (1.0, 0.0) :| []
