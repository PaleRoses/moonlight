module Moonlight.Probability.FiniteSpec
  ( tests,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Data.Set qualified as Set
import Moonlight.Probability.Core (mkProb, positiveProbValue, probOne, probZero)
import Moonlight.Probability.TestSupport.Generators
  ( PositiveWeightSample,
    withFiniteDistributionFromPositiveWeights,
  )
import Moonlight.Probability.Distribution.Finite
  ( finiteFoldMap,
    finiteSupport,
    finiteTraverse,
    finiteWeightedOutcomes,
    mkFiniteDistribution,
    sampleAt,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck
  ( Positive (..),
    Property,
    counterexample,
    testProperty,
  )

approxEq :: Double -> Double -> Double -> Bool
approxEq tolerance left right = abs (left - right) <= tolerance

tests :: TestTree
tests =
  testGroup
    "finite"
    [ testProperty "sampleAt always returns supported outcome" propSampleAtReturnsSupportedOutcome,
      testProperty "sampleAt respects support endpoints" propSampleAtRespectsSupportEndpoints,
      testCase "finiteTraverse merges collided outcomes and preserves normalization" $
        case mkFiniteDistribution (Map.fromList [('a', 1.0), ('b', 3.0)]) of
          Left err -> assertBool (show err) False
          Right distribution ->
            let Identity traversed =
                  finiteTraverse (\_ -> Identity 'x') distribution
                totalProbability =
                  getSum (finiteFoldMap (Sum . positiveProbValue . snd) traversed)
             in assertBool
                  "traversed support collapses and remains normalized"
                  (finiteSupport traversed == Set.singleton 'x' && approxEq 1.0e-12 totalProbability 1.0)
    ]

propSampleAtReturnsSupportedOutcome ::
  PositiveWeightSample ->
  Positive Int ->
  Positive Int ->
  Property
propSampleAtReturnsSupportedOutcome weightSample numeratorSeed denominatorSeed =
  withFiniteDistributionFromPositiveWeights weightSample
    (\distribution ->
       case mkProb (quantizedThreshold numeratorSeed denominatorSeed) of
         Left err -> counterexample (show err) False
         Right threshold ->
           let sampledOutcome = sampleAt threshold distribution
               supportOutcomes = finiteSupport distribution
            in counterexample
                 ("sampled=" <> show sampledOutcome <> ", support=" <> show supportOutcomes)
                 (Set.member sampledOutcome supportOutcomes)
    )

propSampleAtRespectsSupportEndpoints :: PositiveWeightSample -> Property
propSampleAtRespectsSupportEndpoints weightSample =
  withFiniteDistributionFromPositiveWeights
    weightSample
    (\distribution ->
       let support = finiteWeightedOutcomes distribution
           firstOutcome = fst (NonEmpty.head support)
           lastOutcome = fst (NonEmpty.last support)
        in counterexample
             ("support=" <> show (fmap fst support))
             (sampleAt probZero distribution == firstOutcome && sampleAt probOne distribution == lastOutcome)
    )
quantizedThreshold :: Positive Int -> Positive Int -> Double
quantizedThreshold numeratorSeed denominatorSeed =
  let denominator = getPositive denominatorSeed
      numerator = getPositive numeratorSeed `mod` (denominator + 1)
   in fromIntegral numerator / fromIntegral denominator
