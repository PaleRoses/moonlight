module Moonlight.Sheaf.Bench.Inference
  ( inferenceBenchmarks,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Sheaf.Inference
  ( BlueprintError,
    FactorSpec (..),
    InferenceConfig,
    WeightedBlueprint,
    buildWeightedBlueprint,
    defaultInferenceConfig,
    inferPosteriorExact,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)

inferenceBenchmarks :: IO Benchmark
inferenceBenchmarks =
  bgroup "exact inference"
    <$> traverse
      (uncurry preparedInferenceBenchmark)
      [ ("binary-chain-8/posterior", 8),
        ("binary-chain-24/posterior", 24)
      ]

preparedInferenceBenchmark :: String -> Int -> IO Benchmark
preparedInferenceBenchmark benchmarkName variableCount =
  either
    (fail . ("exact inference benchmark setup refused: " <>) . show)
    (pure . bench benchmarkName . whnf (inferPosteriorExact inferenceConfiguration))
    (binaryChainBlueprint variableCount)

inferenceConfiguration :: InferenceConfig
inferenceConfiguration =
  defaultInferenceConfig

binaryChainBlueprint :: Int -> Either (BlueprintError Int Bool) (WeightedBlueprint Int Bool)
binaryChainBlueprint variableCount =
  buildWeightedBlueprint
    (Map.fromList (fmap (, Set.fromList [False, True]) variables))
    localLogWeight
    (zipWith pairFactor variables (drop 1 variables))
  where
    variables = [0 .. variableCount - 1]

localLogWeight :: Int -> Bool -> Double
localLogWeight variable value
  | even variable == value = 0.125
  | otherwise = -0.125

pairFactor :: Int -> Int -> FactorSpec Int Bool
pairFactor leftVariable rightVariable =
  FactorSpec
    { fsScope = Set.fromList [leftVariable, rightVariable],
      fsTuples =
        [ ( Map.fromList
              [ (leftVariable, leftValue),
                (rightVariable, rightValue)
              ],
            if leftValue == rightValue then 0.25 else -0.25
          )
        | leftValue <- [False, True],
          rightValue <- [False, True]
        ]
    }
