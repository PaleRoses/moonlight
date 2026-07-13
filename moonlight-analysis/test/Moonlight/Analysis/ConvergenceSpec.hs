module Moonlight.Analysis.ConvergenceSpec (tests) where

import Moonlight.Analysis.Convergence
import Moonlight.Core
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

unsafeAbsTol :: Double -> AbsTol
unsafeAbsTol value = either (error . show) id (absTol value)

unsafeRelTol :: Double -> RelTol
unsafeRelTol value = either (error . show) id (relTol value)

tests :: TestTree
tests =
  testGroup
    "Convergence"
    [ testCase "Exact tolerance converges on stable integer stream" $
        case mkIterationLimit 10 of
          Left err -> assertFailure (show err)
          Right limit ->
            let stream = [0, 1, 2, 3] ++ repeat (3 :: Int)
             in assertEqual "exact" (Converged 3) (evaluateStream Exact limit stream),
      testCase "Residual cadence is explicit and deterministic" $
        case (mkIterationLimit 10, mkResidualCadence 3) of
          (Right limit, Right cadence) ->
            let config =
                  ConvergenceConfig
                    { tolerance = Exact,
                      iterationLimit = limit,
                      residualCadence = cadence
                    }
                stream = [1, 2, 2, 2, 2, 2 :: Int]
             in assertEqual "cadence" (Converged 2) (evaluateStreamWithConfig config stream)
          (Left err, _) -> assertFailure (show err)
          (_, Left err) -> assertFailure (show err),
      testCase "Composite tolerance converges for Newton sequence" $
        case mkIterationLimit 40 of
          Left err -> assertFailure (show err)
          Right limit ->
            let next :: Double -> Double
                next x = 0.5 * (x + 2.0 / x)
                stream = iterate next (1.0 :: Double)
                toleranceValue = CompositeTol (AbsTolBound (unsafeAbsTol 1e-10)) (RelTolBound (unsafeRelTol 1e-10))
             in case evaluateStream toleranceValue limit stream of
                  Converged root -> assertBool "root" (approxEq (unsafeAbsTol 1e-9) root (sqrt 2.0))
                  other -> assertFailure ("expected convergence, got " <> show other)
    ]
