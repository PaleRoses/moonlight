
module Moonlight.Analysis.RootSpec (tests) where

import Moonlight.Analysis
import Moonlight.Core
import Prelude hiding (div)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

unsafeAbsTol :: Double -> AbsTol
unsafeAbsTol value = either (error . show) id (absTol value)

unsafeRelTol :: Double -> RelTol
unsafeRelTol value = either (error . show) id (relTol value)

tests :: TestTree
tests =
  testGroup
    "Root"
    [ testCase "Newton converges on sqrt(2)" $
        case mkIterationLimit 30 of
          Left err -> assertFailure (show err)
          Right limit ->
            let toleranceValue = AbsTolBound (unsafeAbsTol 1e-12)
                result = findRootNewton toleranceValue limit sqrt2Dual 1.5
             in case result of
                  Converged root -> assertBool "newton" (approxEq (unsafeAbsTol 1e-10) root (sqrt 2.0))
                  other -> assertFailure ("expected convergence, got " <> show other),
      testCase "Bisection converges on sqrt(2)" $
        case (mkIterationLimit 80, mkBracket 1.0 2.0) of
          (Right limit, Right bracketValue) ->
            let toleranceValue = AbsTolBound (unsafeAbsTol 1e-12)
                result = findRootBisection toleranceValue limit sqrt2Real bracketValue
             in case result of
                  Converged root -> assertBool "bisection" (approxEq (unsafeAbsTol 1e-10) root (sqrt 2.0))
                  other -> assertFailure ("expected convergence, got " <> show other)
          (Left err, _) -> assertFailure (show err)
          (_, Left err) -> assertFailure (show err),
      testCase "Hybrid Newton-bisection converges inside bracket" $
        case (mkIterationLimit 40, mkBracket 1.0 2.0) of
          (Right limit, Right bracketValue) ->
            let toleranceValue = CompositeTol (AbsTolBound (unsafeAbsTol 1e-12)) (RelTolBound (unsafeRelTol 1e-12))
                result = findRootHybrid toleranceValue limit sqrt2Dual bracketValue 1.9
             in case result of
                  Converged root -> assertBool "hybrid" (approxEq (unsafeAbsTol 1e-10) root (sqrt 2.0))
                  other -> assertFailure ("expected convergence, got " <> show other)
          (Left err, _) -> assertFailure (show err)
          (_, Left err) -> assertFailure (show err),
      testCase "Newton does not claim convergence at stationary non-root point" $
        case mkIterationLimit 12 of
          Left err -> assertFailure (show err)
          Right limit ->
            let toleranceValue = AbsTolBound (unsafeAbsTol 1e-12)
                result = findRootNewton toleranceValue limit noRealRootDual 0.0
             in case result of
                  Converged root ->
                    assertFailure ("expected non-convergence, got converged value " <> show root)
                  IterationLimitReached _ _ -> pure ()
                  Diverged -> pure (),
      testCase "Bisection accepts a root on the bracket boundary" $
        case (mkIterationLimit 80, mkBracket 0.0 2.0) of
          (Right limit, Right bracketValue) ->
            let toleranceValue = AbsTolBound (unsafeAbsTol 1e-12)
                result = findRootBisection toleranceValue limit boundaryRootReal bracketValue
             in case result of
                  Converged root -> assertBool "boundary root" (approxEq (unsafeAbsTol 1e-10) root 0.0)
                  other -> assertFailure ("expected convergence, got " <> show other)
          (Left err, _) -> assertFailure (show err)
          (_, Left err) -> assertFailure (show err),
      testCase "Analytic AD derivative matches central finite difference within ULP tolerance" $
        case mkStepSize 1e-2 of
          Left err -> assertFailure (show err)
          Right step ->
            let point = 0.45
                analytic = derivative objectiveCubicDual point
                numeric = richardsonExtrapolation step objectiveCubicReal point
             in assertBool "ulps" (approxEq (mkUlpTol 100) analytic numeric)
    ]

sqrt2Dual :: forall s. Dual s Double -> Dual s Double
sqrt2Dual value = sub (mul value value) (liftDual 2.0)

sqrt2Real :: Double -> Double
sqrt2Real value = value * value - 2.0

objectiveCubicDual :: forall s. Dual s Double -> Dual s Double
objectiveCubicDual value = mul value (mul value value)

objectiveCubicReal :: Double -> Double
objectiveCubicReal value = value * value * value

noRealRootDual :: forall s. Dual s Double -> Dual s Double
noRealRootDual value = add (mul value value) (liftDual 1.0)

boundaryRootReal :: Double -> Double
boundaryRootReal value = value * value
