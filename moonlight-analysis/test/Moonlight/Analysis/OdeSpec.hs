module Moonlight.Analysis.ODESpec
  ( tests,
  )
where

import Moonlight.Analysis (StepSizeControl (..), integrateAdaptive, integrateRK4, odeState)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

expGrowth :: Double -> [Double] -> [Double]
expGrowth _ [value] = [value]
expGrowth _ _ = []

decay :: Double -> [Double] -> [Double]
decay _ [value] = [-value]
decay _ _ = []

closeTo :: Double -> Double -> Double -> Bool
closeTo tolerance expected actual = abs (expected - actual) <= tolerance

finalState :: [[Double]] -> Maybe [Double]
finalState states =
  case states of
    [] -> Nothing
    [value] -> Just value
    _ : remaining -> finalState remaining

tests :: TestTree
tests =
  testGroup
    "ode"
    [ testCase "integrateRK4 approximates exponential growth" $
        let maybeState = finalState (fmap odeState (integrateRK4 0.1 0.0 1.0 expGrowth [1.0]))
         in assertBool
              "rk4 should approximate exp(1)"
              (case maybeState of
                 Just [value] -> closeTo 2.0e-2 2.718281828 value
                 _ -> False),
      testCase "integrateAdaptive approximates exponential decay" $
        let control = StepSizeControl { minimumStepSize = 1.0e-3, maximumStepSize = 0.2, initialStepSize = 0.1, errorTolerance = 1.0e-6, safetyFactor = 0.9 }
            maybeState = finalState (fmap odeState (integrateAdaptive control 0.0 1.0 decay [1.0]))
         in assertBool
              "adaptive DP should approximate exp(-1)"
              (case maybeState of
                 Just [value] -> closeTo 2.0e-2 0.367879441 value
                 _ -> False)
    ]
