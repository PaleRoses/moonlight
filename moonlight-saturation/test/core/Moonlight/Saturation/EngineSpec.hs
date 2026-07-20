module Moonlight.Saturation.EngineSpec
  ( engineTests,
  )
where

import Moonlight.Saturation.Core
import Moonlight.Saturation.Test.CoreFixture
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

engineTests :: TestTree
engineTests =
  testGroup
    "engine termination paths"
    [ saturationCase "apply rounds terminate at the explicit goal" (SaturationBudget 4 4) unobservedToyKernel (initialToy 3) ReachedGoal (completedUnobservedToyState 3),
      saturationCase "an empty planned round terminates at a fixed point" (SaturationBudget 4 4) fixedPointToyKernel (initialToy 3) ReachedFixedPoint (completedUnobservedToyState 3),
      saturationCase "post-rebuild convergence wins at the iteration boundary" (SaturationBudget 3 3) convergedToyKernel (initialToy 3) ReachedFixedPoint (completedUnobservedToyState 3),
      saturationCase "advance-only rounds terminate at the iteration limit" (SaturationBudget 3 1) idleKernel (initialToy 0) HitIterationLimit ((initialToy 0) {tsIteration = 3}),
      testCase "step adapter preserves convergence semantics" $
        runSaturationSteps
          (SaturationBudget 10 10)
          id
          id
          (>= 4)
          (Right . (+ 1) :: Int -> Either String Int)
          0
          @?= Right
            SaturationRun
              { srTermination = ReachedFixedPoint,
                srFinalState = 4
              }
    ]

saturationCase :: String -> SaturationBudget -> SaturationKernel ToyState ToyRound Int Int String -> ToyState -> SaturationTermination -> ToyState -> TestTree
saturationCase label budget kernel initial termination final =
  testCase label $
    runSaturation budget kernel initial
      @?= Right (SaturationRun termination final)

completedUnobservedToyState :: Int -> ToyState
completedUnobservedToyState target =
  ToyState
    { tsIteration = target,
      tsFacts = target,
      tsTarget = target,
      tsObserved = [0]
    }
