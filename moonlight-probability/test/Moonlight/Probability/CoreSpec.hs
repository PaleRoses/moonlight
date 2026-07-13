module Moonlight.Probability.CoreSpec
  ( tests,
  )
where

import Moonlight.Probability.Core
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

approxEq :: Double -> Double -> Double -> Bool
approxEq tolerance left right = abs (left - right) <= tolerance

tests :: TestTree
tests =
  testGroup
    "core"
    [ testCase "mkProb validates unit interval" $ do
        assertBool "accepts midpoint" (mkProb 0.5 == Right (mkProbValue 0.5))
        assertBool "rejects negative" (case mkProb (-0.1) of Left _ -> True; Right _ -> False)
        assertBool "rejects greater than one" (case mkProb 1.1 of Left _ -> True; Right _ -> False),
      testCase "logProb round-trips a probability" $
        case mkProb 0.25 of
          Left err -> assertBool (show err) False
          Right probability ->
            assertBool "roundtrip" (approxEq 1.0e-12 (logProbValue (probToLogProb probability)) 0.25)
    ]

mkProbValue :: Double -> Prob
mkProbValue value =
  case mkProb value of
    Left _ -> error "unreachable probability literal"
    Right probability -> probability
