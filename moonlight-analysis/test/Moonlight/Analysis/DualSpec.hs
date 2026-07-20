module Moonlight.Analysis.DualSpec (tests) where

import Moonlight.Analysis.Dual
import Moonlight.Core
import Prelude hiding (div, exp, log, sin, sqrt)
import qualified Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

unsafeAbsTol :: Double -> AbsTol
unsafeAbsTol value = either (error . show) id (absTol value)

tests :: TestTree
tests =
  testGroup
    "Dual"
    [ testCase "cubic derivative matches analytic value" $
        let (value, slope) = diff cubic (2.0 :: Double)
         in do
              assertBool "value" (approxEq (unsafeAbsTol 1e-12) value 8.0)
              assertBool "slope" (approxEq (unsafeAbsTol 1e-12) slope 12.0),
      testCase "chain rule on exp . sin" $
        let x = (0.37 :: Double)
            (_, slope) = diff (expDual . sinDual) x
            expected = Prelude.cos x * Prelude.exp (Prelude.sin x)
         in assertBool "chain rule" (approxEq (unsafeAbsTol 1e-12) slope expected),
      testCase "nested dual computes second derivative without cross-scope leakage" $
        let seed = (Dual (Dual 2.0 1.0) (Dual 1.0 0.0) :: Dual () (Dual () Double))
            result = cubic seed
            secondDerivative = tangent (tangent result)
         in assertBool "second derivative" (approxEq (unsafeAbsTol 1e-12) secondDerivative 12.0)
    ]

cubic :: (Ring a) => Dual s a -> Dual s a
cubic value = mul value (mul value value)
