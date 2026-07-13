module Moonlight.EGraph.Egg.MathSpec
  ( tests,
  )
where

import Moonlight.EGraph.Test.Case (HUnitCase (..), hunitCases)
import Moonlight.EGraph.Test.Front.Math
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup "egg-math (egg's math.rs)" . hunitCases $
    [ HUnitCase "powers: (* (pow 2 x) (pow 2 y)) = (pow 2 (+ x y))" $
        assertMathEquivalent
          (mMul (mPow (mConst 2) (mSym "x")) (mPow (mConst 2) (mSym "y")))
          (mPow (mConst 2) (mAdd (mSym "x") (mSym "y"))),
      HUnitCase "integ_one: (i 1 x) = x" $
        assertMathEquivalent
          (mInteg (mConst 1) (mSym "x"))
          (mSym "x"),
      HUnitCase "integ_sin: (i (cos x) x) = (sin x)" $
        assertMathEquivalent
          (mInteg (mCos (mSym "x")) (mSym "x"))
          (mSin (mSym "x"))
    ]
