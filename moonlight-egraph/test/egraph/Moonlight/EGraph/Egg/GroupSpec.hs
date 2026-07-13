module Moonlight.EGraph.Egg.GroupSpec
  ( tests,
  )
where

import Moonlight.EGraph.Test.Front.Group
import Moonlight.EGraph.Test.Case (HUnitCase (..), hunitCases)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "egg-group (egglog calc.egg)"
    ( hunitCases
        ( fmap
            ( \(caseName, left, right) ->
                HUnitCase caseName (assertGroupEquivalent left right)
            )
            [ ("b * inv(b) = I", gMul gB (gInv gB), gI),
              ("b * (inv(a) * a) * inv(b) = b * inv(b)", gMul (gMul gB (gMul (gInv gA) gA)) (gInv gB), gMul gB (gInv gB)),
              ("A^4 = I (cyclic group of order 4)", a4, gI),
              ("A^4 * A^4 = (A^2 * A^2) * (A^2 * A^2)", gMul a4 a4, gMul (gMul a2 a2) (gMul a2 a2)),
              ("(A^2*A^2)*(A^2*A^2) = A^2*(A^2*(A^2*A^2))", gMul (gMul a2 a2) (gMul a2 a2), gMul a2 (gMul a2 (gMul a2 a2))),
              ("A^8 = I (power of cyclic period)", gMul a4 a4, gI)
            ]
        )
    )
  where
    a2 = gMul gA gA
    a4 = gMul a2 a2
