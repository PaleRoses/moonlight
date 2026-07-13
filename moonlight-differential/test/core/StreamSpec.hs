module StreamSpec
  ( tests,
  )
where

import Moonlight.Differential.Order.LocallyFinite
  ( interval,
    mobiusSupport,
  )
import Numeric.Natural
  ( Natural,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertEqual,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "stream and Möbius laws"
    [ testCase "Natural-time Möbius support stays sparse" naturalMobiusSupportIsSparse,
      testCase "Product Natural-time Möbius support stays tensor-sparse" productNaturalMobiusSupportIsSparse,
      testCase "Product intervals are empty exactly when the product order rejects the endpoints" productIntervalRejectsIncomparableEndpoints
    ]

naturalMobiusSupportIsSparse :: IO ()
naturalMobiusSupportIsSparse =
  assertEqual
    "chain-order Möbius support contains only predecessor and target"
    [(9, -1), (10, 1)]
    (mobiusSupport 0 (10 :: Natural))

productNaturalMobiusSupportIsSparse :: IO ()
productNaturalMobiusSupportIsSparse =
  assertEqual
    "product chain-order Möbius support tensors the component sparse supports"
    [ (((9, 9) :: (Natural, Natural)), 1),
      (((9, 10) :: (Natural, Natural)), -1),
      (((10, 9) :: (Natural, Natural)), -1),
      (((10, 10) :: (Natural, Natural)), 1)
    ]
    (mobiusSupport (0, 0) ((10, 10) :: (Natural, Natural)))

productIntervalRejectsIncomparableEndpoints :: IO ()
productIntervalRejectsIncomparableEndpoints =
  assertEqual
    "componentwise product intervals reject an incomparable upper endpoint"
    []
    (interval ((1, 3) :: (Natural, Natural)) (3, 1))
