module FiniteTests
  ( tests,
  )
where

import qualified DenseReachabilitySpec
import qualified FinPresentationSpec
import qualified InvertibilitySpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "finite"
    [ FinPresentationSpec.tests,
      InvertibilitySpec.tests,
      DenseReachabilitySpec.tests
    ]
