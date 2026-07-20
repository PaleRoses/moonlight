module Moonlight.Geometry.Gluing.SafetySpec (tests) where

import Moonlight.Geometry.Gluing.Safety
import Moonlight.Geometry.Site.Token
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "Safety"
    [ testCase "hard booleans are full algebra on zero set semantics" $ do
        lawfulnessFor ZeroSetSemantics (HardUnion () ()) @?= FullBooleanAlgebra,
      testCase "hard booleans are degenerate only on metric semantics" $ do
        lawfulnessFor MetricSemantics (HardUnion () ()) @?= DegenerateOnly,
      testCase "primitives stay opaque" $ do
        lawfulnessFor ZeroSetSemantics (Prim undefined) @?= Opaque
    ]
