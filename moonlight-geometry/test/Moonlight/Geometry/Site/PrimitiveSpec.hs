module Moonlight.Geometry.Site.PrimitiveSpec (tests) where

import Moonlight.Geometry.Site.Primitive
import Moonlight.LinAlg.Geometry (Vec3 (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "Primitive"
    [ testCase "primitive ordering is stable" $ do
        let primitives =
              [ Sphere 1.0,
                Box (Vec3 1.0 1.0 1.0),
                Capsule 0.5 2.0,
                Prism 6 0.5 1.0
              ]
        fmap primitiveOrderingKey primitives
          @?= [ (0, [1.0], []),
                (1, [1.0, 1.0, 1.0], []),
                (2, [0.5, 2.0], []),
                (9, [0.5, 1.0], [6])
              ]
    ]
