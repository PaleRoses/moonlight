module Moonlight.Sketch.NormalizeSpec
  ( tests,
  )
where

import Moonlight.Sketch
  ( SchemaNode (..),
    normalize,
  )
import Moonlight.Sketch.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import qualified Test.Tasty.QuickCheck as QC

tests :: TestTree
tests =
  testGroup
    "normalize"
    [ testCase "flatten nested unions" $
        normalize (SUnion [SUnion [SBool, SNull], SString Nothing Nothing])
          @?= normalize (SUnion [SBool, SNull, SString Nothing Nothing]),
      testCase "collapse empty union" $
        normalize (SUnion []) @?= SVoid,
      testCase "collapse singleton union" $
        normalize (SUnion [SBool]) @?= SBool,
      testCase "idempotent optional wrapper" $
        normalize (SOptional (SOptional SBool)) @?= SOptional SBool,
      QC.testProperty "normalize idempotent" $ \(node :: SchemaNode) ->
        normalize (normalize node) == normalize node
    ]
