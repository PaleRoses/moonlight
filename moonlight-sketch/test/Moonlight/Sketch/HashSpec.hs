module Moonlight.Sketch.HashSpec
  ( tests,
  )
where

import Moonlight.Sketch
  ( CanonicalNumber (..),
    LiteralValue (..),
    SchemaNode,
    SchemaNode (..),
    normalize,
    schemaEq,
    schemaHash,
  )
import Moonlight.Sketch.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import qualified Test.Tasty.QuickCheck as QC

tests :: TestTree
tests =
  testGroup
    "hash"
    [ testCase "different primitives hash differently" $
        (schemaHash SBool /= schemaHash SNull) @?= True,
      testCase "non-finite numeric literals collapse to void under normalization" $ do
        normalize (SLiteral (LitNumber NaN)) @?= SVoid
        schemaHash (SLiteral (LitNumber PosInf)) @?= schemaHash SVoid,
      QC.testProperty "deterministic" $ \(node :: SchemaNode) ->
        schemaHash node == schemaHash node,
      QC.testProperty "post-normalization stable" $ \(left :: SchemaNode) (right :: SchemaNode) ->
        if normalize left == normalize right
          then schemaHash left == schemaHash right
          else True,
      QC.testProperty "schemaEq symmetric" $ \(left :: SchemaNode) (right :: SchemaNode) ->
        schemaEq left right == schemaEq right left,
      testCase "schemaEq reflexive" $
        schemaEq SBool SBool @?= True
    ]
