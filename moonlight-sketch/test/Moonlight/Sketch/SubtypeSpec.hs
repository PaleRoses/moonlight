module Moonlight.Sketch.SubtypeSpec
  ( tests,
  )
where

import qualified Data.Map.Strict as Map
import Moonlight.Sketch
  ( LiteralValue (..),
    ObjectProperty (..),
    SchemaNode (..),
    isSubtype,
  )
import Moonlight.Sketch.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import qualified Test.Tasty.QuickCheck as QC

tests :: TestTree
tests =
  testGroup
    "subtype"
    [ testCase "literal string subtype string" $
        isSubtype (SLiteral (LitString "hello")) (SString Nothing Nothing) @?= True,
      testCase "string is not subtype literal" $
        isSubtype (SString Nothing Nothing) (SLiteral (LitString "hello")) @?= False,
      testCase "void is bottom" $
        isSubtype SVoid SBool @?= True,
      testCase "unknown is top" $
        isSubtype SBool SUnknown @?= True,
      testCase "array covariance" $
        isSubtype (SArray SBool Nothing) (SArray SUnknown Nothing) @?= True,
      testCase "null is subtype of nullable" $
        isSubtype SNull (SNullable SBool) @?= True,
      testCase "undefined is subtype of optional" $
        isSubtype SUndefined (SOptional SBool) @?= True,
      testCase "object width subtype" $
        isSubtype
          (SObject (Map.fromList [("x", ObjectProperty True False SBool), ("y", ObjectProperty False False SBool)]))
          (SObject (Map.fromList [("x", ObjectProperty True False SBool)]))
          @?= True,
      QC.testProperty "reflexive" $ \(node :: SchemaNode) ->
        isSubtype node node
    ]
