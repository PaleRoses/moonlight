module Moonlight.Sketch.FormatSpec
  ( tests,
  )
where

import qualified Data.Text as Text
import Moonlight.Sketch
  ( CharClass (..),
    FormatElement (..),
    Quantifier (..),
    SemanticFormat (..),
    StringFormat (..),
    matchFormat,
  )
import Moonlight.Sketch.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import qualified Test.Tasty.QuickCheck as QC

tests :: TestTree
tests =
  testGroup
    "format"
    [ testCase "uuid semantic format" $
        matchFormat (Semantic FUuid) "550e8400-e29b-41d4-a716-446655440000" @?= True,
      testCase "email semantic format" $
        matchFormat (Semantic FEmail) "user@example.com" @?= True,
      testCase "startsWith semantic format" $
        matchFormat (Semantic (FStartsWith "pre")) "prefix" @?= True,
      testCase "structural format exact digits" $
        matchFormat
          (Structural (Chars Digit (Exact 3)))
          "123"
          @?= True,
      testCase "structural sequence" $
        matchFormat
          (Structural (Sequence [FLiteral "ID-", Chars Digit (Exact 2)]))
          "ID-42"
          @?= True,
      QC.testProperty "deterministic" $ \formatValue input ->
        let textValue = Text.pack input
         in matchFormat formatValue textValue == matchFormat formatValue textValue
    ]
