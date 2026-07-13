{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Pale.Test.Global.EitherSpec
  ( tests,
  )
where

import Moonlight.Pale.Test.Global.Either (stringifyLeft, stringifyLeftWith)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

data PredicateFailure
  = PredicateRejected Int
  deriving stock (Eq, Show)

tests :: TestTree
tests =
  testGroup
    "Moonlight.Pale.Test.Global.Either"
    [ testCase "preserves a satisfied assertion as Right" $
        stringifyLeft satisfiedAssertion @?= Right satisfiedValue,
      testCase "renders a violated typed assertion as Left" $
        stringifyLeft violatedAssertion @?= Left renderedViolation,
      testCase "custom left rendering distinguishes pass from fail" $ do
        stringifyLeftWith renderViolation satisfiedAssertion @?= Right satisfiedValue
        stringifyLeftWith renderViolation violatedAssertion @?= Left contextualViolation
    ]

satisfiedValue :: Int
satisfiedValue =
  13

violatedValue :: Int
violatedValue =
  12

satisfiedAssertion :: Either PredicateFailure Int
satisfiedAssertion =
  Right satisfiedValue

violatedAssertion :: Either PredicateFailure Int
violatedAssertion =
  Left (PredicateRejected violatedValue)

renderedViolation :: String
renderedViolation =
  "PredicateRejected 12"

contextualViolation :: String
contextualViolation =
  "global assertion failed: " <> renderedViolation

renderViolation :: String -> String
renderViolation violation =
  "global assertion failed: " <> violation
