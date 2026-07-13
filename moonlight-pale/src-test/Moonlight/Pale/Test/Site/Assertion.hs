module Moonlight.Pale.Test.Site.Assertion
  ( expectRight,
    expectRightWithLabel,
    expectSome,
    assertApproxEqual,
    assertNonEmpty,
    assertSubsetOf,
    withResult,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure)

expectRight :: (HasCallStack, Show e) => Either e a -> IO a
expectRight result =
  case result of
    Left err -> assertFailure ("expected Right, got Left: " <> show err)
    Right val -> pure val

expectRightWithLabel :: (HasCallStack, Show e) => String -> Either e a -> IO a
expectRightWithLabel label result =
  case result of
    Left err -> assertFailure (label <> ": expected Right, got Left: " <> show err)
    Right val -> pure val

expectSome :: HasCallStack => String -> Maybe a -> IO a
expectSome label result =
  case result of
    Nothing -> assertFailure ("expected Just for " <> label <> ", got Nothing")
    Just val -> pure val

assertApproxEqual :: HasCallStack => String -> Double -> Double -> Double -> Assertion
assertApproxEqual label epsilon expected actual =
  assertBool
    (label <> ": expected " <> show expected <> " ± " <> show epsilon <> ", got " <> show actual)
    (abs (expected - actual) < epsilon)

assertNonEmpty :: HasCallStack => [a] -> Assertion
assertNonEmpty xs =
  assertBool "expected non-empty list" (not (null xs))

assertSubsetOf :: (HasCallStack, Ord a, Show a) => Set a -> Set a -> Assertion
assertSubsetOf subset superset =
  let missing = Set.difference subset superset
   in assertBool
        ("expected subset, missing: " <> show (Set.toList missing))
        (Set.null missing)

withResult :: (HasCallStack, Show e) => Either e a -> (a -> Assertion) -> Assertion
withResult result check =
  case result of
    Left err -> assertFailure ("expected Right, got Left: " <> show err)
    Right val -> check val
