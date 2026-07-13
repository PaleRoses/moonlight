module Moonlight.EGraph.Binding.SlottedLambdaBindingGoalSpec
  ( tests,
  )
where

import Data.List (isInfixOf)
import Pale.Test.Interop.SlottedLambdaBindingGoal qualified as Slotted
import Moonlight.EGraph.Test.Case (HUnitCase (..), hunitCases)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=))

tests :: TestTree
tests =
  testGroup
    "slotted-lambda-binding-goal"
    ( hunitCases
        ( fmap
            slottedCase
            [ ("alpha-equivalence survives the slotted comparison", Slotted.AlphaEquivalenceScenario, assertSupportedEquivalent),
              ("beta reduction survives the slotted comparison", Slotted.DynamicBetaScenario, assertSupportedEquivalent),
              ("capture avoidance survives the slotted comparison", Slotted.CaptureAvoidanceScenario, assertSupportedEquivalentWithForbidden),
              ("eta safety split survives the slotted comparison", Slotted.EtaScenario, assertSupportedEquivalentWithForbidden),
              ("deep nesting survives the slotted comparison", Slotted.DeepNestingScenario 32, assertSupportedEquivalent),
              ("outward let-float stays outside the slotted fragment", Slotted.LetFloatScenario, assertUnsupportedContaining "outward let-float"),
              ("lattice growth stays outside the slotted fragment", Slotted.LatticeGrowthScenario, assertUnsupportedContaining "context lattice")
            ]
        )
    )

slottedCase :: (String, Slotted.LambdaBindingGoalScenario, Slotted.SlottedScenarioOutcome -> IO ()) -> HUnitCase
slottedCase (caseName, scenario, assertion) =
  HUnitCase caseName (requireScenario scenario >>= assertion)

requireScenario :: Slotted.LambdaBindingGoalScenario -> IO Slotted.SlottedScenarioOutcome
requireScenario scenario =
  Slotted.runSlottedLambdaBindingGoalScenario scenario >>= either failWithInterop pure
  where
    failWithInterop err = assertFailure ("slotted interop failed for " <> show scenario <> ": " <> err) >> fail err

assertSupportedEquivalent :: Slotted.SlottedScenarioOutcome -> IO ()
assertSupportedEquivalent outcome = do
  assertBool
    ("expected supported slotted scenario, got: " <> show outcome)
    (Slotted.ssoSupported outcome)
  Slotted.ssoEquivalent outcome @?= Just True

assertSupportedEquivalentWithForbidden :: Slotted.SlottedScenarioOutcome -> IO ()
assertSupportedEquivalentWithForbidden outcome = do
  assertSupportedEquivalent outcome
  Slotted.ssoForbiddenEquivalent outcome @?= Just False

assertUnsupportedContaining :: String -> Slotted.SlottedScenarioOutcome -> IO ()
assertUnsupportedContaining needle outcome = do
  assertBool
    ("expected unsupported slotted scenario, got: " <> show outcome)
    (not (Slotted.ssoSupported outcome))
  assertBool
    ("expected unsupported note containing " <> show needle <> ", got: " <> show outcome)
    (maybe False (isInfixOf needle) (Slotted.ssoNote outcome))
