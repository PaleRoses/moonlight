module Moonlight.EGraph.Test.Assertions
  ( expectSaturation,
    expectEqualitySaturation,
    expectCompile,
    requireMaybe,
    requireExtraction,
    isStructuralMismatch,
    isContextBarrier,
    isRestrictionBarrier,
    isPropagationBarrier,
  )
where

import Test.Tasty.HUnit (assertFailure)

import Moonlight.EGraph.Pure.Extraction (ExtractionResult (..))
import Moonlight.Sheaf.Obstruction (Obstruction (..))

expectSaturation :: Either saturationError report -> IO report
expectSaturation saturationResult =
  case saturationResult of
    Right saturationReport -> pure saturationReport
    Left _ -> assertFailure "expected saturation to succeed"

expectEqualitySaturation :: Either saturationError result -> IO result
expectEqualitySaturation saturationResult =
  case saturationResult of
    Right resultValue -> pure resultValue
    Left _ -> assertFailure "expected equality saturation to succeed"

expectCompile :: Either compileError compiledValue -> IO compiledValue
expectCompile compileResult =
  case compileResult of
    Right compiledValue -> pure compiledValue
    Left _ -> assertFailure "expected compilation to succeed"

requireMaybe :: String -> Maybe value -> IO value
requireMaybe failureMessage =
  maybe (assertFailure failureMessage) pure

requireExtraction :: Maybe (ExtractionResult f c) -> IO (ExtractionResult f c)
requireExtraction =
  requireMaybe "expected extraction result"

isStructuralMismatch :: Obstruction eq node rule ctx subst stat failure -> Bool
isStructuralMismatch obstructionValue =
  case obstructionValue of
    StructuralMismatch _ -> True
    _ -> False

isContextBarrier :: Obstruction eq node rule ctx subst stat failure -> Bool
isContextBarrier obstructionValue =
  case obstructionValue of
    ContextBarrier _ -> True
    _ -> False

isRestrictionBarrier :: Obstruction eq node rule ctx subst stat failure -> Bool
isRestrictionBarrier obstructionValue =
  case obstructionValue of
    RestrictionBarrier _ -> True
    _ -> False

isPropagationBarrier :: Obstruction eq node rule ctx subst stat failure -> Bool
isPropagationBarrier obstructionValue =
  case obstructionValue of
    PropagationBarrier _ -> True
    _ -> False
