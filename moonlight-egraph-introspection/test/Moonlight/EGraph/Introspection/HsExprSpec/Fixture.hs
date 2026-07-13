module Moonlight.EGraph.Introspection.HsExprSpec.Fixture
  ( expectConvertedSingleton,
    expectConvertedExpressions,
    expectRight,
    assertSingletonRewriteRuleName,
    demoSource,
    caseSource,
    etaOkSource,
    etaBadSource,
    compositionOkSource,
    betaOkSource,
    betaShiftSource,
    letReduceSource,
    analysisSource,
    rdrNameF,
    rdrNameArg,
  )
where

import Data.List (isPrefixOf)
import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.Name.Reader (RdrName, mkRdrUnqual)
import Moonlight.Core (Pattern)
import Moonlight.EGraph.Introspection.Core.HsExpr (ConvertedModule (..), HsExprF, TopLevelBinding (..), convertHaskellSource)
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteMorphism, mkRewriteSystem, rewriteMorphismName, rsCategory)
import Moonlight.Rewrite.Algebra (frcRewrites)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure)

expectConvertedSingleton :: FilePath -> String -> (Pattern HsExprF -> Assertion) -> Assertion
expectConvertedSingleton sourcePath sourceText assertConverted =
  case convertHaskellSource sourcePath sourceText of
    Left failureValue ->
      assertFailure ("unexpected conversion failure: " <> show failureValue)
    Right convertedModule
      | [convertedValue] <- cmBindings convertedModule ->
      assertConverted (tlbTerm convertedValue)
      | otherwise ->
          assertFailure ("expected exactly one converted expression, got " <> show (length (cmBindings convertedModule)))

expectConvertedExpressions :: FilePath -> String -> ([Pattern HsExprF] -> Assertion) -> Assertion
expectConvertedExpressions sourcePath sourceText assertConverted =
  case convertHaskellSource sourcePath sourceText of
    Left failureValue ->
      assertFailure ("unexpected conversion failure: " <> show failureValue)
    Right convertedModule ->
      assertConverted (fmap tlbTerm (cmBindings convertedModule))

expectRight :: Show failure => Either failure success -> IO success
expectRight =
  either
    (\failureValue -> assertFailure ("unexpected span construction failure: " <> show failureValue))
    pure

assertSingletonRewriteRuleName :: String -> RewriteMorphism HsExprF -> Assertion
assertSingletonRewriteRuleName expectedRuleName rewriteSpan =
  case frcRewrites (rsCategory (mkRewriteSystem [rewriteSpan])) of
    [spanValue] ->
      assertBool
        "rewrite system should retain the expected span prefix"
        (expectedRuleName `isPrefixOf` rewriteMorphismName spanValue)
    otherValues ->
      assertFailure ("expected exactly one span in rewrite system, got " <> show (length otherValues))

demoSource :: String
demoSource =
  unlines
    [ "module Demo where",
      "identity x = x",
      "apply f x = f x",
      "keep y = let x = y in x"
    ]

caseSource :: String
caseSource =
  unlines
    [ "module CaseDemo where",
      "branch z = case z of",
      "  (a, b) -> a"
    ]

etaOkSource :: String
etaOkSource =
  unlines
    [ "module EtaOk where",
      "etaOk x = f x"
    ]

etaBadSource :: String
etaBadSource =
  unlines
    [ "module EtaBad where",
      "etaBad x = (g x) x"
    ]

compositionOkSource :: String
compositionOkSource =
  unlines
    [ "module CompositionOk where",
      "composeOk x = f (g x)"
    ]

betaOkSource :: String
betaOkSource =
  unlines
    [ "module BetaOk where",
      "betaOk = (\\x -> f x) arg"
    ]

betaShiftSource :: String
betaShiftSource =
  unlines
    [ "module BetaShift where",
      "betaShift = (\\x -> \\y -> f x y) arg"
    ]

letReduceSource :: String
letReduceSource =
  unlines
    [ "module LetReduce where",
      "letReduce = let x = arg in f x"
    ]

analysisSource :: String
analysisSource =
  unlines
    [ "module Analysis where",
      "etaOk x = f x",
      "composeOk x = f (g x)",
      "betaOk = (\\x -> f x) arg",
      "letReduce = let x = arg in f x"
    ]

rdrNameF :: RdrName
rdrNameF = mkRdrUnqual (mkVarOcc "f")

rdrNameArg :: RdrName
rdrNameArg = mkRdrUnqual (mkVarOcc "arg")
