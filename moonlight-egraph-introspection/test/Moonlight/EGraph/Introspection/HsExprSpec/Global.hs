module Moonlight.EGraph.Introspection.HsExprSpec.Global
  ( tests,
  )
where

import Data.Foldable (traverse_)
import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core (Pattern (..))
import Moonlight.EGraph.Introspection.Core.HsExpr
import Moonlight.EGraph.Introspection.HsExprSpec.Fixture
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "global"
    [ testCase "beta reduction rewrites a root beta-redex" testBetaReductionCorrect,
      testCase "beta reduction shifts surviving bound indices" testBetaReductionShiftsIndices,
      testCase "let reduction rewrites let-bound bodies" testLetReductionCorrect,
      testCase "analyzeHaskellSource records constructor-tag diagnostics" testAnalyzeHaskellSourceDiagnostics,
      testCase "analyzeHaskellSource records span match sites" testMatchSiteCounts
    ]

testBetaReductionCorrect :: IO ()
testBetaReductionCorrect =
  expectConvertedSingleton "BetaOk.hs" betaOkSource $ \convertedValue -> do
    betaSpan <- expectRight (betaReductionSpanFor (rootBetaBinder convertedValue))
    assertEqual
      "beta reduction should substitute the argument through the body"
      ( Just
          ( PatternNode
              ( AppF
                  (PatternNode (VarF (GlobalName rdrNameF)))
                  (PatternNode (VarF (GlobalName rdrNameArg)))
              )
          )
      )
      (applyHsExprSpanAtRoot betaSpan convertedValue)

testBetaReductionShiftsIndices :: IO ()
testBetaReductionShiftsIndices =
  expectConvertedSingleton "BetaShift.hs" betaShiftSource $ \convertedValue -> do
    betaSpan <- expectRight (betaReductionSpanFor (rootBetaBinder convertedValue))
    case applyHsExprSpanAtRoot betaSpan convertedValue of
      Just (PatternNode (LamF binderAnn (PatternNode (AppF (PatternNode (AppF (PatternNode (VarF (GlobalName nameF))) (PatternNode (VarF (GlobalName nameArg))))) (PatternNode (VarF (LocalName localBinder)))))))
        | nameF == rdrNameF && nameArg == rdrNameArg && localBinder == binderAnn ->
            pure ()
      otherValue ->
        assertFailure ("unexpected beta-shift result: " <> show otherValue)

testLetReductionCorrect :: IO ()
testLetReductionCorrect =
  expectConvertedSingleton "LetReduce.hs" letReduceSource $ \convertedValue -> do
    letSpan <- expectRight (letReductionSpanFor (rootLetBinder convertedValue) LetSyntax)
    assertEqual
      "let reduction should substitute the bound value into the body"
      ( Just
          ( PatternNode
              ( AppF
                  (PatternNode (VarF (GlobalName rdrNameF)))
                  (PatternNode (VarF (GlobalName rdrNameArg)))
              )
          )
      )
      (applyHsExprSpanAtRoot letSpan convertedValue)

testAnalyzeHaskellSourceDiagnostics :: IO ()
testAnalyzeHaskellSourceDiagnostics =
  case analyzeHaskellSource "Analysis.hs" analysisSource of
    Left failureValue ->
      assertFailure ("unexpected analysis failure: " <> show failureValue)
    Right analysisValue -> do
      assertEqual
        "analysis should preserve all converted source terms"
        4
        (length (heaSourceTerms analysisValue))
      assertBool
        "analysis tag profile should record application syntax"
        (AppTag `Set.member` heaTagProfile analysisValue)
      assertBool
        "analysis tag profile should record lambda syntax"
        (LamTag `Set.member` heaTagProfile analysisValue)
      assertBool
        "analysis tag profile should record let syntax"
        (LetTag `Set.member` heaTagProfile analysisValue)

testMatchSiteCounts :: IO ()
testMatchSiteCounts =
  case analyzeHaskellSource "Analysis.hs" analysisSource of
    Left failureValue ->
      assertFailure ("unexpected analysis failure: " <> show failureValue)
    Right analysisValue ->
      let matchSiteCounts = heaMatchSiteCounts analysisValue
       in traverse_ (assertPositiveMatchCount matchSiteCounts) ["eta", "composition", "beta", "let-reduce"]

assertPositiveMatchCount :: Map.Map String Int -> String -> IO ()
assertPositiveMatchCount matchSiteCounts spanName =
  case [matchCount | (matchName, matchCount) <- Map.toList matchSiteCounts, spanName `isPrefixOf` matchName] of
    [] ->
      assertFailure ("missing diagnostic match count for span prefix " <> show spanName)
    matchingCounts ->
      assertBool
        ("expected a positive diagnostic match count for span prefix " <> show spanName)
        (any (> 0) matchingCounts)

rootBetaBinder :: Pattern HsExprF -> BinderAnn
rootBetaBinder = \case
  PatternNode (AppF (PatternNode (ParF (PatternNode (LamF binderAnn _)))) _) ->
    binderAnn
  _ ->
    error "expected beta redex root"

rootLetBinder :: Pattern HsExprF -> BinderAnn
rootLetBinder = \case
  PatternNode (LetF _ ((PVarP binderAnn, _) : _) _) ->
    binderAnn
  _ ->
    error "expected let root"
