module Moonlight.EGraph.Introspection.HsExprSpec.Gluing
  ( tests,
  )
where

import Moonlight.Core (Pattern (..))
import Moonlight.EGraph.Introspection.Core.HsExpr
import Moonlight.EGraph.Introspection.HsExprSpec.Fixture
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "gluing"
    [ testCase "capturing eta candidates still match syntactically before support restriction" testCapturedEtaStillMatchesSyntactically,
      testCase "eta reduction span matches closed lambdas" testEtaReductionSpanValid,
      testCase "composition span matches closed composition lambdas" testCompositionSpanValid
    ]

testCapturedEtaStillMatchesSyntactically :: IO ()
testCapturedEtaStillMatchesSyntactically =
  expectConvertedSingleton "EtaBad.hs" etaBadSource $ \convertedValue -> do
    etaSpan <- expectRight (etaReductionSpanFor (rootLambdaBinder convertedValue))
    assertBool
      "eta span should still match syntactically before contextual support blocks it"
      (matchesHsExprSpanLhs etaSpan convertedValue)

testEtaReductionSpanValid :: IO ()
testEtaReductionSpanValid =
  expectConvertedSingleton "EtaOk.hs" etaOkSource $ \convertedValue -> do
    etaSpan <- expectRight (etaReductionSpanFor (rootLambdaBinder convertedValue))
    assertBool
      "eta reduction span should match a closed eta-redex"
      (matchesHsExprSpanLhs etaSpan convertedValue)
    assertSingletonRewriteRuleName "eta" etaSpan

testCompositionSpanValid :: IO ()
testCompositionSpanValid =
  expectConvertedSingleton "CompositionOk.hs" compositionOkSource $ \convertedValue -> do
    compositionSpan <- expectRight (compositionSpanFor (rootLambdaBinder convertedValue))
    assertBool
      "composition span should match a closed composition lambda"
      (matchesHsExprSpanLhs compositionSpan convertedValue)
    assertSingletonRewriteRuleName "composition" compositionSpan

rootLambdaBinder :: Pattern HsExprF -> BinderAnn
rootLambdaBinder = \case
  PatternNode (LamF binderAnn _) ->
    binderAnn
  _ ->
    error "expected lambda root"
