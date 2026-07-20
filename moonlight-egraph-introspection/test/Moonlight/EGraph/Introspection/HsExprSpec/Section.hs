module Moonlight.EGraph.Introspection.HsExprSpec.Section
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
    "section"
    [ testCase "convertModule builds binder-annotated expressions from parsed source" testConvertModuleRoundTrip,
      testCase "case conversion preserves tuple pattern structure" testCaseTuplePattern
    ]

testConvertModuleRoundTrip :: IO ()
testConvertModuleRoundTrip =
  expectConvertedExpressions "Demo.hs" demoSource $ \expressions ->
    case expressions of
      [identityExpr, applyExpr, keepExpr] -> do
        assertBool "identity should bind and reference the same binder" (isIdentity identityExpr)
        assertBool "apply should preserve binder identity across nested binders" (isApply applyExpr)
        assertBool "keep should preserve let binder identity in the body" (isKeep keepExpr)
      _ ->
        assertBool "expected three converted expressions" False

testCaseTuplePattern :: IO ()
testCaseTuplePattern =
  expectConvertedSingleton "CaseDemo.hs" caseSource $ \convertedValue ->
    assertBool
      "tuple case alternatives should preserve faithful tuple pattern structure"
      (case convertedValue of
         PatternNode (LamF binderAnn (PatternNode (CaseF (PatternNode (VarF (LocalName localBinder))) [(PTupleP [PVarP firstBinder, PVarP secondBinder], PatternNode (VarF (LocalName branchBinder)))])))
           -> binderAnn == localBinder && firstBinder /= secondBinder && branchBinder == firstBinder
         _ ->
           False)

isIdentity :: Pattern HsExprF -> Bool
isIdentity = \case
  PatternNode (LamF binderAnn (PatternNode (VarF (LocalName localBinder)))) ->
    binderAnn == localBinder
  _ ->
    False

isApply :: Pattern HsExprF -> Bool
isApply = \case
  PatternNode (LamF functionBinder (PatternNode (LamF argumentBinder (PatternNode (AppF (PatternNode (VarF (LocalName localFunction))) (PatternNode (VarF (LocalName localArgument)))))))) ->
    functionBinder == localFunction && argumentBinder == localArgument
  _ ->
    False

isKeep :: Pattern HsExprF -> Bool
isKeep = \case
  PatternNode
    ( LamF
        outerBinder
        ( PatternNode
            ( LetF
                (LetMode NonRecursiveBinds LetSyntax)
                [(PVarP innerBinder, PatternNode (VarF (LocalName innerRhsBinder)))]
                (PatternNode (VarF (LocalName innerBodyBinder)))
            )
        )
    ) ->
    outerBinder == innerRhsBinder && innerBinder == innerBodyBinder
  _ ->
    False
