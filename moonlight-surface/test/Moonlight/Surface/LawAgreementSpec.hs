module Moonlight.Surface.LawAgreementSpec
  ( tests,
  )
where

import Moonlight.Core (Pattern (..), RewriteRuleId (..), mkPatternVar)
import Moonlight.Rewrite.System (RewriteCondition)
import Moonlight.Rewrite.System (RawRewriteRule (..))
import Moonlight.Surface.Language (SurfaceF (..))
import Moonlight.Surface.Laws
  ( SurfaceCapability,
    SurfaceLawError,
    SurfaceRewriteRule,
    surfaceLawRuleIdBase,
    surfaceTranslateUnionHoistRule,
    surfaceUnionCommutativityRule,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "law agreement"
    [ testCase "union commutativity agrees with structural oracle" $
        assertLawAgreement (Right surfaceUnionCommutativityOracle) surfaceUnionCommutativityRule,
      testCase "translate union hoist agrees with structural oracle" $
        assertLawAgreement (Right surfaceTranslateUnionHoistOracle) surfaceTranslateUnionHoistRule
    ]

assertLawAgreement :: Either SurfaceLawError SurfaceRewriteRule -> Either SurfaceLawError SurfaceRewriteRule -> Assertion
assertLawAgreement oracleResult equationResult =
  case (oracleResult, equationResult) of
    (Right oracleRule, Right equationRuleValue)
      | lawRuleProjection oracleRule == lawRuleProjection equationRuleValue ->
          pure ()
      | otherwise ->
          assertFailure ("law mismatch\noracle: " <> show (lawRuleProjection oracleRule) <> "\nequation: " <> show (lawRuleProjection equationRuleValue))
    (leftResult, rightResult) ->
      assertFailure ("law emission mismatch\noracle: " <> showEither leftResult <> "\nequation: " <> showEither rightResult)

showEither :: Either SurfaceLawError SurfaceRewriteRule -> String
showEither =
  either show (show . lawRuleProjection)

data LawRuleProjection = LawRuleProjection
  { lrpId :: !RewriteRuleId,
    lrpLhs :: !(Pattern SurfaceF),
    lrpRhs :: !(Pattern SurfaceF),
    lrpCondition :: !(Maybe (RewriteCondition SurfaceCapability SurfaceF)),
    lrpApplicationConditionAbsent :: !Bool,
    lrpPostSubstAbsent :: !Bool
  }
  deriving stock (Eq, Show)

lawRuleProjection :: SurfaceRewriteRule -> LawRuleProjection
lawRuleProjection rule =
  LawRuleProjection
    { lrpId = rrId rule,
      lrpLhs = rrLhs rule,
      lrpRhs = rrRhs rule,
      lrpCondition = rrCondition rule,
      lrpApplicationConditionAbsent = maybe True (const False) (rrApplicationCondition rule),
      lrpPostSubstAbsent = maybe True (const False) (rrPostSubst rule)
    }

surfaceUnionCommutativityOracle :: SurfaceRewriteRule
surfaceUnionCommutativityOracle =
  RawRewriteRule
    { rrId = RewriteRuleId ((surfaceLawRuleIdBase + 1) * 100),
      rrLhs = PatternNode (SurfaceUnion patternX patternY),
      rrRhs = PatternNode (SurfaceUnion patternY patternX),
      rrCondition = Nothing,
      rrApplicationCondition = Nothing,
      rrPostSubst = Nothing
    }

surfaceTranslateUnionHoistOracle :: SurfaceRewriteRule
surfaceTranslateUnionHoistOracle =
  RawRewriteRule
    { rrId = RewriteRuleId ((surfaceLawRuleIdBase + 4) * 100),
      rrLhs = PatternNode (SurfaceUnion (PatternNode (SurfaceTranslate patternV patternA)) (PatternNode (SurfaceTranslate patternV patternB))),
      rrRhs = PatternNode (SurfaceTranslate patternV (PatternNode (SurfaceUnion patternA patternB))),
      rrCondition = Nothing,
      rrApplicationCondition = Nothing,
      rrPostSubst = Nothing
    }

patternV :: Pattern SurfaceF
patternV =
  PatternVar (mkPatternVar 0)

patternX :: Pattern SurfaceF
patternX =
  PatternVar (mkPatternVar 0)

patternY :: Pattern SurfaceF
patternY =
  PatternVar (mkPatternVar 1)

patternA :: Pattern SurfaceF
patternA =
  PatternVar (mkPatternVar 1)

patternB :: Pattern SurfaceF
patternB =
  PatternVar (mkPatternVar 2)
