{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedLabels #-}

module Moonlight.EGraph.Introspection.HsExprSpec.Equation
  ( tests,
  )
where

import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.Name.Reader (RdrName, mkRdrUnqual)
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( hsExprAppendAssociativityLawId,
    hsExprFilterFusionLawId,
    hsExprMapFusionLawId,
    hsExprParErasureLawId,
  )
import Moonlight.EGraph.Introspection.Core.HsExpr.Equation
  ( HsExprLawRule,
    equationRule,
    rewriteRule,
  )
import Moonlight.EGraph.Introspection.Core.HsExpr.Front (HsExprLawEmitError)
import Moonlight.EGraph.Introspection.Core.HsExpr.Front qualified as HsExprFront
import Moonlight.Rewrite.System (LawId, lawIdKey)
import Moonlight.Rewrite.System (RewriteCondition)
import Moonlight.Rewrite.System (RawRewriteRule (..))
import Moonlight.Core (Pattern, RewriteRuleId)
import Moonlight.Pale.Ghc.Expr (BinderAnn (..))
import Moonlight.Pale.Ghc.Expr (HsExprF, ScopeCtx)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "equation"
    [ testCase "map fusion equation agrees with combinator oracle" $
        assertLawAgreement mapFusionOracle mapFusionEquation,
      testCase "filter fusion equation agrees with binder combinator oracle" $
        assertLawAgreement filterFusionOracle filterFusionEquation,
      testCase "append associativity equation agrees with combinator oracle" $
        assertLawAgreement appendAssociativityOracle appendAssociativityEquation,
      testCase "par erasure equation agrees with combinator oracle" $
        assertLawAgreement parErasureOracle parErasureEquation
    ]

assertLawAgreement :: Either HsExprLawEmitError HsExprLawRule -> Either HsExprLawEmitError HsExprLawRule -> Assertion
assertLawAgreement oracleResult equationResult =
  case (oracleResult, equationResult) of
    (Right oracleRule, Right equationRuleValue)
      | lawRuleProjection oracleRule == lawRuleProjection equationRuleValue ->
          pure ()
      | otherwise ->
          assertFailure ("law mismatch\noracle: " <> show (lawRuleProjection oracleRule) <> "\nequation: " <> show (lawRuleProjection equationRuleValue))
    (leftResult, rightResult) ->
      assertFailure ("law emission mismatch\noracle: " <> showEither leftResult <> "\nequation: " <> showEither rightResult)

showEither :: Either HsExprLawEmitError HsExprLawRule -> String
showEither =
  either show (show . lawRuleProjection)

data LawRuleProjection = LawRuleProjection
  { lrpId :: !RewriteRuleId,
    lrpLhs :: !(Pattern HsExprF),
    lrpRhs :: !(Pattern HsExprF),
    lrpCondition :: !(Maybe (RewriteCondition ScopeCtx HsExprF)),
    lrpApplicationConditionAbsent :: !Bool,
    lrpPostSubstAbsent :: !Bool
  }
  deriving stock (Eq, Show)

lawRuleProjection :: HsExprLawRule -> LawRuleProjection
lawRuleProjection rule =
  LawRuleProjection
    { lrpId = rrId rule,
      lrpLhs = rrLhs rule,
      lrpRhs = rrRhs rule,
      lrpCondition = rrCondition rule,
      lrpApplicationConditionAbsent = maybe True (const False) (rrApplicationCondition rule),
      lrpPostSubstAbsent = maybe True (const False) (rrPostSubst rule)
    }

mapFusionOracle :: Either HsExprLawEmitError HsExprLawRule
mapFusionOracle =
  rewriteRule
    hsExprMapFusionLawId
    0
    ["f", "g", "xs"]
    (HsExprFront.app2 mapName #f (HsExprFront.par (HsExprFront.app2 mapName #g #xs)))
    (HsExprFront.app2 mapName (HsExprFront.par (HsExprFront.op #f composeName #g)) #xs)

mapFusionEquation :: Either HsExprLawEmitError HsExprLawRule
mapFusionEquation =
  equationRule
    hsExprMapFusionLawId
    0
    ["f", "g", "xs"]
    [("map", mapName), (".", composeName)]
    "map f (map g xs) = map (f . g) xs"

filterFusionOracle :: Either HsExprLawEmitError HsExprLawRule
filterFusionOracle =
  rewriteRule
    hsExprFilterFusionLawId
    0
    ["outerPredicate", "innerPredicate", "xs"]
    (HsExprFront.app2 filterName #outerPredicate (HsExprFront.par (HsExprFront.app2 filterName #innerPredicate #xs)))
    ( HsExprFront.app2
        filterName
        ( HsExprFront.par
            ( HsExprFront.lam
                (filterBinder hsExprFilterFusionLawId)
                ( HsExprFront.op
                    (HsExprFront.app #innerPredicate (HsExprFront.local (filterBinder hsExprFilterFusionLawId)))
                    andName
                    (HsExprFront.app #outerPredicate (HsExprFront.local (filterBinder hsExprFilterFusionLawId)))
                )
            )
        )
        #xs
    )

filterFusionEquation :: Either HsExprLawEmitError HsExprLawRule
filterFusionEquation =
  equationRule
    hsExprFilterFusionLawId
    0
    ["outerPredicate", "innerPredicate", "xs"]
    [("filter", filterName), ("&&", andName), ("x", filterBinderName)]
    "filter outerPredicate (filter innerPredicate xs) = filter (\\x -> innerPredicate x && outerPredicate x) xs"

appendAssociativityOracle :: Either HsExprLawEmitError HsExprLawRule
appendAssociativityOracle =
  rewriteRule
    hsExprAppendAssociativityLawId
    0
    ["xs", "ys", "zs"]
    (HsExprFront.op (HsExprFront.par (HsExprFront.op #xs appendName #ys)) appendName #zs)
    (HsExprFront.op #xs appendName (HsExprFront.par (HsExprFront.op #ys appendName #zs)))

appendAssociativityEquation :: Either HsExprLawEmitError HsExprLawRule
appendAssociativityEquation =
  equationRule
    hsExprAppendAssociativityLawId
    0
    ["xs", "ys", "zs"]
    [("++", appendName)]
    "(xs ++ ys) ++ zs = xs ++ (ys ++ zs)"

parErasureOracle :: Either HsExprLawEmitError HsExprLawRule
parErasureOracle =
  rewriteRule
    hsExprParErasureLawId
    0
    ["x"]
    (HsExprFront.par #x)
    #x

parErasureEquation :: Either HsExprLawEmitError HsExprLawRule
parErasureEquation =
  equationRule
    hsExprParErasureLawId
    0
    ["x"]
    []
    "(x) = x"

filterBinder :: LawId -> BinderAnn
filterBinder lawIdValue =
  BinderAnn
    { baId = toEnum (negate (lawIdKey lawIdValue)),
      baName = filterBinderName
    }

mapName :: RdrName
mapName =
  mkRdrUnqual (mkVarOcc "map")

composeName :: RdrName
composeName =
  mkRdrUnqual (mkVarOcc ".")

filterName :: RdrName
filterName =
  mkRdrUnqual (mkVarOcc "filter")

andName :: RdrName
andName =
  mkRdrUnqual (mkVarOcc "&&")

appendName :: RdrName
appendName =
  mkRdrUnqual (mkVarOcc "++")

filterBinderName :: RdrName
filterBinderName =
  mkRdrUnqual (mkVarOcc "nebulaFilterArg")
