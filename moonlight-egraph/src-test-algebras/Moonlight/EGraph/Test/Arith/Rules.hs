{-# LANGUAGE PatternSynonyms #-}

module Moonlight.EGraph.Test.Arith.Rules
  ( ArithRewriteFixture (..),
    ArithFactFixture (..),
    addZeroRightRule,
    addCommuteRule,
    zeroFactRule,
    arithRewriteFixture,
    arithFactFixture,
  )
where

import Data.Kind ( Type )
import Moonlight.EGraph.Effect.CoveringSurface
    ( SurfaceKind(Matching) )
import Moonlight.EGraph.Pure.Types ( RewriteRuleId(..) )
import Moonlight.EGraph.Test.Arith.Core ( ArithF(..) )
import Moonlight.Rewrite.System
    ( GuardRef,
      RewriteCondition(..),
      data GuardRoot,
      data GuardVar,
      guardFalse,
      guardHasCapability,
      guardHasFact )
import Moonlight.Rewrite.System
    ( FactRule, RawFactRule(..), FactRuleId(..) )
import Moonlight.Rewrite.System ( FactId(..) )
import Moonlight.Core
import Moonlight.Core qualified as EGraph
import Moonlight.Rewrite.Runtime
import Moonlight.Rewrite.System
  ( RawRewriteRule(..)
  )

type ArithRewriteFixture :: Type
data ArithRewriteFixture
  = AddZeroRightFixture
  | AddCommuteFixture
  | AddZeroLeftFixture
  | CommuteAddFixture
  | BlockedAddZeroRightFixture
  | GuardedAddZeroRightFixture
  | CapabilityAddZeroRightFixture
  deriving stock (Eq, Ord, Show)

type ArithFactFixture :: Type
data ArithFactFixture
  = ZeroFactFixture
  | CapabilityZeroFactFixture
  deriving stock (Eq, Ord, Show)

mkRewriteRule ::
  RewriteRuleId ->
  Pattern ArithF ->
  Pattern ArithF ->
  Maybe (RewriteCondition capability ArithF) ->
  RawRewriteRule (RewriteCondition capability ArithF) ArithF
mkRewriteRule rewriteRuleId lhsPattern rhsPattern rewriteCondition =
  RawRewriteRule
    { rrId = rewriteRuleId,
      rrLhs = lhsPattern,
      rrRhs = rhsPattern,
      rrCondition = rewriteCondition,
      rrApplicationCondition = Nothing,
      rrPostSubst = Nothing
    }

mkFactRule ::
  FactRuleId ->
  String ->
  Pattern ArithF ->
  [GuardRef] ->
  FactId ->
  Maybe (RewriteCondition capability ArithF) ->
  FactRule capability ArithF
mkFactRule factRuleId factRuleName factPattern factProjection factId factCondition =
  FactRule
    { frId = factRuleId,
      frName = factRuleName,
      frPattern = factPattern,
      frProjection = factProjection,
      frFactId = factId,
      frCondition = factCondition
    }

addZeroRightRule :: RawRewriteRule (RewriteCondition capability ArithF) ArithF
addZeroRightRule =
  mkRewriteRule
    (RewriteRuleId 0)
    (PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternNode (Num 0))))
    (PatternVar (EGraph.mkPatternVar 0))
    Nothing

addCommuteRule :: RawRewriteRule (RewriteCondition capability ArithF) ArithF
addCommuteRule =
  mkRewriteRule
    (RewriteRuleId 1)
    (PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1))))
    (PatternNode (Add (PatternVar (EGraph.mkPatternVar 1)) (PatternVar (EGraph.mkPatternVar 0))))
    Nothing

zeroFactRule :: FactRule capability ArithF
zeroFactRule =
  mkFactRule
    (FactRuleId 0)
    "derive-zero-fact"
    (PatternNode (Num 0))
    [GuardRoot]
    (FactId 0)
    Nothing

arithRewriteFixture :: RewriteRuleId -> ArithRewriteFixture -> RawRewriteRule (RewriteCondition SurfaceKind ArithF) ArithF
arithRewriteFixture rewriteRuleId fixture =
  case fixture of
    AddZeroRightFixture ->
      addZeroRightRule
        { rrId = rewriteRuleId
        }
    AddCommuteFixture ->
      addCommuteRule
        { rrId = rewriteRuleId
        }
    AddZeroLeftFixture ->
      mkRewriteRule
        rewriteRuleId
        (PatternNode (Add (PatternNode (Num 0)) (PatternVar (EGraph.mkPatternVar 0))))
        (PatternVar (EGraph.mkPatternVar 0))
        Nothing
    CommuteAddFixture ->
      addCommuteRule
        { rrId = rewriteRuleId
        }
    BlockedAddZeroRightFixture ->
      addZeroRightRule
        { rrId = rewriteRuleId,
          rrCondition = Just (RewriteCondition guardFalse)
        }
    GuardedAddZeroRightFixture ->
      mkRewriteRule
        rewriteRuleId
        (PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1))))
        (PatternVar (EGraph.mkPatternVar 0))
        (Just (RewriteCondition (guardHasFact (FactId 0) [GuardVar (EGraph.mkPatternVar 1)])))
    CapabilityAddZeroRightFixture ->
      addZeroRightRule
        { rrId = rewriteRuleId,
          rrCondition = Just (RewriteCondition (guardHasCapability Matching [GuardRoot, GuardVar (EGraph.mkPatternVar 0)]))
        }

arithFactFixture :: FactRuleId -> ArithFactFixture -> FactRule SurfaceKind ArithF
arithFactFixture factRuleId fixture =
  case fixture of
    ZeroFactFixture ->
      zeroFactRule
        { frId = factRuleId
        }
    CapabilityZeroFactFixture ->
      mkFactRule
        factRuleId
        "derive-zero-fact-under-capability"
        (PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1))))
        [GuardVar (EGraph.mkPatternVar 1)]
        (FactId 0)
        (Just (RewriteCondition (guardHasCapability Matching [GuardRoot, GuardVar (EGraph.mkPatternVar 1)])))
