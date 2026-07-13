{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Test.Ring.Rules
  ( commutativityRules,
    distributionRules,
    explosionRules,
    identityRules,
    annihilationRules,
    negationRules,
    saturationRingRules,
    ringRewrite,
  )
where

import Moonlight.Core
  ( Pattern (..)
  )
import Moonlight.Core qualified as EGraph
import Moonlight.Rewrite.System
  ( RawRewriteRule (..)
  )
import Moonlight.Rewrite.System (RewriteCondition)
import Moonlight.EGraph.Pure.Types (RewriteRuleId (..))
import Moonlight.EGraph.Test.Ring.Core (RingF (..))
commutativityRules :: [RawRewriteRule (RewriteCondition capability RingF) RingF]
commutativityRules =
  [ ringRewrite 0 "add-commute"
      (PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1))))
      (PatternNode (Add (PatternVar (EGraph.mkPatternVar 1)) (PatternVar (EGraph.mkPatternVar 0)))),
    ringRewrite 1 "mul-commute"
      (PatternNode (Mul (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1))))
      (PatternNode (Mul (PatternVar (EGraph.mkPatternVar 1)) (PatternVar (EGraph.mkPatternVar 0))))
  ]

distributionRules :: [RawRewriteRule (RewriteCondition capability RingF) RingF]
distributionRules =
  [ ringRewrite 20 "distribute-left"
      (PatternNode (Mul (PatternVar (EGraph.mkPatternVar 0)) (PatternNode (Add (PatternVar (EGraph.mkPatternVar 1)) (PatternVar (EGraph.mkPatternVar 2))))))
      (PatternNode (Add (PatternNode (Mul (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))) (PatternNode (Mul (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 2)))))),
    ringRewrite 21 "distribute-right"
      (PatternNode (Mul (PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))) (PatternVar (EGraph.mkPatternVar 2))))
      (PatternNode (Add (PatternNode (Mul (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 2)))) (PatternNode (Mul (PatternVar (EGraph.mkPatternVar 1)) (PatternVar (EGraph.mkPatternVar 2))))))
  ]

explosionRules :: [RawRewriteRule (RewriteCondition capability RingF) RingF]
explosionRules =
  commutativityRules <> distributionRules

identityRules :: [RawRewriteRule (RewriteCondition capability RingF) RingF]
identityRules =
  [ ringRewrite 30 "add-zero-right"
      (PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternNode RZero)))
      (PatternVar (EGraph.mkPatternVar 0)),
    ringRewrite 31 "add-zero-left"
      (PatternNode (Add (PatternNode RZero) (PatternVar (EGraph.mkPatternVar 0))))
      (PatternVar (EGraph.mkPatternVar 0)),
    ringRewrite 32 "mul-one-right"
      (PatternNode (Mul (PatternVar (EGraph.mkPatternVar 0)) (PatternNode ROne)))
      (PatternVar (EGraph.mkPatternVar 0)),
    ringRewrite 33 "mul-one-left"
      (PatternNode (Mul (PatternNode ROne) (PatternVar (EGraph.mkPatternVar 0))))
      (PatternVar (EGraph.mkPatternVar 0))
  ]

annihilationRules :: [RawRewriteRule (RewriteCondition capability RingF) RingF]
annihilationRules =
  [ ringRewrite 40 "mul-zero-right"
      (PatternNode (Mul (PatternVar (EGraph.mkPatternVar 0)) (PatternNode RZero)))
      (PatternNode RZero),
    ringRewrite 41 "mul-zero-left"
      (PatternNode (Mul (PatternNode RZero) (PatternVar (EGraph.mkPatternVar 0))))
      (PatternNode RZero)
  ]

negationRules :: [RawRewriteRule (RewriteCondition capability RingF) RingF]
negationRules =
  [ ringRewrite 50 "double-neg"
      (PatternNode (Neg (PatternNode (Neg (PatternVar (EGraph.mkPatternVar 0))))))
      (PatternVar (EGraph.mkPatternVar 0)),
    ringRewrite 51 "add-neg-self"
      (PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternNode (Neg (PatternVar (EGraph.mkPatternVar 0))))))
      (PatternNode RZero)
  ]

saturationRingRules :: [RawRewriteRule (RewriteCondition capability RingF) RingF]
saturationRingRules =
  distributionRules
    <> identityRules
    <> annihilationRules
    <> negationRules

ringRewrite :: Int -> String -> Pattern RingF -> Pattern RingF -> RawRewriteRule (RewriteCondition capability RingF) RingF
ringRewrite ruleId _ruleName lhsPattern rhsPattern =
  RawRewriteRule
    { rrId = RewriteRuleId ruleId,
      rrLhs = lhsPattern,
      rrRhs = rhsPattern,
      rrCondition = Nothing,
      rrApplicationCondition = Nothing,
      rrPostSubst = Nothing
    }
