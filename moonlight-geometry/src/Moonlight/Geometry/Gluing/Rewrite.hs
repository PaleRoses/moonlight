module Moonlight.Geometry.Gluing.Rewrite
  ( SdfRewriteError (..),
    sdfBaseRules,
    compileSdfBaseRules,
  )
where

import Data.Bifunctor (first)
import Moonlight.Core (Pattern (..), RewriteRuleId (..))
import Moonlight.Core qualified as EGraph
import Moonlight.Rewrite.System qualified as Rewrite
import Moonlight.Geometry.Site.Parameters
import Moonlight.Geometry.Site.Token

data SdfRewriteError capability
  = SdfRewriteRuleNameInvalid !Rewrite.RuleNameError
  | SdfRewriteRuleSetInvalid !(Rewrite.RewriteError capability SDFTokenF)
  | SdfRewritePlanningInvalid !Rewrite.RulePlanError
  deriving stock (Eq, Show)

sdfBaseRules :: Either (SdfRewriteError capability) [Rewrite.RuleSpec capability SDFTokenF]
sdfBaseRules =
  sequenceA
    ( [ rewriteRule 0 "hard-union-empty-right" (PatternNode (HardUnion var0 emptyPattern)) var0,
        rewriteRule 1 "hard-union-empty-left" (PatternNode (HardUnion emptyPattern var0)) var0,
        rewriteRule 2 "hard-union-idempotent" (PatternNode (HardUnion var0 var0)) var0,
        rewriteRule 3 "hard-union-commutative" (PatternNode (HardUnion var0 var1)) (PatternNode (HardUnion var1 var0)),
        rewriteRule 4 "hard-union-associative" (PatternNode (HardUnion (PatternNode (HardUnion var0 var1)) var2)) (PatternNode (HardUnion var0 (PatternNode (HardUnion var1 var2)))),
        rewriteRule 5 "hard-intersect-empty-right" (PatternNode (HardIntersect var0 emptyPattern)) emptyPattern,
        rewriteRule 6 "hard-intersect-empty-left" (PatternNode (HardIntersect emptyPattern var0)) emptyPattern,
        rewriteRule 7 "hard-intersect-commutative" (PatternNode (HardIntersect var0 var1)) (PatternNode (HardIntersect var1 var0)),
        rewriteRule 8 "hard-intersect-associative" (PatternNode (HardIntersect (PatternNode (HardIntersect var0 var1)) var2)) (PatternNode (HardIntersect var0 (PatternNode (HardIntersect var1 var2)))),
        rewriteRule 9 "hard-subtract-empty-right" (PatternNode (HardSubtract var0 emptyPattern)) var0,
        rewriteRule 10 "hard-subtract-empty-left" (PatternNode (HardSubtract emptyPattern var0)) emptyPattern,
        rewriteRule 11 "chamfer-zero" (PatternNode (Chamfer 0.0 var0 var1)) (PatternNode (HardUnion var0 var1)),
        rewriteRule 12 "round-zero" (PatternNode (Round 0.0 var0)) var0,
        rewriteRule 13 "twist-zero" (PatternNode (Twist 0.0 var0)) var0,
        rewriteRule 14 "bend-zero" (PatternNode (Bend 0.0 var0)) var0,
        rewriteRule 15 "onion-zero" (PatternNode (Onion 0.0 var0)) var0
      ]
        <> buildNeutralNoiseRules 16 NoisePerturbation "noise-zero"
        <> buildNeutralNoiseRules (16 + length allNoiseKernels) DomainWarp "domain-warp-zero"
    )

compileSdfBaseRules :: Ord capability => Either (SdfRewriteError capability) (Rewrite.RulePlanSet capability SDFTokenF)
compileSdfBaseRules = do
  rules <- sdfBaseRules
  checkedRules <- first SdfRewriteRuleSetInvalid (Rewrite.checkRuleSet (Rewrite.ruleSet rules))
  first SdfRewritePlanningInvalid (Rewrite.planRuleSet checkedRules)

buildNeutralNoiseRules ::
  Int ->
  (NoiseParams -> Pattern SDFTokenF -> SDFTokenF (Pattern SDFTokenF)) ->
  String ->
  [Either (SdfRewriteError capability) (Rewrite.RuleSpec capability SDFTokenF)]
buildNeutralNoiseRules startRuleId constructor rulePrefix =
  zipWith
    (\offset kernel -> rewriteRule (startRuleId + offset) (rulePrefix <> "-" <> show kernel) (PatternNode (constructor (neutralNoiseParams kernel) var0)) var0)
    [0 ..]
    allNoiseKernels

rewriteRule :: Int -> String -> Pattern SDFTokenF -> Pattern SDFTokenF -> Either (SdfRewriteError capability) (Rewrite.RuleSpec capability SDFTokenF)
rewriteRule ruleId ruleName lhsPattern rhsPattern =
  first SdfRewriteRuleNameInvalid $
    (\name -> Rewrite.ruleWithId (RewriteRuleId ruleId) name lhsPattern rhsPattern)
      <$> Rewrite.mkRuleName ruleName

emptyPattern :: Pattern SDFTokenF
emptyPattern = PatternNode SDFEmpty

var0 :: Pattern SDFTokenF
var0 = PatternVar (EGraph.mkPatternVar 0)

var1 :: Pattern SDFTokenF
var1 = PatternVar (EGraph.mkPatternVar 1)

var2 :: Pattern SDFTokenF
var2 = PatternVar (EGraph.mkPatternVar 2)
