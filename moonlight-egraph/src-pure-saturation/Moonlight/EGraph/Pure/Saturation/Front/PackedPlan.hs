{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Pure.Saturation.Front.PackedPlan
  ( PackedPlanError (..),
    packPatternQuery,
    unpackPatternQuery,
    packCompiledPatternQuery,
    unpackCompiledPatternQuery,
    packGuardTerm,
    unpackGuardTerm,
    packGuardAtom,
    unpackGuardAtom,
    packGuardExpr,
    unpackGuardExpr,
    packRewriteCondition,
    unpackRewriteCondition,
    packCompiledGuard,
    unpackCompiledGuard,
    packPostMatchTerm,
    unpackPostMatchTerm,
    packPostMatchSubst,
    unpackPostMatchSubst,
    packCompiledPatternExtension,
    unpackCompiledPatternExtension,
    packCompiledApplicationCondition,
    unpackCompiledApplicationCondition,
    packRulePlan,
    unpackRulePlan,
    packRulePlanSet,
    unpackRulePlanSet,
    packFactRule,
    unpackFactRule,
    packCompiledFactRule,
    unpackCompiledFactRule,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Moonlight.Core
  ( PatternVar,
  )
import Moonlight.EGraph.Pure.Saturation.Front.PackedNode
  ( PackedNode,
    packNode,
    packPattern,
    unpackPattern,
    packedNode,
  )
import Moonlight.Rewrite.DSL
  ( Node,
    NodeTag,
    RewriteSignature,
  )
import Moonlight.Rewrite.Algebra
  ( ApplicationConditionCompileError,
    CompiledApplicationCondition,
    CompiledPatternExtension,
    cpeAnchorVars,
    cpeQuery,
    compiledApplicationCondition,
    compiledApplicationConditionExpression,
    recompilePatternExtension,
  )
import Moonlight.Rewrite.Algebra
  ( CompiledPatternQuery,
    PatternQuery (..),
    compilePatternQuery,
    cpqQuery,
  )
import Moonlight.Rewrite.Runtime
  ( PostMatchSubst (..),
    PostMatchTerm (..),
  )
import Moonlight.Rewrite.Runtime
  ( rhsInstantiationSpec,
  )
import Moonlight.Rewrite.Runtime
  ( RulePlan,
    RulePlanError,
    certifyRulePlan,
    rpApplicationCondition,
    rpId,
    rpQuery,
    rulePlanPostSubst,
    rulePlanRhsPattern,
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
    GuardAtom (..),
    GuardExpr,
    GuardTerm (..),
    RewriteCondition (..),
    combineCompiledGuards,
    mapCompiledGuard,
  )
import Moonlight.Rewrite.System
  ( CompiledFactRule,
    FactRule,
    FactRuleCompileError,
    RawFactRule (..),
    compileFactRule,
    compiledFactRuleToRawFactRule,
  )
import Moonlight.Rewrite.System
  ( RulePlanSet,
    traverseRulePlanSet,
  )

type PackedPlanError :: Type
data PackedPlanError
  = PackedPlanQueryVariablesUnbound ![PatternVar]
  | PackedPlanApplicationConditionInvalid !ApplicationConditionCompileError
  | PackedPlanRuleInvalid !RulePlanError
  | PackedPlanFactRuleInvalid !FactRuleCompileError
  deriving stock (Eq, Show)

packPatternQuery ::
  RewriteSignature sig =>
  (guard -> packedGuard) ->
  PatternQuery guard (Node sig) ->
  PatternQuery packedGuard (PackedNode sig)
packPatternQuery packGuard =
  \case
    SinglePatternQuery patternValue ->
      SinglePatternQuery (packPattern patternValue)
    ConjunctivePatternQuery queries ->
      ConjunctivePatternQuery (fmap (packPatternQuery packGuard) queries)
    GuardedPatternQuery query guardValue ->
      GuardedPatternQuery (packPatternQuery packGuard query) (packGuard guardValue)

unpackPatternQuery ::
  RewriteSignature sig =>
  (packedGuard -> guard) ->
  PatternQuery packedGuard (PackedNode sig) ->
  PatternQuery guard (Node sig)
unpackPatternQuery unpackGuard =
  \case
    SinglePatternQuery patternValue ->
      SinglePatternQuery (unpackPattern patternValue)
    ConjunctivePatternQuery queries ->
      ConjunctivePatternQuery (fmap (unpackPatternQuery unpackGuard) queries)
    GuardedPatternQuery query guardValue ->
      GuardedPatternQuery (unpackPatternQuery unpackGuard query) (unpackGuard guardValue)

packCompiledPatternQuery ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  CompiledPatternQuery (CompiledGuard capability (Node sig)) (Node sig) ->
  Either PackedPlanError (CompiledPatternQuery (CompiledGuard capability (PackedNode sig)) (PackedNode sig))
packCompiledPatternQuery compiledQuery =
  first PackedPlanQueryVariablesUnbound $
    compilePatternQuery
      combineCompiledGuards
      (\_ compiledGuard -> Right compiledGuard)
      (packPatternQuery packCompiledGuard (cpqQuery compiledQuery))

unpackCompiledPatternQuery ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  CompiledPatternQuery (CompiledGuard capability (PackedNode sig)) (PackedNode sig) ->
  Either PackedPlanError (CompiledPatternQuery (CompiledGuard capability (Node sig)) (Node sig))
unpackCompiledPatternQuery compiledQuery =
  first PackedPlanQueryVariablesUnbound $
    compilePatternQuery
      combineCompiledGuards
      (\_ compiledGuard -> Right compiledGuard)
      (unpackPatternQuery unpackCompiledGuard (cpqQuery compiledQuery))

packGuardTerm ::
  RewriteSignature sig =>
  GuardTerm (Node sig) ->
  GuardTerm (PackedNode sig)
packGuardTerm =
  \case
    GuardRefTerm guardRef ->
      GuardRefTerm guardRef
    GuardProjectTerm guardTerm childIndex ->
      GuardProjectTerm (packGuardTerm guardTerm) childIndex
    GuardNodeTerm nodeValue ->
      GuardNodeTerm (packNode (fmap packGuardTerm nodeValue))

unpackGuardTerm ::
  RewriteSignature sig =>
  GuardTerm (PackedNode sig) ->
  GuardTerm (Node sig)
unpackGuardTerm =
  \case
    GuardRefTerm guardRef ->
      GuardRefTerm guardRef
    GuardProjectTerm guardTerm childIndex ->
      GuardProjectTerm (unpackGuardTerm guardTerm) childIndex
    GuardNodeTerm packed ->
      GuardNodeTerm (fmap unpackGuardTerm (packedNode packed))

packGuardAtom ::
  RewriteSignature sig =>
  GuardAtom capability (Node sig) ->
  GuardAtom capability (PackedNode sig)
packGuardAtom =
  \case
    ClassesEquivalent leftTerm rightTerm ->
      ClassesEquivalent (packGuardTerm leftTerm) (packGuardTerm rightTerm)
    HasFact factId guardTerms ->
      HasFact factId (fmap packGuardTerm guardTerms)
    HasCapability capability guardTerms ->
      HasCapability capability (fmap packGuardTerm guardTerms)

unpackGuardAtom ::
  RewriteSignature sig =>
  GuardAtom capability (PackedNode sig) ->
  GuardAtom capability (Node sig)
unpackGuardAtom =
  \case
    ClassesEquivalent leftTerm rightTerm ->
      ClassesEquivalent (unpackGuardTerm leftTerm) (unpackGuardTerm rightTerm)
    HasFact factId guardTerms ->
      HasFact factId (fmap unpackGuardTerm guardTerms)
    HasCapability capability guardTerms ->
      HasCapability capability (fmap unpackGuardTerm guardTerms)

packGuardExpr ::
  RewriteSignature sig =>
  GuardExpr capability (Node sig) ->
  GuardExpr capability (PackedNode sig)
packGuardExpr =
  fmap packGuardAtom

unpackGuardExpr ::
  RewriteSignature sig =>
  GuardExpr capability (PackedNode sig) ->
  GuardExpr capability (Node sig)
unpackGuardExpr =
  fmap unpackGuardAtom

packRewriteCondition ::
  RewriteSignature sig =>
  RewriteCondition capability (Node sig) ->
  RewriteCondition capability (PackedNode sig)
packRewriteCondition (RewriteCondition guardExpr) =
  RewriteCondition (packGuardExpr guardExpr)

unpackRewriteCondition ::
  RewriteSignature sig =>
  RewriteCondition capability (PackedNode sig) ->
  RewriteCondition capability (Node sig)
unpackRewriteCondition (RewriteCondition guardExpr) =
  RewriteCondition (unpackGuardExpr guardExpr)

packCompiledGuard ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  CompiledGuard capability (Node sig) ->
  CompiledGuard capability (PackedNode sig)
packCompiledGuard =
  mapCompiledGuard packGuardExpr

unpackCompiledGuard ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  CompiledGuard capability (PackedNode sig) ->
  CompiledGuard capability (Node sig)
unpackCompiledGuard =
  mapCompiledGuard unpackGuardExpr

packPostMatchTerm ::
  RewriteSignature sig =>
  PostMatchTerm (Node sig) ->
  PostMatchTerm (PackedNode sig)
packPostMatchTerm =
  \case
    PostMatchVar patternVar ->
      PostMatchVar patternVar
    PostMatchPattern patternValue ->
      PostMatchPattern (packPattern patternValue)

unpackPostMatchTerm ::
  RewriteSignature sig =>
  PostMatchTerm (PackedNode sig) ->
  PostMatchTerm (Node sig)
unpackPostMatchTerm =
  \case
    PostMatchVar patternVar ->
      PostMatchVar patternVar
    PostMatchPattern patternValue ->
      PostMatchPattern (unpackPattern patternValue)

packPostMatchSubst ::
  RewriteSignature sig =>
  PostMatchSubst (Node sig) ->
  PostMatchSubst (PackedNode sig)
packPostMatchSubst =
  \case
    SubstBinder binderId argumentTerm ->
      SubstBinder binderId (packPostMatchTerm argumentTerm)
    SequentialPostMatchSubst leftSubst rightSubst ->
      SequentialPostMatchSubst (packPostMatchSubst leftSubst) (packPostMatchSubst rightSubst)

unpackPostMatchSubst ::
  RewriteSignature sig =>
  PostMatchSubst (PackedNode sig) ->
  PostMatchSubst (Node sig)
unpackPostMatchSubst =
  \case
    SubstBinder binderId argumentTerm ->
      SubstBinder binderId (unpackPostMatchTerm argumentTerm)
    SequentialPostMatchSubst leftSubst rightSubst ->
      SequentialPostMatchSubst (unpackPostMatchSubst leftSubst) (unpackPostMatchSubst rightSubst)

packCompiledPatternExtension ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  CompiledPatternExtension (CompiledGuard capability (Node sig)) (Node sig) ->
  Either PackedPlanError (CompiledPatternExtension (CompiledGuard capability (PackedNode sig)) (PackedNode sig))
packCompiledPatternExtension extension = do
  query <- packCompiledPatternQuery (cpeQuery extension)
  first PackedPlanApplicationConditionInvalid
    (recompilePatternExtension (cpeAnchorVars extension) query extension)

unpackCompiledPatternExtension ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  CompiledPatternExtension (CompiledGuard capability (PackedNode sig)) (PackedNode sig) ->
  Either PackedPlanError (CompiledPatternExtension (CompiledGuard capability (Node sig)) (Node sig))
unpackCompiledPatternExtension extension = do
  query <- unpackCompiledPatternQuery (cpeQuery extension)
  first PackedPlanApplicationConditionInvalid
    (recompilePatternExtension (cpeAnchorVars extension) query extension)

packCompiledApplicationCondition ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  CompiledApplicationCondition (CompiledGuard capability (Node sig)) (Node sig) ->
  Either PackedPlanError (CompiledApplicationCondition (CompiledGuard capability (PackedNode sig)) (PackedNode sig))
packCompiledApplicationCondition =
  fmap compiledApplicationCondition
    . traverse packCompiledPatternExtension
    . compiledApplicationConditionExpression

unpackCompiledApplicationCondition ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  CompiledApplicationCondition (CompiledGuard capability (PackedNode sig)) (PackedNode sig) ->
  Either PackedPlanError (CompiledApplicationCondition (CompiledGuard capability (Node sig)) (Node sig))
unpackCompiledApplicationCondition =
  fmap compiledApplicationCondition
    . traverse unpackCompiledPatternExtension
    . compiledApplicationConditionExpression

packRulePlan ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  RulePlan (CompiledGuard capability (Node sig)) (Node sig) ->
  Either PackedPlanError (RulePlan (CompiledGuard capability (PackedNode sig)) (PackedNode sig))
packRulePlan rulePlan = do
  query <- packCompiledPatternQuery (rpQuery rulePlan)
  applicationCondition <- traverse packCompiledApplicationCondition (rpApplicationCondition rulePlan)
  first PackedPlanRuleInvalid $
    certifyRulePlan
      (rpId rulePlan)
      query
      (rhsInstantiationSpec (packPostMatchSubst <$> rulePlanPostSubst rulePlan) (packPattern (rulePlanRhsPattern rulePlan)))
      applicationCondition

unpackRulePlan ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  RulePlan (CompiledGuard capability (PackedNode sig)) (PackedNode sig) ->
  Either PackedPlanError (RulePlan (CompiledGuard capability (Node sig)) (Node sig))
unpackRulePlan rulePlan = do
  query <- unpackCompiledPatternQuery (rpQuery rulePlan)
  applicationCondition <- traverse unpackCompiledApplicationCondition (rpApplicationCondition rulePlan)
  first PackedPlanRuleInvalid $
    certifyRulePlan
      (rpId rulePlan)
      query
      (rhsInstantiationSpec (unpackPostMatchSubst <$> rulePlanPostSubst rulePlan) (unpackPattern (rulePlanRhsPattern rulePlan)))
      applicationCondition

packRulePlanSet ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  RulePlanSet capability (Node sig) ->
  Either PackedPlanError (RulePlanSet capability (PackedNode sig))
packRulePlanSet =
  traverseRulePlanSet packRulePlan

unpackRulePlanSet ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  RulePlanSet capability (PackedNode sig) ->
  Either PackedPlanError (RulePlanSet capability (Node sig))
unpackRulePlanSet =
  traverseRulePlanSet unpackRulePlan

packFactRule ::
  RewriteSignature sig =>
  FactRule capability (Node sig) ->
  FactRule capability (PackedNode sig)
packFactRule factRuleValue =
  FactRule
    { frId = frId factRuleValue,
      frName = frName factRuleValue,
      frPattern = packPattern (frPattern factRuleValue),
      frProjection = frProjection factRuleValue,
      frFactId = frFactId factRuleValue,
      frCondition = packRewriteCondition <$> frCondition factRuleValue
    }

unpackFactRule ::
  RewriteSignature sig =>
  FactRule capability (PackedNode sig) ->
  FactRule capability (Node sig)
unpackFactRule factRuleValue =
  FactRule
    { frId = frId factRuleValue,
      frName = frName factRuleValue,
      frPattern = unpackPattern (frPattern factRuleValue),
      frProjection = frProjection factRuleValue,
      frFactId = frFactId factRuleValue,
      frCondition = unpackRewriteCondition <$> frCondition factRuleValue
    }

packCompiledFactRule ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  CompiledFactRule capability (Node sig) ->
  Either PackedPlanError (CompiledFactRule capability (PackedNode sig))
packCompiledFactRule =
  first PackedPlanFactRuleInvalid
    . compileFactRule
    . packFactRule
    . compiledFactRuleToRawFactRule

unpackCompiledFactRule ::
  (RewriteSignature sig, Ord capability, Ord (NodeTag sig)) =>
  CompiledFactRule capability (PackedNode sig) ->
  Either PackedPlanError (CompiledFactRule capability (Node sig))
unpackCompiledFactRule =
  first PackedPlanFactRuleInvalid
    . compileFactRule
    . unpackFactRule
    . compiledFactRuleToRawFactRule
