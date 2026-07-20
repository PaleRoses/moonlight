module Moonlight.EGraph.Pure.Rewrite.Guard
  ( acceptRewriteCondition,
    acceptRewriteConditionWith,
  )
where

import Moonlight.Core (Language)
import Moonlight.EGraph.Pure.Guard.Evaluation
  ( GuardGraphView (..),
    graphGuardView,
    resolveGuardTermWith,
  )
import Moonlight.EGraph.Pure.Types
  ( EGraph,
  )
import Moonlight.Rewrite.Runtime (ExecutableRewriteMatch (..))
import Moonlight.Rewrite.System
  ( CompiledGuard,
    GuardCapabilityResolver,
    GuardEvidence,
    evaluateCompiledGuardWithEvidenceAndCapabilities,
  )
import Moonlight.Rewrite.System (FactStore)
import Moonlight.Rewrite.Runtime
  ( RewriteApplicationError (..),
    RulePlan,
    rpId,
    rulePlanCondition
  )

-- | Evaluate a match's compiled guard against a quotient view. The fact
-- store must already be canonical with respect to the view at call time:
-- guard tuples are resolved through the view's canonicalizer, so a store
-- keyed by stale class identifiers would silently miss its facts. Callers
-- that evaluate a batch of matches against one view canonicalize the store
-- once at the batch boundary; callers whose quotient evolves between
-- matches canonicalize per match.
acceptRewriteConditionWith ::
  Traversable f =>
  FactStore ->
  GuardCapabilityResolver capability ->
  ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence guideEvidence f ->
  GuardGraphView f ->
  Either RewriteApplicationError (Maybe GuardEvidence)
acceptRewriteConditionWith factStore capabilityResolver rewriteMatch view =
  traverse acceptCompiledGuard (rulePlanCondition (ermRule rewriteMatch))
  where
    acceptCompiledGuard compiledGuard =
      maybe
        (Left (RewriteConditionRejected (rpId (ermRule rewriteMatch))))
        Right
        ( evaluateCompiledGuardWithEvidenceAndCapabilities
            factStore
            capabilityResolver
            (ggvCanonicalize view)
            resolveTerm
            compiledGuard
        )

    rootClassId =
      ggvCanonicalize view (ermRootClass rewriteMatch)

    substitution =
      ermSubstitution rewriteMatch

    resolveTerm =
      resolveGuardTermWith view rootClassId substitution

acceptRewriteCondition ::
  Language f =>
  FactStore ->
  GuardCapabilityResolver capability ->
  ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence guideEvidence f ->
  EGraph f a ->
  Either RewriteApplicationError (Maybe GuardEvidence)
acceptRewriteCondition factStore capabilityResolver rewriteMatch =
  acceptRewriteConditionWith factStore capabilityResolver rewriteMatch . graphGuardView
