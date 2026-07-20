-- | Public execution surface for applying compiled rule matches to an e-graph.
-- It owns no extra execution semantics; it exposes the internal RHS compilation,
-- canonicalization, and class-merge program boundary used by runtime clients.
module Moonlight.Rewrite.Runtime.Exec
  ( InstantiationRef (..),
    InstantiationInput (..),
    InstantiationStep (..),
    InstantiationPlan (..),
    ExecutableRewriteMatch (..),
    ExecutedRewrite (..),
    compileInstantiationPlan,
    compileRewriteRhs,
    compileExecutableRewriteMatch,
    executableRewriteMatchRuleKey,
  )
where

import Moonlight.Rewrite.Runtime.Exec.Internal
  ( ExecutableRewriteMatch (..),
    ExecutedRewrite (..),
    InstantiationInput (..),
    InstantiationPlan (..),
    InstantiationRef (..),
    InstantiationStep (..),
    compileExecutableRewriteMatch,
    compileInstantiationPlan,
    compileRewriteRhs,
    executableRewriteMatchRuleKey,
  )
