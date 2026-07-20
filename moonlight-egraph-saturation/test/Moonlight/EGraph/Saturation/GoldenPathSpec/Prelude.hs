{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
module Moonlight.EGraph.Saturation.GoldenPathSpec.Prelude
  ( module Moonlight.EGraph.Saturation.GoldenPathSpec.Fixture,
    module Moonlight.EGraph.Pure.Extraction,
    module Moonlight.EGraph.Pure.Kernel.HashCons,
    module Moonlight.EGraph.Pure.Saturation.Matching,
    module Moonlight.Rewrite.System,
    module Moonlight.Core,
    module Moonlight.EGraph.Pure.Relational,
    module Moonlight.EGraph.Pure.Rebuild,
    
    module Moonlight.EGraph.Test.Saturation,
    principalSupport,
    module Moonlight.EGraph.Pure.Types,
    module Moonlight.EGraph.Pure.Context,
    module Moonlight.EGraph.Pure.Context.Proof,
    module Moonlight.EGraph.Saturation.Context.State,
    BackoffConfig, backoffConfig,
    module Test.Tasty,
    module Test.Tasty.HUnit,
    expectRight,
    classesEquivalentAt,
    withGoldenProofGraph,
    runGoldenSupportCase,
  )
where

import Moonlight.Pale.Ghc.Expr (ScopeCtx)
import Moonlight.EGraph.Pure.Extraction
import Moonlight.EGraph.Pure.Kernel.HashCons
import Moonlight.EGraph.Pure.Saturation.Matching
import Moonlight.Rewrite.System
import Moonlight.Core hiding (emptySupport)
import Moonlight.EGraph.Pure.Relational
import Moonlight.EGraph.Pure.Rebuild
import Moonlight.EGraph.Test.Saturation
import Moonlight.Rewrite.ProofContext hiding
  ( ProofGraph (..),
    proofBetween,
    proofClassWitnesses,
    proofClassesReachableFrom,
    proofGraph,
    proofReachability,
    proofRelated,
    recordAnnotatedProofStep,
    recordProofStepWith,
    serializeProofLog,
    summarizeProofLog,
  )
import Moonlight.EGraph.Pure.Types
import Moonlight.EGraph.Pure.Context
import Moonlight.EGraph.Pure.Context.Proof
import Moonlight.EGraph.Saturation.Context.State
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.Rewrite.ProofContext (defaultProofAnnotationBuilder)
import Moonlight.Rewrite.System (RewriteCondition)
import Moonlight.Rewrite.System (RawRewriteRule)
import Moonlight.Saturation.Context.Driver (crrResult)
import Moonlight.Saturation.Context.Error (SaturationError)
import Moonlight.Saturation.Context.Program.Spec (staticRewriteContextSnapshot)
import Moonlight.Saturation.Support.Core
  ( SupportSaturationReportFor,
    SupportScheduleGroup,
  )
import Moonlight.Sheaf.Twist.SupportedRuleSpec qualified as Twist
import Moonlight.Control.Schedule (BackoffConfig, backoffConfig)
import Moonlight.EGraph.Saturation.GoldenPathSpec.Fixture
import Moonlight.Sheaf.Context.Algebra (contextEquivalentAt)
import Test.Tasty
import Test.Tasty.HUnit

expectRight :: (Show e) => Either e a -> IO a
expectRight =
  either
    (\errorValue -> assertFailure ("expected Right, got Left: " <> show errorValue))
    pure

classesEquivalentAt :: TestScope -> ClassId -> ClassId -> ContextEGraph owner TestF () TestScope -> Bool
classesEquivalentAt contextValue leftClass rightClass =
  either (const False) id . contextEquivalentAt contextValue leftClass rightClass

withGoldenProofGraph ::
  EGraph TestF () ->
  (forall owner. SaturatingProofEGraph owner ScopeCtx TestF () TestScope () -> Assertion) ->
  Assertion
withGoldenProofGraph graphValue useProofGraph =
  withEmptyContextEGraph testScopeLattice graphValue
    (useProofGraph . emptySaturatingProofEGraph)

runGoldenSupportCase ::
  EGraphSaturationConfig owner ScopeCtx TestF () TestScope ->
  Twist.SupportedRuleBook owner TestScope (RawRewriteRule (RewriteCondition ScopeCtx TestF) TestF) ->
  SaturatingProofEGraph owner ScopeCtx TestF () TestScope () ->
  Either
    (SaturationError (EGraphU owner ScopeCtx TestF () TestScope) (SupportScheduleGroup (EGraphU owner ScopeCtx TestF () TestScope)))
    (SupportSaturationReportFor (EGraphU owner ScopeCtx TestF () TestScope) (SaturatingProofEGraph owner ScopeCtx TestF () TestScope ()))
runGoldenSupportCase saturationConfig ruleBook proofGraph = do
  supportPlan <-
    prepareEGraphSupportPlan
      Nothing
      (const (staticRewriteContextSnapshot emptyRewriteRuntimeCapabilities))
      saturationConfig
      ruleBook
      mempty
      proofGraph
  crrResult
    <$> runEGraphSupportPlan
      defaultProofAnnotationBuilder
      mempty
      supportPlan
      proofGraph
