module Moonlight.EGraph.Test.Config
  ( toBudget,
    testConfig,
    testConfigWith,
    tracingTestConfig,
  )
where

import Moonlight.Pale.Test.Site.Core (TestBudget (..))
import Moonlight.EGraph.Test.Saturation qualified as TestSaturation
import Moonlight.EGraph.Pure.Saturation.Matching (MatchingStrategy (..))
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.Core (HasConstructorTag (..), ConstructorTag, RewriteRuleId)
import Moonlight.Saturation.Context.Program.Spec
  ( PlanSpec,
    deterministicSchedulerConfig,
    planSpec,
    traceAllSchedulerConfig,
    withSchedulerConfig,
  )
import Moonlight.Saturation.Core (SaturationBudget (..))
import Moonlight.Saturation.Substrate (SatGraph)
import Moonlight.Rewrite.Runtime (emptyRewriteRuntimeCapabilities)

toBudget :: TestBudget -> SaturationBudget
toBudget testBudget =
  SaturationBudget
    { sbMaxIterations = testBudgetMaxIterations testBudget,
      sbMaxNodes = testBudgetMaxNodes testBudget
    }

testConfig :: (HasConstructorTag f, Show (ConstructorTag f), Show (f ()), Ord capability, Show capability, Ord c) => TestBudget -> PlanSpec (EGraphU capability f a c) (SatGraph (EGraphU capability f a c)) RewriteRuleId
testConfig =
  TestSaturation.genericJoinSaturationConfig . toBudget

testConfigWith :: TestBudget -> MatchingStrategy c capability f a -> PlanSpec (EGraphU capability f a c) (SatGraph (EGraphU capability f a c)) RewriteRuleId
testConfigWith budget strategy =
  withSchedulerConfig
    deterministicSchedulerConfig
    (planSpec (toBudget budget) strategy emptyRewriteRuntimeCapabilities)

tracingTestConfig :: (HasConstructorTag f, Show (ConstructorTag f), Show (f ()), Ord capability, Show capability, Ord c) => TestBudget -> PlanSpec (EGraphU capability f a c) (SatGraph (EGraphU capability f a c)) RewriteRuleId
tracingTestConfig budget =
  withSchedulerConfig
    (traceAllSchedulerConfig deterministicSchedulerConfig)
    (planSpec (toBudget budget) GenericJoinMatching emptyRewriteRuntimeCapabilities)
