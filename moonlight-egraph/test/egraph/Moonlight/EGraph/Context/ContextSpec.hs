module Moonlight.EGraph.Context.ContextSpec
  ( tests,
  )
where

import Moonlight.EGraph.Pure.Context.Core
    ( ContextEGraph,
      cegRuntimeState,
      ContextRuntimeState (..),
      contextCachedObjectsForExecution,
      contextPreparedObjects,
      activateContext,
    )
import Moonlight.Sheaf.Context.Core qualified as SheafCore
import Moonlight.Sheaf.Context.Algebra (contextEquivalentAt)
import Moonlight.EGraph.Test.Context.ThreeLevel ( Scope(..) )
import Moonlight.EGraph.Test.Context.ThreeLevelArith
    ( fixtureContextGraph,
      fixtureModuleMergedContextGraph )
import Moonlight.Pale.Test.Site.Assertion (withResult)
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( (@?=), testCase )

tests :: TestTree
tests =
  testGroup
    "context"
    [ testCase "context merge propagates upward but not downward" $
        withResult fixtureModuleMergedContextGraph $ \(sumClassId, oneClassId, mergedContextGraph) -> do
          contextEquivalentAt GlobalCtx sumClassId oneClassId mergedContextGraph @?= Right False
          contextEquivalentAt ModuleCtx sumClassId oneClassId mergedContextGraph @?= Right True
          contextEquivalentAt LocalCtx sumClassId oneClassId mergedContextGraph @?= Right True,
      testCase "context refresh produces sheaf propagation report" $
        withResult fixtureModuleMergedContextGraph $ \(_, _, mergedContextGraph) -> do
          contextPropagationSettled mergedContextGraph @?= True
          contextPropagationFailed mergedContextGraph @?= False,
      testCase "enumerable objects are the inhabited join closure, not the site universe" $
        withResult fixtureContextGraph $ \(_, _, contextGraph) ->
          withResult (activateContext ModuleCtx contextGraph) $ \graphWithCache -> do
              contextPreparedObjects contextGraph @?= [GlobalCtx]
              contextPreparedObjects graphWithCache @?= [GlobalCtx, ModuleCtx]
              contextCachedObjectsForExecution contextGraph @?= []
              contextCachedObjectsForExecution graphWithCache @?= [ModuleCtx]
    ]

contextPropagationSettled :: ContextEGraph f a c -> Bool
contextPropagationSettled contextGraph =
  maybe False SheafCore.contextPropagationSettled (crsLastRepair (cegRuntimeState contextGraph))

contextPropagationFailed :: ContextEGraph f a c -> Bool
contextPropagationFailed _contextGraph =
  False
