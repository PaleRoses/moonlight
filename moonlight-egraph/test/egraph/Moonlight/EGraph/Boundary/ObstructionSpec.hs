module Moonlight.EGraph.Boundary.ObstructionSpec
  ( tests,
  )
where

import Moonlight.EGraph.Effect.Harness
import Moonlight.EGraph.Test.Assertions
  ( isContextBarrier,
    isPropagationBarrier,
    isRestrictionBarrier,
    isStructuralMismatch,
  )
import Moonlight.Sheaf.Obstruction (obstructionReport, whyNotMerged)
import Moonlight.EGraph.Test.Context.ThreeLevel (Scope (..))
import Moonlight.EGraph.Test.Context.ThreeLevelArith (fixtureModuleMergedContextGraph)
import Moonlight.Pale.Test.Site.Assertion (withResult)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertEqual, assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "obstruction"
    [ testCase "obstruction report is non-empty when classes only merge contextually" $
        withResult
          ( fixtureModuleMergedContextGraph $ \(sumClassId, oneClassId, contextGraph) -> do
              assertEqual "exactly one structural mismatch" 1 (length (filter isStructuralMismatch (obstructionReport sumClassId oneClassId GlobalCtx contextGraph)))
              assertEqual "exactly one context barrier" 1 (length (filter isContextBarrier (obstructionReport sumClassId oneClassId GlobalCtx contextGraph)))
              assertEqual "exactly one restriction barrier" 1 (length (filter isRestrictionBarrier (obstructionReport sumClassId oneClassId GlobalCtx contextGraph)))
              assertEqual "no propagation barrier" 0 (length (filter isPropagationBarrier (obstructionReport sumClassId oneClassId GlobalCtx contextGraph)) )
          )
          id,
      testCase "obstruction report includes restriction barrier when gluing fails across contexts" $
        withResult
          ( fixtureModuleMergedContextGraph $ \(sumClassId, oneClassId, contextGraph) ->
              assertBool "expected restriction barrier" (any isRestrictionBarrier (obstructionReport sumClassId oneClassId GlobalCtx contextGraph))
          )
          id,
      testCase "whyNotMerged includes a context barrier" $
        withResult
          ( fixtureModuleMergedContextGraph $ \(sumClassId, oneClassId, contextGraph) ->
              assertBool "expected context barrier" (any isContextBarrier (whyNotMerged sumClassId oneClassId contextGraph))
          )
          id,
      testCase "obstruction completeness harness holds" $
        withResult
          ( fixtureModuleMergedContextGraph $ \(sumClassId, oneClassId, contextGraph) ->
              obstructionComplete sumClassId oneClassId GlobalCtx contextGraph @?= True
          )
          id
    ]
