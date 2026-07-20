module Moonlight.EGraph.Core.QuotientPatchSpec
  ( tests,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Bifunctor
  ( first,
  )
import Moonlight.Core
  ( mkQuotientEpoch,
  )
import Moonlight.EGraph.Pure.Rebuild
  ( EGraphRebuildDelta (..),
    merge,
    rebuildWithDelta,
  )
import Moonlight.EGraph.Pure.Relational
  ( EGraphPreparedBase,
    atomizeCompiledPatternQuery,
    buildPreparedBase,
    patchPreparedBaseWith,
    preparedBaseRowBlocks,
    quotientPatchFromRowDeltas,
  )
import Moonlight.EGraph.Test.Ring.Core
  ( RingF (..),
    ringAdd,
  )
import Moonlight.EGraph.Test.Saturation.Helpers
  ( addXYPattern,
    buildGraph,
    compileRingPatternQuery,
  )
import Data.Fix
  ( Fix (..),
  )
import Moonlight.Flow.Model.Delta
  ( QuotientPatch (..),
    atomPatchRows
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Differential.Row.Patch
  ( composePlainRowPatch,
  )
import Moonlight.Differential.Row.Delta
  ( rowBlockToRowDelta,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@?=),
  )

tests :: TestTree
tests =
  testGroup
    "egraph quotient patch seam"
    [ testCase "real rebuild repair emits quotient patch matching rebuilt atom rows" quotientPatchAssertion
    ]

quotientPatchAssertion :: Assertion
quotientPatchAssertion = do
  case buildGraph egraphTerms of
    Right (graph0, c1 : c2 : _remainingClasses) -> do
      compiledQuery <- expectRight (compileRingPatternQuery addXYPattern)
      queryPlan <- expectRight (atomizeCompiledPatternQuery compiledQuery)
      let initialPreparedBase =
            buildPreparedBase queryPlan graph0
      let (rebuildDelta, rebuiltGraph) = rebuildWithDelta (merge c1 c2 graph0)
          (_patchedPreparedBase, atomInputDeltas) =
            patchPreparedBaseWith rebuiltGraph (erdDirtyResultKeys rebuildDelta) initialPreparedBase
      let repairPatch =
            quotientPatchFromRowDeltas
              (mkQuotientEpoch 1)
              (mkQuotientEpoch 2)
              (erdDirtyResultKeys rebuildDelta)
              (erdTopologyClassKeys rebuildDelta)
              atomInputDeltas
      initialRows <-
        expectRight $
          preparedBaseRowDeltas initialPreparedBase
      let rebuiltPreparedBase =
            buildPreparedBase queryPlan rebuiltGraph
      expectedRows <-
        expectRight $
          preparedBaseRowDeltas rebuiltPreparedBase
      assertBool "expected real rebuild to emit atom deltas" (not (IntMap.null (qpEvents repairPatch)))
      foldPatchRows initialRows repairPatch @?= expectedRows
    Right _ ->
      assertFailure "expected at least two egraph classes"
    Left allocationError ->
      assertFailure ("egraph fixture allocation failed: " <> show allocationError)

egraphTerms :: [Fix RingF]
egraphTerms =
  [ ringNum 1,
    ringNum 2,
    ringNum 3,
    ringAdd (ringNum 1) (ringNum 3),
    ringAdd (ringNum 2) (ringNum 3)
  ]

ringNum :: Int -> Fix RingF
ringNum value =
  Fix (Num value)

foldPatchRows ::
  IntMap RowDelta ->
  QuotientPatch ->
  IntMap RowDelta
foldPatchRows initialRows patch =
  IntMap.unionWith composePlainRowPatch initialRows (fmap atomPatchRows (qpEvents patch))


expectRight :: Show error => Either error value -> IO value
expectRight eitherValue =
  case eitherValue of
    Left errorValue ->
      assertFailure (show errorValue) *> fail "expected Right"
    Right value ->
      pure value

preparedBaseRowDeltas ::
  EGraphPreparedBase capability f ->
  Either String (IntMap RowDelta)
preparedBaseRowDeltas preparedBase =
  first show (preparedBaseRowBlocks 0 preparedBase)
    >>= first show . traverse rowBlockToRowDelta
