module Moonlight.Flow.Execution.Dense.WCOJ.DeltaAgreementSpec
  ( tests,
  )
where

import Control.Monad.ST
  ( ST,
  )
import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Primitive.PrimArray qualified as PrimArray
import Data.Primitive.SmallArray qualified as SmallArray
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Differential.Index.RowSet
  ( RowSet,
    rowSetToIntSet,
  )
import Moonlight.Differential.Index.RowIdSet
  ( RowIdSet,
  )
import Moonlight.Differential.Join.WCOJ.Delta qualified as Differential
import Moonlight.Differential.Join.WCOJ.Dense.Executor qualified as DenseExecutor
import Moonlight.Differential.Row.Block
  ( RowLayout,
  )
import Moonlight.Differential.Row.Tuple
  ( AssignmentTupleKey,
    RepKey (..),
    RowTupleKey,
    tupleKeyFromRepKeys,
  )
import Moonlight.Flow.Execution.Dense.Plan
  ( DenseArrangement,
    DenseArrangementId (..),
    DenseJoinPlan,
    denseArrangementDeltaJoinSource,
    denseArrangementWithDirtyKeys,
    denseAtomSourceFromRows,
    denseJoinPlanFullSchema,
    denseJoinPlanProblem,
    denseJoinPlanSources,
    mkDenseJoinPlan,
    mkDenseJoinPlanWithSupportSources,
    selectedOutputDomainFromKeys,
  )
import Moonlight.Flow.Plan.Query.Core
  ( SlotId,
    mkAtomId,
    mkSlotId,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )

type NormalizedLeaf = ([(Int, Int)], [(Int, [Int])])

tests :: TestTree
tests =
  testGroup
    "dense delta WCOJ agreement"
    [ testCase "triangle join with one dirty source agrees" (assertFixtureAgreement triangleSingleDirtyPlan),
      testCase "dirty rows in multiple sources agree" (assertFixtureAgreement multipleDirtySourcesPlan),
      testCase "zero dirty rows emits zero leaves" (assertFixtureAgreement zeroDirtyPlan),
      testCase "selected output domain restricts full leaves identically" (assertFixtureAgreement selectedOutputPlan),
      testCase "full-only shared-slot values absent from dirty rows are rejected identically" (assertFixtureAgreement fullOnlySharedSlotValuePlan)
    ]

assertFixtureAgreement :: Either String DenseJoinPlan -> Assertion
assertFixtureAgreement fixture =
  case fixture of
    Left failure ->
      assertFailure failure
    Right plan ->
      case flowDeltaLeaves plan of
        Left failure ->
          assertFailure failure
        Right flowLeaves ->
          assertEqual
            "normalized delta leaves"
            (normalizedDifferentialLeaves plan)
            (List.sort flowLeaves)

flowDeltaLeaves :: DenseJoinPlan -> Either String [NormalizedLeaf]
flowDeltaLeaves plan =
  fmap List.sort $
    DenseExecutor.foldDenseDeltaWCOJ
      (denseJoinPlanProblem plan)
      (collectFlowLeaf plan)
      (Right [])

collectFlowLeaf :: DenseJoinPlan -> DenseExecutor.DeltaDenseFrame s -> Either String [NormalizedLeaf] -> ST s (Either String [NormalizedLeaf])
collectFlowLeaf plan frame accumulated =
  case accumulated of
    Left failure ->
      pure (Left failure)
    Right leaves -> do
      normalized <- normalizeFlowLeaf plan frame
      pure (fmap (: leaves) normalized)

normalizeFlowLeaf :: DenseJoinPlan -> DenseExecutor.DeltaDenseFrame s -> ST s (Either String NormalizedLeaf)
normalizeFlowLeaf plan frame = do
  envEntries <- traverse (readFlowEnvEntry frame) (planSlots plan)
  supportEntries <- traverse (readFlowSupportEntry frame) (sourceIndexes plan)
  pure ((,) <$> sequence envEntries <*> pure supportEntries)

readFlowEnvEntry :: DenseExecutor.DeltaDenseFrame s -> Int -> ST s (Either String (Int, Int))
readFlowEnvEntry frame slotKey = do
  maybeRep <- DenseExecutor.readDeltaEnv frame slotKey
  pure $ case maybeRep of
    Nothing ->
      Left ("flow delta leaf missing bound slot " <> show slotKey)
    Just (RepKey repKey) ->
      Right (slotKey, repKey)

readFlowSupportEntry :: DenseExecutor.DeltaDenseFrame s -> Int -> ST s (Int, [Int])
readFlowSupportEntry frame sourceId = do
  rows <- DenseExecutor.readDeltaFullFeasible frame sourceId
  pure (sourceId, rowSetInts rows)

normalizedDifferentialLeaves :: DenseJoinPlan -> [NormalizedLeaf]
normalizedDifferentialLeaves =
  List.sort . fmap normalizeDifferentialLeaf . Differential.deltaWCOJLeaves . deltaProblemFromDensePlan

normalizeDifferentialLeaf :: (IntMap Int, IntMap RowSet) -> NormalizedLeaf
normalizeDifferentialLeaf (env, supports) =
  (IntMap.toAscList env, fmap (fmap rowSetInts) (IntMap.toAscList supports))

deltaProblemFromDensePlan :: DenseJoinPlan -> Differential.DeltaJoinProblem
deltaProblemFromDensePlan plan =
  Differential.mkDeltaJoinProblem
    (planSlots plan)
    (fmap deltaSourceFromDenseArrangement (densePlanSources plan))
    ( maybe
        []
        ((: []) . deltaConstraintFromSelectedOutput)
        (DenseExecutor.denseDeltaProblemSelectedOutputData (denseJoinPlanProblem plan))
    )

deltaSourceFromDenseArrangement :: DenseArrangement -> Differential.DeltaJoinSource
deltaSourceFromDenseArrangement =
  denseArrangementDeltaJoinSource

deltaConstraintFromSelectedOutput :: (RowSet, IntMap (IntMap RowIdSet)) -> Differential.DeltaJoinConstraint
deltaConstraintFromSelectedOutput (rows, rowsBySlotValue) =
  Differential.DeltaJoinConstraint
    { Differential.deltaConstraintRows = rows,
      Differential.deltaConstraintValueIndex = rowsBySlotValue
    }

planSlots :: DenseJoinPlan -> [Int]
planSlots =
  PrimArray.primArrayToList . denseJoinPlanFullSchema

densePlanSources :: DenseJoinPlan -> [DenseArrangement]
densePlanSources plan =
  fmap (SmallArray.indexSmallArray (denseJoinPlanSources plan)) (sourceIndexes plan)

sourceIndexes :: DenseJoinPlan -> [Int]
sourceIndexes plan =
  [0 .. SmallArray.sizeofSmallArray (denseJoinPlanSources plan) - 1]

rowSetInts :: RowSet -> [Int]
rowSetInts =
  IntSet.toAscList . rowSetToIntSet

triangleSingleDirtyPlan :: Either String DenseJoinPlan
triangleSingleDirtyPlan =
  trianglePlan
    [ (0, [[1, 10]])
    ]

multipleDirtySourcesPlan :: Either String DenseJoinPlan
multipleDirtySourcesPlan =
  trianglePlan
    [ (0, [[1, 11]]),
      (2, [[2, 200]])
    ]

zeroDirtyPlan :: Either String DenseJoinPlan
zeroDirtyPlan =
  trianglePlan []

selectedOutputPlan :: Either String DenseJoinPlan
selectedOutputPlan = do
  sources <- triangleSources [(0, [[1, 10], [1, 11]])]
  selected <-
    maybe
      (Left "selected output domain rejected nonempty fixture")
      Right
      (selectedOutputDomainFromKeys (slots [0, 2]) (Set.singleton (assignment [1, 100])))
  first show
    ( mkDenseJoinPlanWithSupportSources
        (slots [0, 1, 2])
        (slots [0, 2])
        (IntSet.fromDistinctAscList [0 .. length sources - 1])
        (Just selected)
        sources
    )

fullOnlySharedSlotValuePlan :: Either String DenseJoinPlan
fullOnlySharedSlotValuePlan = do
  sources <-
    traverse
      buildSource
      [ SourceRows 0 [0, 1] [[1, 10], [1, 11]] [[1, 10]],
        SourceRows 1 [1, 2] [[10, 100], [11, 100]] [],
        SourceRows 2 [0, 2] [[1, 100]] []
      ]
  first show (mkDenseJoinPlan (slots [0, 1, 2]) (slots [0, 1, 2]) sources)

trianglePlan :: [(Int, [[Int]])] -> Either String DenseJoinPlan
trianglePlan dirtyRows = do
  sources <- triangleSources dirtyRows
  first show (mkDenseJoinPlan (slots [0, 1, 2]) (slots [0, 1, 2]) sources)

triangleSources :: [(Int, [[Int]])] -> Either String [DenseArrangement]
triangleSources dirtyRows =
  traverse
    buildSource
    [ SourceRows 0 [0, 1] [[1, 10], [1, 11], [2, 20], [3, 30]] (dirtyRowsFor 0),
      SourceRows 1 [1, 2] [[10, 100], [11, 100], [20, 200], [40, 400]] (dirtyRowsFor 1),
      SourceRows 2 [0, 2] [[1, 100], [2, 200], [3, 300]] (dirtyRowsFor 2)
    ]
  where
    dirtyRowsFor sourceId =
      maybe [] id (lookup sourceId dirtyRows)

data SourceRows = SourceRows
  { srSourceId :: !Int,
    srSchema :: ![Int],
    srRows :: ![[Int]],
    srDirtyRows :: ![[Int]]
  }

buildSource :: SourceRows -> Either String DenseArrangement
buildSource SourceRows {srSourceId, srSchema, srRows, srDirtyRows} =
  fmap markDirty $
    first show $
      denseAtomSourceFromRows
        (DenseArrangementId srSourceId)
        (mkAtomId srSourceId)
        (rowLayout srSchema)
        (fmap row srRows)
  where
    markDirty =
      denseArrangementWithDirtyKeys (Set.fromList (fmap assignment srDirtyRows))

slots :: [Int] -> [SlotId]
slots =
  fmap mkSlotId

rowLayout :: [Int] -> RowLayout
rowLayout =
  Vector.fromList . slots

row :: [Int] -> RowTupleKey
row =
  tupleKeyFromRepKeys . fmap RepKey

assignment :: [Int] -> AssignmentTupleKey
assignment =
  tupleKeyFromRepKeys . fmap RepKey
