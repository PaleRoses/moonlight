module OperatorSpec
  ( tests,
  )
where

import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Core
  ( PartialOrder,
  )
import Moonlight.Differential.Algebra.ZSet qualified as ZSet
import Moonlight.Differential.Arrangement
  ( Arrangement,
    appendArrangementBatch,
    arrangeByKey,
  )
import Moonlight.Differential.Batch
  ( Batch,
    batchToUpdates,
    fromUpdates,
    mergeBatch,
  )
import Moonlight.Differential.Operator.Fixpoint
  ( SemiNaiveBudget (..),
    SemiNaiveDivergence (..),
    semiNaiveFixpoint,
  )
import Moonlight.Differential.Operator.Join
  ( foldDeltaJoin,
  )
import Moonlight.Differential.Operator.Linear
  ( mapZSet,
  )
import Moonlight.Differential.Trace
  ( traceFromBatch,
    traceFromUpdates,
  )
import Moonlight.Differential.Update
  ( Update (..),
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "operator laws"
    [ testCase "Delta join multiplies delta rows by the arranged prefix only" deltaJoinUsesArrangedPrefix,
      testCase "Delta join is additive over arranged sections" deltaJoinDistributesOverArrangementAppend,
      testCase "semiNaiveFixpoint reports budget exhaustion on a growing orbit" semiNaiveGrowingOrbitDiverges
    ]

type TestBatch = Batch Int String Char Int

type TestTraceUpdate = Update Int String Char Int

materializedFoldDeltaJoin ::
  (PartialOrder time, Ord time, Ord key, Ord outKey, Ord outVal) =>
  (key -> leftVal -> rightVal -> Maybe (outKey, outVal)) ->
  Batch time key leftVal Int ->
  Arrangement time key rightVal Int ->
  Batch time outKey outVal Int
materializedFoldDeltaJoin project delta arrangement =
  fromUpdates (foldDeltaJoin project collectMaterializedUpdate [] delta arrangement)

collectMaterializedUpdate ::
  [Update time outKey outVal weight] ->
  time ->
  outKey ->
  outVal ->
  weight ->
  [Update time outKey outVal weight]
collectMaterializedUpdate updates time outKey outVal weight =
  Update
    { updateTime = time,
      updateKey = outKey,
      updateVal = outVal,
      updateWeight = weight
    }
    : updates

testBatch :: [Update Int String Char Int] -> TestBatch
testBatch =
  fromUpdates

deltaJoinUsesArrangedPrefix :: IO ()
deltaJoinUsesArrangedPrefix = do
  let rightArrangement =
        arrangeByKey
          ( traceFromUpdates
              ( [ Update 0 "join" 'r' 2,
                  Update 1 "join" 'r' 5,
                  Update 2 "join" 'r' 11,
                  Update 0 "join" 'x' 7,
                  Update 0 "skip" 'z' 99
                ] ::
                  [TestTraceUpdate]
              )
          )
      leftDelta =
        testBatch
          [ Update 1 "join" 'l' 3,
            Update 1 "missing" 'm' 13
          ]
      joined =
        materializedFoldDeltaJoin pairJoinProjection leftDelta rightArrangement
  assertEqual
    "delta join reads the same-key prefix, multiplies weights, and excludes future arrangement cells"
    [ Update 1 "join" ('l', 'r') 21,
      Update 1 "join" ('l', 'x') 21
    ]
    (batchToUpdates joined)

deltaJoinDistributesOverArrangementAppend :: IO ()
deltaJoinDistributesOverArrangementAppend = do
  let leftDelta =
        testBatch [Update 3 "join" 'l' 4]
      baseRightBatch =
        testBatch
          [ Update 0 "join" 'a' 2,
            Update 4 "join" 'f' 100
          ]
      appendedRightBatch =
        testBatch
          [ Update 1 "join" 'b' 5,
            Update 2 "other" 'c' 7
          ]
      baseRight =
        arrangeByKey (traceFromBatch baseRightBatch)
      appendedRight =
        arrangeByKey (traceFromBatch appendedRightBatch)
      combinedRight =
        appendArrangementBatch appendedRightBatch baseRight
  assertEqual
    "joining against an appended arrangement section equals the sum of joining against each section"
    (batchToUpdates (materializedFoldDeltaJoin pairJoinProjection leftDelta combinedRight))
    ( batchToUpdates
        ( mergeBatch
            (materializedFoldDeltaJoin pairJoinProjection leftDelta baseRight)
            (materializedFoldDeltaJoin pairJoinProjection leftDelta appendedRight)
        )
    )

pairJoinProjection :: key -> leftVal -> rightVal -> Maybe (key, (leftVal, rightVal))
pairJoinProjection key leftVal rightVal =
  Just (key, (leftVal, rightVal))

semiNaiveGrowingOrbitDiverges :: IO ()
semiNaiveGrowingOrbitDiverges =
  case semiNaiveFixpoint (SemiNaiveBudget 3) (mapZSet (+ 1)) (ZSet.zsetSingleton (0 :: Int) (1 :: Int)) of
    Left divergence ->
      assertEqual
        "growing orbit should preserve the exhausted frontier as a typed obstruction"
        SemiNaiveDivergence
          { sndRoundsSpent = 3,
            sndResidualDelta = operatorSupportZSet (Set.singleton 3),
            sndAccumulated = operatorSupportZSet (Set.fromList [0, 1, 2, 3])
          }
        divergence
    Right result ->
      assertFailure ("expected budget exhaustion, got " <> show result)

operatorSupportZSet :: Set Int -> ZSet.ZSet Int Int
operatorSupportZSet support =
  ZSet.zsetFromList (fmap (\value -> (value, 1 :: Int)) (Set.toAscList support))
