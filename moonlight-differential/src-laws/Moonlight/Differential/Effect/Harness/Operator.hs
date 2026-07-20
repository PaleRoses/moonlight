module Moonlight.Differential.Effect.Harness.Operator
  ( TestBatch,
    TestTraceUpdate,
    arrangedReachabilityStep,
    countByKeyLinear,
    emptyOperatorIndex,
    flattenOperatorIndex,
    foldDeltaJoinConsolidatesThroughBatch,
    foldedDeltaJoinBatchUpdates,
    indexedDeltaJoinIntegratesBilinearDeltas,
    indexByPartitionsReflatten,
    linearOperatorsDeltaTransparent,
    materializedFoldDeltaJoin,
    materializedFoldDeltaJoinBatchUpdates,
    naiveReachabilitySupport,
    operatorAllWeightsOne,
    operatorArrangementFromIndex,
    operatorArrangementIndexedSections,
    operatorExpectedVanishedChanges,
    operatorGroupReducer,
    operatorGroupView,
    operatorIndex,
    operatorJoinIntegrationOracle,
    operatorKeep,
    operatorMap,
    operatorOrderedStarDelta,
    operatorStarDeltaOracle,
    operatorSupportZSet,
    positiveZSetSupport,
    reachabilityArrangement,
    reachabilityStep,
    starDeltaDecompositionEqualsRecomputation,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Core (AdditiveGroup (..))
import Moonlight.Core
  ( PartialOrder,
  )
import Moonlight.Differential.Algebra.ZSet
  ( Timed (..),
  )
import Moonlight.Differential.Algebra.ZSet qualified as ZSet
import Moonlight.Differential.Arrangement
  ( Arrangement,
    appendArrangementKeyRows,
    arrangeByKey,
    emptyArrangement,
    foldArrangementKey,
  )
import Moonlight.Differential.Batch
  ( Batch,
    batchToUpdates,
    fromUpdates,
  )
import Moonlight.Differential.Operator.Aggregate
  ( GroupChange (..),
    GroupView,
    countByKey,
    mkGroupView,
  )
import Moonlight.Differential.Operator.Join
  ( arrangedKeyZSet,
    foldDeltaJoin,
    indexedDeltaJoin,
    joinIndexed,
  )
import Moonlight.Differential.Operator.Linear
  ( filterZSet,
    indexBy,
    mapZSet,
  )
import Moonlight.Differential.Trace
  ( traceFromUpdates,
  )
import Moonlight.Differential.Update
  ( Update (..),
  )

type TestBatch = Batch Int String Char Int

type TestTraceUpdate = Update Int String Char Int

linearOperatorsDeltaTransparent :: ZSet.ZSet Int Int -> ZSet.ZSet Int Int -> Bool
linearOperatorsDeltaTransparent left right =
  mapZSet operatorMap (left <> right) == mapZSet operatorMap left <> mapZSet operatorMap right
    && mapZSet operatorMap (neg left) == neg (mapZSet operatorMap left)
    && filterZSet operatorKeep (left <> right) == filterZSet operatorKeep left <> filterZSet operatorKeep right
    && filterZSet operatorKeep (neg left) == neg (filterZSet operatorKeep left)

indexByPartitionsReflatten :: ZSet.ZSet Int Int -> Bool
indexByPartitionsReflatten values =
  flattenOperatorIndex (operatorIndex values) == values

indexedDeltaJoinIntegratesBilinearDeltas ::
  ZSet.ZSet Int Int ->
  ZSet.ZSet Int Int ->
  ZSet.ZSet Int Int ->
  ZSet.ZSet Int Int ->
  Bool
indexedDeltaJoinIntegratesBilinearDeltas integratedLeftRows deltaLeftRows integratedRightRows deltaRightRows =
  indexedDeltaJoin integratedLeft deltaLeft integratedRight deltaRight
    == operatorJoinIntegrationOracle integratedLeft deltaLeft integratedRight deltaRight
  where
    integratedLeft =
      operatorIndex integratedLeftRows

    deltaLeft =
      operatorIndex deltaLeftRows

    integratedRight =
      operatorIndex integratedRightRows

    deltaRight =
      operatorIndex deltaRightRows

starDeltaDecompositionEqualsRecomputation ::
  ZSet.ZSet Int Int ->
  ZSet.ZSet Int Int ->
  ZSet.ZSet Int Int ->
  ZSet.ZSet Int Int ->
  ZSet.ZSet Int Int ->
  ZSet.ZSet Int Int ->
  Bool
starDeltaDecompositionEqualsRecomputation integratedLeftRows deltaLeftRows integratedMiddleRows deltaMiddleRows integratedRightRows deltaRightRows =
  operatorOrderedStarDelta integratedLeft deltaLeft integratedMiddle deltaMiddle integratedRight deltaRight
    == operatorStarDeltaOracle integratedLeft deltaLeft integratedMiddle deltaMiddle integratedRight deltaRight
  where
    integratedLeft =
      operatorIndex integratedLeftRows

    deltaLeft =
      operatorIndex deltaLeftRows

    integratedMiddle =
      operatorIndex integratedMiddleRows

    deltaMiddle =
      operatorIndex deltaMiddleRows

    integratedRight =
      operatorIndex integratedRightRows

    deltaRight =
      operatorIndex deltaRightRows

foldDeltaJoinConsolidatesThroughBatch :: [TestTraceUpdate] -> [TestTraceUpdate] -> Bool
foldDeltaJoinConsolidatesThroughBatch leftUpdates rightUpdates =
  materializedFoldDeltaJoinBatchUpdates leftUpdates rightUpdates == foldedDeltaJoinBatchUpdates leftUpdates rightUpdates

materializedFoldDeltaJoinBatchUpdates :: [TestTraceUpdate] -> [TestTraceUpdate] -> [Update Int String (Char, Char) Int]
materializedFoldDeltaJoinBatchUpdates leftUpdates rightUpdates =
  batchToUpdates (materializedFoldDeltaJoin pairJoinProjection leftDelta rightArrangement)
  where
    leftDelta =
      testBatch leftUpdates

    rightArrangement =
      arrangeByKey (traceFromUpdates rightUpdates)

foldedDeltaJoinBatchUpdates :: [TestTraceUpdate] -> [TestTraceUpdate] -> [Update Int String (Char, Char) Int]
foldedDeltaJoinBatchUpdates leftUpdates rightUpdates =
  batchToUpdates foldedBatch
  where
    leftDelta =
      testBatch leftUpdates

    rightArrangement =
      arrangeByKey (traceFromUpdates rightUpdates)

    foldedBatch =
      fromUpdates
        ( foldDeltaJoin
            pairJoinProjection
            collectJoinedUpdate
            []
            leftDelta
            rightArrangement
        ) ::
        Batch Int String (Char, Char) Int

countByKeyLinear :: ZSet.ZSet Int Int -> ZSet.ZSet Int Int -> Bool
countByKeyLinear leftRows rightRows =
  countByKey (left <> right) == countByKey left <> countByKey right
    && countByKey (neg left) == neg (countByKey left)
  where
    left =
      operatorIndex leftRows

    right =
      operatorIndex rightRows

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

collectJoinedUpdate ::
  [Update Int String (Char, Char) Int] ->
  Int ->
  String ->
  (Char, Char) ->
  Int ->
  [Update Int String (Char, Char) Int]
collectJoinedUpdate updates time key value weight =
  Update
    { updateTime = time,
      updateKey = key,
      updateVal = value,
      updateWeight = weight
    }
    : updates

testBatch :: [Update Int String Char Int] -> TestBatch
testBatch =
  fromUpdates

pairJoinProjection :: key -> leftVal -> rightVal -> Maybe (key, (leftVal, rightVal))
pairJoinProjection key leftVal rightVal =
  Just (key, (leftVal, rightVal))

operatorMap :: Int -> Int
operatorMap value =
  value `mod` 11

operatorKeep :: Int -> Bool
operatorKeep value =
  value `mod` 3 /= 1

operatorKey :: Int -> Int
operatorKey value =
  value `mod` 5

operatorIndex :: ZSet.ZSet Int Int -> ZSet.IndexedZSet Int Int Int
operatorIndex =
  indexBy operatorKey

emptyOperatorIndex :: ZSet.IndexedZSet Int Int Int
emptyOperatorIndex =
  ZSet.indexedZSetEmpty

flattenOperatorIndex :: ZSet.IndexedZSet Int Int Int -> ZSet.ZSet Int Int
flattenOperatorIndex =
  ZSet.indexedZSetFold (\acc _key values -> acc <> values) ZSet.zsetEmpty

insertOperatorGroup :: Int -> ZSet.ZSet Int Int -> ZSet.IndexedZSet Int Int Int -> ZSet.IndexedZSet Int Int Int
insertOperatorGroup key group indexed =
  ZSet.zsetFold (\acc value weight -> ZSet.indexedZSetInsert key value weight acc) indexed group

operatorJoinIntegrationOracle ::
  ZSet.IndexedZSet Int Int Int ->
  ZSet.IndexedZSet Int Int Int ->
  ZSet.IndexedZSet Int Int Int ->
  ZSet.IndexedZSet Int Int Int ->
  ZSet.ZSet (Int, Int, Int) Int
operatorJoinIntegrationOracle integratedLeft deltaLeft integratedRight deltaRight =
  joinIndexed (integratedLeft <> deltaLeft) (integratedRight <> deltaRight)
    <> neg (joinIndexed integratedLeft integratedRight)

operatorStarDeltaOracle ::
  ZSet.IndexedZSet Int Int Int ->
  ZSet.IndexedZSet Int Int Int ->
  ZSet.IndexedZSet Int Int Int ->
  ZSet.IndexedZSet Int Int Int ->
  ZSet.IndexedZSet Int Int Int ->
  ZSet.IndexedZSet Int Int Int ->
  ZSet.ZSet (Int, Int, Int, Int) Int
operatorStarDeltaOracle integratedLeft deltaLeft integratedMiddle deltaMiddle integratedRight deltaRight =
  starJoin3 (integratedLeft <> deltaLeft) (integratedMiddle <> deltaMiddle) (integratedRight <> deltaRight)
    <> neg (starJoin3 integratedLeft integratedMiddle integratedRight)

operatorOrderedStarDelta ::
  ZSet.IndexedZSet Int Int Int ->
  ZSet.IndexedZSet Int Int Int ->
  ZSet.IndexedZSet Int Int Int ->
  ZSet.IndexedZSet Int Int Int ->
  ZSet.IndexedZSet Int Int Int ->
  ZSet.IndexedZSet Int Int Int ->
  ZSet.ZSet (Int, Int, Int, Int) Int
operatorOrderedStarDelta integratedLeft deltaLeft integratedMiddle deltaMiddle integratedRight deltaRight =
  starJoin3 deltaLeft currentMiddle currentRight
    <> starJoin3 integratedLeft deltaMiddle currentRight
    <> starJoin3 integratedLeft integratedMiddle deltaRight
  where
    currentMiddle =
      integratedMiddle <> deltaMiddle

    currentRight =
      integratedRight <> deltaRight

starJoin3 ::
  ZSet.IndexedZSet Int Int Int ->
  ZSet.IndexedZSet Int Int Int ->
  ZSet.IndexedZSet Int Int Int ->
  ZSet.ZSet (Int, Int, Int, Int) Int
starJoin3 left middle right =
  ZSet.indexedZSetFold
    ( \acc key leftGroup ->
        case (ZSet.indexedZSetLookup key middle, ZSet.indexedZSetLookup key right) of
          (Just middleGroup, Just rightGroup) ->
            joinStarKeyGroups acc key leftGroup middleGroup rightGroup
          _ ->
            acc
    )
    ZSet.zsetEmpty
    left

joinStarKeyGroups ::
  ZSet.ZSet (Int, Int, Int, Int) Int ->
  Int ->
  ZSet.ZSet Int Int ->
  ZSet.ZSet Int Int ->
  ZSet.ZSet Int Int ->
  ZSet.ZSet (Int, Int, Int, Int) Int
joinStarKeyGroups initial key leftGroup middleGroup rightGroup =
  ZSet.zsetFold
    ( \accLeft leftValue leftWeight ->
        ZSet.zsetFold
          ( \accMiddle middleValue middleWeight ->
              ZSet.zsetFold
                ( \accRight rightValue rightWeight ->
                    ZSet.zsetInsert
                      (key, leftValue, middleValue, rightValue)
                      (leftWeight * middleWeight * rightWeight)
                      accRight
                )
                accMiddle
                rightGroup
          )
          accLeft
          middleGroup
    )
    initial
    leftGroup

operatorArrangementFromIndex :: ZSet.IndexedZSet Int Int Int -> Arrangement Int Int Int Int
operatorArrangementFromIndex =
  ZSet.indexedZSetFold
    (\arrangement key group -> appendArrangementKeyRows key (operatorTimedRows group) arrangement)
    emptyArrangement

operatorTimedRows :: ZSet.ZSet Int Int -> ZSet.ZSet (Timed Int Int) Int
operatorTimedRows =
  ZSet.zsetFold (\acc value weight -> ZSet.zsetInsert (Timed 0 value) weight acc) ZSet.zsetEmpty

operatorArrangementIndexedSections :: ZSet.IndexedZSet Int Int Int -> Arrangement Int Int Int Int -> ZSet.IndexedZSet Int Int Int
operatorArrangementIndexedSections source arrangement =
  ZSet.indexedZSetFold
    (\acc key _group -> insertOperatorGroup key (arrangedKeyZSet key arrangement) acc)
    ZSet.indexedZSetEmpty
    source

positiveZSetSupport :: ZSet.ZSet Int Int -> Set Int
positiveZSetSupport =
  ZSet.zsetFold
    ( \support value weight ->
        if weight > 0
          then Set.insert value support
          else support
    )
    Set.empty

operatorSupportZSet :: Set Int -> ZSet.ZSet Int Int
operatorSupportZSet support =
  ZSet.zsetFromList (fmap (\value -> (value, 1 :: Int)) (Set.toAscList support))

operatorAllWeightsOne :: ZSet.ZSet Int Int -> Bool
operatorAllWeightsOne =
  ZSet.zsetFold (\allOne _value weight -> allOne && weight == (1 :: Int)) True

operatorGroupReducer :: ZSet.ZSet Int Int -> ZSet.ZSet Int Int
operatorGroupReducer =
  id

operatorGroupView :: ZSet.IndexedZSet Int Int Int -> GroupView Int Int Int (ZSet.ZSet Int Int)
operatorGroupView =
  mkGroupView operatorGroupReducer

operatorExpectedVanishedChanges ::
  ZSet.IndexedZSet Int Int Int ->
  ZSet.IndexedZSet Int Int Int ->
  Map.Map Int (GroupChange (ZSet.ZSet Int Int))
operatorExpectedVanishedChanges delta advanced =
  ZSet.indexedZSetFold
    ( \vanished key _deltaGroup ->
        case ZSet.indexedZSetLookup key advanced of
          Nothing ->
            Map.insert key GroupVanished vanished
          Just _group ->
            vanished
    )
    Map.empty
    delta

reachabilityStep :: Set (Int, Int) -> ZSet.ZSet Int Int -> ZSet.ZSet Int Int
reachabilityStep edges frontier =
  operatorSupportZSet (successorNodes edges (positiveZSetSupport frontier))

reachabilityArrangement :: Set (Int, Int) -> Arrangement Int Int Int Int
reachabilityArrangement edges =
  arrangeByKey
    ( traceFromUpdates
        ( fmap
            ( \(source, target) ->
                Update
                  { updateTime = 0,
                    updateKey = source,
                    updateVal = target,
                    updateWeight = 1
                  }
            )
            (Set.toAscList edges)
        )
    )

arrangedReachabilityStep :: Arrangement Int Int Int Int -> ZSet.ZSet Int Int -> ZSet.ZSet Int Int
arrangedReachabilityStep arrangement frontier =
  ZSet.zsetFold
    ( \acc source weight ->
        if weight > 0
          then foldArrangementKey source insertReachableTarget acc arrangement
          else acc
    )
    ZSet.zsetEmpty
    frontier

insertReachableTarget :: ZSet.ZSet Int Int -> Int -> Int -> Int -> ZSet.ZSet Int Int
insertReachableTarget acc _time target weight =
  if weight > 0
    then ZSet.zsetInsert target 1 acc
    else acc

naiveReachabilitySupport :: Set (Int, Int) -> Set Int -> Set Int
naiveReachabilitySupport edges seed =
  foldr (\_round reached -> reached <> successorNodes edges reached) seed ([0 .. 7] :: [Int])

successorNodes :: Set (Int, Int) -> Set Int -> Set Int
successorNodes edges sources =
  Set.foldr
    ( \(source, target) successors ->
        if Set.member source sources
          then Set.insert target successors
          else successors
    )
    Set.empty
    edges
