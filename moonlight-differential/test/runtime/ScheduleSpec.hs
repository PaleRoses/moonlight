{-# LANGUAGE DerivingStrategies #-}

module ScheduleSpec
  ( tests,
  )
where

import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Differential.Frontier
  ( RuntimeCapability,
    RuntimeFrontierError (..),
    emptyRuntimeFrontier,
    frontierPendingCount,
    mintRootRuntimeCapability,
    runtimeCapabilityTime,
  )
import Moonlight.Differential.Runtime.Schedule
  ( ProgressSchedule,
    ScheduleCell (..),
    ScheduledWork (..),
    mkProgressSchedule,
    scheduleCellsEmpty,
    scheduleComplete,
    scheduleDequeue,
    scheduleEnqueue,
    scheduleFrontier,
    schedulePendingPointstamps,
    scheduleQuiescent,
    scheduleWork,
  )
import Moonlight.Differential.Time
  ( RuntimeTime,
    emptyRuntimeScope,
    frontierStamp,
    runtimeTime,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    choose,
    conjoin,
    counterexample,
    elements,
    forAll,
    forAllShrink,
    frequency,
    listOf1,
    property,
    shrinkList,
    shuffle,
    testProperty,
    vectorOf,
    (.&&.),
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "progress schedule"
    [ testProperty "advance observables agree with an independent model" prop_scheduleAgreesWithModel,
      mergeSinglePending,
      sharedTimeAcrossPriorities,
      dequeuePreservesPending,
      customOrderDrain,
      lifecycleToQuiescence,
      completeMissingPending,
      doubleCompleteFails
    ]

type Ctx = Int

type Epoch = Int

type Phase = Int

type Priority = Int

type Payload = [Int]

type Time = RuntimeTime Ctx Epoch Phase

type Capability = RuntimeCapability Ctx Epoch Phase

type Work = ScheduledWork Ctx Epoch Phase Priority Payload

type Sched = ProgressSchedule Ctx Epoch Phase Priority Payload

mkTime :: Int -> Time
mkTime tick =
  runtimeTime 0 emptyRuntimeScope 0 0 (frontierStamp (fromIntegral tick))

mkCap :: Int -> Capability
mkCap =
  mintRootRuntimeCapability . mkTime

emptySchedule :: [Priority] -> Sched
emptySchedule priorities =
  mkProgressSchedule priorities emptyRuntimeFrontier

workPayload :: Work -> Payload
workPayload =
  scheduleCellPayload . scheduledWorkCell

-- The independent oracle: a FLAT agenda keyed by (priority, time) beside a
-- separately maintained pending refcount, so nothing about the nested carrier's
-- structure is assumed.  Every schedule observable is recomputed from this
-- parallel state and checked to agree after each operation.

data Model = Model
  { modelCells :: Map (Priority, Time) Payload,
    modelPending :: Map Time Int,
    modelOrder :: [Priority]
  }

emptyModel :: [Priority] -> Model
emptyModel order =
  Model Map.empty Map.empty order

modelEnqueue :: Priority -> Capability -> Payload -> Model -> Model
modelEnqueue priority capability payload model =
  let time = runtimeCapabilityTime capability
      hadCell = Map.member (priority, time) (modelCells model)
   in model
        { modelCells =
            Map.insertWith (\new old -> old <> new) (priority, time) payload (modelCells model),
          modelPending =
            if hadCell
              then modelPending model
              else Map.insertWith (+) time 1 (modelPending model)
        }

modelDequeue :: Model -> Maybe (Work, Model)
modelDequeue model =
  firstPriorityWithCell (modelOrder model)
  where
    firstPriorityWithCell [] =
      Nothing
    firstPriorityWithCell (priority : rest) =
      case cellsOfPriority priority of
        [] ->
          firstPriorityWithCell rest
        ((time, payload) : _) ->
          Just
            ( ScheduledWork priority time (ScheduleCell (mintRootRuntimeCapability time) payload),
              model {modelCells = Map.delete (priority, time) (modelCells model)}
            )
    cellsOfPriority priority =
      [ (time, payload)
        | ((priority', time), payload) <- Map.toAscList (modelCells model),
          priority' == priority
      ]

modelComplete :: Capability -> Model -> Either (RuntimeFrontierError Ctx Epoch Phase) Model
modelComplete capability model =
  let time = runtimeCapabilityTime capability
   in case Map.lookup time (modelPending model) of
        Just count
          | count >= 1 ->
              Right (model {modelPending = Map.update decrement time (modelPending model)})
        _ ->
          Left (RuntimeFrontierMissingPendingComplete time)
  where
    decrement :: Int -> Maybe Int
    decrement count
      | count <= 1 = Nothing
      | otherwise = Just (count - 1)

modelWork :: Model -> [Work]
modelWork model =
  concatMap workAtPriority (modelOrder model)
  where
    workAtPriority priority =
      [ ScheduledWork priority time (ScheduleCell (mintRootRuntimeCapability time) payload)
        | ((priority', time), payload) <- Map.toAscList (modelCells model),
          priority' == priority
      ]

modelPendingPointstamps :: Model -> Set Time
modelPendingPointstamps =
  Map.keysSet . Map.filter (> 0) . modelPending

modelCellsEmpty :: Model -> Bool
modelCellsEmpty =
  Map.null . modelCells

modelQuiescent :: Model -> Bool
modelQuiescent model =
  modelCellsEmpty model && Set.null (modelPendingPointstamps model)

observablesMatch :: Sched -> Model -> Property
observablesMatch sched model =
  conjoin
    [ counterexample "scheduleWork" (scheduleWork sched === modelWork model),
      counterexample "pendingPointstamps" (schedulePendingPointstamps sched === modelPendingPointstamps model),
      counterexample "cellsEmpty" (scheduleCellsEmpty sched === modelCellsEmpty model),
      counterexample "quiescent" (scheduleQuiescent sched === modelQuiescent model)
    ]

data Op
  = OpEnqueue Priority Int Payload
  | OpDequeue
  | OpComplete Int
  deriving stock (Show)

stepOp :: Sched -> Model -> Op -> (Property, Sched, Model)
stepOp sched model op =
  case op of
    OpEnqueue priority tick payload ->
      let capability = mkCap tick
          sched' = scheduleEnqueue priority capability payload sched
          model' = modelEnqueue priority capability payload model
       in (observablesMatch sched' model', sched', model')
    OpDequeue ->
      case (scheduleDequeue sched, modelDequeue model) of
        (Nothing, Nothing) ->
          (observablesMatch sched model, sched, model)
        (Just (realWork, sched'), Just (expectedWork, model')) ->
          ( counterexample "dequeued work" (realWork === expectedWork) .&&. observablesMatch sched' model',
            sched',
            model'
          )
        (realResult, modelResult) ->
          ( counterexample
              ( "dequeue shape mismatch: real="
                  ++ show (fmap (scheduledWorkPriority . fst) realResult)
                  ++ " model="
                  ++ show (fmap (scheduledWorkPriority . fst) modelResult)
              )
              (property False),
            sched,
            model
          )
    OpComplete tick ->
      let capability = mkCap tick
       in case (scheduleComplete capability sched, modelComplete capability model) of
            (Right sched', Right model') ->
              (observablesMatch sched' model', sched', model')
            (Left realError, Left modelError) ->
              (counterexample "completion error" (realError === modelError), sched, model)
            (realResult, modelResult) ->
              ( counterexample
                  ( "completion shape mismatch: real="
                      ++ show (fmap (const ()) realResult)
                      ++ " model="
                      ++ show (fmap (const ()) modelResult)
                  )
                  (property False),
                sched,
                model
              )

runOps :: [Priority] -> [Op] -> Property
runOps order ops0 =
  conjoin (go (emptySchedule order) (emptyModel order) ops0)
  where
    go _ _ [] =
      []
    go sched model (op : rest) =
      let (prop, sched', model') = stepOp sched model op
       in prop : go sched' model' rest

genOrder :: Gen [Priority]
genOrder =
  shuffle [0, 1, 2, 3]

genOp :: [Priority] -> Gen Op
genOp order =
  frequency
    [ (3, OpEnqueue <$> elements order <*> genTick <*> genPayload),
      (2, pure OpDequeue),
      (2, OpComplete <$> genTick)
    ]
  where
    genTick = choose (0, 5)
    genPayload = listOf1 (choose (1, 9))

genOps :: [Priority] -> Gen [Op]
genOps order = do
  count <- choose (0, 40)
  vectorOf count (genOp order)

prop_scheduleAgreesWithModel :: Property
prop_scheduleAgreesWithModel =
  forAll genOrder $ \order ->
    forAllShrink (genOps order) (shrinkList (const [])) $ \ops ->
      runOps order ops

mergeSinglePending :: TestTree
mergeSinglePending =
  testCase "same-cell re-enqueue merges payload and mints exactly one pending" $ do
    let capability = mkCap 3
        sched =
          scheduleEnqueue 0 capability [2] $
            scheduleEnqueue 0 capability [1] $
              emptySchedule [0]
    case scheduleWork sched of
      [single] ->
        assertEqual "payload accumulates old-then-new" [1, 2] (workPayload single)
      other ->
        assertFailure ("expected exactly one work item, got " ++ show (length other))
    assertEqual "one pending pointstamp" (Set.singleton (mkTime 3)) (schedulePendingPointstamps sched)
    assertEqual "pending refcount is one, not two" 1 (frontierPendingCount (mkTime 3) (scheduleFrontier sched))

sharedTimeAcrossPriorities :: TestTree
sharedTimeAcrossPriorities =
  testCase "distinct priorities sharing a time stack the pending refcount" $ do
    let capability = mkCap 8
        sched =
          scheduleEnqueue 1 capability [20] $
            scheduleEnqueue 0 capability [10] $
              emptySchedule [0, 1]
    assertEqual "two work items" 2 (length (scheduleWork sched))
    assertEqual "one pending pointstamp" (Set.singleton (mkTime 8)) (schedulePendingPointstamps sched)
    assertEqual "refcount two" 2 (frontierPendingCount (mkTime 8) (scheduleFrontier sched))
    case scheduleComplete capability sched of
      Left err ->
        assertFailure ("first completion should succeed: " ++ show err)
      Right sched' -> do
        assertEqual "still pending after one completion" (Set.singleton (mkTime 8)) (schedulePendingPointstamps sched')
        assertEqual "refcount decremented to one" 1 (frontierPendingCount (mkTime 8) (scheduleFrontier sched'))

dequeuePreservesPending :: TestTree
dequeuePreservesPending =
  testCase "dequeue consumes the cell but leaves the capability pending" $ do
    let capability = mkCap 4
        sched = scheduleEnqueue 0 capability [7] (emptySchedule [0])
    case scheduleDequeue sched of
      Nothing ->
        assertFailure "expected a dequeued work item"
      Just (work, sched') -> do
        assertEqual "dequeued payload" [7] (workPayload work)
        assertBool "cells drained" (scheduleCellsEmpty sched')
        assertEqual "pending still outstanding" (Set.singleton (mkTime 4)) (schedulePendingPointstamps sched')
        assertBool "not quiescent while capability outstanding" (not (scheduleQuiescent sched'))

customOrderDrain :: TestTree
customOrderDrain =
  testCase "dequeue drains in the supplied priority order, not Ord order" $ do
    let order = [2, 0, 3, 1]
        sched =
          foldr
            (\priority acc -> scheduleEnqueue priority (mkCap priority) [priority] acc)
            (emptySchedule order)
            order
    assertEqual "drain order equals the construction knob" order (drainPriorities sched)
  where
    drainPriorities :: Sched -> [Priority]
    drainPriorities sched =
      case scheduleDequeue sched of
        Nothing ->
          []
        Just (work, sched') ->
          scheduledWorkPriority work : drainPriorities sched'

lifecycleToQuiescence :: TestTree
lifecycleToQuiescence =
  testCase "quiescence requires both cell drain and pending completion" $ do
    let ticks = [10, 11, 12]
        sched =
          foldr
            (\tick acc -> scheduleEnqueue 0 (mkCap tick) [tick] acc)
            (emptySchedule [0, 1])
            ticks
    assertBool "enqueued: not quiescent" (not (scheduleQuiescent sched))
    let drained = drainAll sched
    assertBool "drained: cells empty" (scheduleCellsEmpty drained)
    assertBool "drained but pending: not quiescent" (not (scheduleQuiescent drained))
    case completeAll (fmap mkCap ticks) drained of
      Left err ->
        assertFailure ("completion should succeed: " ++ show err)
      Right settled ->
        assertBool "completed: quiescent" (scheduleQuiescent settled)
  where
    drainAll :: Sched -> Sched
    drainAll sched =
      maybe sched (\(_, sched') -> drainAll sched') (scheduleDequeue sched)
    completeAll :: [Capability] -> Sched -> Either (RuntimeFrontierError Ctx Epoch Phase) Sched
    completeAll capabilities sched =
      foldl (\acc capability -> acc >>= scheduleComplete capability) (Right sched) capabilities

completeMissingPending :: TestTree
completeMissingPending =
  testCase "completing a never-pending time fails with the typed error" $ do
    let sched = emptySchedule [0]
    case scheduleComplete (mkCap 99) sched of
      Left err ->
        assertEqual "names the missing time" (RuntimeFrontierMissingPendingComplete (mkTime 99)) err
      Right _ ->
        assertFailure "expected Left on a non-pending completion"

doubleCompleteFails :: TestTree
doubleCompleteFails =
  testCase "second completion of a singly-pending time fails" $ do
    let capability = mkCap 5
        sched = scheduleEnqueue 0 capability [1] (emptySchedule [0])
    case scheduleComplete capability sched of
      Left err ->
        assertFailure ("first completion should succeed: " ++ show err)
      Right sched' -> do
        assertBool "pending cleared" (Set.null (schedulePendingPointstamps sched'))
        case scheduleComplete capability sched' of
          Left err ->
            assertEqual "second completion fails typed" (RuntimeFrontierMissingPendingComplete (mkTime 5)) err
          Right _ ->
            assertFailure "second completion must fail"
