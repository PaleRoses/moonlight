{-# LANGUAGE OverloadedStrings #-}

module Moonlight.Flow.Runtime.DiagnosticsDataflowSpec
  ( tests,
  )
where

import Data.Set qualified as Set
import Data.ByteString.Lazy qualified as BSL
import Data.Word
  ( Word64,
  )
import Moonlight.Delta.Time
  ( Timed (..),
  )
import Moonlight.Core
  ( QueryId,
    initialLiveEpoch,
    initialQuotientEpoch,
    mkAtomId,
    mkQueryId,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
import Moonlight.Differential.Time
  ( frontierStamp,
  )
import Moonlight.Differential.Runtime.Schedule
  ( ScheduleError,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
    queryAtomCarrier,
    queryRootCarrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    RestrictKey,
    carrierAddr,
    restrictKey,
  )
import Moonlight.Flow.Carrier.Core.Frontier
  ( emptyRelDiffFrontier,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    mkRelationalCarrierTime,
  )
import Moonlight.Flow.Carrier.Core.Topology
  ( CarrierEdge (..),
    CarrierTopology,
    TouchKey (..),
    emptyCarrierTopology,
    insertCarrierEdge,
    insertCarrierTouch,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (..),
  )
import Moonlight.Flow.Runtime.Engine.Dataflow
import Moonlight.Flow.Runtime.Engine.Queue.Frontier
  ( enqueueScheduledRuntimeDataflowOp,
  )
import Moonlight.Flow.Runtime.Engine.Queue.Types
  ( RuntimeDataflowQueue,
    emptyRuntimeDataflowQueue,
  )
import Moonlight.Flow.Runtime.Engine.Queue.Scheduler
  ( runtimeDataflowPriorityPlan,
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( restrictCarrierDataflowOp,
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

tests :: TestTree
tests =
  testGroup
    "runtime dataflow diagnostics"
    [ testCase "topology snapshot exposes touch and restriction edges" topologySnapshotAssertion,
      testCase "queued operation becomes an operation node with schedule edges" queuedOperationAssertion,
      testCase "queued operation read/write carriers become graph nodes" queuedCarrierNodeAssertion,
      testCase "frontier summary reports queued pending operations" frontierSummaryAssertion,
      testCase "CBOR encoding emits a nonempty versioned artifact" cborEncodingAssertion
    ]

topologySnapshotAssertion :: IO ()
topologySnapshotAssertion =
  withQueueFixture "empty runtime dataflow queue" emptyQueue $ \queue -> do
    let snapshot = runtimeDataflowSnapshotFromTopologyQueue renderers touchedRestrictedTopology queue
        nodeIds = Set.fromList (fmap rdnId (rdsNodes snapshot))
        edgeKinds = Set.fromList (fmap rdeKind (rdsEdges snapshot))
    assertBool "touch node should exist" (Set.member (DataflowNodeId "5:touch|11:TouchAtom 7") nodeIds)
    assertBool "atom node should exist" (Set.member atomNodeId nodeIds)
    assertBool "root node should exist" (Set.member rootNodeId nodeIds)
    assertBool "touch edge should exist" (Set.member DataflowEdgeTouch edgeKinds)
    assertBool "restriction edge should exist" (Set.member DataflowEdgeRestriction edgeKinds)
    assertEqual "snapshot should not contain orphan edge endpoints" Set.empty (runtimeDataflowOrphanEdges snapshot)

queuedOperationAssertion :: IO ()
queuedOperationAssertion =
  withQueueFixture "restricted runtime dataflow queue" restrictedQueue $ \queue -> do
    let snapshot = runtimeDataflowSnapshotFromTopologyQueue renderers touchedRestrictedTopology queue
        nodeKinds = Set.fromList (fmap rdnKind (rdsNodes snapshot))
        edgeKinds = Set.fromList (fmap rdeKind (rdsEdges snapshot))
        opKinds = Set.fromList (fmap rdoKind (rdsOps snapshot))
    assertBool "restrict op should be recorded" (Set.member DataflowOpViewRestrictCarrier opKinds)
    assertBool "operation node should be present" (Set.member DataflowNodeOperation nodeKinds)
    assertBool "schedule read edge should be derived" (Set.member DataflowEdgeScheduleRead edgeKinds)
    assertBool "schedule write edge should be derived" (Set.member DataflowEdgeScheduleWrite edgeKinds)
    assertEqual "operation snapshot should not contain orphan edge endpoints" Set.empty (runtimeDataflowOrphanEdges snapshot)

queuedCarrierNodeAssertion :: IO ()
queuedCarrierNodeAssertion =
  withQueueFixture "restricted runtime dataflow queue" restrictedQueue $ \queue -> do
    let snapshot = runtimeDataflowSnapshotFromTopologyQueue renderers emptyCarrierTopology queue
        nodeIds = Set.fromList (fmap rdnId (rdsNodes snapshot))
    assertBool "queued read carrier should be present without topology edge" (Set.member atomNodeId nodeIds)
    assertBool "queued write carrier should be present without topology edge" (Set.member rootNodeId nodeIds)
    assertEqual "queue-derived carrier nodes should prevent schedule orphans" Set.empty (runtimeDataflowOrphanEdges snapshot)

frontierSummaryAssertion :: IO ()
frontierSummaryAssertion =
  withQueueFixture "restricted runtime dataflow queue" restrictedQueue $ \queue -> do
    let snapshot = runtimeDataflowSnapshotFromTopologyQueue renderers touchedRestrictedTopology queue
        frontier = rdsFrontier snapshot
    assertEqual "one pointstamp should have pending operations" 1 (rdfPendingPointstampCount frontier)
    assertEqual "one scheduled operation should be pending" 1 (rdfPendingOpCount frontier)

cborEncodingAssertion :: IO ()
cborEncodingAssertion =
  withQueueFixture "restricted runtime dataflow queue" restrictedQueue $ \queue -> do
    let snapshot = runtimeDataflowSnapshotFromTopologyQueue renderers touchedRestrictedTopology queue
        encoded = encodeRuntimeDataflowCBOR snapshot
    assertEqual "runtime dataflow CBOR version should match browser schema" 2 (rdsVersion snapshot)
    assertBool "runtime dataflow CBOR should not be empty" (not (BSL.null encoded))

renderers :: RuntimeDataflowRenderers Int Int
renderers = defaultRuntimeDataflowRenderers

emptyQueue :: Either (ScheduleError RelationalPhase) (RuntimeDataflowQueue Int Int () ())
emptyQueue =
  (\priorityPlan -> emptyRuntimeDataflowQueue priorityPlan emptyRelDiffFrontier)
    <$> runtimeDataflowPriorityPlan

restrictedQueue :: Either (ScheduleError RelationalPhase) (RuntimeDataflowQueue Int Int () ())
restrictedQueue = do
  queue <- emptyQueue
  enqueueScheduledRuntimeDataflowOp
    (Timed (carrierTime 0 PhaseRestrict 0) (restrictCarrierDataflowOp restrictionKey))
    queue

withQueueFixture ::
  String ->
  Either (ScheduleError RelationalPhase) (RuntimeDataflowQueue Int Int () ()) ->
  (RuntimeDataflowQueue Int Int () () -> IO ()) ->
  IO ()
withQueueFixture fixtureName fixture continuation =
  either
    (assertFailure . ((fixtureName <> " failed: ") <>) . show)
    continuation
    fixture

touchedRestrictedTopology :: CarrierTopology Int Carrier Int
touchedRestrictedTopology =
  insertCarrierTouch (TouchAtom 7) atomAddr $
    insertCarrierEdge atomAddr (EdgeRestriction restrictionKey) emptyCarrierTopology

atomNodeId :: DataflowNodeId
atomNodeId = dataflowNodeIdForCarrierAddr renderers atomAddr

rootNodeId :: DataflowNodeId
rootNodeId = dataflowNodeIdForCarrierAddr renderers rootAddr

carrierTime :: Int -> RelationalPhase -> Word64 -> RelationalCarrierTime Int
carrierTime contextValue phaseValue stamp =
  mkRelationalCarrierTime
    contextValue
    initialQuotientEpoch
    initialLiveEpoch
    phaseValue
    (frontierStamp (fromIntegral stamp))

queryId :: QueryId
queryId = mkQueryId 0

atomAddr :: CarrierAddr Int Carrier Int
atomAddr = carrierAddr 0 propKey (queryAtomCarrier queryId (mkAtomId 0))

rootAddr :: CarrierAddr Int Carrier Int
rootAddr = carrierAddr 0 propKey (queryRootCarrier queryId)

restrictionKey :: RestrictKey Int Carrier Int
restrictionKey = restrictKey atomAddr rootAddr

propKey :: PropositionKey Int
propKey = PropositionKey 0
