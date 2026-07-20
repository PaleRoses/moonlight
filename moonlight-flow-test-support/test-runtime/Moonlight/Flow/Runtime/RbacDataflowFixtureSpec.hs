module Moonlight.Flow.Runtime.RbacDataflowFixtureSpec
  ( tests,
  )
where

import Data.Foldable
  ( traverse_,
  )
import Data.Set qualified as Set
import Moonlight.Flow.Runtime.Engine.Dataflow
  ( RuntimeDataflowDiagnostics (..),
    RuntimeDataflowRepairStats (..),
    RuntimeDataflowSignedSummary (..),
    RuntimeDataflowSnapshot (..),
    RuntimeDataflowVersionTrace (..),
    RuntimeDataflowWorkload (..),
    runtimeDataflowOrphanEdges,
  )
import Moonlight.Flow.Runtime.RbacDataflowFixture
  ( rbacDataflowLiveSnapshots,
    rbacDataflowSnapshot,
    runtimeDataflowSnapshotHex,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertBool,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "rbac dataflow fixture"
    [ testCase "captures a rich public-runtime workload without orphan edges" $
        case rbacDataflowSnapshot of
          Left err ->
            assertFailure (show err)
          Right snapshot ->
            assertRbacDataflowSnapshot snapshot,
      testCase "live stream steps advance through valid workload snapshots" $
        case rbacDataflowLiveSnapshots 2 of
          Left err ->
            assertFailure (show err)
          Right snapshots ->
            assertBool "expected at least two live snapshots" (length snapshots == 2)
              *> traverse_ assertRbacDataflowSnapshot snapshots
    ]

assertRbacDataflowSnapshot :: RuntimeDataflowSnapshot -> IO ()
assertRbacDataflowSnapshot snapshot =
  do
    assertRuntimeDataflowArtifactPayload snapshot
    assertBool
      "expected no orphan dataflow edges"
      (Set.null (runtimeDataflowOrphanEdges snapshot))
    assertBool
      "expected SEL-45-scale carrier graph"
      (rddNodeCount diagnostics > 50 && rddEdgeCount diagnostics > 80)
    assertBool
      "expected queued live runtime operations"
      (rddOpCount diagnostics > 8)
    case rdsWorkload snapshot of
      Nothing ->
        assertFailure "expected captured patch workload trace"
      Just workload ->
        assertRbacWorkloadTrace workload
  where
    diagnostics = rdsDiagnostics snapshot

assertRuntimeDataflowArtifactPayload :: RuntimeDataflowSnapshot -> IO ()
assertRuntimeDataflowArtifactPayload snapshot =
  do
    assertBool
      "expected non-empty hexadecimal CBOR runtime-dataflow artifact"
      (not (null hexPayload))
    assertBool
      "expected even hexadecimal stream payload"
      (even (length hexPayload))
  where
    hexPayload = runtimeDataflowSnapshotHex snapshot

assertRbacWorkloadTrace :: RuntimeDataflowWorkload -> IO ()
assertRbacWorkloadTrace workload =
  do
    assertBool
      "expected touched carriers in captured patch"
      (rdwTouchedCarrierCount workload > 12)
    assertBool
      "expected queued operations in captured patch"
      (rdwQueuedOperationCount workload > 8)
    assertBool
      "expected actual repair maintenance stats"
      (rdrsFactorRepairs repairStats > 0)
    assertBool
      "expected emitted incremental carrier deltas"
      (rdrsEmittedCarrierDeltas repairStats > 0)
    assertBool
      "expected captured signed insertions"
      (rdsdsInsertedRowMultiplicity deltaSummary > 0)
    assertBool
      "expected captured signed deletions"
      (rdsdsRemovedRowMultiplicity deltaSummary > 0)
    assertBool
      "expected quotient version transition"
      (rdvtQuotientBefore versionTrace /= rdvtQuotientAfter versionTrace)
  where
    repairStats = rdwRepairStats workload
    deltaSummary = rdwDeltaSummary workload
    versionTrace = rdwVersionTrace workload
