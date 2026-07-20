module Test.Moonlight.Flow.Property.Debug.CompactionFrontier
  ( tests,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Word
  ( Word64,
  )
import Moonlight.Core
  ( AtomId,
    QueryId,
    initialLiveEpoch,
    initialQuotientEpoch,
    mkAtomId,
    mkQueryId,
    mkSlotId,
  )
import Moonlight.Differential.Frontier
  ( emptyRuntimeFrontier,
    emptyTraceRetention,
    frontierAdvanceVisibleMin,
    frontierWithTraceRetention,
  )
import Moonlight.Differential.Time
  ( emptyRuntimeScope,
    enterRuntimeScope,
    frontierStamp,
    enterRuntimeTimeScope,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    queryAtomCarrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    carrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Frontier
  ( RelDiffFrontier,
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierStore,
    CarrierStoreDiagnostics (..),
    CarrierStoreError (..),
    CarrierStoreRuntime (..),
    CarrierStoreSummaryEntry (..),
    carrierReadCapabilityFromTime,
    carrierStoreDiagnostics,
    commitCarrierDelta,
    compactCarrierStoreBefore,
    emptyCarrierHeldReads,
    emptyCarrierStore,
    readCarrierSince,
  )
import Moonlight.Flow.Carrier.View.Query
  ( carrierCurrentDeltaLatestTraceNow,
    visibleCarrierNow,
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( OriginEvent (OriginAtom, OriginCompacted),
    RelationalOrigin (..),
    emptyDerivationRoute,
    originAddParent,
    originMerge,
  )
import Moonlight.Flow.Carrier.Core.Summary
  ( CarrierBatchSummaryOps (..),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    mkRelationalCarrierTime,
    relationalTimeScope,
  )
import Moonlight.Delta.Signed
  ( Multiplicity (..),
    MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchFromList,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (..),
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    RepKey (..),
    tupleKeyFromRepKeys,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    RuntimeBoundaryError,
    boundaryDigest,
    mkRuntimeBoundary,
  )
import Moonlight.Flow.Runtime.Engine.Compaction
  ( compactRuntimeBefore,
  )
import Test.Moonlight.Flow.Runtime.Diagnostics.Validate
  ( validateRuntimeTrace,
  )
import Test.Moonlight.Flow.Runtime.Diagnostics.Observation
  ( RuntimeObservation (..),
    observeRuntimeWithEvidenceView,
  )
import Test.Moonlight.Flow.Trace.EngineClosureFixture
  ( ClosureFixture (..),
    closureFixture,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    singletonContextLattice
  )
import Moonlight.FiniteLattice
  ( principalSupport
  )


tests :: TestTree
tests =
  testGroup
    "runtime compaction observability"
    [ testCase "rich fixture contains nonempty carrier authority evidence" $ withFixture richFixtureCarriesAuthorityEvidence,
      testCase "compaction preserves carrier-store observation over rich trace fixture" $ withFixture compactPreservesObservation,
      testCase "compaction replay validates over rich trace fixture" $ withFixture compactReplayValid,
      testCase "pinned trace ids stop prefix compaction without changing observation" $ withFixture pinnedTraceStopsPrefixCompaction,
      testCase "pending operations stops matching partition compaction without changing observation" $ withFixture pendingOpsStopsMatchingPartitionCompaction,
      testCase "ended runtime scope compacts only its scoped trace prefix" scopedTracePrefixCompactsAfterScopeEnd
    ]

richFixtureCarriesAuthorityEvidence :: ClosureFixture -> Assertion
richFixtureCarriesAuthorityEvidence fixture = do
  let observation =
        observeRuntimeWithEvidenceView (cfEvidenceView fixture) (cfRichRuntime fixture)
  assertBool
    "rich fixture must expose boundaries through carrier-store observation"
    (not (Map.null (roBoundaryLatestTraceByCarrier observation)))
  assertBool
    "rich fixture must expose evidence through carrier-store observation"
    (not (Map.null (roEvidenceLiveSeedByCarrier observation)))
  assertBool
    "rich fixture must expose restriction failures from trace evidence"
    (not (null (roRestrictionFailures observation)))
  assertBool
    "rich fixture must expose propagation failures from trace evidence"
    (not (null (roPropagationFailures observation)))
  assertBool
    "rich fixture must expose cohomological failures from trace evidence"
    (not (null (roCohomologicalFailures observation)))
compactPreservesObservation :: ClosureFixture -> Assertion
compactPreservesObservation fixture = do
  compacted <-
    assertRight
      "expected rich runtime compaction to succeed"
      (compactRuntimeBefore (cfCompactingFrontier fixture) (cfRichRuntime fixture))
  assertEqual
    "observable carrier-store state must be unchanged by compaction"
    (observeRuntimeWithEvidenceView (cfEvidenceView fixture) (cfRichRuntime fixture))
    (observeRuntimeWithEvidenceView (cfEvidenceView fixture) compacted)

compactReplayValid :: ClosureFixture -> Assertion
compactReplayValid fixture = do
  compacted <-
    assertRight
      "expected rich runtime compaction to succeed"
      (compactRuntimeBefore (cfCompactingFrontier fixture) (cfRichRuntime fixture))
  assertRight_
    "compacted rich runtime replay must validate"
    (validateRuntimeTrace compacted)

pinnedTraceStopsPrefixCompaction :: ClosureFixture -> Assertion
pinnedTraceStopsPrefixCompaction fixture =
  compactWithFrontierPreservesObservation
    "pinned-trace frontier compaction"
    cfPinnedFrontier
    fixture

pendingOpsStopsMatchingPartitionCompaction :: ClosureFixture -> Assertion
pendingOpsStopsMatchingPartitionCompaction fixture =
  compactWithFrontierPreservesObservation
    "pending-op frontier compaction"
    cfPendingFrontier
    fixture

scopedTracePrefixCompactsAfterScopeEnd :: Assertion
scopedTracePrefixCompactsAfterScopeEnd = do
  boundary <-
    assertRight
      "expected scoped lifecycle boundary"
      scopedLifecycleBoundary
  indexed <-
    assertRight
      "expected scoped lifecycle trace insertion"
      (scopedLifecycleStore boundary)
  compacted <-
    assertRight
      "expected ended scoped prefix to compact"
      ( compactCarrierStoreBefore
          scopedLifecycleSummaryOps
          scopedLifecycleLattice
          emptyCarrierHeldReads
          scopedLifecycleFrontier
          indexed
      )

  assertEqual
    "fixture starts with two ended-scope entries and one active-scope entry"
    3
    (csdTraceEntries (carrierStoreDiagnostics indexed))

  assertEqual
    "ended scope should collapse while active sibling remains separate"
    2
    (csdTraceEntries (carrierStoreDiagnostics compacted))

  assertCompactedReadObstruction
    "read from before compacted prefix must report compaction"
    boundary
    compacted

  assertActiveSiblingLatestTrace compacted

  assertEqual
    "current carrier rows are preserved after scoped lifecycle compaction"
    ( Map.fromList
        [ (scopedLifecycleRow 10, Multiplicity 1),
          (scopedLifecycleRow 11, Multiplicity 1),
          (scopedLifecycleRow 12, Multiplicity 1)
        ]
    )
    (visibleCarrierNow scopedLifecycleAddr compacted)

assertCompactedReadObstruction ::
  String ->
  RuntimeBoundary ->
  CarrierStore Int Carrier Int RuntimeBoundary () ->
  Assertion
assertCompactedReadObstruction label boundary store =
  case readCarrierSince scopedLifecycleRuntime capability store of
    Left (CarrierStoreReadCompacted addr _frontier _traceId) ->
      assertEqual label scopedLifecycleAddr addr
    Left err ->
      assertFailure (label <> ": expected CarrierStoreReadCompacted, got " <> show err)
    Right delta ->
      assertFailure (label <> ": expected CarrierStoreReadCompacted, got readable delta " <> show delta)
  where
    capability =
      carrierReadCapabilityFromTime
        scopedLifecycleAddr
        (boundaryDigest boundary)
        (scopedLifecycleTime 1 0)

assertActiveSiblingLatestTrace ::
  CarrierStore Int Carrier Int RuntimeBoundary () ->
  Assertion
assertActiveSiblingLatestTrace store =
  case carrierCurrentDeltaLatestTraceNow scopedLifecycleAddr store of
    Nothing ->
      assertFailure "missing compacted current delta"
    Just delta ->
      assertEqual
        "active sibling scope entry must remain the latest semantic carrier delta"
        (enterRuntimeScope 2 emptyRuntimeScope)
        (relationalTimeScope (deTime delta))

compactWithFrontierPreservesObservation ::
  String ->
  (ClosureFixture -> RelDiffFrontier Int RelationalPhase) ->
  ClosureFixture ->
  Assertion
compactWithFrontierPreservesObservation label frontierOf fixture = do
  compacted <-
    assertRight
      (label <> " should succeed")
      (compactRuntimeBefore (frontierOf fixture) (cfRichRuntime fixture))
  assertRight_
    (label <> " replay must validate")
    (validateRuntimeTrace compacted)
  assertEqual
    (label <> " must preserve observable carrier-store state")
    (observeRuntimeWithEvidenceView (cfEvidenceView fixture) (cfRichRuntime fixture))
    (observeRuntimeWithEvidenceView (cfEvidenceView fixture) compacted)

scopedLifecycleStore ::
  RuntimeBoundary ->
  Either
    (CarrierStoreError Int Carrier Int RuntimeBoundary ())
    (CarrierStore Int Carrier Int RuntimeBoundary ())
scopedLifecycleStore boundary =
  foldM
    ( \indexState delta ->
        commitCarrierDelta scopedLifecycleLattice delta indexState
    )
    emptyCarrierStore
    (scopedLifecycleDeltas boundary)

scopedLifecycleDeltas ::
  RuntimeBoundary ->
  [RelationalCarrierDelta Int Carrier Int RuntimeBoundary ()]
scopedLifecycleDeltas boundary =
  [ scopedLifecycleDelta boundary 1 0 (scopedLifecycleRow 10),
    scopedLifecycleDelta boundary 1 1 (scopedLifecycleRow 11),
    scopedLifecycleDelta boundary 2 2 (scopedLifecycleRow 12)
  ]

scopedLifecycleDelta ::
  RuntimeBoundary ->
  Int ->
  Word64 ->
  RowTupleKey ->
  RelationalCarrierDelta Int Carrier Int RuntimeBoundary ()
scopedLifecycleDelta boundary scopeKey stamp row =
  RelationalCarrierDelta
    { deAddr = scopedLifecycleAddr,
      deTime = scopedLifecycleTime scopeKey stamp,
      deSupport = principalSupport 0,
      deBoundary = boundary,
      deEvidence = (),
      deOrigin =
        RelationalOrigin
          { roEvent = OriginAtom scopedLifecycleQuery scopedLifecycleAtom,
            roRoute = emptyDerivationRoute
          },
      deScope = mempty,
      deRows = (plainRowPatchFromList [(row, MultiplicityChange 1)]),
      dePayload = ()
    }

scopedLifecycleFrontier :: RelDiffFrontier Int RelationalPhase
scopedLifecycleFrontier =
  frontierAdvanceVisibleMin
    scopedLifecycleCutoff
    (frontierWithTraceRetention (Just emptyTraceRetention) emptyRuntimeFrontier)

scopedLifecycleCutoff :: RelationalCarrierTime Int
scopedLifecycleCutoff =
  scopedLifecycleTime 1 10

scopedLifecycleTime ::
  Int ->
  Word64 ->
  RelationalCarrierTime Int
scopedLifecycleTime scopeKey stamp =
  enterRuntimeTimeScope
    scopeKey
    ( mkRelationalCarrierTime
        0
        initialQuotientEpoch
        initialLiveEpoch
        PhaseProject
        (frontierStamp (fromIntegral stamp))
    )

scopedLifecycleSummaryOps ::
  CarrierBatchSummaryOps
    Int
    Carrier
    Int
    RuntimeBoundary
    ()
    (CarrierStoreSummaryEntry Int Carrier Int RuntimeBoundary ())
scopedLifecycleSummaryOps =
  CarrierBatchSummaryOps
    { cbsoSummaryBoundary =
        \_addr entries -> csseBoundary (NonEmpty.last entries),
      cbsoSummaryEvidence =
        \_addr _entries -> (),
      cbsoSummaryOrigin =
        \addr entries ->
          originAddParent
            addr
            (originMerge OriginCompacted (fmap csseOrigin entries))
    }

scopedLifecycleBoundary :: Either RuntimeBoundaryError RuntimeBoundary
scopedLifecycleBoundary =
  mkRuntimeBoundary [mkSlotId 0] IntSet.empty IntMap.empty

scopedLifecycleRuntime :: CarrierStoreRuntime Int RuntimeBoundary
scopedLifecycleRuntime =
  CarrierStoreRuntime
    { csrContextLattice = scopedLifecycleLattice,
      csrBoundaryDigest = boundaryDigest
    }

scopedLifecycleLattice :: ContextLattice Int
scopedLifecycleLattice =
  singletonContextLattice 0

scopedLifecycleAddr :: CarrierAddr Int Carrier Int
scopedLifecycleAddr =
  carrierAddr 0 scopedLifecycleProp (queryAtomCarrier scopedLifecycleQuery scopedLifecycleAtom)

scopedLifecycleQuery :: QueryId
scopedLifecycleQuery =
  mkQueryId 777

scopedLifecycleAtom :: AtomId
scopedLifecycleAtom =
  mkAtomId 3

scopedLifecycleProp :: PropositionKey Int
scopedLifecycleProp =
  PropositionKey 0

scopedLifecycleRow :: Int -> RowTupleKey
scopedLifecycleRow value =
  tupleKeyFromRepKeys [RepKey value]

withFixture :: (ClosureFixture -> Assertion) -> Assertion
withFixture assertion =
  case closureFixture of
    Left fixtureError ->
      assertFailure (show fixtureError)
    Right fixture ->
      assertion fixture

assertRight ::
  Show errorValue =>
  String ->
  Either errorValue value ->
  IO value
assertRight label =
  either
    (\errorValue -> assertFailure (label <> ": " <> show errorValue))
    pure

assertRight_ ::
  Show errorValue =>
  String ->
  Either errorValue () ->
  Assertion
assertRight_ label =
  either
    (\errorValue -> assertFailure (label <> ": " <> show errorValue))
    pure
