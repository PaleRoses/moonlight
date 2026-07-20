{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Store.TraceCharacterizationSpec
  ( spec,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Word
  ( Word64,
  )
import Moonlight.Core
  ( QueryId,
    mkAtomId,
    mkLiveEpoch,
    mkQueryId,
    mkQuotientEpoch,
    mkSlotId,
  )
import Moonlight.Differential.Frontier
  ( emptyRuntimeFrontier,
    emptyTraceRetention,
    frontierAdvanceVisibleMin,
    frontierWithTraceRetention,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
import Moonlight.Differential.Time
  ( frontierStamp,
  )
import Moonlight.Differential.Trace.Indexed
  ( itEntries,
    itIndexes,
    validateIndexedTraceIndexes,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
    queryAtomCarrier,
    queryRootCarrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    carrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Frontier
  ( RelDiffFrontier,
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( OriginEvent (OriginCompacted, OriginLocal),
    RelationalOrigin (..),
    emptyDerivationRoute,
    originAddParent,
    originMerge,
  )
import Moonlight.Flow.Carrier.Core.Summary
  ( CarrierBatchSummaryOps (..),
    CarrierStoreSummaryEntry (..),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    mkRelationalCarrierTime,
  )
import Moonlight.Flow.Carrier.Store.Core.Error
  ( CarrierStoreError (..),
  )
import Moonlight.Flow.Carrier.Store.Core.Read
  ( CarrierHeldReads,
    CarrierReadFrontier (..),
    carrierHeldReadsFromList,
    emptyCarrierHeldReads,
  )
import Moonlight.Flow.Carrier.Store.Core.State
  ( CarrierCurrentRows (..),
    CarrierSnapshot (..),
    CarrierStore (..),
    CarrierTraceIndexes (..),
    CarrierViews (..),
    ccpSnapshots,
    cstTrace,
    cstViews,
    cvCurrent,
  )
import Moonlight.Flow.Carrier.Store.Engine.Commit
  ( commitCarrierDelta,
    emptyCarrierStore,
  )
import Moonlight.Flow.Carrier.Store.Engine.Compact
  ( compactCarrierStoreBefore,
  )
import Moonlight.Flow.Carrier.Store.Engine.Replay
  ( compareCarrierStoreReplay,
    replayCarrierStore,
    validateCarrierStore,
  )
import Moonlight.Flow.Carrier.Store.Journal.Trace
  ( carrierTraceIndexOps,
    carrierTraceKeysSince,
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  ( PlainRowPatch,
    plainRowPatchFromList
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (PhaseProject),
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    RuntimeBoundaryError,
    mkRuntimeBoundary,
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
    WitnessReverseIndex (..),
    relationalScopeFromSets,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    RepKey (..),
    tupleKeyFromRepKeys,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertFailure,
    testCase,
    (@?=),
  )
import Test.Moonlight.Differential.Trace.Indexed
  ( indexedTraceWithIndexesForValidation,
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    singletonContextLattice
  )
import Moonlight.FiniteLattice
  ( principalSupport
  )


type Ctx = Int

type Prop = Int

type Boundary = RuntimeBoundary

type Evidence = String

type TestStore = CarrierStore Ctx Carrier Prop Boundary Evidence

type TestDelta = RelationalCarrierDelta Ctx Carrier Prop Boundary Evidence

type TestTime = RelationalCarrierTime Ctx

spec :: TestTree
spec =
  testGroup
    "carrier trace characterization"
    [ testCase "trace axes remain coherent with committed entries" traceAxesRemainCoherent,
      testCase "store validation rejects trace axis corruption before replay comparison" traceValidationRejectsAxisCorruption,
      testCase "read-frontier trace keys are address/epoch exact and stamp-open" traceKeysSinceAreExact,
      testCase "compaction is replay-equivalent and preserves rebuilt projections" compactionReplayEquivalent,
      testCase "held reads block compaction that would hide readable entries" heldReadsBlockUnsafeCompaction
    ]

traceAxesRemainCoherent :: Assertion
traceAxesRemainCoherent = do
  store <- mixedTraceStore
  let traceValue = cstTrace store
      indexes = itIndexes traceValue
  validateIndexedTraceIndexes carrierTraceIndexOps traceValue @?= Right ()
  ctiByAddr indexes
    @?= Map.fromList
      [ (sourceAddr, traceIds [0, 1, 2, 4]),
        (otherAddr, traceIds [3])
      ]
  ctiByContext indexes @?= Map.singleton contextValue (traceIds [0, 1, 2, 3, 4])
  ctiByCarrier indexes
    @?= Map.fromList
      [ (sourceCarrier, traceIds [0, 1, 2, 4]),
        (otherCarrier, traceIds [3])
      ]
  ctiByProp indexes
    @?= Map.fromList
      [ (sourceProp, traceIds [0, 1, 2, 4]),
        (otherProp, traceIds [3])
      ]
  ctiByDep indexes
    @?= WitnessReverseIndex
      ( IntMap.fromList
          [ (10, traceIds [0, 1]),
            (11, traceIds [1]),
            (12, traceIds [3]),
            (13, traceIds [4])
          ]
      )
  ctiByTopo indexes
    @?= WitnessReverseIndex
      ( IntMap.fromList
          [ (20, traceIds [0, 2]),
            (21, traceIds [1]),
            (22, traceIds [2]),
            (23, traceIds [4])
          ]
      )
  ctiByRoot indexes
    @?= WitnessReverseIndex
      ( IntMap.fromList
          [ (30, traceIds [0, 2]),
            (31, traceIds [1]),
            (32, traceIds [3]),
            (33, traceIds [4])
          ]
      )
  ctiByResult indexes
    @?= WitnessReverseIndex
      ( IntMap.fromList
          [ (40, traceIds [0, 3]),
            (41, traceIds [1]),
            (42, traceIds [2]),
            (43, traceIds [4])
          ]
      )

traceValidationRejectsAxisCorruption :: Assertion
traceValidationRejectsAxisCorruption = do
  store <- mixedTraceStore
  let traceValue = cstTrace store
      brokenIndexes = (itIndexes traceValue) {ctiByAddr = Map.empty}
      brokenStore = store {cstTrace = indexedTraceWithIndexesForValidation brokenIndexes traceValue}
  case validateCarrierStore testLattice brokenStore of
    Left CarrierStoreTraceIndexesInvalid {} ->
      pure ()
    other ->
      assertFailure ("expected CarrierStoreTraceIndexesInvalid, got " <> show other)

traceKeysSinceAreExact :: Assertion
traceKeysSinceAreExact = do
  store <- mixedTraceStore
  carrierTraceKeysSince (readFrontier 1 0) sourceAddr (cstTrace store) @?= traceIds [1, 2]
  carrierTraceKeysSince (readFrontier 1 1) sourceAddr (cstTrace store) @?= traceIds [2]
  carrierTraceKeysSince (readFrontier 1 2) sourceAddr (cstTrace store) @?= IntSet.empty
  carrierTraceKeysSince (readFrontier 2 3) sourceAddr (cstTrace store) @?= traceIds [4]
  carrierTraceKeysSince (readFrontier 1 0) otherAddr (cstTrace store) @?= traceIds [3]

compactionReplayEquivalent :: Assertion
compactionReplayEquivalent = do
  store <- compactableStore
  compacted <- expectRight (compactStore emptyCarrierHeldReads store)
  validateIndexedTraceIndexes carrierTraceIndexOps (cstTrace compacted) @?= Right ()
  validateCarrierStore testLattice compacted @?= Right ()
  replayed <- expectRight (replayCarrierStore testLattice compacted)
  compareCarrierStoreReplay compacted replayed @?= Right ()
  IntMap.size (itEntries (cstTrace compacted)) @?= 1
  currentRowsAt sourceAddr compacted
    @?= Just
      ( plainRowPatchFromList
          [ (row 100, MultiplicityChange 1),
            (row 101, MultiplicityChange 1),
            (row 102, MultiplicityChange 1)
          ]
      )

heldReadsBlockUnsafeCompaction :: Assertion
heldReadsBlockUnsafeCompaction = do
  store <- compactableStore
  case compactStore (carrierHeldReadsFromList [(sourceAddr, readFrontier 1 0)]) store of
    Left (CarrierStoreCompactionWouldInvalidateHeldRead addr frontier _traceId) -> do
      addr @?= sourceAddr
      frontier @?= readFrontier 1 0
    other ->
      assertFailure ("expected held-read compaction obstruction, got " <> show other)
  compacted <- expectRight (compactStore (carrierHeldReadsFromList [(sourceAddr, readFrontier 1 10)]) store)
  validateCarrierStore testLattice compacted @?= Right ()

mixedTraceStore :: AssertionValue TestStore
mixedTraceStore = do
  boundary <- expectRight testBoundary
  expectRight
    ( commitDeltas
        [ carrierDelta boundary sourceAddr (testTime 1 0) (row 10) (scope [10] [20] [30] [40]) "source-0",
          carrierDelta boundary sourceAddr (testTime 1 1) (row 11) (scope [10, 11] [21] [31] [41]) "source-1",
          carrierDelta boundary sourceAddr (testTime 1 2) (row 12) (scope [] [20, 22] [30] [42]) "source-2",
          carrierDelta boundary otherAddr (testTime 1 3) (row 20) (scope [12] [] [32] [40]) "other",
          carrierDelta boundary sourceAddr (testTime 2 4) (row 13) (scope [13] [23] [33] [43]) "source-live-2"
        ]
    )

compactableStore :: AssertionValue TestStore
compactableStore = do
  boundary <- expectRight testBoundary
  expectRight
    ( commitDeltas
        [ carrierDelta boundary sourceAddr (testTime 1 0) (row 100) mempty "compact-0",
          carrierDelta boundary sourceAddr (testTime 1 1) (row 101) mempty "compact-1",
          carrierDelta boundary sourceAddr (testTime 1 2) (row 102) mempty "compact-2"
        ]
    )

commitDeltas :: [TestDelta] -> Either (CarrierStoreError Ctx Carrier Prop Boundary Evidence) TestStore
commitDeltas =
  foldM
    (\store deltaValue -> commitCarrierDelta testLattice deltaValue store)
    emptyCarrierStore

compactStore ::
  CarrierHeldReads Ctx Carrier Prop ->
  TestStore ->
  Either (CarrierStoreError Ctx Carrier Prop Boundary Evidence) TestStore
compactStore heldReads =
  compactCarrierStoreBefore summaryOps testLattice heldReads compactionFrontier

currentRowsAt ::
  CarrierAddr Ctx Carrier Prop ->
  TestStore ->
  Maybe (PlainRowPatch RowTupleKey)
currentRowsAt addr store =
  fmap (ccrRows . csCurrentRows) (Map.lookup addr (ccpSnapshots (cvCurrent (cstViews store))))

carrierDelta ::
  Boundary ->
  CarrierAddr Ctx Carrier Prop ->
  TestTime ->
  RowTupleKey ->
  RelationalScope ->
  Evidence ->
  TestDelta
carrierDelta boundary addr timeValue rowValue scopeValue evidenceValue =
  RelationalCarrierDelta
    { deAddr = addr,
      deTime = timeValue,
      deSupport = principalSupport (caContext addr),
      deBoundary = boundary,
      deEvidence = evidenceValue,
      deRows = plainRowPatchFromList [(rowValue, MultiplicityChange 1)],
      deOrigin = RelationalOrigin {roEvent = OriginLocal queryId, roRoute = emptyDerivationRoute},
      deScope = scopeValue,
      dePayload = ()
    }

summaryOps ::
  CarrierBatchSummaryOps
    Ctx
    Carrier
    Prop
    Boundary
    Evidence
    (CarrierStoreSummaryEntry Ctx Carrier Prop Boundary Evidence)
summaryOps =
  CarrierBatchSummaryOps
    { cbsoSummaryBoundary = \_addr entries -> csseBoundary (NonEmpty.last entries),
      cbsoSummaryEvidence = \_addr entries -> csseEvidence (NonEmpty.last entries),
      cbsoSummaryOrigin =
        \addr entries ->
          originAddParent
            addr
            (originMerge OriginCompacted (fmap csseOrigin entries))
    }

compactionFrontier :: RelDiffFrontier Ctx RelationalPhase
compactionFrontier =
  frontierAdvanceVisibleMin
    (testTime 1 10)
    (frontierWithTraceRetention (Just emptyTraceRetention) emptyRuntimeFrontier)

readFrontier :: Int -> Word64 -> CarrierReadFrontier
readFrontier liveEpoch stamp =
  CarrierReadFrontier
    { crfQuotientEpoch = mkQuotientEpoch 1,
      crfLiveEpoch = mkLiveEpoch liveEpoch,
      crfFrontierStamp = frontierStamp (fromIntegral stamp)
    }

testTime :: Int -> Word64 -> TestTime
testTime liveEpoch stamp =
  mkRelationalCarrierTime
    contextValue
    (mkQuotientEpoch 1)
    (mkLiveEpoch liveEpoch)
    PhaseProject
    (frontierStamp (fromIntegral stamp))

testBoundary :: Either RuntimeBoundaryError Boundary
testBoundary =
  mkRuntimeBoundary [mkSlotId 0] IntSet.empty IntMap.empty

scope :: [Int] -> [Int] -> [Int] -> [Int] -> RelationalScope
scope deps topo roots results =
  relationalScopeFromSets
    (IntSet.fromList deps)
    (IntSet.fromList topo)
    (IntSet.fromList roots)
    (IntSet.fromList results)
    IntSet.empty

testLattice :: ContextLattice Ctx
testLattice =
  singletonContextLattice contextValue

sourceAddr :: CarrierAddr Ctx Carrier Prop
sourceAddr =
  carrierAddr contextValue sourceProp sourceCarrier

otherAddr :: CarrierAddr Ctx Carrier Prop
otherAddr =
  carrierAddr contextValue otherProp otherCarrier

sourceCarrier :: Carrier
sourceCarrier =
  queryRootCarrier queryId

otherCarrier :: Carrier
otherCarrier =
  queryAtomCarrier queryId (mkAtomId 7)

sourceProp :: PropositionKey Prop
sourceProp =
  PropositionKey 0

otherProp :: PropositionKey Prop
otherProp =
  PropositionKey 1

queryId :: QueryId
queryId =
  mkQueryId 77

contextValue :: Ctx
contextValue =
  1

row :: Int -> RowTupleKey
row value =
  tupleKeyFromRepKeys [RepKey value]

traceIds :: [Int] -> IntSet
traceIds =
  IntSet.fromList

expectRight :: Show error => Either error value -> IO value
expectRight value =
  case value of
    Left errorValue ->
      assertFailure (show errorValue)
    Right rightValue ->
      pure rightValue

type AssertionValue value = IO value
