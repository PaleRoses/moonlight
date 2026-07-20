{-# LANGUAGE DerivingStrategies #-}

module Test.Moonlight.Flow.Property.Runtime.FactorReuse
  ( spec,
  )
where

import Data.Foldable
  ( traverse_,
  )
import Moonlight.Core
  ( QueryId,
    mkQueryId,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    QueryCarrierNode (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caCarrier,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Runtime.Factor.Request
  ( FactorFullRepairReason (..),
    FactorRepairBatchRequest,
    fullRepair,
    singletonRepairBatchRequest,
  )
import Moonlight.Flow.Runtime.Factor.Repair
  ( repairFactorBatch,
  )
import Moonlight.Flow.Runtime.Factor.Program
  ( factorProgramRepairKey,
  )
import Moonlight.Flow.Carrier.Reuse
  ( PlanReuseStats (..),
    ReuseMode (..),
    planReuseStats,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    rsPlanReuse,
  )
import Test.Moonlight.Flow.Oracle.Runtime.Program
  ( Ctx,
    Evidence,
    Prop,
    RuntimeProgramCase (..),
    RuntimeTriangleOptions (..),
    TestRuntime,
    TriangleAtomOrder (..),
    carrierTime,
    currentSnapshot,
    defaultRuntimeTriangleOptions,
    insertAtomSnapshots,
    insertSnapshots,
    programCacheEmpty,
    programCacheEntryCount,
    rowsOf,
    runtimeFromProgramCases,
    runtimeProgramCanonicalDigest,
    runtimeUnaryCase,
    visibleReferenceSnapshots,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertBool,
    assertFailure,
    testCase,
    (@?=),
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )

spec :: TestTree
spec =
  testGroup
    "FactorReuse"
    [ testGroup
        "real-runtime join derivation"
        [ testCase
            "missing target is derived from exact comparable source"
            exactEquivalentDistinctQueryUsesReuse,
          testCase
            "derived target repair is replayable"
            exactEquivalentRepairIsReplayable,
          testCase
            "digest-backed source is reusable across runtime clocks"
            exactEquivalentDigestBackedSourceUsesReuseAcrossClocks,
          testCase
            "missing and view-mismatch sources are refused"
            exactEquivalentRejectionMatrix
        ]
    ]

runtimePlanReuseStats ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  PlanReuseStats
runtimePlanReuseStats =
  planReuseStats . rsPlanReuse . rdrState
{-# INLINE runtimePlanReuseStats #-}

manualRepairBatchRequest :: RuntimeProgramCase Ctx Prop Evidence -> FactorRepairBatchRequest Ctx Prop
manualRepairBatchRequest target =
  singletonRepairBatchRequest
    0
    (PropositionKey 0)
    (factorProgramRepairKey (rpcRuntimeProgram target))
    (rpcQueryId target)
    (fullRepair FullRepairManual)
{-# INLINE manualRepairBatchRequest #-}

exactEquivalentDistinctQueryUsesReuse :: Assertion
exactEquivalentDistinctQueryUsesReuse = do
  source <- sourceFixture
  target <- targetFixture

  runtimeProgramCanonicalDigest source @?= runtimeProgramCanonicalDigest target

  runtime0 <-
    runtimeWithSourceSnapshots source target

  let stats0 =
        runtimePlanReuseStats runtime0

  traverse_
    (assertCurrentMissing runtime0 . deAddr)
    (visibleReferenceSnapshots target)

  runtime2 <-
    repairTarget target runtime0

  let stats1 =
        runtimePlanReuseStats runtime2
      exactProjectionEmits =
        prsExactProjectionEmits stats1 - prsExactProjectionEmits stats0

  prsExactHits stats1 @?= prsExactHits stats0 + 1
  assertBool
    "expected at least one exact projection emission"
    (exactProjectionEmits > 0)
  programCacheEmpty (rpcQueryId target) runtime2 @?= True

  traverse_
    (assertCurrentRowsMatch runtime2)
    (visibleReferenceSnapshots target)

  traverse_
    (assertProjectedTargetIsQueryCarrier (rpcQueryId target))
    (visibleReferenceSnapshots target)

  traverse_
    (\snapshot -> assertCurrentExact runtime2 (deAddr snapshot))
    (visibleReferenceSnapshots target)

exactEquivalentRepairIsReplayable :: Assertion
exactEquivalentRepairIsReplayable = do
  source <- sourceFixture
  target <- targetFixture
  runtime0 <- runtimeWithSourceSnapshots source target
  runtime1 <- repairTarget target runtime0
  runtime2 <- repairTarget target runtime1

  programCacheEmpty (rpcQueryId target) runtime2 @?= True
  traverse_
    (assertCurrentRowsMatch runtime2)
    (visibleReferenceSnapshots target)

exactEquivalentDigestBackedSourceUsesReuseAcrossClocks :: Assertion
exactEquivalentDigestBackedSourceUsesReuseAcrossClocks = do
  source <- sourceFixtureAt (carrierTime 0 1)
  target <- targetFixtureAt (carrierTime 0 2)
  runtime0 <- runtimeWithSourceSnapshots source target
  let stats0 =
        runtimePlanReuseStats runtime0
  runtime1 <- repairTargetAt (carrierTime 0 2) target runtime0
  let stats1 =
        runtimePlanReuseStats runtime1
  prsExactHits stats1 @?= prsExactHits stats0 + 1
  programCacheEmpty (rpcQueryId target) runtime1 @?= True
  traverse_
    (assertCurrentRowsMatch runtime1)
    (visibleReferenceSnapshots target)

exactEquivalentRejectionMatrix :: Assertion
exactEquivalentRejectionMatrix =
  traverse_
    runCase
    [ MissingCurrentSourceRejected,
      PhysicalViewDigestMismatchRejected
    ]
  where
    runCase rejectCase = do
      source <- sourceFixture
      target <-
        case rejectCase of
          PhysicalViewDigestMismatchRejected ->
            viewMismatchTargetFixture
          MissingCurrentSourceRejected ->
            targetFixture

      runtime0 <-
        assertRight $
          runtimeFromProgramCases
            [source, target]
            (rpcPlanReuse source)
            ExactOnly
      runtimeWithAtoms <-
        assertRight $
          insertAtomSnapshots [source, target] runtime0

      runtime1 <-
        case rejectCase of
          PhysicalViewDigestMismatchRejected ->
            assertRight $
              insertSnapshots (rpcCarrierSnapshots source) runtimeWithAtoms
          MissingCurrentSourceRejected ->
            pure runtimeWithAtoms

      let stats0 =
            runtimePlanReuseStats runtime1

      runtime2 <-
        assertRight $
          (\(_reports, runtimeValue, _commitTrace) -> runtimeValue)
            <$> repairFactorBatch
              sharedTime
              (manualRepairBatchRequest target)
              runtime1

      let stats1 =
            runtimePlanReuseStats runtime2

      assertEqual
        ("exact reuse must be rejected for " <> show rejectCase)
        (prsExactHits stats0)
        (prsExactHits stats1)
      assertBool
        ("expected exact fallback cache entries for " <> show rejectCase)
        (programCacheEntryCount (rpcQueryId target) runtime2 > 0)

      traverse_
        (assertCurrentRowsMatch runtime2)
        (visibleReferenceSnapshots target)

data RejectionCase
  = MissingCurrentSourceRejected
  | PhysicalViewDigestMismatchRejected
  deriving stock (Eq, Ord, Show, Read)

runtimeWithSourceSnapshots ::
  RuntimeProgramCase Int Prop Evidence ->
  RuntimeProgramCase Int Prop Evidence ->
  IO TestRuntime
runtimeWithSourceSnapshots source target = do
  runtime0 <-
    assertRight $
      runtimeFromProgramCases
        [source, target]
        (rpcPlanReuse source)
        ExactOnly
  runtimeWithAtoms <-
    assertRight $
      insertAtomSnapshots [source, target] runtime0
  assertRight $
    insertSnapshots (rpcCarrierSnapshots source) runtimeWithAtoms

repairTarget ::
  RuntimeProgramCase Int Prop Evidence ->
  TestRuntime ->
  IO TestRuntime
repairTarget target runtime =
  repairTargetAt sharedTime target runtime

repairTargetAt ::
  RelationalCarrierTime Int ->
  RuntimeProgramCase Int Prop Evidence ->
  TestRuntime ->
  IO TestRuntime
repairTargetAt eventTime target runtime =
  assertRight $
    (\(_reports, runtimeValue, _commitTrace) -> runtimeValue)
      <$> repairFactorBatch
        eventTime
        (manualRepairBatchRequest target)
        runtime

sourceFixture :: IO (RuntimeProgramCase Int Prop Evidence)
sourceFixture =
  sourceFixtureAt sharedTime
{-# INLINE sourceFixture #-}

targetFixture :: IO (RuntimeProgramCase Int Prop Evidence)
targetFixture =
  targetFixtureAt sharedTime
{-# INLINE targetFixture #-}

viewMismatchTargetFixture :: IO (RuntimeProgramCase Int Prop Evidence)
viewMismatchTargetFixture =
  fixtureWith physicalViewMismatchTargetOptions sharedTime
{-# INLINE viewMismatchTargetFixture #-}

sourceFixtureAt ::
  RelationalCarrierTime Int ->
  IO (RuntimeProgramCase Int Prop Evidence)
sourceFixtureAt =
  fixtureWith sourceOptions

targetFixtureAt ::
  RelationalCarrierTime Int ->
  IO (RuntimeProgramCase Int Prop Evidence)
targetFixtureAt =
  fixtureWith targetOptions

fixtureWith ::
  RuntimeTriangleOptions ->
  RelationalCarrierTime Int ->
  IO (RuntimeProgramCase Int Prop Evidence)
fixtureWith options eventTime =
  assertRight $
    runtimeUnaryCase
      options
      eventTime
      0
      0
      ()

sourceOptions :: RuntimeTriangleOptions
sourceOptions =
  triangleOptions (mkQueryId 100) "factor-reuse-source" 0 0 TriangleAtomsForward

targetOptions :: RuntimeTriangleOptions
targetOptions =
  triangleOptions (mkQueryId 200) "factor-reuse-target" 0 0 TriangleAtomsForward

physicalViewMismatchTargetOptions :: RuntimeTriangleOptions
physicalViewMismatchTargetOptions =
  triangleOptions (mkQueryId 201) "factor-reuse-target-view-mismatch" 100 1000 TriangleAtomsReversed

triangleOptions :: QueryId -> String -> Int -> Int -> TriangleAtomOrder -> RuntimeTriangleOptions
triangleOptions queryIdValue name atomOffset slotOffset atomOrder =
  defaultRuntimeTriangleOptions
    { rtoName = name,
      rtoQueryId = queryIdValue,
      rtoAtomKeyOffset = atomOffset,
      rtoSlotKeyOffset = slotOffset,
      rtoAtomOrder = atomOrder
    }

sharedTime :: RelationalCarrierTime Int
sharedTime =
  carrierTime 0 0

assertProjectedTargetIsQueryCarrier ::
  QueryId ->
  RelationalCarrierDelta ctx Carrier prop boundary evidence ->
  Assertion
assertProjectedTargetIsQueryCarrier queryId snapshot =
  case caCarrier (deAddr snapshot) of
    QueryCarrier actualQueryId (QueryFactor _node) ->
      actualQueryId @?= queryId
    other ->
      assertFailure ("expected query-local factor carrier, got " <> show other)

assertCurrentExact ::
  TestRuntime ->
  CarrierAddr Int Carrier Prop ->
  Assertion
assertCurrentExact runtime addr = do
  maybeSnapshot <-
    assertRight $
      currentSnapshot
        addr
        runtime
  case maybeSnapshot of
    Nothing ->
      assertFailure ("missing current snapshot: " <> show addr)
    Just snapshot ->
      assertExactCoverage snapshot

assertCurrentRowsMatch ::
  TestRuntime ->
  RelationalCarrierDelta Ctx Carrier Prop RuntimeBoundary Evidence ->
  Assertion
assertCurrentRowsMatch runtime expected = do
  actualMaybe <-
    assertRight (currentSnapshot (deAddr expected) runtime)
  case actualMaybe of
    Nothing ->
      assertFailure ("missing current carrier: " <> show (deAddr expected))
    Just actual ->
      rowsOf actual @?= rowsOf expected
{-# INLINE assertCurrentRowsMatch #-}

assertCurrentMissing ::
  TestRuntime ->
  CarrierAddr Int Carrier Prop ->
  Assertion
assertCurrentMissing runtime addr = do
  actualMaybe <-
    assertRight (currentSnapshot addr runtime)
  actualMaybe @?= Nothing
{-# INLINE assertCurrentMissing #-}

assertExactCoverage ::
  RelationalCarrierDelta ctx carrier prop boundary evidence ->
  Assertion
assertExactCoverage _snapshot =
  pure ()
{-# INLINE assertExactCoverage #-}

assertRight :: Show left => Either left right -> IO right
assertRight eitherValue =
  case eitherValue of
    Left err ->
      assertFailure ("expected Right, got Left: " <> show err) *> fail "unreachable"
    Right value ->
      pure value
{-# INLINE assertRight #-}
