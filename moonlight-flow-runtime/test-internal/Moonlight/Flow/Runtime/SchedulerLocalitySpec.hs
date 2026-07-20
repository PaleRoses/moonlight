{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Runtime.SchedulerLocalitySpec
  ( tests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( mapMaybe,
  )
import Data.Set qualified as Set
import Data.Word
  ( Word64,
  )
import Moonlight.Delta.Time
  ( Timed (..),
  )
import Moonlight.Core
  ( initialLiveEpoch,
    initialQuotientEpoch,
  )
import Moonlight.Differential.Frontier
  ( RuntimeCapability,
    downgradeRuntimeCapability,
    frontierAdvanceVisibleMin,
    frontierPendingCounts,
    frontierTimeCompactable,
    frontierWithPendingCounts,
    mintRootRuntimeCapability,
  )
import Moonlight.Differential.Time
  ( frontierStamp,
    RuntimeTime,
    delayRuntimeTimeFeedback,
    enterRuntimeTimeScope,
    rtPhase,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    DerivedCarrierId (..),
    SubsumptionWitnessDigest (..),
    derivedCarrier,
    queryAtomCarrier,
    queryBagCarrier,
    queryRootCarrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    caProp,
    RestrictKey,
    carrierAddr,
    restrictKey,
  )
import Moonlight.Flow.Carrier.Core.Topology
  ( CarrierEdge (..),
    CarrierTopology,
    TouchKey (..),
    emptyCarrierTopology,
    insertCarrierEdge,
    insertCarrierFamily,
    insertCarrierTouch,
  )
import Moonlight.Differential.Carrier.Topology
  ( CarrierFamily,
    CarrierFamilyError,
    carrierCover,
    mkCarrierFamily,
  )
import Moonlight.Flow.Carrier.Core.Frontier
  ( RelDiffFrontier,
    emptyRelDiffFrontier,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    mkRelationalCarrierTime,
  )
import Moonlight.Flow.Model.Delta
  ( QuotientPatch (..)
  )
import Moonlight.Flow.Model.Delta
  ( atomPatchFromRowDelta
  )
import Moonlight.Differential.Row.Patch
  (
    EpochTransition (..),
    emptyPlainRowPatch,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (..),
  )
import Moonlight.Flow.Runtime.Engine.Queue.Frontier
  ( enqueueScheduledRuntimeDataflowOp,
    runtimeDataflowQueueProgressFrontier,
  )
import Moonlight.Flow.Runtime.Engine.Queue.Types
  ( RuntimeDataflowQueue,
    emptyRuntimeDataflowQueue,
    runtimeDataflowQueueFrontier,
  )
import Moonlight.Flow.Runtime.Engine.Queue.Scheduler
  ( runtimeDataflowPriorityPlan,
  )
import Moonlight.Flow.Runtime.Engine.Capability
  ( RelationalCapabilityTransport (..),
    RelationalDrainEmission (..),
    relationalDrainEmissionForOp,
    validateRelationalCapabilityTransport,
  )
import Moonlight.Flow.Runtime.Engine.Schedule.Feedback
  ( delayScheduledRuntimeDataflowOpFeedback,
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( RuntimeDataflowOp,
    RuntimeDataflowOpKey (..),
    amalgamateCarrierFamilyDataflowOp,
    deriveSubsumedCarrierDataflowOp,
    restrictCarrierDataflowOp,
    runtimeDataflowContractReads,
    runtimeDataflowContractWrites,
    runtimeDataflowOpContract,
    runtimeDataflowOpKey,
    runtimeDataflowOpProgressPointstamps,
  )
import Moonlight.Flow.Runtime.Topology.Lowering.Impact
  ( impactFromPatch,
    lowerImpactToDataflowOps,
  )
import Moonlight.Flow.Runtime.Topology.Lowering.Types
  ( RuntimeRepairRoute (..),
    RuntimeRepairRouting (..),
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( RepairProgramKey (..),
  )
import Moonlight.Flow.Runtime.Factor.Request
  ( FactorFullRepairReason (FullRepairContextInstalled),
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
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
import Moonlight.Flow.Model.Scope
import Moonlight.FiniteLattice
  ( principalSupport
  )

tests :: TestTree
tests =
  testGroup
    "scheduler locality"
    [ testCase "dirty dep schedules repair op and restriction fanout" $
        let graph =
              touchGraph TouchDep 11 (Set.singleton bagAddr) $
                insertCarrierEdge bagAddr (EdgeRestriction restrictionKey) emptyCarrierTopology
            patch =
              (patchForAtom 99)
                { qpEvents = IntMap.empty,
                  qpScope =
                    mempty
                      { rsDeps = DepsDelta (IntSet.singleton 11),
                        rsTopo = TopoDelta IntSet.empty
                      }
                }
         in assertEqual
              "dep key must only schedule the bag repair and restriction"
              ( Set.fromList
                  [ repairDataflowOpKeyFor bagAddr,
                    RestrictCarrierKey restrictionKey
                  ]
              )
              (dataflowOpKeysForPatch graph patch),
      testCase "dirty topo schedules repair op and amalgamation fanout" $
        case closureFamily of
          Left familyError ->
            assertFailure ("invalid closure family fixture: " <> show familyError)
          Right family ->
            let graph =
                  touchGraph TouchTopo 13 (Set.singleton rootAddr) $
                    insertCarrierFamily family emptyCarrierTopology
                patch =
                  (patchForAtom 99)
                    { qpEvents = IntMap.empty,
                      qpScope =
                        mempty
                          { rsDeps = DepsDelta IntSet.empty,
                            rsTopo = TopoDelta (IntSet.singleton 13)
                          }
                    }
             in assertEqual
                  "topo key must only schedule the root repair and amalgamation"
                  ( Set.fromList
                      [ repairDataflowOpKeyFor rootAddr,
                        AmalgamateCarrierFamilyKey family
                      ]
                  )
                  (dataflowOpKeysForPatch graph patch),
      testCase "derived touch root schedules owner reuse repair" $
        let graph =
              touchGraph TouchAtom 23 (Set.singleton derivedAddr) $
                insertCarrierEdge sourceAddr (EdgeSubsumption reuseId sourceAddr derivedAddr) emptyCarrierTopology
         in assertEqual
              "derived touch must use owner reuse ids"
              (Set.singleton (DeriveSubsumedCarrierKey reuseId sourceAddr derivedAddr))
              (dataflowOpKeysForPatch graph (patchForAtom 23)),
      testCase "carrier operation contracts expose restriction and subsumption carrier authority" $ do
        let restrictionContract =
              runtimeDataflowOpContract (restrictCarrierDataflowOp restrictionKey)
            subsumptionContract =
              runtimeDataflowOpContract (deriveSubsumedCarrierDataflowOp reuseId sourceAddr derivedAddr)
        assertEqual
          "restriction reads the source carrier"
          (Set.singleton bagAddr)
          (runtimeDataflowContractReads restrictionContract)
        assertEqual
          "restriction writes the target carrier"
          (Set.singleton restrictedAddr)
          (runtimeDataflowContractWrites restrictionContract)
        assertEqual
          "subsumption reads the source carrier"
          (Set.singleton sourceAddr)
          (runtimeDataflowContractReads subsumptionContract)
        assertEqual
          "subsumption writes the derived target"
          (Set.singleton derivedAddr)
          (runtimeDataflowContractWrites subsumptionContract),
      testCase "operation progress uses operation contract phase" $
        let scheduledTime =
              carrierTime 0 PhaseProject 12
            timedOp =
              Timed scheduledTime (restrictCarrierDataflowOp restrictionKey) ::
                Timed
                  (RelationalCarrierTime Int)
                  (RuntimeDataflowOp Int Int () ())
            progressTimes =
              runtimeDataflowOpProgressPointstamps timedOp
         in assertEqual
              "restriction progress must be retimed to the operation contract phase"
              [PhaseRestrict]
              (fmap rtPhase progressTimes),
      testCase "queue pending is inserted from operation progress" $ do
        let scheduledTime =
              carrierTime 0 PhaseProject 14
            progressTime =
              carrierTime 0 PhaseRestrict 14
            timedOp =
              Timed scheduledTime (restrictCarrierDataflowOp restrictionKey) ::
                Timed
                  (RelationalCarrierTime Int)
                  (RuntimeDataflowOp Int Int () ())
        withEnqueuedRuntimeDataflowOp timedOp emptyRelDiffFrontier $ \queue ->
          assertEqual
              "queue pending must use contract-derived progress, not the raw scheduled phase"
              (Map.singleton progressTime 1)
              (frontierPendingCounts (runtimeDataflowQueueFrontier queue)),
      testCase "feedback delay advances scheduled dataflow time before contract progress" $
        let scheduledTime =
              carrierTime 0 PhaseProject 18
            delayedScheduleTime =
              carrierTime 0 PhaseProject 19
            delayedProgressTime =
              carrierTime 0 PhaseRestrict 19
            timedOp =
              Timed scheduledTime (restrictCarrierDataflowOp restrictionKey) ::
                Timed
                  (RelationalCarrierTime Int)
                  (RuntimeDataflowOp Int Int () ())
         in case delayScheduledRuntimeDataflowOpFeedback timedOp of
              Nothing ->
                assertFailure "feedback delay must advance this finite frontier stamp"
              Just delayedOp -> do
                assertEqual
                  "delay advances the scheduled frontier stamp without inventing a runtime op"
                  delayedScheduleTime
                  (timedAt delayedOp)
                assertEqual
                  "delayed pending progress is still derived from the operation contract phase"
                  [delayedProgressTime]
                  (runtimeDataflowOpProgressPointstamps delayedOp)
                withEnqueuedRuntimeDataflowOp delayedOp emptyRelDiffFrontier $ \queue ->
                  assertEqual
                    "queue pending must reflect the delayed contract progress"
                    (Map.singleton delayedProgressTime 1)
                    (frontierPendingCounts (runtimeDataflowQueueFrontier queue)),
      testCase "queue progress frontier owns pending times" $ do
        let staleExternalTime =
              carrierTime 0 PhaseProject 10
            queueOwnedTime =
              carrierTime 0 PhaseSubsumption 11
            requestedFrontier =
              frontierWithPendingCounts
                (Map.singleton staleExternalTime 7)
                emptyRelDiffFrontier
            queueFrontier =
              frontierWithPendingCounts
                (Map.singleton queueOwnedTime 2)
                emptyRelDiffFrontier
        withEmptyRuntimeDataflowQueue queueFrontier $ \queue ->
          let progressFrontier =
                runtimeDataflowQueueProgressFrontier requestedFrontier queue
           in assertEqual
              "compaction must see queue-owned pending, not stale caller metadata"
              (Map.singleton queueOwnedTime 2)
              (frontierPendingCounts progressFrontier),
      testCase "nested runtime scopes isolate compaction frontiers" $
        let rootCutoff =
              carrierTime 0 PhaseProject 30
            nestedOldTime =
              enterRuntimeTimeScope 1 (carrierTime 0 PhaseProject 20)
            rootFrontier =
              frontierAdvanceVisibleMin rootCutoff emptyRelDiffFrontier
         in assertEqual
              "a root visible frontier must not compact a nested scope time"
              False
              (frontierTimeCompactable rootFrontier nestedOldTime),
      testCase "feedback delay downgrades capability only inside the same scope" $
        let rootTime =
              carrierTime 0 PhaseProject 40
            nestedTime =
              enterRuntimeTimeScope 1 (carrierTime 0 PhaseProject 41)
            delayedCapability =
              mintRootRuntimeCapability <$> delayRuntimeTimeFeedback rootTime
         in case delayedCapability of
              Nothing ->
                assertFailure "feedback delay must advance this finite frontier stamp"
              Just capability ->
                assertEqual
                  "feedback advances the carrier frontier stamp and stays downgradeable"
                  (Right (mintRootRuntimeCapability (carrierTime 0 PhaseProject 41)))
                  (downgradeRuntimeCapability (carrierTime 0 PhaseProject 41) capability)
                  *> case downgradeRuntimeCapability nestedTime (mintRootRuntimeCapability rootTime) of
                    Left _ ->
                      pure ()
                    Right _ ->
                      assertFailure "capabilities are not downgradeable across nested scope boundaries",
      testCase "restriction transport authorizes cross-context capability emission" $
        let parentCapability =
              mintRootRuntimeCapability (carrierTime 0 PhaseProject 50)
            targetTime =
              carrierTime 1 PhaseRestrict 51
         in assertEqual
              "restriction witness licenses source context 0 to target context 1"
              (Right (mintRootRuntimeCapability targetTime))
              (authorizeCapabilityTransport validateRelationalCapabilityTransport parentCapability (TransportViaRestriction restrictionKey) targetTime),
      testCase "restriction transport rejects mismatched source context" $
        let parentCapability =
              mintRootRuntimeCapability (carrierTime 2 PhaseProject 52)
            targetTime =
              carrierTime 1 PhaseRestrict 53
         in case authorizeCapabilityTransport validateRelationalCapabilityTransport parentCapability (TransportViaRestriction restrictionKey) targetTime of
              Left _ ->
                pure ()
              Right _ ->
                assertFailure "restriction transport accepted a parent capability from the wrong source context",
      testCase "cross-context restriction followup is emitted as transport, not pass-through" $
        let parentCapability =
              mintRootRuntimeCapability (carrierTime 0 PhaseProject 54)
            targetTime =
              carrierTime 1 PhaseRestrict 55
         in assertEqual
              "restrict carrier op must carry its restriction witness into drain authorization"
              (Right (EmitTransport (TransportViaRestriction restrictionKey) targetTime))
              (relationalDrainEmissionForOp parentCapability targetTime (restrictCarrierDataflowOp restrictionKey)),
      testCase "amalgamation transport authorizes cover-member capability emission" $
        case closureFamily of
          Left familyError ->
            assertFailure ("invalid closure family fixture: " <> show familyError)
          Right family ->
            let parentCapability =
                  mintRootRuntimeCapability (carrierTime 1 PhaseRestrict 56)
                targetTime =
                  carrierTime 2 PhaseAmalgamate 57
             in assertEqual
                  "amalgamation witness licenses member context 1 to cover target context 2"
                  (Right (mintRootRuntimeCapability targetTime))
                  (authorizeCapabilityTransport validateRelationalCapabilityTransport parentCapability (TransportViaAmalgamation family) targetTime),
      testCase "cross-context amalgamation followup is emitted as transport, not pass-through" $
        case closureFamily of
          Left familyError ->
            assertFailure ("invalid closure family fixture: " <> show familyError)
          Right family ->
            let parentCapability =
                  mintRootRuntimeCapability (carrierTime 0 PhaseRestrict 58)
                targetTime =
                  carrierTime 2 PhaseAmalgamate 59
             in assertEqual
                  "amalgamation op must carry its family witness into drain authorization"
                  (Right (EmitTransport (TransportViaAmalgamation family) targetTime))
                  (relationalDrainEmissionForOp parentCapability targetTime (amalgamateCarrierFamilyDataflowOp family))
    ]

withEmptyRuntimeDataflowQueue ::
  RelDiffFrontier Int RelationalPhase ->
  (RuntimeDataflowQueue Int Int () () -> IO ()) ->
  IO ()
withEmptyRuntimeDataflowQueue frontier continuation =
  either
    (assertFailure . ("invalid runtime dataflow priority plan: " <>) . show)
    (\priorityPlan -> continuation (emptyRuntimeDataflowQueue priorityPlan frontier))
    runtimeDataflowPriorityPlan

withEnqueuedRuntimeDataflowOp ::
  Timed (RelationalCarrierTime Int) (RuntimeDataflowOp Int Int () ()) ->
  RelDiffFrontier Int RelationalPhase ->
  (RuntimeDataflowQueue Int Int () () -> IO ()) ->
  IO ()
withEnqueuedRuntimeDataflowOp timedOp frontier continuation =
  withEmptyRuntimeDataflowQueue frontier $ \queue ->
    either
      (assertFailure . ("runtime dataflow enqueue failed: " <>) . show)
      continuation
      (enqueueScheduledRuntimeDataflowOp timedOp queue)


touchGraph ::
  (Int -> TouchKey) ->
  Int ->
  Set.Set (CarrierAddr Int Carrier Int) ->
  CarrierTopology Int Carrier Int ->
  CarrierTopology Int Carrier Int
touchGraph touch key addrs graph0 =
  Set.foldl'
    (\graph addr -> insertCarrierTouch (touch key) addr graph)
    graph0
    addrs

repairDataflowOpKeyFor ::
  CarrierAddr Int Carrier Int ->
  RuntimeDataflowOpKey Int Int
repairDataflowOpKeyFor addr =
  RepairFactorBatchKey (caContext addr) (caProp addr)

dataflowOpKeysForPatch ::
  CarrierTopology Int Carrier Int ->
  QuotientPatch ->
  Set.Set (RuntimeDataflowOpKey Int Int)
dataflowOpKeysForPatch graph patch =
  Set.fromList
    ( mapMaybe
        runtimeDataflowOpKey
        ( lowerImpactToDataflowOps
            schedulerRepairRouting
            FullRepairContextInstalled
            graph
            (impactFromPatch patch)
        )
    )

schedulerRepairRouting :: RuntimeRepairRouting
schedulerRepairRouting =
  RuntimeRepairRouting
    { rrRepairRouteOfQuery =
        \candidateQueryId ->
          if candidateQueryId == queryId
            then
              Just
                RuntimeRepairRoute
                  { rrtRepairKey = repairKey,
                    rrtRepresentativeQueryId = queryId
                  }
            else Nothing,
      rrRepairIsCold = const False
    }

patchForAtom ::
  Int ->
  QuotientPatch
patchForAtom atomKey =
  QuotientPatch
    { qpEpoch = EpochTransition { etBefore = initialQuotientEpoch, etAfter = initialQuotientEpoch },
      qpScope =
        mempty
          { rsDeps = DepsDelta IntSet.empty,
            rsTopo = TopoDelta IntSet.empty
          },
      qpAtomScopeByAtom = IntMap.empty,
      qpEvents =
        IntMap.singleton
          atomKey
          (atomPatchFromRowDelta emptyPlainRowPatch)
    }

carrierTime ::
  Int ->
  RelationalPhase ->
  Word64 ->
  RelationalCarrierTime Int
carrierTime contextValue phaseValue stamp =
  mkRelationalCarrierTime
    contextValue
    initialQuotientEpoch
    initialLiveEpoch
    phaseValue
    (frontierStamp (fromIntegral stamp))

authorizeCapabilityTransport ::
  (RuntimeCapability ctx epoch phase -> witness -> RuntimeTime ctx epoch phase -> Either err ()) ->
  RuntimeCapability ctx epoch phase ->
  witness ->
  RuntimeTime ctx epoch phase ->
  Either err (RuntimeCapability ctx epoch phase)
authorizeCapabilityTransport validateTransport capability witness targetTime =
  case validateTransport capability witness targetTime of
    Left err ->
      Left err
    Right () ->
      Right (mintRootRuntimeCapability targetTime)

queryId :: QueryId
queryId = mkQueryId 0

repairKey :: RepairProgramKey
repairKey = RepairProgramKey (StableDigest128 31 32)

atomId :: AtomId
atomId = mkAtomId 0

bagId :: BagId
bagId = BagId 0

propKey :: PropositionKey Int
propKey = PropositionKey 0

atomAddr :: CarrierAddr Int Carrier Int
atomAddr =
  carrierAddr (0 :: Int) propKey (queryAtomCarrier queryId atomId)

bagAddr :: CarrierAddr Int Carrier Int
bagAddr =
  carrierAddr (0 :: Int) propKey (queryBagCarrier queryId bagId)

rootAddr :: CarrierAddr Int Carrier Int
rootAddr =
  carrierAddr (0 :: Int) propKey (queryRootCarrier queryId)

restrictedAddr :: CarrierAddr Int Carrier Int
restrictedAddr =
  carrierAddr (1 :: Int) propKey (queryRootCarrier queryId)

restrictionKey :: RestrictKey Int Carrier Int
restrictionKey =
  restrictKey bagAddr restrictedAddr

closureFamily :: Either (CarrierFamilyError Int Carrier Int) (CarrierFamily Int Carrier Int)
closureFamily =
  mkCarrierFamily targetAddr cover members
  where
    targetAddr =
      rootAddr {caContext = 2}

    cover =
      carrierCover 2 (Set.fromList [0, 1]) True (principalSupport 2)

    members =
      Set.fromList [rootAddr, restrictedAddr]

sourceAddr :: CarrierAddr Int Carrier Int
sourceAddr =
  carrierAddr
    (0 :: Int)
    propKey
    (derivedCarrier (DerivedCarrierId (SubsumptionWitnessDigest (StableDigest128 1 2)) (StableDigest128 3 4)))

derivedAddr :: CarrierAddr Int Carrier Int
derivedAddr =
  carrierAddr
    (0 :: Int)
    propKey
    (derivedCarrier (DerivedCarrierId (SubsumptionWitnessDigest (StableDigest128 5 6)) (StableDigest128 7 8)))

reuseId :: CarrierReuseId Int Int
reuseId =
  CarrierReuseId
    { cridPayload =
        CarrierReuseKeyPayload
          { crkpSource = sourceAddr,
            crkpWitnessTarget = derivedAddr,
            crkpExpectedTarget = Just derivedAddr,
            crkpWitnessDigest = SubsumptionWitnessDigest (StableDigest128 9 10),
            crkpSourceShapeDigest = StableDigest128 11 12,
            crkpTargetShapeDigest = StableDigest128 13 14,
            crkpTargetBoundaryDigest = StableDigest128 15 16,
            crkpTargetViewDigest = Nothing,
            crkpCoverageRuleDigest = StableDigest128 17 18
          },
      cridDigest = StableDigest128 19 20
    }
