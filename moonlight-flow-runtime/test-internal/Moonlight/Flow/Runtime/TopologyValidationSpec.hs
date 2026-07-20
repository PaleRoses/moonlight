module Moonlight.Flow.Runtime.TopologyValidationSpec
  ( tests,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word
  ( Word64,
  )
import Moonlight.Core
  ( QueryId,
    mkAtomId,
    mkQueryId,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( RuntimeRouting,
    Shard (..),
  )
import Moonlight.Flow.Runtime.Topology.Routing.Internal
  ( mkRuntimeRouting,
  )
import Moonlight.Flow.Carrier.Core.Topology.Validate
  ( CarrierTopologyValidationError (..),
  )
import Moonlight.Flow.Runtime.Topology.Validate
  ( RuntimeTopologyBindingError (..),
    RuntimeTopologyValidationError (..),
    validateRuntimeTopology,
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
    RestrictKey,
    carrierAddr,
    restrictKey,
  )
import Moonlight.Flow.Carrier.Morphism.Core.Program
  ( CarrierMorphismRuntime,
    emptyCarrierMorphismRuntime,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
import Moonlight.Flow.Plan.Query.Core
  ( BagId (..),
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
  )
import Moonlight.Flow.Carrier.Core.Topology
  ( CarrierEdge (..),
    CarrierTopology,
    TouchKey (..),
    emptyCarrierTopology,
    insertCarrierEdge,
    insertCarrierTouch,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "runtime topology validation"
    [ testCase "reports edge anchor mismatches" edgeAnchorMismatchAssertion,
      testCase "reports unregistered factor carriers" unregisteredFactorCarrierAssertion,
      testCase "reports unowned derived touch roots" unownedDerivedTouchRootAssertion,
      testCase "reports duplicate derived carrier owners" duplicateDerivedCarrierOwnersAssertion,
      testCase "reports missing index routes" missingIndexRouteAssertion,
      testCase "reports missing index shards" missingIndexShardAssertion,
      testCase "reports missing restrict routes" missingRestrictRouteAssertion,
      testCase "reports missing restrict shards" missingRestrictShardAssertion,
      testCase "reports missing restriction programs" missingRestrictionProgramAssertion
    ]

edgeAnchorMismatchAssertion :: Assertion
edgeAnchorMismatchAssertion = do
  routing <-
    routingFromRoutes
      (Map.singleton bagAddr shard0)
      (indexRoutes [atomAddr, bagAddr, restrictedAddr])
  assertValidationContains
    (RuntimeTopologyIntrinsicInvalid (CarrierTopologyEdgeAnchorMismatch atomAddr (EdgeRestriction restrictionKey)))
    routing
    (insertCarrierEdge atomAddr (EdgeRestriction restrictionKey) emptyCarrierTopology)
    (IntMap.singleton 0 emptyCarrierMorphismRuntime)
    liveIndexShards

unregisteredFactorCarrierAssertion :: Assertion
unregisteredFactorCarrierAssertion = do
  routing <-
    routingFromRoutes
      Map.empty
      (indexRoutes [rootAddr])
  assertValidationContains
    (RuntimeTopologyBindingInvalid (RuntimeTopologyUnregisteredFactorCarrier rootAddr queryId))
    routing
    (insertCarrierTouch (TouchAtom 0) rootAddr emptyCarrierTopology)
    IntMap.empty
    liveIndexShards

unownedDerivedTouchRootAssertion :: Assertion
unownedDerivedTouchRootAssertion = do
  routing <-
    routingFromRoutes
      Map.empty
      (indexRoutes [derivedAddr])
  assertValidationContains
    (RuntimeTopologyIntrinsicInvalid (CarrierTopologyUnownedDerivedTouchRoot derivedAddr))
    routing
    (insertCarrierTouch (TouchAtom 7) derivedAddr emptyCarrierTopology)
    IntMap.empty
    liveIndexShards

duplicateDerivedCarrierOwnersAssertion :: Assertion
duplicateDerivedCarrierOwnersAssertion = do
  routing <-
    routingFromRoutes
      Map.empty
      (indexRoutes [sourceAddr, alternateSourceAddr, derivedAddr])
  assertValidationContains
    ( RuntimeTopologyIntrinsicInvalid
        ( CarrierTopologyDuplicateDerivedCarrierOwners
            derivedAddr
            ( Set.fromList
                [ (reuseId, sourceAddr),
                  (alternateReuseId, alternateSourceAddr)
                ]
            )
        )
    )
    routing
    ( insertCarrierEdge alternateSourceAddr (EdgeSubsumption alternateReuseId alternateSourceAddr derivedAddr) $
        insertCarrierEdge sourceAddr (EdgeSubsumption reuseId sourceAddr derivedAddr) emptyCarrierTopology
    )
    IntMap.empty
    liveIndexShards

missingIndexRouteAssertion :: Assertion
missingIndexRouteAssertion = do
  routing <-
    routingFromRoutes Map.empty Map.empty
  assertValidationContains
    (RuntimeTopologyBindingInvalid (RuntimeTopologyMissingIndexRoute atomAddr))
    routing
    (insertCarrierTouch (TouchAtom 0) atomAddr emptyCarrierTopology)
    IntMap.empty
    IntMap.empty

missingIndexShardAssertion :: Assertion
missingIndexShardAssertion = do
  routing <-
    routingFromRoutes
      Map.empty
      (indexRoutes [atomAddr])
  assertValidationContains
    (RuntimeTopologyBindingInvalid (RuntimeTopologyMissingIndexShard atomAddr shard0))
    routing
    (insertCarrierTouch (TouchAtom 0) atomAddr emptyCarrierTopology)
    IntMap.empty
    IntMap.empty

missingRestrictRouteAssertion :: Assertion
missingRestrictRouteAssertion = do
  routing <-
    routingFromRoutes
      Map.empty
      (indexRoutes [bagAddr, restrictedAddr])
  assertValidationContains
    (RuntimeTopologyBindingInvalid (RuntimeTopologyMissingRestrictRoute bagAddr))
    routing
    (insertCarrierEdge bagAddr (EdgeRestriction restrictionKey) emptyCarrierTopology)
    IntMap.empty
    liveIndexShards

missingRestrictShardAssertion :: Assertion
missingRestrictShardAssertion = do
  routing <-
    routingFromRoutes
      (Map.singleton bagAddr shard0)
      (indexRoutes [bagAddr, restrictedAddr])
  assertValidationContains
    (RuntimeTopologyBindingInvalid (RuntimeTopologyMissingRestrictShard bagAddr shard0))
    routing
    (insertCarrierEdge bagAddr (EdgeRestriction restrictionKey) emptyCarrierTopology)
    IntMap.empty
    liveIndexShards

missingRestrictionProgramAssertion :: Assertion
missingRestrictionProgramAssertion = do
  routing <-
    routingFromRoutes
      (Map.singleton bagAddr shard0)
      (indexRoutes [bagAddr, restrictedAddr])
  assertValidationContains
    (RuntimeTopologyBindingInvalid (RuntimeTopologyMissingRestrictionProgram restrictionKey shard0))
    routing
    (insertCarrierEdge bagAddr (EdgeRestriction restrictionKey) emptyCarrierTopology)
    (IntMap.singleton 0 emptyCarrierMorphismRuntime)
    liveIndexShards

assertValidationContains ::
  RuntimeTopologyValidationError Int Int ->
  RuntimeRouting Int Int ->
  CarrierTopology Int Carrier Int ->
  IntMap (CarrierMorphismRuntime Int Carrier Int () ()) ->
  IntMap () ->
  Assertion
assertValidationContains expectedError routing graph restrictStates indexStates =
  case validateRuntimeTopology routing Set.empty restrictStates indexStates graph of
    Right () ->
      assertFailure ("expected topology validation error: " <> show expectedError)
    Left errors ->
      assertBool
        ("expected " <> show expectedError <> " in " <> show errors)
        (expectedError `elem` errors)

routingFromRoutes ::
  Map (CarrierAddr Int Carrier Int) Shard ->
  Map (CarrierAddr Int Carrier Int) Shard ->
  IO (RuntimeRouting Int Int)
routingFromRoutes restrictRoutes indexRouteMap =
  shouldRight
    ( mkRuntimeRouting
        IntMap.empty
        (emptyCarrierTopology :: CarrierTopology Int Carrier Int)
        IntMap.empty
        IntMap.empty
        restrictRoutes
        indexRouteMap
        Map.empty
    )

shouldRight :: Show err => Either err value -> IO value
shouldRight result =
  case result of
    Left err ->
      assertFailure (show err)
    Right value ->
      pure value

indexRoutes ::
  [CarrierAddr Int Carrier Int] ->
  Map (CarrierAddr Int Carrier Int) Shard
indexRoutes =
  Map.fromList . fmap (\addr -> (addr, shard0))

liveIndexShards :: IntMap ()
liveIndexShards =
  IntMap.singleton 0 ()

shard0 :: Shard
shard0 =
  Shard 0

queryId :: QueryId
queryId =
  mkQueryId 0

propKey :: PropositionKey Int
propKey =
  PropositionKey 0

atomAddr :: CarrierAddr Int Carrier Int
atomAddr =
  carrierAddr 0 propKey (queryAtomCarrier queryId (mkAtomId 0))

bagAddr :: CarrierAddr Int Carrier Int
bagAddr =
  carrierAddr 0 propKey (queryBagCarrier queryId (BagId 0))

rootAddr :: CarrierAddr Int Carrier Int
rootAddr =
  carrierAddr 0 propKey (queryRootCarrier queryId)

restrictedAddr :: CarrierAddr Int Carrier Int
restrictedAddr =
  carrierAddr 1 propKey (queryRootCarrier queryId)

restrictionKey :: RestrictKey Int Carrier Int
restrictionKey =
  restrictKey bagAddr restrictedAddr

sourceAddr :: CarrierAddr Int Carrier Int
sourceAddr =
  derivedCarrierAddr 0 (StableDigest128 1 2) (StableDigest128 3 4)

alternateSourceAddr :: CarrierAddr Int Carrier Int
alternateSourceAddr =
  derivedCarrierAddr 0 (StableDigest128 5 6) (StableDigest128 7 8)

derivedAddr :: CarrierAddr Int Carrier Int
derivedAddr =
  derivedCarrierAddr 1 (StableDigest128 9 10) (StableDigest128 11 12)

derivedCarrierAddr ::
  Int ->
  StableDigest128 ->
  StableDigest128 ->
  CarrierAddr Int Carrier Int
derivedCarrierAddr contextValue witnessDigest shapeDigest =
  carrierAddr
    contextValue
    propKey
    ( derivedCarrier
        DerivedCarrierId
          { dciWitness = SubsumptionWitnessDigest witnessDigest,
            dciShape = shapeDigest
          }
    )

reuseId :: CarrierReuseId Int Int
reuseId =
  reuseIdFromWords 13 14 sourceAddr derivedAddr

alternateReuseId :: CarrierReuseId Int Int
alternateReuseId =
  reuseIdFromWords 15 16 alternateSourceAddr derivedAddr

reuseIdFromWords ::
  Word64 ->
  Word64 ->
  CarrierAddr Int Carrier Int ->
  CarrierAddr Int Carrier Int ->
  CarrierReuseId Int Int
reuseIdFromWords digestLeft digestRight source target =
  CarrierReuseId
    { cridPayload =
        CarrierReuseKeyPayload
          { crkpSource = source,
            crkpWitnessTarget = target,
            crkpExpectedTarget = Just target,
            crkpWitnessDigest = SubsumptionWitnessDigest (StableDigest128 17 18),
            crkpSourceShapeDigest = StableDigest128 19 20,
            crkpTargetShapeDigest = StableDigest128 21 22,
            crkpTargetBoundaryDigest = StableDigest128 23 24,
            crkpTargetViewDigest = Nothing,
            crkpCoverageRuleDigest = StableDigest128 25 26
          },
      cridDigest = StableDigest128 digestLeft digestRight
    }
