module Moonlight.Flow.Runtime.CarrierDelta.FactorSpec
  ( tests,
  )
where

import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Word
  ( Word64,
  )
import Moonlight.Core
  ( initialLiveEpoch,
    initialQuotientEpoch,
  )
import Moonlight.Delta.Patch qualified as CorePatch
import Moonlight.Flow.Carrier.Core.Address
  ( CarrierAddressBook (..),
    queryBagCarrier,
    queryRootCarrier,
    querySeparatorCarrier,
  )
import Moonlight.Differential.Carrier.Address
  ( caCarrier,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDeltaP (..),
  )
import Moonlight.Delta.Signed
  ( Multiplicity (..)
  )
import Moonlight.Differential.Row.Patch
  ( ShapedPatch (..),
    positivePlainRowPatchRows
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( OriginEvent (OriginFactor),
    RelationalOrigin (..),
    emptyDerivationRoute,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    mkRelationalCarrierTime,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (PhaseProject),
  )
import Moonlight.Flow.Model.Scope
  ( DepsDelta (..),
    RelationalScope (..),
    TopoDelta (..),
  )
import Moonlight.Flow.Execution.Factor.Types
  ( FactorCache (..),
    FactorEntry (..),
    emptyFactorCache,
    factorCacheInsert,
  )
import Moonlight.Flow.Execution.Factor.Core
  ( Factor,
    mkFactor,
  )
import Moonlight.Flow.Execution.Factor.Delta
  ( FactorDelta,
  )
import Moonlight.Flow.Execution.Factor.Contribution
  ( emptyFactorContributionIndex,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvVal (..),
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( MaintenanceMetrics,
    NodeAction (..),
    NodeMaintenance (..),
    emptyRepairTelemetry,
    recordNodeMaintenance,
  )
import Moonlight.Flow.Runtime.Carrier.Emit
  ( FactorCarrierEmitSpec,
    FactorCarrierPayload (..),
    factorCarrierEmitSpec,
    factorMaintenanceDeltas,
    factorSnapshotDeltas,
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
import Moonlight.Differential.Time
  ( frontierStamp,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
import Moonlight.FiniteLattice
  ( principalSupport
  )

type Ctx = Int

type Prop = Int

type Evidence = [FactorNode]

type Boundary = [SlotId]

type TestEventTime = RelationalCarrierTime Ctx

tests :: TestTree
tests =
  testGroup
    "factor-carrier"
    [ testCase "patched bag message and root emit disjoint carrier deltas" patchedMaintenanceDeltas,
      testCase "row-stable provenance changes emit no carrier row delta" rowStableProvenanceRefreshDeltas,
      testCase "built factor op emits snapshots while reused op stays silent" builtFactorOpEmitsSnapshots,
      testCase "snapshot installation emits bag message and root carriers" snapshotDeltas
    ]

patchedMaintenanceDeltas :: IO ()
patchedMaintenanceDeltas = do
  let deltas = factorMaintenanceDeltas emitSpec (eventTime 0) queryId carrierRelationalScope patchedTrace cacheWithDeltas
  fmap (caCarrier . deAddr) deltas
    @?= [ queryBagCarrier queryId bagOne,
          querySeparatorCarrier queryId bagOne bagTwo,
          queryRootCarrier queryId
        ]
  fmap deEvidence deltas
    @?= [ [FactorNodeBag bagOne],
          [FactorNodeSeparator bagOne bagTwo],
          [FactorNodeRoot]
        ]
  fmap (positivePlainRowPatchRows . deRows) deltas
    @?= replicate 3 (Map.singleton rowSeven (Multiplicity 1))
  fmap deScope deltas
    @?= replicate 3 carrierRelationalScope
  fmap deOrigin deltas
    @?= [ RelationalOrigin {roEvent = OriginFactor queryId (FactorNodeBag bagOne), roRoute = emptyDerivationRoute},
          RelationalOrigin {roEvent = OriginFactor queryId (FactorNodeSeparator bagOne bagTwo), roRoute = emptyDerivationRoute},
          RelationalOrigin {roEvent = OriginFactor queryId FactorNodeRoot, roRoute = emptyDerivationRoute}
        ]

rowStableProvenanceRefreshDeltas :: IO ()
rowStableProvenanceRefreshDeltas = do
  let deltas = factorMaintenanceDeltas emitSpec (eventTime 0) queryId carrierRelationalScope patchedTrace cacheWithRefreshDelta
  fmap (caCarrier . deAddr) deltas
    @?= []

builtFactorOpEmitsSnapshots :: IO ()
builtFactorOpEmitsSnapshots = do
  let deltas = factorMaintenanceDeltas emitSpec (eventTime 0) queryId carrierRelationalScope builtReusedTrace cacheWithDeltas
  fmap (caCarrier . deAddr) deltas
    @?= [ queryBagCarrier queryId bagOne,
          querySeparatorCarrier queryId bagOne bagTwo,
          queryRootCarrier queryId
        ]

snapshotDeltas :: IO ()
snapshotDeltas = do
  let deltas = factorSnapshotDeltas emitSpec (eventTime 0) queryId carrierRelationalScope cacheWithDeltas
  fmap (caCarrier . deAddr) deltas
    @?= [ queryBagCarrier queryId bagOne,
          querySeparatorCarrier queryId bagOne bagTwo,
          queryRootCarrier queryId
        ]
  fmap (positivePlainRowPatchRows . deRows) deltas
    @?= replicate 3 (Map.singleton rowSeven (Multiplicity 1))

emitSpec :: FactorCarrierEmitSpec Ctx Prop Boundary Evidence
emitSpec =
  factorCarrierEmitSpec
    CarrierAddressBook
      { cabContextOfQuery = const 0,
        cabPropOfQuery = const (PropositionKey 0)
      }
    (\_ _ -> principalSupport 0)
    (\_ _ schema -> schema)
    (\_ payload -> [payloadNode payload])

payloadNode :: FactorCarrierPayload -> FactorNode
payloadNode =
  fcpNode

carrierRelationalScope :: RelationalScope
carrierRelationalScope =
  mempty
    { rsDeps = DepsDelta (IntSet.singleton 7),
      rsTopo = TopoDelta (IntSet.singleton 1)
    }

cacheWithDeltas :: FactorCache
cacheWithDeltas =
  foldr
    (\(node, entry) cache -> factorCacheInsert node entry cache)
    emptyFactorCache
    [ (FactorNodeBag bagOne, factorEntry factorValue factorDelta),
      (FactorNodeSeparator bagOne bagTwo, factorEntry factorValue factorDelta),
      (FactorNodeRoot, factorEntry factorValue factorDelta)
    ]

cacheWithRefreshDelta :: FactorCache
cacheWithRefreshDelta =
  factorCacheInsert
    (FactorNodeBag bagOne)
    (factorEntry factorValue factorRefreshDelta)
    emptyFactorCache

factorEntry :: Factor -> FactorDelta -> FactorEntry
factorEntry factorValue' deltaValue =
  FactorEntry
    { feFactor = factorValue',
      feDelta = deltaValue,
      feContributions = emptyFactorContributionIndex
    }

patchedTrace :: MaintenanceMetrics
patchedTrace =
  metricsFor
    NodePatched
    [ FactorNodeBag bagOne,
      FactorNodeSeparator bagOne bagTwo,
      FactorNodeRoot
    ]

builtReusedTrace :: MaintenanceMetrics
builtReusedTrace =
  metricsFor
    NodeBuilt
    [ FactorNodeBag bagOne,
      FactorNodeSeparator bagOne bagTwo,
      FactorNodeRoot
    ]
    <> metricsFor
      NodeReused
      [ FactorNodeBag bagTwo,
        FactorNodeRoot
      ]

metricsFor :: NodeAction -> [FactorNode] -> MaintenanceMetrics
metricsFor action =
  foldr
    ( \node ->
        recordNodeMaintenance
          node
          NodeMaintenance
            { nmAction = action,
              nmAffectedKeys = 0,
              nmRecomputedCells = 0,
              nmWorkKeys = 0,
              nmJoinRuns = 0,
              nmJoinLeaves = 0,
              nmRepairTelemetry = emptyRepairTelemetry
            }
    )
    mempty

factorDelta :: FactorDelta
factorDelta =
  ShapedPatch
    { spdShape = [slotZero],
      spdDelta = CorePatch.singleton assignmentKey (CorePatch.insert PVOne)
    }

factorRefreshDelta :: FactorDelta
factorRefreshDelta =
  ShapedPatch
    { spdShape = [slotZero],
      spdDelta = CorePatch.singleton assignmentKey (CorePatch.replace PVOne PVZero)
    }

factorValue :: Factor
factorValue =
  mkFactor [slotZero] (Map.singleton assignmentKey PVOne)

assignmentKey :: AssignmentTupleKey
assignmentKey =
  tupleKeyFromRepKeys [RepKey 7]

rowSeven :: RowTupleKey
rowSeven =
  tupleKeyFromRepKeys [RepKey 7]

slotZero :: SlotId
slotZero =
  mkSlotId 0

bagOne :: BagId
bagOne =
  BagId 1

bagTwo :: BagId
bagTwo =
  BagId 2

queryId :: QueryId
queryId =
  mkQueryId 0

eventTime :: Word64 -> TestEventTime
eventTime stamp =
  mkRelationalCarrierTime
    0
    initialQuotientEpoch
    initialLiveEpoch
    PhaseProject
    (frontierStamp (fromIntegral stamp))
