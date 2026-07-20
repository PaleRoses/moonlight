{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Runtime.RbacIncrementalStatsSpec
  ( tests,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Flow.Execution.Factor.Run
  ( FactorRunError (..),
    runFactor,
  )
import Moonlight.Flow.Execution.Factor.Types
  ( FactorCache (..),
    FactorEntry (..),
    FactorInput (..),
    FactorRunResult (..),
    FactorRunSpec (..),
    FactorDemand (FactorDemandMaintenance),
    emptyFactorCache,
  )
import Moonlight.Flow.Execution.Observe.Provenance.GC
  ( defaultProvGCConfig,
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( defaultRepairTelemetryConfig,
    MaintenanceMetrics,
    NodeAction (..),
    maintenanceActionCount,
    maintenanceAffectedKeyCount,
    maintenanceRecomputedCellCount,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchFromList
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    tupleKeyFromInts,
    tupleKeyToInts,
  )
import Moonlight.Core qualified as R
import Moonlight.Differential.Proposition qualified as R
import Moonlight.Flow.Patch qualified as R
import Moonlight.Flow.Query qualified as R
import Moonlight.Flow.Runtime.Apply qualified as R
import Moonlight.Flow.Runtime.Create qualified as R
import Moonlight.Flow.Runtime.Inspect qualified as R
import Moonlight.Flow.Runtime.Spec.Schema qualified as R
import Moonlight.Flow.Runtime.Types qualified as R
import Moonlight.Flow.Plan.Query.Core
  ( BagId (..),
    DecompPlan,
    FactorNode (..),
    SlotId,
    mkAtomId,
    mkDecompBag,
    mkDecompPlan,
    mkSlotId,
  )
import Moonlight.Differential.Row.Block
import Moonlight.Flow.Model.RowIdentity
  ( rowBlockIdentityForAtom,
  )
import Moonlight.Differential.Index.IndexedRows
  ( indexedRowsPayloadMap,
  )
import Moonlight.Flow.Storage.Relation
  ( atomRowsFromTupleKeys,
    relationFromAtomRows,
    RelationPatchError,
  )
import Moonlight.Flow.Storage.Store
  ( Store,
    storeFromRelations,
  )
import Moonlight.Flow.Storage.View
  ( unrestrictedView,
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
    (@?=),
  )

type RowsByAtom = IntMap (Set RowTupleKey)

data RbacIncrementalStats = RbacIncrementalStats
  { risNodesPatched :: !Int,
    risNodesBuilt :: !Int,
    risAffectedKeys :: !Int,
    risRecomputedCells :: !Int
  }
  deriving stock (Eq, Show)

data RbacIncrementalStatsFailure
  = RbacUnknownAtom !Int
  | RbacRowsFailed !RowBuildError
  | RbacRelationFailed !RelationPatchError
  | RbacFactorFailed !FactorRunError
  deriving stock (Show)

data RbacIncrementalStatsReport = RbacIncrementalStatsReport
  { risrStats :: !RbacIncrementalStats,
    risrIncrementalFactorRows :: !(Map.Map FactorNode (Set [Int])),
    risrFreshFactorRows :: !(Map.Map FactorNode (Set [Int]))
  }
  deriving stock (Eq, Show)

tests :: TestTree
tests =
  testGroup
    "rbac incremental stats"
    [ testCase "role_action patch repairs warmed conditional-grant factors without rebuilding" $
        assertRbacRoleActionIncrementalStats,
      testCase "user_attr patch does not invalidate baseline grant factor registrations" $
        assertRbacReuseScopeIsolation
    ]

assertRbacRoleActionIncrementalStats :: Assertion
assertRbacRoleActionIncrementalStats =
  case rbacRoleActionIncrementalStats of
    Left err ->
      assertFailure (show err)
    Right report -> do
      let stats =
            risrStats report
      assertBool
        "expected at least one patched factor node"
        (risNodesPatched stats > 0)
      risNodesBuilt stats @?= 0
      assertBool
        "expected affected keys to be non-zero"
        (risAffectedKeys stats > 0)
      assertBool
        "expected recomputed cells to be non-zero"
        (risRecomputedCells stats > 0)
      risrIncrementalFactorRows report @?= risrFreshFactorRows report
      Map.lookup (FactorNodeBag bag3) (risrIncrementalFactorRows report)
        @?= Just (Set.fromList [[tenant, role, actionRead], [tenant, role, actionWrite]])

assertRbacReuseScopeIsolation :: Assertion
assertRbacReuseScopeIsolation =
  case rbacReuseScopeIsolation of
    Left err ->
      assertFailure err
    Right (warmDiagnostics, patchedDiagnostics) -> do
      let registeredBefore =
            R.rrdRegisteredFactorShapes warmDiagnostics
          registeredAfter =
            R.rrdRegisteredFactorShapes patchedDiagnostics
          staleDelta =
            R.rrsStaleRejected (R.rrdStats patchedDiagnostics)
              - R.rrsStaleRejected (R.rrdStats warmDiagnostics)
          registeredNewDelta =
            R.rrsRegisteredNew (R.rrdStats patchedDiagnostics)
              - R.rrsRegisteredNew (R.rrdStats warmDiagnostics)
      assertBool
        "expected warm runtime to register factor shapes"
        (registeredBefore > 0)
      staleDelta @?= 0
      assertEqual
        "expected user_attr churn not to create new factor-shape registrations"
        0
        registeredNewDelta
      assertEqual
        "expected user_attr churn to retain canonical factor-shape ownership"
        registeredBefore
        registeredAfter

rbacRoleActionIncrementalStats :: Either RbacIncrementalStatsFailure RbacIncrementalStatsReport
rbacRoleActionIncrementalStats = do
  warmResult <-
    runRbacMaintenance initialRows emptyFactorCache IntMap.empty
  incrementalResult <-
    runRbacMaintenance patchedRows (frrCache warmResult) roleActionPatch
  freshResult <-
    runRbacMaintenance patchedRows emptyFactorCache IntMap.empty
  pure
    RbacIncrementalStatsReport
      { risrStats = incrementalStats (frrMetrics incrementalResult),
        risrIncrementalFactorRows = factorRows incrementalResult,
        risrFreshFactorRows = factorRows freshResult
      }

data RuntimeRbacContext
  = RuntimeRbacGlobal
  deriving stock (Eq, Ord, Show, Read)

data RuntimeRbacProp
  = RuntimeRbacEntitlement
  deriving stock (Eq, Ord, Show, Read)

rbacReuseScopeIsolation ::
  Either String (R.RuntimeReuseDiagnostics, R.RuntimeReuseDiagnostics)
rbacReuseScopeIsolation = do
  runtime0 <- runtimeReuseIsolationInitialRuntime
  warmPatchValue <- runtimeReuseIsolationWarmPatch
  runtimeWarm <-
    first show (R.applyPatch warmPatchValue runtime0)
  userAttrPatchValue <-
    first show
      ( R.insert
          runtimeUserAttrAtom
          [row [tenant, user + 2, attribute + 2]]
      )
  runtimeAfterUserAttr <-
    first show (R.applyPatch userAttrPatchValue runtimeWarm)
  pure
    ( R.rdReuseDiagnostics (R.runtimeDiagnostics runtimeWarm),
      R.rdReuseDiagnostics (R.runtimeDiagnostics runtimeAfterUserAttr)
    )

runtimeReuseIsolationInitialRuntime ::
  Either String (R.Runtime RuntimeRbacContext RuntimeRbacProp)
runtimeReuseIsolationInitialRuntime = do
  grantQuery <-
    first show $
      R.query
        [ R.runtimeMatch runtimeMemberAtom,
          R.runtimeMatch runtimeGroupScopeAtom,
          R.runtimeMatch runtimeGroupRoleAtom,
          R.runtimeMatch runtimeRoleActionAtom,
          R.runtimeMatch runtimeResourceScopeAtom
        ]
        (R.select [slotT, slotU, slotRes, slotA])
  conditionalQuery <-
    first show $
      R.query
        [ R.runtimeMatch runtimeMemberAtom,
          R.runtimeMatch runtimeGroupScopeAtom,
          R.runtimeMatch runtimeGroupRoleAtom,
          R.runtimeMatch runtimeRoleActionAtom,
          R.runtimeMatch runtimeResourceScopeAtom,
          R.runtimeMatch runtimeRoleAttrAtom,
          R.runtimeMatch runtimeUserAttrAtom
        ]
        (R.select [slotT, slotU, slotRes, slotA])
  grantPlan <-
    first show (R.runtimePlanQuery RuntimeRbacGlobal runtimeEntitlementProp grantQuery)
  conditionalPlan <-
    first show
      ( R.runtimePlanWithDecompQuery
          RuntimeRbacGlobal
          runtimeEntitlementProp
          conditionalQuery
          rbacConditionalGrantDecomp
      )
  initialPatchValue <-
    runtimePatchForRows runtimeInitialRows
  first show $
    R.createRuntime
      ( R.withInitialData
          (R.runtimeInitialData initialPatchValue)
          ( R.runtimeSpec
              runtimeRbacSchema
              [grantPlan, conditionalPlan]
          )
      )

runtimeRbacSchema :: R.RuntimeSchema RuntimeRbacContext RuntimeRbacProp
runtimeRbacSchema =
  R.runtimeSchema
    [ ( RuntimeRbacGlobal,
        R.runtimeContextSchema
          runtimeAtoms
          [runtimeEntitlementProp]
      )
    ]

runtimeEntitlementProp :: R.PropositionKey RuntimeRbacProp
runtimeEntitlementProp =
  R.PropositionKey RuntimeRbacEntitlement

runtimeReuseIsolationWarmPatch :: Either String R.Patch
runtimeReuseIsolationWarmPatch =
  runtimePatchForRows
    [ (runtimeMemberAtom, [row [tenant, user + 1, group + 1]]),
      (runtimeGroupScopeAtom, [row [tenant, group + 1, scope + 1]]),
      (runtimeGroupRoleAtom, [row [tenant, group + 1, role + 1]]),
      (runtimeRoleActionAtom, [row [tenant, role + 1, actionWrite + 1]]),
      (runtimeResourceScopeAtom, [row [tenant, resource + 1, scope + 1]]),
      (runtimeRoleAttrAtom, [row [tenant, role + 1, attribute + 1]]),
      (runtimeUserAttrAtom, [row [tenant, user + 1, attribute + 1]])
    ]

runtimeInitialRows ::
  [(R.RuntimeAtom RuntimeRbacContext RuntimeRbacProp, [RowTupleKey])]
runtimeInitialRows =
  [ (runtimeMemberAtom, [row [tenant, user, group]]),
    (runtimeGroupScopeAtom, [row [tenant, group, scope]]),
    (runtimeGroupRoleAtom, [row [tenant, group, role]]),
    (runtimeRoleActionAtom, [initialRoleActionRow]),
    (runtimeResourceScopeAtom, [row [tenant, resource, scope]]),
    (runtimeRoleAttrAtom, [row [tenant, role, attribute]]),
    (runtimeUserAttrAtom, [row [tenant, user, attribute]])
  ]

runtimePatchForRows ::
  [(R.RuntimeAtom RuntimeRbacContext RuntimeRbacProp, [RowTupleKey])] ->
  Either String R.Patch
runtimePatchForRows atomRows =
  R.patch <$> traverse insertRows atomRows
  where
    insertRows ::
      (R.RuntimeAtom RuntimeRbacContext RuntimeRbacProp, [RowTupleKey]) ->
      Either String R.Patch
    insertRows (atomValue, rowsValue) =
      first show (R.insert atomValue rowsValue)

runtimeAtoms :: [R.RuntimeAtom RuntimeRbacContext RuntimeRbacProp]
runtimeAtoms =
  [ runtimeMemberAtom,
    runtimeGroupScopeAtom,
    runtimeGroupRoleAtom,
    runtimeRoleActionAtom,
    runtimeResourceScopeAtom,
    runtimeRoleAttrAtom,
    runtimeUserAttrAtom
  ]

runtimeMemberAtom, runtimeGroupScopeAtom, runtimeGroupRoleAtom, runtimeRoleActionAtom, runtimeResourceScopeAtom, runtimeRoleAttrAtom, runtimeUserAttrAtom :: R.RuntimeAtom RuntimeRbacContext RuntimeRbacProp
runtimeMemberAtom = R.runtimeAtom (R.mkAtomId memberAtom) [slotT, slotU, slotG]
runtimeGroupScopeAtom = R.runtimeAtom (R.mkAtomId groupScopeAtom) [slotT, slotG, slotS]
runtimeGroupRoleAtom = R.runtimeAtom (R.mkAtomId groupRoleAtom) [slotT, slotG, slotR]
runtimeRoleActionAtom = R.runtimeAtom (R.mkAtomId roleActionAtom) [slotT, slotR, slotA]
runtimeResourceScopeAtom = R.runtimeAtom (R.mkAtomId resourceScopeAtom) [slotT, slotRes, slotS]
runtimeRoleAttrAtom = R.runtimeAtom (R.mkAtomId roleAttrAtom) [slotT, slotR, slotX]
runtimeUserAttrAtom = R.runtimeAtom (R.mkAtomId userAttrAtom) [slotT, slotU, slotX]

runRbacMaintenance ::
  RowsByAtom ->
  FactorCache ->
  IntMap RowDelta ->
  Either RbacIncrementalStatsFailure (FactorRunResult ())
runRbacMaintenance rowsByAtom cache atomDeltas = do
  store <-
    storeFromRbacRows rowsByAtom
  first RbacFactorFailed $
    runFactor
      FactorRunSpec
        { frsDecomp = rbacConditionalGrantDecomp,
          frsInput =
            FactorInput
              { fiStore = store,
                fiView = unrestrictedView,
                fiAtomDeltas = atomDeltas
              },
          frsCache = cache,
          frsGc = defaultProvGCConfig,
            frsRepairTelemetry = defaultRepairTelemetryConfig,
          frsDemand = FactorDemandMaintenance
        }

incrementalStats :: MaintenanceMetrics -> RbacIncrementalStats
incrementalStats metrics =
  RbacIncrementalStats
    { risNodesPatched = maintenanceActionCount NodePatched metrics,
      risNodesBuilt = maintenanceActionCount NodeBuilt metrics,
      risAffectedKeys = maintenanceAffectedKeyCount metrics,
      risRecomputedCells = maintenanceRecomputedCellCount metrics
    }

factorRows :: FactorRunResult () -> Map.Map FactorNode (Set [Int])
factorRows result =
  Map.map
    ( Set.map tupleKeyToInts
        . Map.keysSet
        . indexedRowsPayloadMap
        . feFactor
    )
    (fcFactors (frrCache result))

storeFromRbacRows :: RowsByAtom -> Either RbacIncrementalStatsFailure Store
storeFromRbacRows rowsByAtom = do
  rowBlocks <- IntMap.traverseWithKey relationFromRows rowsByAtom
  relations <- first RbacRelationFailed (traverse relationFromAtomRows rowBlocks)
  pure (storeFromRelations relations)

relationFromRows :: Int -> Set RowTupleKey -> Either RbacIncrementalStatsFailure (RowBlock 'Canonical)
relationFromRows atomKey rows = do
  schema <-
    maybe (Left (RbacUnknownAtom atomKey)) Right (rbacAtomSchema atomKey)
  first RbacRowsFailed $
    atomRowsFromTupleKeys
      (rowBlockIdentityForAtom 0 0 0 (mkAtomId atomKey) 0)
      (Vector.fromList schema)
      rows

rbacAtomSchema :: Int -> Maybe [SlotId]
rbacAtomSchema atomKey
  | atomKey == memberAtom =
      Just [slotT, slotU, slotG]
  | atomKey == groupScopeAtom =
      Just [slotT, slotG, slotS]
  | atomKey == groupRoleAtom =
      Just [slotT, slotG, slotR]
  | atomKey == roleActionAtom =
      Just [slotT, slotR, slotA]
  | atomKey == resourceScopeAtom =
      Just [slotT, slotRes, slotS]
  | atomKey == roleAttrAtom =
      Just [slotT, slotR, slotX]
  | atomKey == userAttrAtom =
      Just [slotT, slotU, slotX]
  | otherwise =
      Nothing

rbacConditionalGrantDecomp :: DecompPlan
rbacConditionalGrantDecomp =
  mkDecompPlan
    bag0
    ( IntMap.fromList
        [ (0, mkDecompBag bag0 [slotT, slotU, slotG, slotR, slotX] (IntSet.fromList [memberAtom, groupRoleAtom, roleAttrAtom, userAttrAtom])),
          (1, mkDecompBag bag1 [slotT, slotG, slotS] (IntSet.singleton groupScopeAtom)),
          (2, mkDecompBag bag2 [slotT, slotS, slotRes] (IntSet.singleton resourceScopeAtom)),
          (3, mkDecompBag bag3 [slotT, slotR, slotA] (IntSet.singleton roleActionAtom))
        ]
    )
    ( IntMap.fromList
        [ (1, bag0),
          (2, bag1),
          (3, bag0)
        ]
    )
    ( IntMap.fromList
        [ (0, [bag1, bag3]),
          (1, [bag2])
        ]
    )
    ( Map.fromList
        [ ((bag1, bag0), [slotT, slotG]),
          ((bag2, bag1), [slotT, slotS]),
          ((bag3, bag0), [slotT, slotR])
        ]
    )
    ( IntMap.fromList
        [ (memberAtom, bag0),
          (groupRoleAtom, bag0),
          (roleAttrAtom, bag0),
          (userAttrAtom, bag0),
          (groupScopeAtom, bag1),
          (resourceScopeAtom, bag2),
          (roleActionAtom, bag3)
        ]
    )

initialRows :: RowsByAtom
initialRows =
  IntMap.fromList
    [ (memberAtom, Set.singleton (row [tenant, user, group])),
      (groupScopeAtom, Set.singleton (row [tenant, group, scope])),
      (groupRoleAtom, Set.singleton (row [tenant, group, role])),
      (roleActionAtom, Set.singleton initialRoleActionRow),
      (resourceScopeAtom, Set.singleton (row [tenant, resource, scope])),
      (roleAttrAtom, Set.singleton (row [tenant, role, attribute])),
      (userAttrAtom, Set.singleton (row [tenant, user, attribute]))
    ]

patchedRows :: RowsByAtom
patchedRows =
  IntMap.adjust
    (Set.insert patchedRoleActionRow)
    roleActionAtom
    initialRows

roleActionPatch :: IntMap RowDelta
roleActionPatch =
  IntMap.singleton
    roleActionAtom
    (plainRowPatchFromList [(patchedRoleActionRow, MultiplicityChange 1)])

initialRoleActionRow :: RowTupleKey
initialRoleActionRow =
  row [tenant, role, actionRead]

patchedRoleActionRow :: RowTupleKey
patchedRoleActionRow =
  row [tenant, role, actionWrite]

row :: [Int] -> RowTupleKey
row =
  tupleKeyFromInts

bag0, bag1, bag2, bag3 :: BagId
bag0 = BagId 0
bag1 = BagId 1
bag2 = BagId 2
bag3 = BagId 3

memberAtom, groupScopeAtom, groupRoleAtom, roleActionAtom, resourceScopeAtom, roleAttrAtom, userAttrAtom :: Int
memberAtom = 0
groupScopeAtom = 1
groupRoleAtom = 2
roleActionAtom = 3
resourceScopeAtom = 4
roleAttrAtom = 5
userAttrAtom = 6

slotT, slotU, slotG, slotR, slotA, slotS, slotRes, slotX :: SlotId
slotT = mkSlotId 0
slotU = mkSlotId 1
slotG = mkSlotId 2
slotR = mkSlotId 3
slotA = mkSlotId 4
slotS = mkSlotId 5
slotRes = mkSlotId 6
slotX = mkSlotId 7

tenant, user, group, role, scope, resource, attribute, actionRead, actionWrite :: Int
tenant = 0
user = 10
group = 20
role = 30
scope = 40
resource = 50
attribute = 70
actionRead = 80
actionWrite = 81
