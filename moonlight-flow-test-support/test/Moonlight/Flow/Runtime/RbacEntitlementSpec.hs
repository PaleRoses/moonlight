{-# LANGUAGE LambdaCase #-}

module Moonlight.Flow.Runtime.RbacEntitlementSpec
  ( tests,
  )
where

import Control.Monad
  ( when,
  )
import Data.Bifunctor
  ( first,
  )
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core qualified as R
import Moonlight.Delta.Signed
  ( Multiplicity (..)
  )
import Moonlight.Differential.Proposition qualified as R
import Moonlight.Differential.Row.Tuple qualified as R
import Moonlight.Flow.Patch qualified as R
import Moonlight.Flow.Query qualified as R
import Moonlight.Flow.Read qualified as R
import Moonlight.Flow.Runtime.Apply qualified as R
import Moonlight.Flow.Runtime.Create qualified as R
import Moonlight.Flow.Runtime.Inspect qualified as R
import Moonlight.Flow.Runtime.Spec.Schema qualified as R
import Moonlight.Flow.Runtime.Types qualified as R
import Moonlight.Flow.Runtime.RbacFixture.Config
  ( allRbacLocalityScenarios,
    emptyRbacPatchShape,
    localityScenarioPatchShape,
    localityScenarioShouldStayNarrow,
    localityWarmupPatchShape,
    testRbacPatchShape,
    testRbacSeedCounts,
    testRbacSize,
  )
import Moonlight.Flow.Runtime.RbacFixture.Patch
  ( freshRowsFrom,
    generatePatchBatch,
    mutateRelation,
    patchSchedule,
    projectRowsByIndices,
  )
import Moonlight.Flow.Runtime.RbacFixture.Plans
  ( conditionalDecompPlan,
    conditionalReferencePlan,
    conditionalSeedAtoms,
    fullSoakPlans,
    rbacAtoms,
    rbacSchema,
    resourceScopeReproducerCases,
    slotG,
    slotT,
    slotU,
  )
import Moonlight.Flow.Runtime.RbacFixture.Truth
  ( buildRuntimeFromTruth,
    buildRuntimeFromTruthForPlans,
    readAll,
    seedTruth,
    truthRelation,
  )
import Moonlight.Flow.Runtime.RbacFixture.Types
  ( RbacAtomName (..),
    RbacAtoms (..),
    RbacContext (..),
    RbacLocalityScenario,
    RbacPatchShape (..),
    RbacPatchSummary (..),
    RbacProp (..),
    RbacResourceScopeReproducerCase (..),
    RbacTruth,
    Rng,
    allAtomNames,
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

tests :: TestTree
tests =
  testGroup
    "rbac entitlement runtime"
    [ testCase "patch generation emits no empty relation patch" testEmptyPatchCanonicalization,
      testCase "generated moves insert as many rows as they delete" testMoveCounts,
      testCase "replacement rows do not reinsert deleted rows" testMoveDoesNotReinsertDeletedRows,
      testCase "resource_scope patch applies across isolated plan sets" testResourceScopeFrontier,
      testCase "incremental output matches fresh rebuild after normal churn" testIncrementalFresh,
      testCase "conditional decomp matches undecomposed plan" testConditionalDecomp,
      testCase "grant projection plans equal projected grant rows" testGrantProjections,
      testCase "sibling grant projections share one canonical repair owner" testCanonicalRepairProjectionStats,
      testCase "insert/delete cancellation preserves output" testCancellation,
      testCase "invalid delete is rejected" testInvalidDelete,
      testCase "locality patches invalidate registered factors without global collapse" testLocality,
      testCase "duplicate public inserts require duplicate deletes before disappearance" testDuplicatePublicInsertCanonicalization
    ]

testEmptyPatchCanonicalization :: Assertion
testEmptyPatchCanonicalization =
  patchSchedule emptyRbacPatchShape @?= []

testMoveCounts :: Assertion
testMoveCounts = do
  let (truth0, rng0) = baseTruth
      atomsValue = rbacAtoms
  case generatePatchBatch atomsValue testRbacSize testRbacPatchShape truth0 rng0 of
    Left err ->
      assertFailure (show err)
    Right (_truth1, _patchValue, _rng1, summary) ->
      assertEqual "generated move deletes and inserts symmetrically" (rpsDeletedRows summary) (rpsInsertedRows summary)

testMoveDoesNotReinsertDeletedRows :: Assertion
testMoveDoesNotReinsertDeletedRows = do
  let (truth0, _rng0) = baseTruth
      currentRows = truthRelation ResourceScope truth0
      deletedRows = Set.fromList (take 4 (Set.toAscList currentRows))
  case freshRowsFrom testRbacSize ResourceScope 4 currentRows 0 of
    Left err ->
      assertFailure (show err)
    Right (insertedRows, _nextOrdinal) ->
      assertBool
        "replacement rows are not deleted rows"
        (Set.null (Set.intersection deletedRows insertedRows))

testResourceScopeFrontier :: Assertion
testResourceScopeFrontier = do
  let atomsValue = rbacAtoms
      (truth0, rng0) = baseTruth
  case mutateRelation atomsValue testRbacSize ResourceScope 1 truth0 rng0 of
    Left err ->
      assertFailure (show err)
    Right (truth1, patchValue, _rng1, _summary) ->
      Foldable.traverse_ (assertResourceScopeCase atomsValue truth0 truth1 patchValue) resourceScopeReproducerCases

testIncrementalFresh :: Assertion
testIncrementalFresh = do
  let atomsValue = rbacAtoms
      (truth0, rng0) = baseTruth
  plansValue <- expectRight "full plans" (fullSoakPlans atomsValue)
  runtime0 <- expectRight "initial runtime" (buildRuntimeFromTruth atomsValue plansValue truth0)
  (truth1, patchValue, _rng1, _summary) <-
    expectRight "normal churn patch" (generatePatchBatch atomsValue testRbacSize testRbacPatchShape truth0 rng0)
  assertPatchMatchesFresh atomsValue plansValue allAtomNames patchValue truth1 runtime0

testConditionalDecomp :: Assertion
testConditionalDecomp = do
  let atomsValue = rbacAtoms
      (truth0, _rng0) = baseTruth
  referencePlan <- expectRight "conditional reference plan" (conditionalReferencePlan atomsValue)
  decompPlan <- expectRight "conditional decomp plan" (conditionalDecompPlan atomsValue)
  referenceRuntime <-
    expectRight
      "conditional reference runtime"
      (buildRuntimeFromTruthForPlans atomsValue [referencePlan] conditionalSeedAtoms truth0)
  decompRuntime <-
    expectRight
      "conditional decomposed runtime"
      (buildRuntimeFromTruthForPlans atomsValue [decompPlan] conditionalSeedAtoms truth0)
  referenceRows <- expectRight "reference rows" (readAll [referencePlan] referenceRuntime)
  decompRows <- expectRight "decomp rows" (readAll [decompPlan] decompRuntime)
  assertEqual "decomposed conditional grant matches plain conditional grant" referenceRows decompRows

testGrantProjections :: Assertion
testGrantProjections = do
  let atomsValue = rbacAtoms
      (truth0, _rng0) = baseTruth
  plansValue <- expectRight "full plans" (fullSoakPlans atomsValue)
  runtime0 <- expectRight "initial runtime" (buildRuntimeFromTruth atomsValue plansValue truth0)
  rowsValue <- expectRight "full plan rows" (readAll plansValue runtime0)
  case rowsValue of
    grantRows : _conditionalRows : _deniedRows : grantUserActionRows : grantResourceSubjectRows : _grantScopeActionRows : [] -> do
      assertProjectedRows "grant_user_action" [0, 1, 3] grantRows grantUserActionRows
      assertProjectedRows "grant_resource_subject" [0, 2, 1] grantRows grantResourceSubjectRows
    _ ->
      assertFailure "fullSoakPlans returned an unexpected plan count"

testCanonicalRepairProjectionStats :: Assertion
testCanonicalRepairProjectionStats = do
  let atomsValue = rbacAtoms
      (truth0, rng0) = baseTruth
      roleActionPatchShape =
        emptyRbacPatchShape {rpsRoleActionMoves = 4}
  plansValue <- expectRight "full plans" (fullSoakPlans atomsValue)
  runtime0 <- expectRight "initial runtime" (buildRuntimeFromTruth atomsValue plansValue truth0)
  (truthWarm, warmPatch, rngWarm, _warmSummary) <-
    expectRight "warmup patch" (generatePatchBatch atomsValue testRbacSize localityWarmupPatchShape truth0 rng0)
  runtimeWarm <- expectRight "apply warmup patch" (first show (R.applyPatch warmPatch runtime0))
  (truth1, roleActionPatch, _rng1, _roleActionSummary) <-
    expectRight "role_action patch" (generatePatchBatch atomsValue testRbacSize roleActionPatchShape truthWarm rngWarm)
  let statsBefore =
        R.rdRepairStats (R.runtimeDiagnostics runtimeWarm)
  runtime1 <- expectRight "apply role_action patch" (first show (R.applyPatch roleActionPatch runtimeWarm))
  assertRuntimeMatchesFresh atomsValue plansValue allAtomNames truth1 runtime1
  let statsAfter =
        R.rdRepairStats (R.runtimeDiagnostics runtime1)
      statDelta getter =
        getter statsAfter - getter statsBefore
  assertEqual "factor repairs" 2 (statDelta R.rprsFactorRepairs)
  assertEqual "canonical repairs" 2 (statDelta R.rprsCanonicalRepairs)
  assertEqual "repair subscribers" 5 (statDelta R.rprsRepairSubscribers)
  assertEqual "canonical input delta rows" 16 (statDelta R.rprsInputDeltaRows)
  assertEqual "warm prepared input rebuilds" 0 (statDelta R.rprsPreparedInputRebuilds)
  assertEqual "warm prepared input patch hits" 2 (statDelta R.rprsPreparedInputPatchHits)
  assertEqual "warm prepared relation rows" 0 (statDelta R.rprsPreparedRelationRows)
  assertEqual "warm store rebuilds" 0 (statDelta R.rprsStoreRebuilds)
  assertEqual "semantic affected keys mirror affected keys" (statDelta R.rprsAffectedKeys) (statDelta R.rprsSemanticAffectedKeys)
  assertEqual "projection rows mirror emitted carrier rows" (statDelta R.rprsEmittedCarrierRows) (statDelta R.rprsProjectionRowsEmitted)

testCancellation :: Assertion
testCancellation = do
  let atomsValue = rbacAtoms
      (truth0, _rng0) = baseTruth
      absentRow = R.tupleKeyFromInts [9001, 9002, 9003]
  plansValue <- expectRight "full plans" (fullSoakPlans atomsValue)
  runtime0 <- expectRight "initial runtime" (buildRuntimeFromTruth atomsValue plansValue truth0)
  beforeRows <- expectRight "before rows" (readAll plansValue runtime0)
  inserted <- expectRight "insert cancellation half" (first show (R.insert (rbaMember atomsValue) [absentRow, absentRow]))
  deleted <- expectRight "delete cancellation half" (first show (R.delete (rbaMember atomsValue) [absentRow, absentRow]))
  let cancelPatch = R.patch [inserted, deleted]
  runtime1 <- expectRight "apply cancellation patch" (first show (R.applyPatch cancelPatch runtime0))
  afterRows <- expectRight "after rows" (readAll plansValue runtime1)
  assertEqual "insert/delete cancellation preserves all outputs" beforeRows afterRows

testInvalidDelete :: Assertion
testInvalidDelete = do
  let atomsValue = rbacAtoms
      (truth0, _rng0) = baseTruth
      absentRow = R.tupleKeyFromInts [9101, 9102, 9103]
  plansValue <- expectRight "full plans" (fullSoakPlans atomsValue)
  runtime0 <- expectRight "initial runtime" (buildRuntimeFromTruth atomsValue plansValue truth0)
  badPatch <- expectRight "invalid delete patch" (first show (R.delete (rbaMember atomsValue) [absentRow]))
  case R.applyPatch badPatch runtime0 of
    Left _err ->
      pure ()
    Right _runtime1 ->
      assertFailure "invalid delete was accepted"

testLocality :: Assertion
testLocality = do
  let atomsValue = rbacAtoms
      (truth0, rng0) = baseTruth
  plansValue <- expectRight "full plans" (fullSoakPlans atomsValue)
  runtime0 <- expectRight "initial runtime" (buildRuntimeFromTruth atomsValue plansValue truth0)
  (truthWarm, warmPatch, rngWarm, _summary) <-
    expectRight "warmup patch" (generatePatchBatch atomsValue testRbacSize localityWarmupPatchShape truth0 rng0)
  runtimeWarm <- expectRight "apply warmup patch" (first show (R.applyPatch warmPatch runtime0))
  let warmDiagnostics = R.rdReuseDiagnostics (R.runtimeDiagnostics runtimeWarm)
      registeredBefore = R.rrdRegisteredFactorShapes warmDiagnostics
  assertBool "warmup registered factor shapes" (registeredBefore > 0)
  Foldable.traverse_
    (assertLocalityScenario atomsValue plansValue truthWarm rngWarm runtimeWarm warmDiagnostics)
    allRbacLocalityScenarios

testDuplicatePublicInsertCanonicalization :: Assertion
testDuplicatePublicInsertCanonicalization = do
  let atomsValue = rbacAtoms
      duplicateRow = R.tupleKeyFromInts [0, 1, 2]
  memberQuery <-
    expectRight
      "member query"
      ( first show $
          R.query
            [R.runtimeMatch (rbaMember atomsValue)]
            (R.select [slotT, slotU, slotG])
      )
  memberPlan <- expectRight "member plan" (first show (R.runtimePlanQuery RbacGlobal (R.PropositionKey RbacEntitlement) memberQuery))
  seedPatch <- expectRight "duplicate seed patch" (first show (R.insert (rbaMember atomsValue) [duplicateRow, duplicateRow]))
  runtime0 <-
    expectRight
      "multiplicity runtime"
      ( first show $
          R.createRuntime
            ( R.withInitialData
                (R.runtimeInitialData seedPatch)
                (R.runtimeSpec (rbacSchema atomsValue) [memberPlan])
            )
      )
  rows0 <- expectSingleRead "initial multiplicity rows" memberPlan runtime0
  R.rowMultiplicity duplicateRow rows0 @?= Multiplicity 1
  deleteOne <- expectRight "delete one duplicate" (first show (R.delete (rbaMember atomsValue) [duplicateRow]))
  runtime1 <- expectRight "apply delete one" (first show (R.applyPatch deleteOne runtime0))
  rows1 <- expectSingleRead "after one duplicate delete" memberPlan runtime1
  R.rowMultiplicity duplicateRow rows1 @?= Multiplicity 1
  runtime2 <- expectRight "apply delete two" (first show (R.applyPatch deleteOne runtime1))
  rows2 <- expectSingleRead "after second duplicate delete" memberPlan runtime2
  R.rowMultiplicity duplicateRow rows2 @?= Multiplicity 0
  case R.applyPatch deleteOne runtime2 of
    Left _err ->
      pure ()
    Right _runtime3 ->
      assertFailure "delete after duplicate source exhaustion was accepted"

baseTruth :: (RbacTruth, Rng)
baseTruth =
  seedTruth testRbacSize testRbacSeedCounts 0x726261635f746573

assertResourceScopeCase ::
  RbacAtoms ->
  RbacTruth ->
  RbacTruth ->
  R.Patch ->
  RbacResourceScopeReproducerCase ->
  Assertion
assertResourceScopeCase atomsValue truth0 truth1 patchValue reproCase = do
  let name = show (rrscPlanSet reproCase)
  plansValue <- expectRight (name <> " plans") (rrscPlans reproCase atomsValue)
  runtime0 <- expectRight (name <> " runtime") (buildRuntimeFromTruthForPlans atomsValue plansValue (rrscSeedAtoms reproCase) truth0)
  assertPatchMatchesFresh atomsValue plansValue (rrscSeedAtoms reproCase) patchValue truth1 runtime0

assertLocalityScenario ::
  RbacAtoms ->
  [R.RuntimePlan RbacContext RbacProp] ->
  RbacTruth ->
  Rng ->
  R.Runtime RbacContext RbacProp ->
  R.RuntimeReuseDiagnostics ->
  RbacLocalityScenario ->
  Assertion
assertLocalityScenario atomsValue plansValue truthWarm rngWarm runtimeWarm warmDiagnostics scenario = do
  let patchShape = localityScenarioPatchShape scenario
      registeredBefore = R.rrdRegisteredFactorShapes warmDiagnostics
  (truth1, patchValue, _rng1, _summary) <-
    expectRight (show scenario <> " patch") (generatePatchBatch atomsValue testRbacSize patchShape truthWarm rngWarm)
  runtime1 <- expectRight (show scenario <> " apply") (first show (R.applyPatch patchValue runtimeWarm))
  assertRuntimeMatchesFresh atomsValue plansValue allAtomNames truth1 runtime1
  let diagnosticsAfter = R.rdReuseDiagnostics (R.runtimeDiagnostics runtime1)
      registeredAfter =
        R.rrdRegisteredFactorShapes diagnosticsAfter
  assertBool (show scenario <> " warmed registered factor shapes") (registeredBefore > 0)
  when (localityScenarioShouldStayNarrow scenario) $
    assertEqual
      (show scenario <> " retained canonical factor-shape ownership")
      registeredBefore
      registeredAfter

assertPatchMatchesFresh ::
  RbacAtoms ->
  [R.RuntimePlan RbacContext RbacProp] ->
  [RbacAtomName] ->
  R.Patch ->
  RbacTruth ->
  R.Runtime RbacContext RbacProp ->
  Assertion
assertPatchMatchesFresh atomsValue plansValue seedAtoms patchValue truth1 runtime0 = do
  runtime1 <- expectRight "apply patch" (first show (R.applyPatch patchValue runtime0))
  assertRuntimeMatchesFresh atomsValue plansValue seedAtoms truth1 runtime1

assertRuntimeMatchesFresh ::
  RbacAtoms ->
  [R.RuntimePlan RbacContext RbacProp] ->
  [RbacAtomName] ->
  RbacTruth ->
  R.Runtime RbacContext RbacProp ->
  Assertion
assertRuntimeMatchesFresh atomsValue plansValue seedAtoms truth1 runtime1 = do
  fresh1 <- expectRight "fresh runtime" (buildRuntimeFromTruthForPlans atomsValue plansValue seedAtoms truth1)
  actual <- expectRight "incremental rows" (readAll plansValue runtime1)
  expected <- expectRight "fresh rows" (readAll plansValue fresh1)
  assertEqual "incremental runtime matches fresh rebuild" expected actual

assertProjectedRows :: String -> [Int] -> R.Rows -> R.Rows -> Assertion
assertProjectedRows name indices sourceRows projectedRows = do
  expected <- expectRight (name <> " external projection") (projectRowsByIndices indices sourceRows)
  let actual = Map.fromList (R.rowsToList projectedRows)
  assertEqual name expected actual

expectSingleRead :: String -> R.RuntimePlan RbacContext RbacProp -> R.Runtime RbacContext RbacProp -> AssertionWithResult R.Rows
expectSingleRead label planValue runtime =
  expectRight label (first show (R.readRows planValue runtime))

type AssertionWithResult value = IO value

expectRight :: Show err => String -> Either err value -> AssertionWithResult value
expectRight label =
  \case
    Left err ->
      assertFailure (label <> ": " <> show err)
    Right value ->
      pure value
