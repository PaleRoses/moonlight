{-# LANGUAGE DataKinds #-}

module Test.Moonlight.Flow.Property.Runtime.PlanReuse
  ( spec,
  )
where

import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( SlotId,
    mkQueryId,
    slotIdKey,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    SubsumptionWitnessDigest (..),
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
import Moonlight.Flow.Execution.Subsumption.Proof
  ( AtomEmbedding (..),
    BoundaryProjectionProof (..),
    ContainmentAtomWitness (..),
    ContainmentProof (..),
    ResidualImplicationProof (..),
  )
import Moonlight.Flow.Model.Schema.Morphism
  ( BoundaryProjection (..),
    compileSchemaProjection,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( BoundaryShape (..),
    RuntimeBoundary,
    boundaryDigest,
    boundaryShape,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
  )
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot (..),
    PlanShape,
    PlanStage (..),
  )
import Moonlight.Flow.Runtime.Carrier.Store
  ( commitCarrierDeltas,
  )
import Moonlight.Flow.Carrier.Reuse
  ( PlanReuseState,
    ReuseMode (..),
    registerCarrierReuse,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    rsPlanReuse,
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Topology
  ( updateRuntimePlanReuse,
  )
import Test.Moonlight.Flow.Oracle.Runtime.Program
  ( Evidence,
    Prop,
    RuntimeProgramCase (..),
    RuntimeTriangleOptions (..),
    TestRuntime,
    carrierTime,
    defaultRuntimeTriangleOptions,
    extraSingletonRowsLike,
    factorSpec,
    insertAtomSnapshots,
    insertSnapshots,
    runtimeFromProgramCases,
    runtimeProgramShapeForSnapshot,
    runtimeTriangleCase,
    visibleReferenceSnapshots,
  )
import Test.Moonlight.Flow.Runtime.Diagnostics.Validate.PlanReuse
  ( PlanReuseSemanticValidationError (..),
    PlanReuseSemanticValidationMode (..),
    validatePlanReuseSemantics,
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
    (@?=),
  )

spec :: TestTree
spec =
  testGroup
    "PlanReuse semantic validation"
    [ testCase
        "detects exact row corruption and lower-bound non-subset"
        semanticValidationCorruptionMatrix
    ]

runtimePlanReuse ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  PlanReuseState ctx prop
runtimePlanReuse =
  rsPlanReuse . rdrState
{-# INLINE runtimePlanReuse #-}

unsafeSetRuntimePlanReuse ::
  (Ord ctx, Ord prop) =>
  PlanReuseState ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
unsafeSetRuntimePlanReuse planReuse runtime =
  runtime
    { rdrState =
        Core.mapRuntimeTopologySection
          (updateRuntimePlanReuse planReuse)
          state0
    }
  where
    state0 =
      rdrState runtime
{-# INLINE unsafeSetRuntimePlanReuse #-}

semanticValidationCorruptionMatrix :: Assertion
semanticValidationCorruptionMatrix = do
  fixture <- exactFixture
  validRuntime <- materializedRuntime fixture ExactOnly

  validatePlanReuseSemantics ValidatePlanReuseAll factorSpec validRuntime
    @?= Right ()

  exactCorrupted <-
    corruptExactCurrentRows fixture validRuntime
  assertContainsValidationError "expected PlanReuseExactRowsMismatch" isExactRowsMismatch exactCorrupted


  lowerBoundNotSubset <-
    corruptLowerBoundReuseTarget fixture validRuntime
  assertContainsValidationError "expected PlanReuseLowerBoundNotSubset" isLowerBoundNotSubset lowerBoundNotSubset

  validatePlanReuseSemantics ValidatePlanReuseOff factorSpec exactCorrupted
    @?= Right ()

exactFixture :: IO (RuntimeProgramCase Int Prop Evidence)
exactFixture =
  assertRight $
    runtimeTriangleCase
      defaultRuntimeTriangleOptions
        { rtoName = "plan-reuse-validation",
          rtoQueryId = mkQueryId 300
        }
      (carrierTime 0 0)
      0
      0
      ()

materializedRuntime ::
  RuntimeProgramCase Int Prop Evidence ->
  ReuseMode ->
  IO TestRuntime
materializedRuntime fixture demandPolicy = do
  runtime0 <-
    assertRight $
      runtimeFromProgramCases
        [fixture]
        (rpcPlanReuse fixture)
        demandPolicy
  runtimeWithAtoms <-
    assertRight $
      insertAtomSnapshots [fixture] runtime0
  assertRight $
    insertSnapshots
      (rpcCarrierSnapshots fixture)
      runtimeWithAtoms

corruptExactCurrentRows ::
  RuntimeProgramCase Int Prop Evidence ->
  TestRuntime ->
  IO TestRuntime
corruptExactCurrentRows fixture runtime = do
  snapshot <- firstVisibleSnapshot fixture
  let corruption =
        snapshot
          { deRows =
              extraSingletonRowsLike snapshot
          }
  assertRight $
    fmap fst $
    commitCarrierDeltas
      [corruption]
      runtime


corruptLowerBoundReuseTarget ::
  RuntimeProgramCase Int Prop Evidence ->
  TestRuntime ->
  IO TestRuntime
corruptLowerBoundReuseTarget fixture runtime0 = do
  snapshot <- firstVisibleSnapshot fixture
  shape <- shapeForSnapshot fixture snapshot
  reuse <- lowerBoundReuseFor snapshot shape
  targetAddr <-
    case carrierReuseExpectedTarget reuse of
      Nothing ->
        assertFailure "lower-bound reuse unexpectedly has no derived target" *> fail "unreachable"
      Just addr ->
        pure addr

  let planReuseWithReuse =
        registerCarrierReuse reuse (runtimePlanReuse runtime0)
      badDerivedSnapshot =
        snapshot
          { deAddr = targetAddr,
            deRows =
              extraSingletonRowsLike snapshot
          }

  assertRight $
    fmap fst $
    commitCarrierDeltas
      [badDerivedSnapshot]
      (unsafeSetRuntimePlanReuse planReuseWithReuse runtime0)

lowerBoundReuseFor ::
  RelationalCarrierDelta Int Carrier Prop RuntimeBoundary Evidence ->
  PlanShape 'FactorShape ->
  IO (CarrierReuse Int Prop)
lowerBoundReuseFor snapshot shape = do
  projection <-
    assertRight $
      compileSchemaProjection
        slotAsCanon
        schema
        schema

  let digest =
        StableDigest128 0x51000001 0x51000002
      witnessDigest =
        SubsumptionWitnessDigest digest
      atomEmbedding =
        AtomEmbedding
          { aeRequiredAtoms = Map.empty,
            aeSourceRemainder = Map.empty,
            aeDigest = StableDigest128 0x51000003 0x51000004
          }
      boundaryProof =
        BoundaryProjectionProof
          { bppSourceBoundaryDigest = boundaryDigest (deBoundary snapshot),
            bppRequestedBoundaryDigest = boundaryDigest (deBoundary snapshot),
            bppProjectionDigest = StableDigest128 0x51000005 0x51000006,
            bppExact = True,
            bppDigest = StableDigest128 0x51000007 0x51000008
          }
      containmentProof =
        ContainmentProof
          { cpSourceShape = shape,
            cpRequestedShape = shape,
            cpSlotProjection = projection,
            cpAtomEmbedding = StructuralAtomEmbedding atomEmbedding,
            cpResidualProof = ResidualBothNone,
            cpBoundaryProof = boundaryProof,
            cpProjectionDigest = StableDigest128 0x51000009 0x5100000a
          }
      witness =
        ReuseWitness
          { rwKind = ContainmentReuse,
            rwWitnessKinds = [WitnessStructuralAtomEmbedding, WitnessBoundaryProjection],
            rwSourceCarrier = deAddr snapshot,
            rwTargetCarrier = deAddr snapshot,
            rwSourceShape = shape,
            rwTargetShape = shape,
            rwProjection = BoundaryProjection projection,
            rwContainmentProof = containmentProof,
            rwAtomProof = Just (StructuralAtomEmbedding atomEmbedding),
            rwResidualProof = ResidualBothNone,
            rwBoundaryProof = boundaryProof,
            rwDigest = witnessDigest
          }

  pure $
    carrierReuseFromWitness
      DowngradeToLowerBound
      (deBoundary snapshot)
      Nothing
      IntSet.empty
      IntSet.empty
      witness
  where
    schema =
      bsSchema (boundaryShape (deBoundary snapshot))

firstVisibleSnapshot ::
  RuntimeProgramCase Int Prop Evidence ->
  IO (RelationalCarrierDelta Int Carrier Prop RuntimeBoundary Evidence)
firstVisibleSnapshot runtimeCase =
  case visibleReferenceSnapshots runtimeCase of
    snapshot : _ ->
      pure snapshot
    [] ->
      assertFailure "runtime program produced no visible factor snapshots" *> fail "unreachable"

shapeForSnapshot ::
  RuntimeProgramCase Int Prop Evidence ->
  RelationalCarrierDelta Int Carrier Prop RuntimeBoundary Evidence ->
  IO (PlanShape 'FactorShape)
shapeForSnapshot runtimeCase snapshot =
  assertRight $
    runtimeProgramShapeForSnapshot runtimeCase snapshot

assertContainsValidationError ::
  String ->
  (PlanReuseSemanticValidationError Int Prop RuntimeBoundary Evidence -> Bool) ->
  TestRuntime ->
  Assertion
assertContainsValidationError label predicate runtime =
  case validatePlanReuseSemantics ValidatePlanReuseAll factorSpec runtime of
    Left errors ->
      assertBool label (any predicate errors)
    Right () ->
      assertFailure "expected semantic validation failure"

isExactRowsMismatch ::
  PlanReuseSemanticValidationError Int Prop boundary Evidence ->
  Bool
isExactRowsMismatch errorValue =
  case errorValue of
    PlanReuseExactRowsMismatch {} ->
      True
    _ ->
      False


isLowerBoundNotSubset ::
  PlanReuseSemanticValidationError Int Prop boundary Evidence ->
  Bool
isLowerBoundNotSubset errorValue =
  case errorValue of
    PlanReuseLowerBoundNotSubset {} ->
      True
    _ ->
      False

assertRight :: Show left => Either left right -> IO right
assertRight eitherValue =
  case eitherValue of
    Left err ->
      assertFailure ("expected Right, got Left: " <> show err) *> fail "unreachable"
    Right value ->
      pure value
{-# INLINE assertRight #-}

slotAsCanon :: SlotId -> CanonSlot
slotAsCanon =
  CanonSlot . slotIdKey
{-# INLINE slotAsCanon #-}
