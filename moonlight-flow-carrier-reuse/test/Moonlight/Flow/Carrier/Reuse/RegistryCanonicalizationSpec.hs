{-# LANGUAGE DataKinds #-}

module Moonlight.Flow.Carrier.Reuse.RegistryCanonicalizationSpec
  ( spec,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.IntMap.Strict qualified as IntMap
import Test.Moonlight.Differential.Index.Registry
  ( indexedRegistryFromPartsForValidation,
    indexedRegistryWithIndexesForValidation,
  )
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core
  ( ClassId (..),
  )
import Moonlight.Core
  ( SlotId,
    initialLiveEpoch,
    initialQuotientEpoch,
    mkAtomId,
    nextLiveEpoch,
    nextQuotientEpoch,
    mkQueryId,
    mkSlotId,
    slotIdKey,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
import Moonlight.Differential.Time
  ( frontierStamp,
    initialFrontierStamp,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    QueryCarrierNode (..),
    SubsumptionWitnessDigest (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    carrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Coverage
  ( CoverageFact (..),
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
  ( CarrierReuse (..),
    CarrierReuseId,
    CoverageProjectionRule (..),
    ReuseKind (..),
    ReuseWitness (..),
    carrierReuseExpectedTarget,
    carrierReuseFromWitness,
    carrierReuseId,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Materialization
  ( InstalledReuseMaterialization (..),
    MaterializationInvariantError (..),
    ReuseMaterializationIndex (..),
    ReuseMaterializationReverseIndex (..),
    emptyReuseMaterializationIndex,
    lookupInstalledReuseMaterialization,
    upsertInstalledReuseMaterialization,
    validateReuseMaterializationIndex,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Reuse
  ( CarrierReuseIndex (..),
    CarrierReuseRegistry (..),
    CarrierReuseRegistryInvariantError (..),
    carrierReuseRegistryIdsForSource,
    carrierReuseRegistryIdsForTarget,
    carrierReuseRegistryStaleEntries,
    crrIndex,
    emptyCarrierReuseRegistry,
    insertCarrierReuseRegistry,
    validateCarrierReuseRegistry,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Shape
  ( ContainmentTrie (..),
    SubsumptionIndex (..),
    SubsumptionIndexInvariantError (..),
    emptySubsumptionIndex,
    insertEntryIndex,
    validateSubsumptionIndex,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Registry
  ( dropCarrierReuse,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Shape
  ( SubsumptionEntry (..),
  )
import Moonlight.Flow.Carrier.Reuse.Internal.State.Diagnostics
  ( planReuseCarrierReuses,
    validatePlanReuseState,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.State.Types
  ( PlanReuseState (..),
    emptyPlanReuseState,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Validity
  ( ReuseValidity (..),
    ReuseValidityRequest (..),
  )
import Moonlight.Flow.Carrier.Reuse.Types
  ( PlanReuseInvariantError (..),
  )
import Moonlight.Flow.Carrier.Reuse
  ( CarrierReuseCandidateGroup (..),
    CarrierReuseStrategy (..),
    PlanReuseMiss (..),
    PlanReuseRequest (..),
    ReuseConfig (..),
    ReuseMode (..),
    planCarrierReuse,
    registerSubsumptionEntry,
  )
import Moonlight.Flow.Execution.Subsumption.Proof
  ( AtomEmbedding (..),
    BoundaryProjectionProof (..),
    ContainmentAtomWitness (..),
    ContainmentProof (..),
    ResidualImplicationProof (..),
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchFromList,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( BoundaryShape (..),
    RuntimeBoundary,
    boundaryDigest,
    emptyRuntimeBoundary,
    mkBoundary,
    runtimeBoundaryDigest,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
  )
import Moonlight.Flow.Model.Schema.Morphism
  ( BoundaryProjection (..),
    SchemaProjection,
    compileSchemaProjection,
  )
import Moonlight.Flow.Plan.Query.Core
  ( FactorNode (..),
    QueryPlanDomain (..),
  )
import Moonlight.Flow.Plan.Residual
  ( ResidualShape (..),
    emptyResidualTheoryRegistry,
  )
import Moonlight.Flow.Plan.Rewrite
  ( FactorShapeNormalization (..),
    FactorShapeNormalizationProof (..),
    PlanClassId (..),
    PlanReuseShapeKey (..),
  )
import Moonlight.Flow.Plan.Shape
  ( CanonicalBoundaryShape,
    FactorShapePayload (..),
    LogicalQueryShape (..),
    emptyCanonAtomMultiset,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot (..),
    FragmentPayload (..),
    PlanShape (..),
    PlanStage (..),
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
    "reuse registry canonicalization"
    [ testCase "CarrierReuseRegistry derives and validates every reverse axis" carrierReuseRegistryAxesAreDerived,
      testCase "PlanReuseState reports reuse target reverse-index obstructions" planReuseStateReportsReuseTargetIndexObstructions,
      testCase "dropping carrier reuse removes installed materialization" droppingReuseRemovesInstalledMaterialization,
      testCase "ReuseMaterializationIndex validates primary reuse key ownership" materializationIndexValidatesPrimaryReuseKey,
      testCase "SubsumptionIndex stores entries once and indexes addresses only" subsumptionIndexStoresEntriesOnce,
      testCase "planCarrierReuse orders exact before cover before lower-bound" planCarrierReuseOrdersStrategies,
      testCase "planCarrierReuse matches digest-backed entries across runtime clocks" planCarrierReuseMatchesDigestBackedEntriesAcrossRuntimeClocks,
      testCase "planCarrierReuse applies containment candidate limits" planCarrierReuseLimitsContainmentCandidates,
      testCase "planCarrierReuse reports typed empty-group misses" planCarrierReuseReportsEmptyMisses,
      testCase "planCarrierReuse does not register selected reuse" planCarrierReuseDoesNotRegisterSelection
    ]

carrierReuseRegistryAxesAreDerived :: Assertion
carrierReuseRegistryAxesAreDerived = do
  projection <- testSchemaProjection
  let reuse0 = exactReuse projection 10 (IntSet.fromList [11]) (IntSet.fromList [13])
      reuseId = carrierReuseId reuse0
      sourceAddr = reuseSourceAddr 10
      targetAddr = reuseTargetAddr 10
      registry0 = insertCarrierReuseRegistry reuse0 emptyCarrierReuseRegistry
  validateCarrierReuseRegistry registry0 @?= Right ()
  carrierReuseRegistryIdsForSource sourceAddr registry0 @?= Set.singleton reuseId
  carrierReuseRegistryIdsForTarget targetAddr registry0 @?= Set.singleton reuseId
  carrierReuseRegistryStaleEntries (IntSet.singleton 11) IntSet.empty registry0 @?= [(reuseId, reuse0)]

  let reuse1 = reuse0 {cruWitnessDeps = IntSet.singleton 17, cruWitnessTopo = IntSet.singleton 19}
      registry1 = insertCarrierReuseRegistry reuse1 registry0
  carrierReuseId reuse1 @?= reuseId
  validateCarrierReuseRegistry registry1 @?= Right ()
  carrierReuseRegistryStaleEntries (IntSet.singleton 11) IntSet.empty registry1 @?= []
  carrierReuseRegistryStaleEntries IntSet.empty (IntSet.singleton 13) registry1 @?= []
  carrierReuseRegistryStaleEntries (IntSet.singleton 17) IntSet.empty registry1 @?= [(reuseId, reuse1)]
  carrierReuseRegistryStaleEntries IntSet.empty (IntSet.singleton 19) registry1 @?= [(reuseId, reuse1)]

planReuseStateReportsReuseTargetIndexObstructions :: Assertion
planReuseStateReportsReuseTargetIndexObstructions = do
  projection <- testSchemaProjection
  let reuseValue = exactReuse projection 20 (IntSet.singleton 3) (IntSet.singleton 5)
      reuseId = carrierReuseId reuseValue
      targetAddr = reuseTargetAddr 20
      staleTargetAddr = reuseTargetAddr 21
      registry0 = insertCarrierReuseRegistry reuseValue emptyCarrierReuseRegistry
      missingTargetRegistry =
        registry0
          { crrRegistry =
              indexedRegistryWithIndexesForValidation
                ((crrIndex registry0) {criByTarget = Map.empty})
                (crrRegistry registry0)
          }
      staleTargetRegistry =
        registry0
          { crrRegistry =
              indexedRegistryWithIndexesForValidation
                ( (crrIndex registry0)
                    { criByTarget =
                        Map.insert staleTargetAddr (Set.singleton reuseId) (criByTarget (crrIndex registry0))
                    }
                )
                (crrRegistry registry0)
          }
  assertLeftContains
    "missing target reverse index"
    (CarrierReuseTargetReverseMissing reuseId targetAddr)
    (validateCarrierReuseRegistry missingTargetRegistry)
  assertLeftContains
    "stale target reverse index"
    (CarrierReuseTargetReverseStale reuseId staleTargetAddr)
    (validateCarrierReuseRegistry staleTargetRegistry)
  assertLeftContains
    "state-level target reverse index obstruction"
    (PlanReuseReuseRegistryInvariant (CarrierReuseTargetReverseMissing reuseId targetAddr))
    (validatePlanReuseState (emptyPlanReuseState {prsReuseRegistry = missingTargetRegistry}))


droppingReuseRemovesInstalledMaterialization :: Assertion
droppingReuseRemovesInstalledMaterialization = do
  projection <- testSchemaProjection
  let reuseValue = exactReuse projection 30 IntSet.empty IntSet.empty
      reuseId = carrierReuseId reuseValue
  targetAddr <- expectTarget reuseValue
  let registry0 = insertCarrierReuseRegistry reuseValue emptyCarrierReuseRegistry
      installed = installedMaterialization reuseId targetAddr
      (_deltaRows, materializations0) = upsertInstalledReuseMaterialization installed emptyReuseMaterializationIndex
      state0 = emptyPlanReuseState {prsReuseRegistry = registry0, prsMaterializations = materializations0}
      state1 = dropCarrierReuse reuseId state0
  lookupInstalledReuseMaterialization reuseId (prsMaterializations state1) @?= Nothing
  validatePlanReuseState state1 @?= Right ()

materializationIndexValidatesPrimaryReuseKey :: Assertion
materializationIndexValidatesPrimaryReuseKey = do
  projection <- testSchemaProjection
  let actualReuseId = carrierReuseId (exactReuse projection 50 IntSet.empty IntSet.empty)
      storedReuseId = carrierReuseId (exactReuse projection 51 IntSet.empty IntSet.empty)
      targetAddr = reuseTargetAddr 50
      installed = installedMaterialization actualReuseId targetAddr
      materializations =
        ReuseMaterializationIndex
          { rmiRegistry =
              indexedRegistryFromPartsForValidation
                (Map.singleton storedReuseId installed)
                ReuseMaterializationReverseIndex
                  { rmriByTarget = Map.singleton targetAddr (Set.singleton storedReuseId),
                    rmriByDep = IntMap.empty,
                    rmriByTopo = IntMap.empty
                  }
          }
      expectedError = MaterializationStoredUnderWrongReuseId storedReuseId actualReuseId
  assertLeftContains
    "materialization stored under wrong reuse id"
    expectedError
    (validateReuseMaterializationIndex materializations)
  assertLeftContains
    "state-level materialization owner obstruction"
    (PlanReuseMaterializationInvariant expectedError)
    (validatePlanReuseState (emptyPlanReuseState {prsMaterializations = materializations}))

subsumptionIndexStoresEntriesOnce :: Assertion
subsumptionIndexStoresEntriesOnce = do
  let entry = subsumptionEntry 60 (IntSet.singleton 61) (IntSet.singleton 62)
      addr = seCarrier entry
      entryShapeKey = seShapeKey entry
      index0 = insertEntryIndex entry emptySubsumptionIndex
      missingAddr = reuseSourceAddr 61
      shapeStaleIndex = index0 {siFactorShapes = Map.adjust (Set.insert missingAddr) entryShapeKey (siFactorShapes index0)}
      trieMissingIndex = index0 {siContainmentTrie = (siContainmentTrie index0) {ctEntries = Set.delete addr (ctEntries (siContainmentTrie index0))}}
  validateSubsumptionIndex index0 @?= Right ()
  Map.lookup addr (siByCarrier index0) @?= Just entry
  Map.lookup entryShapeKey (siFactorShapes index0) @?= Just (Set.singleton addr)
  assertBool "containment trie stores candidate addr membership" (Set.member addr (ctEntries (siContainmentTrie index0)))
  assertLeftContains
    "shape index stale addr"
    (SubsumptionShapeReverseStale missingAddr entryShapeKey)
    (validateSubsumptionIndex shapeStaleIndex)
  assertLeftContains
    "containment trie missing addr"
    (SubsumptionContainmentTrieMissing addr)
    (validateSubsumptionIndex trieMissingIndex)

planCarrierReuseOrdersStrategies :: Assertion
planCarrierReuseOrdersStrategies = do
  (state0, request) <-
    selectionStateAndRequest [70]
  (_state1, groups) <-
    assertRight $
      planCarrierReuse
        ReuseConfig
          { rcMode = ExactOrContainment,
            rcMaxContainmentCandidates = 64
          }
        request
        state0
  fmap crcgStrategy groups
    @?= [ReuseExactEquivalent, ReuseExactByCover, ReuseLowerBound]
  case groups of
    exactGroup : _ ->
      assertBool
        "exact strategy should carry the first reusable exact candidate"
        (not (null (crcgCandidates exactGroup)))
    [] ->
      assertFailure "expected carrier reuse strategy groups"

planCarrierReuseMatchesDigestBackedEntriesAcrossRuntimeClocks :: Assertion
planCarrierReuseMatchesDigestBackedEntriesAcrossRuntimeClocks = do
  state0 <-
    assertRight $
      registerSubsumptionEntry
        (prqShape selectionPlanReuseRequest)
        (selectionSourceAddr 74)
        digestBackedReuseValidity
        (prqBoundary selectionPlanReuseRequest)
        ExactLocal
        (IntSet.singleton 74)
        (IntSet.singleton 1074)
        emptyPlanReuseState
  (_state1, groups) <-
    assertRight $
      planCarrierReuse
        ReuseConfig
          { rcMode = ExactOnly,
            rcMaxContainmentCandidates = 64
          }
        ( selectionPlanReuseRequest
            { prqValidity =
                advancedValidityRequest (rvViewDigest digestBackedReuseValidity)
            }
        )
        state0
  fmap (not . null . crcgCandidates) groups @?= [True]

planCarrierReuseLimitsContainmentCandidates :: Assertion
planCarrierReuseLimitsContainmentCandidates = do
  (state0, request) <-
    selectionStateAndRequest [71, 72]
  (_state1, groups) <-
    assertRight $
      planCarrierReuse
        ReuseConfig
          { rcMode = ContainmentOnly,
            rcMaxContainmentCandidates = 1
          }
        request
        state0
  fmap (length . crcgCandidates) groups @?= [1]

planCarrierReuseReportsEmptyMisses :: Assertion
planCarrierReuseReportsEmptyMisses = do
  let request =
        selectionPlanReuseRequest
  (_state1, groups) <-
    assertRight $
      planCarrierReuse
        ReuseConfig
          { rcMode = ExactOnly,
            rcMaxContainmentCandidates = 64
          }
        request
        emptyPlanReuseState
  fmap crcgMiss groups @?= [ReuseExactRejected]
  fmap crcgCandidates groups @?= [[]]

planCarrierReuseDoesNotRegisterSelection :: Assertion
planCarrierReuseDoesNotRegisterSelection = do
  (state0, request) <-
    selectionStateAndRequest [73]
  (state1, groups) <-
    assertRight $
      planCarrierReuse
        ReuseConfig
          { rcMode = ExactOnly,
            rcMaxContainmentCandidates = 64
          }
        request
        state0
  assertBool
    "selection fixture must prove a candidate exists"
    (any (not . null . crcgCandidates) groups)
  planReuseCarrierReuses state1 @?= []

selectionStateAndRequest :: [Int] -> IO (PlanReuseState Int Int, PlanReuseRequest Int Int)
selectionStateAndRequest sourceSeeds = do
  state <-
    foldM
      registerSelectionSource
      emptyPlanReuseState
      sourceSeeds
  pure (state, selectionPlanReuseRequest)
{-# INLINE selectionStateAndRequest #-}

registerSelectionSource :: PlanReuseState Int Int -> Int -> IO (PlanReuseState Int Int)
registerSelectionSource state seed =
  assertRight $
    registerSubsumptionEntry
      (prqShape selectionPlanReuseRequest)
      (selectionSourceAddr seed)
      reuseValidity
      (prqBoundary selectionPlanReuseRequest)
      ExactLocal
      (IntSet.singleton seed)
      (IntSet.singleton (seed + 1000))
      state
{-# INLINE registerSelectionSource #-}

selectionPlanReuseRequest :: PlanReuseRequest Int Int
selectionPlanReuseRequest =
  PlanReuseRequest
    { prqTargetCarrier = selectionTargetAddr,
      prqShape = factorShape 700,
      prqBoundary = selectionRuntimeBoundary,
      prqValidity = selectionValidityRequest,
      prqResidualTheory = emptyResidualTheoryRegistry
    }
{-# INLINE selectionPlanReuseRequest #-}

selectionRuntimeBoundary :: RuntimeBoundary
selectionRuntimeBoundary =
  mkBoundary
    runtimeBoundaryDigest
    (BoundaryShape [mkSlotId 0] Set.empty Map.empty)
{-# INLINE selectionRuntimeBoundary #-}

selectionValidityRequest :: ReuseValidityRequest
selectionValidityRequest =
  ReuseValidityRequest
    { rvrQuotientEpoch = rvQuotientEpoch reuseValidity,
      rvrLiveEpoch = rvLiveEpoch reuseValidity,
      rvrFrontierStamp = rvFrontierStamp reuseValidity,
      rvrViewDigest = rvViewDigest reuseValidity,
      rvrResidualShape = rvResidualShape reuseValidity
    }
{-# INLINE selectionValidityRequest #-}

advancedValidityRequest :: Maybe StableDigest128 -> ReuseValidityRequest
advancedValidityRequest viewDigest =
  ReuseValidityRequest
    { rvrQuotientEpoch = nextQuotientEpoch (rvQuotientEpoch reuseValidity),
      rvrLiveEpoch = nextLiveEpoch (rvLiveEpoch reuseValidity),
      rvrFrontierStamp = frontierStamp 17,
      rvrViewDigest = viewDigest,
      rvrResidualShape = rvResidualShape reuseValidity
    }
{-# INLINE advancedValidityRequest #-}

selectionSourceAddr :: Int -> CarrierAddr Int Carrier Int
selectionSourceAddr seed =
  carrierAddr selectionContext selectionProp (QueryCarrier (mkQueryId seed) (QueryFactor FactorNodeRoot))
{-# INLINE selectionSourceAddr #-}

selectionTargetAddr :: CarrierAddr Int Carrier Int
selectionTargetAddr =
  carrierAddr selectionContext selectionProp (QueryCarrier (mkQueryId 7999) (QueryFactor FactorNodeRoot))
{-# INLINE selectionTargetAddr #-}

selectionContext :: Int
selectionContext =
  7000
{-# INLINE selectionContext #-}

selectionProp :: PropositionKey Int
selectionProp =
  PropositionKey 7000
{-# INLINE selectionProp #-}

exactReuse :: SchemaProjection SlotId CanonSlot -> Int -> IntSet.IntSet -> IntSet.IntSet -> CarrierReuse Int Int
exactReuse projection seed deps topo =
  carrierReuseFromWitness
    PreserveExact
    emptyRuntimeBoundary
    Nothing
    deps
    topo
    (reuseWitness projection seed)
{-# INLINE exactReuse #-}

reuseWitness :: SchemaProjection SlotId CanonSlot -> Int -> ReuseWitness Int Int
reuseWitness projection seed =
  ReuseWitness
    { rwKind = EquivalentReuse,
      rwWitnessKinds = [],
      rwSourceCarrier = reuseSourceAddr seed,
      rwTargetCarrier = reuseTargetAddr seed,
      rwSourceShape = factorShape seed,
      rwTargetShape = factorShape seed,
      rwProjection = BoundaryProjection projection,
      rwContainmentProof = containmentProof projection seed,
      rwAtomProof = Just (StructuralAtomEmbedding (atomEmbedding seed)),
      rwResidualProof = ResidualBothNone,
      rwBoundaryProof = boundaryProjectionProof seed,
      rwDigest = SubsumptionWitnessDigest (digest (8000 + fromIntegral seed))
    }
{-# INLINE reuseWitness #-}

containmentProof :: SchemaProjection SlotId CanonSlot -> Int -> ContainmentProof
containmentProof projection seed =
  ContainmentProof
    { cpSourceShape = factorShape seed,
      cpRequestedShape = factorShape seed,
      cpSlotProjection = projection,
      cpAtomEmbedding = StructuralAtomEmbedding (atomEmbedding seed),
      cpResidualProof = ResidualBothNone,
      cpBoundaryProof = boundaryProjectionProof seed,
      cpProjectionDigest = digest (8100 + fromIntegral seed)
    }
{-# INLINE containmentProof #-}

atomEmbedding :: Int -> AtomEmbedding
atomEmbedding seed =
  AtomEmbedding
    { aeRequiredAtoms = emptyCanonAtomMultiset,
      aeSourceRemainder = emptyCanonAtomMultiset,
      aeDigest = digest (8200 + fromIntegral seed)
    }
{-# INLINE atomEmbedding #-}

boundaryProjectionProof :: Int -> BoundaryProjectionProof
boundaryProjectionProof seed =
  BoundaryProjectionProof
    { bppSourceBoundaryDigest = boundaryDigest emptyRuntimeBoundary,
      bppRequestedBoundaryDigest = boundaryDigest emptyRuntimeBoundary,
      bppProjectionDigest = digest (8300 + fromIntegral seed),
      bppExact = True,
      bppDigest = digest (8400 + fromIntegral seed)
    }
{-# INLINE boundaryProjectionProof #-}

testSchemaProjection :: IO (SchemaProjection SlotId CanonSlot)
testSchemaProjection =
  assertRight $
    compileSchemaProjection
      slotAsCanon
      [mkSlotId 0]
      [mkSlotId 0]
{-# INLINE testSchemaProjection #-}

slotAsCanon :: SlotId -> CanonSlot
slotAsCanon =
  CanonSlot . slotIdKey
{-# INLINE slotAsCanon #-}

factorShape :: Int -> PlanShape 'FactorShape
factorShape seed =
  PlanShape
    { psDigest = digest (9000 + fromIntegral seed),
      psPayload =
        FactorShapePayload
          { fspPlan = canonicalShape seed,
            fspFragment = fragmentShape seed,
            fspAtoms = emptyCanonAtomMultiset,
            fspSourceSchema = [CanonSlot 0],
            fspOutputSchema = [CanonSlot 0],
            fspSeparator = Nothing,
            fspBoundary = canonicalBoundary seed,
            fspResidual = ResidualNone
          }
    }
{-# INLINE factorShape #-}

canonicalShape :: Int -> PlanShape 'Canonical
canonicalShape seed =
  PlanShape
    { psDigest = digest (9100 + fromIntegral seed),
      psPayload =
        LogicalQueryShape
          { lqsDomain = StructuralQueryPlan,
            lqsAtoms = emptyCanonAtomMultiset,
            lqsRoot = CanonSlot 0,
            lqsOutputs = [CanonSlot 0],
            lqsResidual = ResidualNone
          }
    }
{-# INLINE canonicalShape #-}

fragmentShape :: Int -> PlanShape 'Fragment
fragmentShape seed =
  PlanShape
    { psDigest = digest (9200 + fromIntegral seed),
      psPayload = RootFragmentPayload (digest (9100 + fromIntegral seed))
    }
{-# INLINE fragmentShape #-}

canonicalBoundary :: Int -> CanonicalBoundaryShape
canonicalBoundary seed =
  mkBoundary
    (const (digest (9300 + fromIntegral seed)))
    (BoundaryShape [CanonSlot 0] Set.empty Map.empty)
{-# INLINE canonicalBoundary #-}

subsumptionEntry :: Int -> IntSet.IntSet -> IntSet.IntSet -> SubsumptionEntry Int Int
subsumptionEntry seed deps topo =
  SubsumptionEntry
    { seShape = factorShape seed,
      seShapeKey = shapeKey seed,
      seShapeNormalization = normalization seed,
      seCarrier = reuseSourceAddr seed,
      seValidity = reuseValidity,
      seBoundary = emptyRuntimeBoundary,
      seCoverageHint = ExactLocal,
      seDeps = deps,
      seTopo = topo
    }
{-# INLINE subsumptionEntry #-}

shapeKey :: Int -> PlanReuseShapeKey
shapeKey seed =
  PlanReuseShapeKey
    { prskRewriteSystemDigest = digest (9600 + fromIntegral seed),
      prskRepresentativeDigest = digest (9000 + fromIntegral seed)
    }
{-# INLINE shapeKey #-}

normalization :: Int -> FactorShapeNormalization
normalization seed =
  FactorShapeNormalization
    { fsnProof =
        FactorShapeNormalizationProof
          { fsnpSourceDigest = digest (9000 + fromIntegral seed),
            fsnpKey = shapeKey seed,
            fsnpClassId = planClassId seed,
            fsnpStepDigest = digest (9700 + fromIntegral seed),
            fsnpDigest = digest (9800 + fromIntegral seed)
          }
    }
{-# INLINE normalization #-}

planClassId :: Int -> PlanClassId (PlanShape 'FactorShape)
planClassId =
  PlanClassId . ClassId
{-# INLINE planClassId #-}

reuseValidity :: ReuseValidity
reuseValidity =
  ReuseValidity
    { rvQuotientEpoch = initialQuotientEpoch,
      rvLiveEpoch = initialLiveEpoch,
      rvFrontierStamp = initialFrontierStamp,
      rvViewDigest = Nothing,
      rvResidualShape = ResidualNone,
      rvDependencyDigest = digest 9901,
      rvTopoDigest = digest 9902
    }
{-# INLINE reuseValidity #-}

digestBackedReuseValidity :: ReuseValidity
digestBackedReuseValidity =
  reuseValidity {rvViewDigest = Just (digest 9910)}
{-# INLINE digestBackedReuseValidity #-}

installedMaterialization :: CarrierReuseId Int Int -> CarrierAddr Int Carrier Int -> InstalledReuseMaterialization Int Int
installedMaterialization reuseId targetAddr =
  InstalledReuseMaterialization
    { irmReuseId = reuseId,
      irmTarget = targetAddr,
      irmRows = plainRowPatchFromList [],
      irmBoundaryDigest = boundaryDigest emptyRuntimeBoundary,
      irmSourceCurrentDigest = digest 9903,
      irmDeps = IntSet.empty,
      irmTopo = IntSet.empty
    }
{-# INLINE installedMaterialization #-}

expectTarget :: CarrierReuse Int Int -> IO (CarrierAddr Int Carrier Int)
expectTarget reuseValue =
  case carrierReuseExpectedTarget reuseValue of
    Just target ->
      pure target
    Nothing ->
      assertFailure "exact-by-cover fixture unexpectedly has no target" *> fail "unreachable"
{-# INLINE expectTarget #-}

reuseSourceAddr :: Int -> CarrierAddr Int Carrier Int
reuseSourceAddr seed =
  carrierAddr seed (PropositionKey seed) (QueryCarrier (mkQueryId seed) (QueryAtom (mkAtomId seed)))
{-# INLINE reuseSourceAddr #-}

reuseTargetAddr :: Int -> CarrierAddr Int Carrier Int
reuseTargetAddr seed =
  carrierAddr seed (PropositionKey seed) (QueryCarrier (mkQueryId seed) (QueryFactor FactorNodeRoot))
{-# INLINE reuseTargetAddr #-}

digest :: Word -> StableDigest128
digest word =
  StableDigest128 (fromIntegral word) (fromIntegral word + 1)
{-# INLINE digest #-}

assertLeftContains :: (Eq err, Show err) => String -> err -> Either [err] () -> Assertion
assertLeftContains label expected value =
  case value of
    Left errors ->
      assertBool (label <> ": " <> show errors) (expected `elem` errors)
    Right () ->
      assertFailure (label <> ": expected Left containing " <> show expected)
{-# INLINE assertLeftContains #-}

assertRight :: Show left => Either left right -> IO right
assertRight value =
  case value of
    Right right ->
      pure right
    Left err ->
      assertFailure ("expected Right, got Left: " <> show err) *> fail "unreachable"
{-# INLINE assertRight #-}
