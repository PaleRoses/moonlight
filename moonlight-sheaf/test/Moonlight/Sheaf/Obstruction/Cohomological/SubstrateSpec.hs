{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}

module Moonlight.Sheaf.Obstruction.Cohomological.SubstrateSpec
  ( tests,
  )
where

import Data.Bifunctor (first)
import Data.EqP (EqP (..))
import Data.GADT.Compare (GCompare (..), GEq (..))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.OrdP (OrdP (..))
import Data.Set qualified as Set
import Moonlight.Core (RegionNodeId (..))
import Moonlight.Homology
  ( HomologicalDegree (..),
    HomologyFailure (..),
    emptyBoundaryIncidence,
    mkFiniteChainComplexChecked,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Analysis.Exact
  ( exactCoverageFromLift,
    exactCoverageSupportsObstruction,
    occurrenceDomainConstraints,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Analysis
  ( analyzeCohomologicalRegion,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Core.Cache
  ( ObstructionCacheKey (..),
    emptyCohomologicalCache,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Evidence.Certification
  ( CachePolicy (..),
    SectionCertificationAlgebra (..),
    regionCarrierPlanFromList,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Core.Environment
  ( ObstructionEnvironmentAlgebra (..),
    emptyIndexedEnvironmentAlgebra,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section.Exact
  ( CohomologicalExactMatchEvidence (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Modality
  ( ModalityContribution (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Core.Policy
  ( CohomologicalPolicy (..),
    ExactCoverageBudget (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section.Projection
  ( RelationProjectionConflict,
    defaultSectionProjection,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section.Region
  ( RegionAnalysisOutcome (..),
    RegionExactCoverage,
    RegionExactness (..),
    recExactness,
    rtsCoverage,
    rtsMeasures,
    rtsOutcome,
    regionCoverageFromSectionCoverage,
    skippedRegionCoverage,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Evidence.Pruning
  ( CohomologicalPruningGates,
    CohomologicalPruningObstruction (..),
    PruningEvidence (..),
    buildPruningGates,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section
  ( SectionCoverage (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Substrate
  ( CohomologicalLift (..),
    CohomologicalSubstrate (..),
    CohomologicalSupportAlgebra (..),
    SubstrateRegion,
    cacheKeyForRegion,
    obstructionWitnessFor,
    regionDependencyKeys,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Types
  ( Anchor (..),
    CandidateRegion (..),
    ConstraintId (..),
    ExactConstraint (..),
    ExpandedMorphism,
    ExpandedObstructionCell,
    ExpandedStalk (..),
    ExactLabelCode (..),
    ModalityCoverage (..),
    ObstructionCell,
    ObstructionLift (..),
    ObstructionReason (..),
    ObstructionWitness (..),
    OccurrenceId (..),
    RelationFlavor (..),
    RegionScale (..),
    mkCandidateRegion,
    mkCandidateRegionWithNode,
  )
import Moonlight.Sheaf.Footprint
  ( FootprintMeasure (..),
    FootprintMeasureExactness (..),
    FootprintMeasureUnit (..),
  )
import Moonlight.Sheaf.Kernel.Basis (SheafBasis, mkSheafBasis)
import Moonlight.Sheaf.Operator.GradedComplex
import Moonlight.Sheaf.Section.Restriction (emptyRestrictionIndex)
import Moonlight.Sheaf.Pruning
  ( PruningCertificate (..),
  )
import Moonlight.Sheaf.Verdict (Verdict (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertBool,
    assertFailure,
    testCase,
  )

-- ---------------------------------------------------------------------------
-- Minimal concrete substrate for witness tests
-- ---------------------------------------------------------------------------

-- | Opaque tag for the test substrate.
data TestSubstrate
  = TestSubstrate
  | BudgetedExactTestSubstrate
  | BudgetedWitnessTestSubstrate
  | UnmaterializableSupportTestSubstrate

-- | A trivial request type: carries a query fingerprint.
newtype TestRequest runtime = TestRequest Int

-- | A key type with no constructors — the test substrate uses an empty
-- IndexedEnvironment, so this is never instantiated.
data NoKey :: Type -> Type -> Type where {}

instance Eq (NoKey runtime value) where
  (==) key _ =
    case key of {}

instance Ord (NoKey runtime value) where
  compare key _ =
    case key of {}

instance EqP (NoKey runtime) where
  eqp key _ =
    case key of {}

instance OrdP (NoKey runtime) where
  comparep key _ =
    case key of {}

instance GEq (NoKey runtime) where
  geq key _ =
    case key of {}

instance GCompare (NoKey runtime) where
  gcompare key _ =
    case key of {}

instance CohomologicalSubstrate TestSubstrate where
  type SubstrateRequest TestSubstrate = TestRequest
  type SubstrateQuery TestSubstrate = ()
  type SubstratePattern TestSubstrate = ()
  type SubstrateOccurrence TestSubstrate = ()
  type SubstrateGuard TestSubstrate = ()
  type SubstrateCandidate TestSubstrate = ()
  type SubstrateCapability TestSubstrate = ()
  type SubstrateRoot TestSubstrate = Int
  type SubstrateResult TestSubstrate = ()
  type SubstratePurpose TestSubstrate = ()
  type SubstrateReference TestSubstrate = ()
  type SubstrateKernelFailure TestSubstrate = ()
  type SubstrateSupportEvidence TestSubstrate = ()
  type SubstrateModalityTag TestSubstrate = ()
  type SubstrateModalityKey TestSubstrate = NoKey

  substratePolicy substrate =
    CohomologicalPolicy
      { cpUseHierarchicalPruning = False,
        cpMaxCoarseDepth = 0,
        cpShortCircuitRankGap =
          case substrate of
            BudgetedWitnessTestSubstrate ->
              True
            _ ->
              False,
        cpRequireFactSensitiveCache = False,
        cpPreferExactWitnessOnFailure = False,
        cpMinCycleLength = 0,
        cpLaplacianPruning = Nothing,
        cpExactCoverageBudget =
          case substrate of
            BudgetedExactTestSubstrate ->
              Just (ExactCoverageBudget 1)
            BudgetedWitnessTestSubstrate ->
              Just (ExactCoverageBudget 1)
            _ ->
              Nothing
      }

  substrateCertification _ =
    SectionCertificationAlgebra
      { socCollectOccurrences = \_ -> [],
        socRegionCarrierPlan = \_ _ -> regionCarrierPlanFromList [],
        socRefineRegion = \_ _ _ -> [],
        socOccurrenceDomain = \_ _ _ -> (),
        socGuardDomain = \_ _ _ -> (),
        socCapabilityEnvironment = \_ _ _ _ -> (),
        socKernelVerdict = \_ _ -> Accepted (),
        socPatternFingerprint = \_ -> 0,
        socQueryCachePolicy = \_ -> SharedAcrossEnvironments,
        socEnvironmentFingerprint = \_ -> Nothing
      }

  substrateEnvironment substrate =
    ObstructionEnvironmentAlgebra
      { oeaCollectOccurrences =
          case substrate of
            BudgetedWitnessTestSubstrate ->
              \_ -> [()]
            _ ->
              \_ -> [],
        oeaEnumerateRegions = \_ _ -> [],
        oeaRefineRegion = \_ _ _ -> [],
        oeaIndexedEnvironmentAlgebra = emptyIndexedEnvironmentAlgebra,
        oeaQueryFingerprint = \_ -> 0,
        oeaEnvironmentFingerprint = \_ -> Nothing
      }

  substrateSupportAlgebra substrate =
    CohomologicalSupportAlgebra
      { csaCoverage = ModalityCoverage [] [] [],
        csaMissingOccurrenceDomainCoverage = ModalityCoverage [] [] [],
        csaOccurrenceDomains =
          case substrate of
            UnmaterializableSupportTestSubstrate ->
              \_ -> Nothing
            BudgetedWitnessTestSubstrate ->
              \_ -> Just (Map.singleton (OccurrenceId 0) (IntSet.fromList [1, 2]))
            _ ->
              \_ -> Just Map.empty,
        csaEvaluateSupport =
          case substrate of
            BudgetedWitnessTestSubstrate ->
              \_ -> (ModalityContribution (rootOnlyConstraints 7 [100, 101, 102, 103]) [], ())
            _ ->
              \_ -> (ModalityContribution [] [], ()),
        csaSectionReification = \_ -> mempty,
        csaSectionProjection =
          case substrate of
            BudgetedExactTestSubstrate ->
              Right defaultSectionProjection
            BudgetedWitnessTestSubstrate ->
              Right defaultSectionProjection
            _ ->
              Left [],
        csaMapGap = id
      }

  substrateRequestQuery _ _ = ()
  substrateRequestPattern _ _ = ()
  substrateRequestPurpose _ _ = ()
  substrateCollectGuards _ _ = []
  substrateQueryFingerprint _ _ = 0
  substrateOccurrenceId _ () = OccurrenceId 0
  substrateCanonicalRoot _ _ root = root
  substrateRootKey _ _ root = root
  substrateMemberKey _ _ k = k
  substrateShouldRefine _ _ = False
  substrateRefinedRegions _ _ _ _ = []
  substrateEmptyResult _ = ()
  substrateExactEvidence _ _ _ _ _ =
    CohomologicalExactMatchEvidence [] [] [] []

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

testRegion :: Int -> IntSet -> CandidateRegion Int
testRegion root members =
  mkCandidateRegion root members 0 FineRegion 42

emptySheafBasis :: SheafBasis ExpandedObstructionCell
emptySheafBasis = mkSheafBasis []

minimalLiftWithDomains ::
  Map OccurrenceId IntSet ->
  Either HomologyFailure (CohomologicalLift TestSubstrate)
minimalLiftWithDomains domains =
  fmap
    ( \obstructionLiftValue ->
        CohomologicalLift
          { clQuery = (),
            clOccurrences = [],
            clOccurrenceDomains = domains,
            clGuards = [],
            clExactConstraints = [],
            clExactLoweringGaps = [],
            clSectionReification = mempty,
            clSupportEvidence = (),
            clZeroBasis = emptySheafBasis,
            clOneBasis = emptySheafBasis,
            clTwoBasis = emptySheafBasis,
            clSupportCells = [],
            clObstructionLift = obstructionLiftValue
          }
    )
    (emptyObstructionLiftForTest 0 (testRegion 0 IntSet.empty))

unaryFactConstraint :: Int -> Int -> [Int] -> ExactConstraint (Anchor OccurrenceId)
unaryFactConstraint constraintId occurrenceId labels =
  RelationConstraint
    FactFlavor
    (ConstraintId constraintId)
    [OccurrenceAnchor (OccurrenceId occurrenceId)]
    (fmap (pure . ClassLabelCode) labels)

rootOnlyConstraints :: Int -> [Int] -> [ExactConstraint (Anchor OccurrenceId)]
rootOnlyConstraints rootValue =
  fmap
    ( \constraintId ->
        RelationConstraint
          FactFlavor
          (ConstraintId constraintId)
          [RootAnchor]
          [[ClassLabelCode rootValue]]
    )

emptyObstructionLiftForTest ::
  Int ->
  CandidateRegion Int ->
  Either HomologyFailure (ObstructionLift Int (CandidateRegion Int) ExpandedMorphism)
emptyObstructionLiftForTest rootValue regionValue =
  fmap
    ( \chainComplex ->
        ObstructionLift
          { olRegion = regionValue,
            olExpandedComplex = chainComplex,
            olRestrictions = emptyRestrictionIndex,
            olCoboundaryCache = emptyGradedComplex DegreeIncreasing,
            olRoot = rootValue,
            olExactH1Eligible = True
          }
    )
    ( mkFiniteChainComplexChecked
        (HomologicalDegree 0)
        (const emptyBoundaryIncidence)
    )

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "cohomological substrate"
    [ testCase "regionDependencyKeys includes root key" testRegionDependencyKeysIncludesRoot,
      testCase "regionDependencyKeys maps member keys through substrateMemberKey" testRegionDependencyKeysMembersAreMapped,
      testCase "cacheKeyForRegion uses region fingerprint" testCacheKeyForRegionFingerprint,
      testCase "cacheKeyForRegion uses region scale" testCacheKeyForRegionScale,
      testCase "exact coverage skips instead of partially enumerating beyond budget" testExactCoverageSkipsWhenBudgetExceeded,
      testCase "budget-skipped exact coverage with a witness is truncated, not certified" testBudgetSkippedWitnessIsTruncated,
      testCase "pruned region returns before lift materialization" testPrunedRegionReturnsBeforeLiftMaterialization,
      testCase "exactCoverageSupportsObstruction is True only when coverage is feasible" testExactCoverageSupportsObstruction,
      testCase "occurrenceDomainConstraints produces one constraint per occurrence" testOccurrenceDomainConstraintsCount,
      testCase "occurrenceDomainConstraints assigns synthetic negative ConstraintIds" testOccurrenceDomainConstraintIds,
      testCase "obstructionWitnessFor captures root, region, and rank counts" testObstructionWitnessFor
    ]

testRegionDependencyKeysIncludesRoot :: Assertion
testRegionDependencyKeysIncludesRoot =
  let region = testRegion 7 IntSet.empty
      keys = regionDependencyKeys TestSubstrate (TestRequest 0) region
   in assertBool
        "expected root key (7) to appear in dependency keys"
        (IntSet.member 7 keys)

testRegionDependencyKeysMembersAreMapped :: Assertion
testRegionDependencyKeysMembersAreMapped =
  -- TestSubstrate.substrateMemberKey is identity, so member keys appear as-is.
  let members = IntSet.fromList [3, 5, 9]
      region = testRegion 1 members
      keys = regionDependencyKeys TestSubstrate (TestRequest 0) region
      expected = IntSet.insert 1 members
   in assertEqual
        "expected dependency keys to equal {root} ∪ members"
        expected
        keys

testCacheKeyForRegionFingerprint :: Assertion
testCacheKeyForRegionFingerprint =
  let region = testRegion 4 IntSet.empty  -- crFingerprint = 42 from mkCandidateRegion
      key = cacheKeyForRegion TestSubstrate (TestRequest 0) region
   in assertEqual
        "expected cache key region fingerprint to equal the region's crFingerprint"
        42
        (ockRegionFingerprint key)

testCacheKeyForRegionScale :: Assertion
testCacheKeyForRegionScale =
  let region = testRegion 4 IntSet.empty
      key = cacheKeyForRegion TestSubstrate (TestRequest 0) region
   in assertEqual
        "expected cache key scale to equal FineRegion"
        FineRegion
        (ockScale key)

testExactCoverageSkipsWhenBudgetExceeded :: Assertion
testExactCoverageSkipsWhenBudgetExceeded = do
  baseLift <-
    either
      (assertFailure . show)
      pure
      (minimalLiftWithDomains Map.empty)
  let region =
        testRegion 7 IntSet.empty
      lift =
        baseLift
          { clExactConstraints =
              [ unaryFactConstraint 100 0 [1, 2],
                unaryFactConstraint 101 1 [3, 4]
              ]
          }
      coverage =
        exactCoverageFromLift
          BudgetedExactTestSubstrate
          (TestRequest 0 :: TestRequest ())
          region
          lift
   in assertEqual
        "budget refusal is a skipped exact result, not a feasible prefix"
        ExactCoverageSkipped
        (recExactness coverage)

testBudgetSkippedWitnessIsTruncated :: Assertion
testBudgetSkippedWitnessIsTruncated =
  let region =
        testRegion 7 IntSet.empty
      (_cache, summary) =
        analyzeCohomologicalRegion
          False
          (buildPruningGates [])
          BudgetedWitnessTestSubstrate
          emptyCohomologicalCache
          (TestRequest 0 :: TestRequest ())
          region
   in case rtsOutcome summary of
        RegionAnalysisTruncated witness -> do
          assertEqual
            "budget-exhausted exact coverage should remain skipped"
            ExactCoverageSkipped
            (recExactness (rtsCoverage summary))
          assertEqual
            "truncated lower-bound witness should retain the H1 reason"
            (PositiveFirstCohomology 1)
            (owReason witness)
          assertEqual
            "truncated lower-bound witness should retain the rank lower bound"
            1
            (owKernelRankLowerBound witness)
          assertEqual
            "truncated lower-bound witness should retain the region"
            region
            (owRegion witness)
        RegionAnalysisObstructed _ ->
          assertFailure "budget-truncated lower-bound witness was certified as an obstruction"
        RegionAnalysisPruned _ ->
          assertFailure "budget-truncated lower-bound witness was pruned"
        RegionAnalysisExact _ ->
          assertFailure "budget-truncated lower-bound witness was suppressed as exact coverage"

testPrunedRegionReturnsBeforeLiftMaterialization :: Assertion
testPrunedRegionReturnsBeforeLiftMaterialization =
  let nodeId = RegionNodeId 7
      region = mkCandidateRegionWithNode 7 IntSet.empty 0 FineRegion nodeId 42
      gates :: CohomologicalPruningGates Int
      gates = buildPruningGates [MicrosupportNonCritical (Set.singleton nodeId)]
      (_cache, summary) =
        analyzeCohomologicalRegion
          False
          gates
          UnmaterializableSupportTestSubstrate
          emptyCohomologicalCache
          (TestRequest 0 :: TestRequest ())
          region
   in case rtsOutcome summary of
        RegionAnalysisPruned certificate -> do
          assertEqual
            "expected microsupport pruning certificate"
            [MicrosupportNonCriticalObstruction nodeId]
            (NonEmpty.toList (pcObstructions certificate))
          assertBool
            "expected exact represented region-node measure from pruning footprint"
            (any isExactRepresentedRegionNodeMeasure (rtsMeasures summary))
        RegionAnalysisObstructed _ ->
          assertFailure "lift materialization ran before pruning and produced an obstruction"
        RegionAnalysisTruncated _ ->
          assertFailure "lift materialization ran before pruning and produced a truncated witness"
        RegionAnalysisExact _ ->
          assertFailure "pruned region should not produce exact coverage"

isExactRepresentedRegionNodeMeasure :: FootprintMeasure natural -> Bool
isExactRepresentedRegionNodeMeasure measure =
  fmUnit measure == RegionNodeUnit
    && fmExactness measure == FootprintExactRepresented

testExactCoverageSupportsObstruction :: Assertion
testExactCoverageSupportsObstruction =
  do
    assertBool "ExactCoverageFeasible should support obstruction" $
      exactCoverageSupportsObstruction
        (regionCoverageFromSectionCoverage (SectionCoverage [()] [] :: SectionCoverage () ()))

    assertBool "ExactCoverageSkipped should not support obstruction" $
      not
        (exactCoverageSupportsObstruction
          (skippedRegionCoverage :: RegionExactCoverage () ()))

    assertBool "ExactCoverageInfeasible should not support obstruction" $
      not
        (exactCoverageSupportsObstruction
          (regionCoverageFromSectionCoverage (mempty :: SectionCoverage () ())))

testOccurrenceDomainConstraintsCount :: Assertion
testOccurrenceDomainConstraintsCount = do
  let domains =
        Map.fromList
          [ (OccurrenceId 0, IntSet.singleton 10),
            (OccurrenceId 1, IntSet.fromList [20, 30]),
            (OccurrenceId 2, IntSet.singleton 40)
          ]
  lift <-
    either
      (assertFailure . show)
      pure
      (minimalLiftWithDomains domains)
  let constraints = occurrenceDomainConstraints lift
  assertEqual
    "expected one constraint per occurrence-domain entry"
    3
    (length constraints)

testOccurrenceDomainConstraintIds :: Assertion
testOccurrenceDomainConstraintIds = do
  let domains =
        Map.fromList
          [ (OccurrenceId 0, IntSet.singleton 10),
            (OccurrenceId 3, IntSet.singleton 99)
          ]
  lift <-
    either
      (assertFailure . show)
      pure
      (minimalLiftWithDomains domains)
  let constraints = occurrenceDomainConstraints lift
      extractId :: ExactConstraint anchor -> Maybe ConstraintId
      extractId constraint =
        case constraint of
          EqualityConstraint cid _ _ _ -> Just cid
          GuardConstraint {} -> Nothing
          RelationConstraint {} -> Nothing
      ids = mapMaybe extractId constraints
  assertBool "expected ConstraintId (-1) for OccurrenceId 0" $
    elem (ConstraintId (-1)) ids
  assertBool "expected ConstraintId (-4) for OccurrenceId 3" $
    elem (ConstraintId (-4)) ids

testObstructionWitnessFor :: Assertion
testObstructionWitnessFor =
  let region = testRegion 7 (IntSet.fromList [2, 5])
      root = 7 :: Int
      cells = [] :: [ObstructionCell]
      reason = KernelVerdictObstructed :: ObstructionReason (SubstrateRegion TestSubstrate) (ModalityCoverage () RelationProjectionConflict)
      witness =
        obstructionWitnessFor
          TestSubstrate
          (TestRequest 0)
          region
          root
          cells
          reason
          3
          5
          2
   in do
        assertEqual "owRootClass" 7 (owRootClass witness)
        assertEqual "owRegion" region (owRegion witness)
        assertEqual "owPurpose" () (owPurpose witness)
        assertEqual "owKernelRankLowerBound" 3 (owKernelRankLowerBound witness)
        assertEqual "owImageRankUpperBound" 5 (owImageRankUpperBound witness)
        assertEqual "owRepresentativeCocycleCount" 2 (owRepresentativeCocycleCount witness)
