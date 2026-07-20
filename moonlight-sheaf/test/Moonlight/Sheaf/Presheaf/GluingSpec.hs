{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Presheaf.GluingSpec
  ( tests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Category
  ( FinGeneratorId (..),
    FinMorphismId (..),
    FinObjectId (..),
    mkFinCat,
    mkFinMorphism,
    sampleFinCat,
  )
import Moonlight.Sheaf.Presheaf.Core (CompiledRestriction (..), restrictAlong)
import Moonlight.Sheaf.Section.Restriction
  ( restrictionCount,
  )
import Moonlight.Sheaf.Verdict
  ( ObstructionVerdict,
    Verdict (..),
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( mkObjectIndex,
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra (..),
    StalkRestrictionKernel (..),
    stalkMismatches,
  )
import Moonlight.Sheaf.Sheaf.Gluing
  ( AmalgamationLocalityFailure (..),
    CompatibleMatchingFamily,
    CoverStalkUniverse (..),
    GhostSection (..),
    GluingAlgebra (..),
    GluingFailure (..),
    GluingObstruction (..),
    MatchingFamily,
    MatchingFamilyConstructionError (..),
    MatchingFamilyPruningObstruction (..),
    MatchingFailure (..),
    SeparatedCover,
    SeparatedCoverRefusal (..),
    SeparatedEqualityRefusal (..),
    SeparatedEqualityVerdict (..),
    SeparatedResolutionRefusal (..),
    SeparatedUniquenessRefusal (..),
    amalgamatedStalk,
    amalgamationLocalityFailures,
    amalgamateMatchingFamilyWith,
    certifyAmalgamation,
    certifyMatchingFamilyCompatibility,
    certifyMatchingFamilyCompatibilityFirstObstruction,
    certifySeparatedCover,
    certifyUniqueAmalgamation,
    matchingFamilyPruningVerdict,
    mkMatchingFamily,
    pairwiseCompatibilityFailures,
    pairwiseCompatibilityFailuresFromPlan,
    resolveUniqueAmalgamation,
    separatedLocalEqualityAt,
    uniqueAmalgamationUnderlying,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoverConstructionError,
    CoveringFamily,
    PullbackSquare (..),
    Site (..),
    mkCoveringFamily,
  )
import Moonlight.Sheaf.Site.Construction.FiniteMeet
  ( FiniteMeetMorphism,
    FiniteMeetSite,
    FiniteMeetSiteSpec (..),
    finiteMeetMorphism,
    mkFiniteMeetSite,
  )
import Moonlight.Sheaf.Site.Plan
  ( CoverId (..),
    CoverSlotKey (..),
    EffectiveCoverPlan,
    EffectiveCoverPlanFailure (..),
    SitePlanBuildError (..),
    coverSlotArrow,
    coverSlotKey,
    cpEffectiveCover,
    cpSourceKeys,
    effectiveCoverOverlapPlans,
    effectiveCoverSlotCount,
    effectiveCoverSlotKeys,
    effectiveCoverSlotSources,
    effectiveCoverSlots,
    identityEffectiveCoverPlan,
    opPullbackSquare,
    prepareEffectiveCoverPlan,
    prepareSitePlans,
    pullbackEffectiveCoverPlanAlong,
    siteCoverPlansAt,
    spCoversById,
  )
import Moonlight.Sheaf.Site.Grothendieck
  ( grothendieckFaceMorphismSource,
    grothendieckFaceMorphismTarget,
    grothendieckSiteFaceMorphisms,
    mkGrothendieckSite,
  )
import Moonlight.Sheaf.Site.Stalk.Restriction
  ( buildGrothendieckRestrictions,
  )
import Moonlight.Sheaf.Site.Stalk.Interface
  ( CompositionWitness (..),
    InterfaceMismatch (..),
    InterfaceStalk (..),
    interfaceStalkAlgebra,
  )
import Moonlight.Sheaf.TestFixture.Branch
  ( BranchContext (..),
    BranchMismatch (..),
    BranchStalk,
    branchCompatibleAmalgamatedStalk,
    branchLeftCompatibleStalk,
    branchRightCompatibleStalk,
    branchRightIncompatibleStalk,
    branchStalk,
  )
import Moonlight.Sheaf.TestFixture.Branch.Presheaf
  ( branchCompiledStalkAlgebra,
    branchGluingAlgebra,
  )
import Moonlight.Sheaf.TestFixture.Branch.Site
  ( BranchMorphism,
    BranchSite,
    branchArrow,
    branchRootCover,
    branchSite,
  )
import Moonlight.Sheaf.TestFixture.Assertions (expectJust, expectRight)
import Moonlight.Sheaf.TestFixture.Site
  ( SampleSiteTag,
    SampleSystem (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "gluing"
    [ testCase "compatible local sections amalgamate and restrict back to cover members" testCompatibleFamilyAmalgamates,
      testCase "fraudulent amalgamation is rejected with a typed locality obstruction" testFraudulentAmalgamationIsRejected,
      testCase "missing indexed local sections are locality failures" testMissingIndexedLocalSectionIsLocalityFailure,
      testCase "compatible local sections produce an amalgamating witness" testCompatibleFamilyWitnessAmalgamates,
      testCase "branch root cover rejects base-coordinate ghost sections" testBranchRootCoverRejectsBaseGhostSections,
      testCase "branch root cover separates cover-visible target stalks" testBranchRootCoverSeparatesVisibleTargets,
      testCase "separated branch cover certifies unique compatible amalgamation" testSeparatedBranchCoverCertifiesUniqueAmalgamation,
      testCase "separated branch cover rejects a second local amalgamation for the same family" testSeparatedBranchCoverRejectsSecondLocalAmalgamation,
      testCase "separated cover and uniqueness report typed refusal arms" testSeparatedUniquenessRefusalArms,
      testCase "separated branch cover resolves compatible family by certified target search" testSeparatedBranchCoverResolvesCompatibleFamily,
      testCase "separated branch cover resolution tracks visible local sections" testSeparatedBranchCoverResolutionTracksFamily,
      testCase "separated resolution agrees with unique amalgamation certification" testSeparatedResolutionAgreesWithUniqueness,
      testCase "separated resolution reports typed refusal arms" testSeparatedResolutionRefusalArms,
      testCase "separated local equality distinguishes cover-visible target pairs" testSeparatedLocalEqualityAtSeparatesTargets,
      testCase "separated local equality treats duplicate targets as equal" testSeparatedLocalEqualityAtTreatsDuplicateTargetsAsEqual,
      testCase "overlap disagreement is reported as a typed pullback obstruction" testOverlapDisagreementIsTyped,
      testCase "first-obstruction compatibility stops at the earliest multi-overlap failure" testMultiOverlapFirstObstruction,
      testCase "Grothendieck restrictions preserve parallel local faces" testGrothendieckRestrictionsPreserveParallelLocalFaces,
      testCase "interface stalk compatibility rejects same-class distinct witnesses" testInterfaceWitnessIdentityParticipatesInCompatibility,
      testCase "overlap disagreement produces a pruning verdict" testOverlapDisagreementPrunesMatchingFamily,
      testCase "non-root matching families report gluing unavailability after compatibility" testUnavailableGluingIsTyped,
      testCase "gluing unavailability is not a pruning verdict" testUnavailableGluingDoesNotPrune,
      testCase "site plans precompute cover overlap pullbacks" testSitePlansPrecomputeOverlaps,
      testCase "effective cover constructors assign dense ascending slot keys" testEffectiveCoverPlanSlotsAreDense,
      testCase "foreign cover plans report out-of-range local slots" testForeignCoverPlanReportsMissingLocalSection,
      testCase "site plan preparation rejects missing cover pullbacks" testSitePlanMissingPullbackIsTyped,
      testCase "repeated source cover arrows remain distinct matching slots" testRepeatedSourceCoverSlotsDoNotCollapse
    ]

testCompatibleFamilyAmalgamates :: Assertion
testCompatibleFamilyAmalgamates = do
  let coverValue = branchRootCover
  matchingFamily <-
    expectRight
      ( mkMatchingFamilyForCover branchSite
          coverValue
          (Vector.fromList [branchLeftCompatibleStalk, branchRightCompatibleStalk])
      )
  leftArrow <- expectJust (branchArrow BranchLeft BranchBase)
  rightArrow <- expectJust (branchArrow BranchRight BranchBase)
  case amalgamateMatchingFamilyWith branchCompiledStalkAlgebra (gaAmalgamate branchGluingAlgebra) branchSite matchingFamily of
    Right amalgamation -> do
      let gluedStalk = amalgamatedStalk amalgamation
      gluedStalk @?= branchCompatibleAmalgamatedStalk
      restrictAlong branchSite leftArrow gluedStalk @?= branchLeftCompatibleStalk
      restrictAlong branchSite rightArrow gluedStalk @?= branchRightCompatibleStalk
    Left failure ->
      assertFailure ("expected compatible family to glue, received " <> show failure)

testCompatibleFamilyWitnessAmalgamates :: Assertion
testCompatibleFamilyWitnessAmalgamates = do
  let coverValue = branchRootCover
  matchingFamily <-
    expectRight
      ( mkMatchingFamilyForCover branchSite
          coverValue
          (Vector.fromList [branchLeftCompatibleStalk, branchRightCompatibleStalk])
      )
  case certifyMatchingFamilyCompatibility branchCompiledStalkAlgebra branchSite matchingFamily of
    Right compatibleFamily ->
      case certifyAmalgamation branchCompiledStalkAlgebra branchSite compatibleFamily branchCompatibleAmalgamatedStalk of
        Right amalgamation ->
          amalgamatedStalk amalgamation @?= branchCompatibleAmalgamatedStalk
        Left failures ->
          assertFailure ("expected lawful amalgamation to certify, received " <> show failures)
    Left failures ->
      assertFailure ("expected compatible matching-family witness, received " <> show failures)

testFraudulentAmalgamationIsRejected :: Assertion
testFraudulentAmalgamationIsRejected = do
  let coverValue = branchRootCover
  matchingFamily <-
    expectRight
      ( mkMatchingFamilyForCover branchSite
          coverValue
          (Vector.fromList [branchLeftCompatibleStalk, branchRightCompatibleStalk])
      )
  compatibleFamily <-
    either
      (\failures -> assertFailure ("expected compatible matching-family witness, received " <> show failures))
      pure
      (certifyMatchingFamilyCompatibility branchCompiledStalkAlgebra branchSite matchingFamily)
  case certifyAmalgamation branchCompiledStalkAlgebra branchSite compatibleFamily fraudulentStalk of
    Left (AmalgamationLocalityMismatch _ mismatches :| []) ->
      mismatches @?= [BranchCoordinateConflict BranchLeft 99 10]
    Left failures ->
      assertFailure ("expected a single locality failure at the left slot, received " <> show failures)
    Right _ ->
      assertFailure "expected fraudulent amalgamation to be rejected, received certified amalgamation"
  where
    fraudulentStalk =
      branchStalk [(BranchLeft, 99), (BranchRight, 20), (BranchApex, 7)]

testMissingIndexedLocalSectionIsLocalityFailure :: Assertion
testMissingIndexedLocalSectionIsLocalityFailure = do
  effectiveCover <- branchRootEffectiveCover
  amalgamationLocalityFailures
    branchCompiledStalkAlgebra
    branchSite
    effectiveCover
    (Vector.singleton branchLeftCompatibleStalk)
    branchCompatibleAmalgamatedStalk
    @?= [AmalgamationLocalSectionMissing (CoverSlotKey 1)]

testBranchRootCoverRejectsBaseGhostSections :: Assertion
testBranchRootCoverRejectsBaseGhostSections = do
  effectiveCover <- branchRootEffectiveCover
  let leftTarget = branchBaseInvisibleTarget 0
      rightTarget = branchBaseInvisibleTarget 1
      slotComparands = branchCompatibleSlotComparands effectiveCover
      universe =
        CoverStalkUniverse
          { csuTargetStalks = [leftTarget, rightTarget],
            csuSlotStalks = fmap pure slotComparands
          }
  case certifySeparatedCover branchCompiledStalkAlgebra branchSite effectiveCover universe of
    Left (SeparatedCoverGhostSections (ghost :| [])) -> do
      gsLeftStalk ghost @?= leftTarget
      gsRightStalk ghost @?= rightTarget
      gsMismatches ghost @?= [BranchCoordinateConflict BranchBase 0 1]
      gsSlotComparands ghost @?= slotComparands
      IntMap.keysSet (gsSlotComparands ghost) @?= IntMap.keysSet (effectiveCoverSlots effectiveCover)
    Left refusal ->
      assertFailure ("expected one base-coordinate ghost section, received " <> show refusal)
    Right _ ->
      assertFailure "expected base-coordinate ghost section refusal"

testBranchRootCoverSeparatesVisibleTargets :: Assertion
testBranchRootCoverSeparatesVisibleTargets = do
  effectiveCover <- branchRootEffectiveCover
  _separatedCover <- branchSeparatedCoverFixture effectiveCover
  pure ()

testSeparatedBranchCoverCertifiesUniqueAmalgamation :: Assertion
testSeparatedBranchCoverCertifiesUniqueAmalgamation = do
  effectiveCover <- branchRootEffectiveCover
  separatedCover <- branchSeparatedCoverFixture effectiveCover
  compatibleFamily <- branchCompatibleFamilyFromPlan effectiveCover
  uniqueAmalgamation <-
    expectRight
      ( certifyUniqueAmalgamation
          branchCompiledStalkAlgebra
          branchSite
          separatedCover
          compatibleFamily
          branchCompatibleAmalgamatedStalk
      )
  amalgamatedStalk (uniqueAmalgamationUnderlying uniqueAmalgamation) @?= branchCompatibleAmalgamatedStalk

testSeparatedBranchCoverRejectsSecondLocalAmalgamation :: Assertion
testSeparatedBranchCoverRejectsSecondLocalAmalgamation = do
  effectiveCover <- branchRootEffectiveCover
  separatedCover <- branchSeparatedCoverFixture effectiveCover
  compatibleFamily <- branchCompatibleFamilyFromPlan effectiveCover
  let expectedLeftSlotKeys =
        IntMap.keysSet
          (slotValuesByCoverSource effectiveCover (Map.singleton BranchLeft branchLeftCompatibleStalk))
      uniquenessVerdict =
        certifyUniqueAmalgamation
          branchCompiledStalkAlgebra
          branchSite
          separatedCover
          compatibleFamily
          branchVisibleVariantAmalgamatedStalk
  case uniquenessVerdict of
    Left (UniquenessNotLocal (AmalgamationLocalityMismatch slotKey mismatches :| [])) -> do
      IntSet.singleton (unCoverSlotKey slotKey) @?= expectedLeftSlotKeys
      mismatches @?= [BranchCoordinateConflict BranchLeft 11 10]
    Left refusal ->
      assertFailure ("expected uniqueness locality refusal, received " <> show refusal)
    Right _ ->
      assertFailure "expected second in-universe target to fail locality for the same family"

testSeparatedUniquenessRefusalArms :: Assertion
testSeparatedUniquenessRefusalArms = do
  effectiveCover <- branchRootEffectiveCover
  assertSeparatedCoverUniverseIncomplete effectiveCover
  separatedCover <- branchSeparatedCoverFixture effectiveCover
  compatibleFamily <- branchCompatibleFamilyFromPlan effectiveCover
  assertUniquenessCoverPlanMismatch separatedCover
  assertUniquenessAmalgamatedStalkOutsideUniverse separatedCover compatibleFamily
  assertUniquenessFamilySectionsOutsideUniverse effectiveCover separatedCover

testSeparatedBranchCoverResolvesCompatibleFamily :: Assertion
testSeparatedBranchCoverResolvesCompatibleFamily = do
  effectiveCover <- branchRootEffectiveCover
  separatedCover <- branchSeparatedCoverFixture effectiveCover
  compatibleFamily <- branchCompatibleFamilyFromPlan effectiveCover
  uniqueAmalgamation <-
    expectRight
      ( resolveUniqueAmalgamation
          branchCompiledStalkAlgebra
          separatedCover
          compatibleFamily
      )
  amalgamatedStalk (uniqueAmalgamationUnderlying uniqueAmalgamation) @?= branchCompatibleAmalgamatedStalk

testSeparatedBranchCoverResolutionTracksFamily :: Assertion
testSeparatedBranchCoverResolutionTracksFamily = do
  effectiveCover <- branchRootEffectiveCover
  separatedCover <- branchSeparatedCoverFixture effectiveCover
  variantFamily <- branchVisibleVariantFamilyFromPlan effectiveCover
  uniqueAmalgamation <-
    expectRight
      ( resolveUniqueAmalgamation
          branchCompiledStalkAlgebra
          separatedCover
          variantFamily
      )
  amalgamatedStalk (uniqueAmalgamationUnderlying uniqueAmalgamation) @?= branchVisibleVariantAmalgamatedStalk

testSeparatedResolutionAgreesWithUniqueness :: Assertion
testSeparatedResolutionAgreesWithUniqueness = do
  effectiveCover <- branchRootEffectiveCover
  separatedCover <- branchSeparatedCoverFixture effectiveCover
  compatibleFamily <- branchCompatibleFamilyFromPlan effectiveCover
  resolvedAmalgamation <-
    expectRight
      ( resolveUniqueAmalgamation
          branchCompiledStalkAlgebra
          separatedCover
          compatibleFamily
      )
  let resolvedStalk =
        amalgamatedStalk (uniqueAmalgamationUnderlying resolvedAmalgamation)
  certifiedResolvedAmalgamation <-
    expectRight
      ( certifyUniqueAmalgamation
          branchCompiledStalkAlgebra
          branchSite
          separatedCover
          compatibleFamily
          resolvedStalk
      )
  amalgamatedStalk (uniqueAmalgamationUnderlying certifiedResolvedAmalgamation) @?= resolvedStalk
  mintedAmalgamation <-
    expectRight
      ( certifyUniqueAmalgamation
          branchCompiledStalkAlgebra
          branchSite
          separatedCover
          compatibleFamily
          branchCompatibleAmalgamatedStalk
      )
  resolvedMintedAmalgamation <-
    expectRight
      ( resolveUniqueAmalgamation
          branchCompiledStalkAlgebra
          separatedCover
          compatibleFamily
      )
  amalgamatedStalk (uniqueAmalgamationUnderlying resolvedMintedAmalgamation)
    @?= amalgamatedStalk (uniqueAmalgamationUnderlying mintedAmalgamation)

testSeparatedResolutionRefusalArms :: Assertion
testSeparatedResolutionRefusalArms = do
  effectiveCover <- branchRootEffectiveCover
  separatedCover <- branchSeparatedCoverFixture effectiveCover
  assertResolutionCoverPlanMismatch separatedCover
  assertResolutionFamilySectionsOutsideUniverse effectiveCover separatedCover
  assertResolutionNoLocalTarget effectiveCover

assertResolutionCoverPlanMismatch ::
  SeparatedCover BranchSite BranchStalk ->
  Assertion
assertResolutionCoverPlanMismatch separatedCover = do
  apexToLeft <- expectJust (branchArrow BranchApex BranchLeft)
  leftCover <- expectRight (mkMatchingCover BranchLeft apexToLeft)
  leftEffectiveCover <- expectRight (prepareEffectiveCoverPlan branchSite leftCover)
  leftMatchingFamily <-
    expectRight
      ( mkMatchingFamily
          leftEffectiveCover
          (Vector.singleton (branchStalk [(BranchApex, 7)]))
      )
  leftCompatibleFamily <-
    expectRight
      (certifyMatchingFamilyCompatibility branchCompiledStalkAlgebra branchSite leftMatchingFamily)
  case resolveUniqueAmalgamation branchCompiledStalkAlgebra separatedCover leftCompatibleFamily of
    Left ResolutionCoverPlanMismatch ->
      pure ()
    Left refusal ->
      assertFailure ("expected resolution cover-plan mismatch refusal, received " <> show refusal)
    Right _ ->
      assertFailure "expected resolution cover-plan mismatch refusal"

assertResolutionFamilySectionsOutsideUniverse ::
  EffectiveCoverPlan BranchContext BranchMorphism ->
  SeparatedCover BranchSite BranchStalk ->
  Assertion
assertResolutionFamilySectionsOutsideUniverse effectiveCover separatedCover = do
  let foreignLeftSection = branchStalk [(BranchLeft, 55), (BranchApex, 7)]
      foreignSections =
        Vector.fromList [foreignLeftSection, branchRightCompatibleStalk]
      expectedForeignSlots =
        IntMap.keysSet
          (slotValuesByCoverSource effectiveCover (Map.singleton BranchLeft foreignLeftSection))
  foreignMatchingFamily <- expectRight (mkMatchingFamily effectiveCover foreignSections)
  foreignCompatibleFamily <-
    expectRight
      (certifyMatchingFamilyCompatibility branchCompiledStalkAlgebra branchSite foreignMatchingFamily)
  case resolveUniqueAmalgamation branchCompiledStalkAlgebra separatedCover foreignCompatibleFamily of
    Left (ResolutionFamilySectionsOutsideUniverse foreignSlots) ->
      foreignSlots @?= expectedForeignSlots
    Left refusal ->
      assertFailure ("expected resolution family-sections-outside-universe refusal, received " <> show refusal)
    Right _ ->
      assertFailure "expected resolution family-sections-outside-universe refusal"

assertResolutionNoLocalTarget ::
  EffectiveCoverPlan BranchContext BranchMorphism ->
  Assertion
assertResolutionNoLocalTarget effectiveCover = do
  compatibleFamily <- branchCompatibleFamilyFromPlan effectiveCover
  separatedCover <-
    expectRight
      ( certifySeparatedCover
          branchCompiledStalkAlgebra
          branchSite
          effectiveCover
          ( (branchSeparatedCoverUniverse effectiveCover)
              { csuTargetStalks = [branchVisibleVariantAmalgamatedStalk]
              }
          )
      )
  let expectedLeftSlotKeys =
        IntMap.keysSet
          (slotValuesByCoverSource effectiveCover (Map.singleton BranchLeft branchLeftCompatibleStalk))
  case resolveUniqueAmalgamation branchCompiledStalkAlgebra separatedCover compatibleFamily of
    Left (ResolutionNoLocalTarget failuresByTarget) -> do
      IntMap.keysSet failuresByTarget @?= IntSet.singleton 0
      case IntMap.lookup 0 failuresByTarget of
        Just (AmalgamationLocalityMismatch slotKey mismatches :| []) -> do
          IntSet.singleton (unCoverSlotKey slotKey) @?= expectedLeftSlotKeys
          mismatches @?= [BranchCoordinateConflict BranchLeft 11 10]
        Just failures ->
          assertFailure ("expected one conflicting left-slot target failure, received " <> show failures)
        Nothing ->
          assertFailure "expected target-index 0 locality failures"
    Left refusal ->
      assertFailure ("expected resolution no-local-target refusal, received " <> show refusal)
    Right _ ->
      assertFailure "expected resolution no-local-target refusal"

testSeparatedLocalEqualityAtSeparatesTargets :: Assertion
testSeparatedLocalEqualityAtSeparatesTargets = do
  effectiveCover <- branchRootEffectiveCover
  separatedCover <- branchSeparatedCoverFixture effectiveCover
  leftSlotKey <- branchLeftSlotKeyFor effectiveCover
  let targetsByIndex =
        IntMap.fromList (zip [0 :: Int ..] (csuTargetStalks (branchSeparatedCoverUniverse effectiveCover)))
  sequence_
    [ assertSeparatedLocalEqualityPair separatedCover targetsByIndex leftSlotKey (leftIndex, rightIndex)
    | leftIndex <- [0, 1],
      rightIndex <- [0, 1]
    ]
  case separatedLocalEqualityAt branchCompiledStalkAlgebra separatedCover 0 2 of
    Left (EqualityTargetIndexOutOfRange invalidIndices) ->
      invalidIndices @?= IntSet.singleton 2
    Right verdict ->
      assertFailure ("expected target-index-out-of-range equality refusal, received " <> show verdict)

assertSeparatedLocalEqualityPair ::
  SeparatedCover BranchSite BranchStalk ->
  IntMap.IntMap BranchStalk ->
  CoverSlotKey ->
  (Int, Int) ->
  Assertion
assertSeparatedLocalEqualityPair separatedCover targetsByIndex leftSlotKey (leftIndex, rightIndex) = do
  leftStalk <- expectJust (IntMap.lookup leftIndex targetsByIndex)
  rightStalk <- expectJust (IntMap.lookup rightIndex targetsByIndex)
  let expectedVerdict =
        if leftStalk == rightStalk
          then SeparatedStalksEqual
          else SeparatedStalksDistinguished leftSlotKey
  (leftStalk == rightStalk) @?= (leftIndex == rightIndex)
  case separatedLocalEqualityAt branchCompiledStalkAlgebra separatedCover leftIndex rightIndex of
    Right verdict -> do
      verdict @?= expectedVerdict
      (verdict == SeparatedStalksEqual) @?= (leftStalk == rightStalk)
    Left refusal ->
      assertFailure ("expected separated local equality verdict, received " <> show refusal)

testSeparatedLocalEqualityAtTreatsDuplicateTargetsAsEqual :: Assertion
testSeparatedLocalEqualityAtTreatsDuplicateTargetsAsEqual = do
  effectiveCover <- branchRootEffectiveCover
  duplicateTargetCover <-
    expectRight
      ( certifySeparatedCover
          branchCompiledStalkAlgebra
          branchSite
          effectiveCover
          ( (branchSeparatedCoverUniverse effectiveCover)
              { csuTargetStalks =
                  [ branchCompatibleAmalgamatedStalk,
                    branchCompatibleAmalgamatedStalk
                  ]
              }
          )
      )
  separatedLocalEqualityAt branchCompiledStalkAlgebra duplicateTargetCover 0 1
    @?= Right SeparatedStalksEqual

assertSeparatedCoverUniverseIncomplete ::
  EffectiveCoverPlan BranchContext BranchMorphism ->
  Assertion
assertSeparatedCoverUniverseIncomplete effectiveCover = do
  let slotStalks =
        slotValuesByCoverSource
          effectiveCover
          (Map.singleton BranchRight [branchRightCompatibleStalk])
      expectedMissingSlots =
        IntSet.difference (IntMap.keysSet (effectiveCoverSlots effectiveCover)) (IntMap.keysSet slotStalks)
      universe =
        CoverStalkUniverse
          { csuTargetStalks = [branchCompatibleAmalgamatedStalk],
            csuSlotStalks = slotStalks
          }
  case certifySeparatedCover branchCompiledStalkAlgebra branchSite effectiveCover universe of
    Left (SeparatedCoverUniverseIncomplete missingSlots) ->
      missingSlots @?= expectedMissingSlots
    Left refusal ->
      assertFailure ("expected universe-incomplete refusal, received " <> show refusal)
    Right _ ->
      assertFailure "expected universe-incomplete refusal"

assertUniquenessCoverPlanMismatch ::
  SeparatedCover BranchSite BranchStalk ->
  Assertion
assertUniquenessCoverPlanMismatch separatedCover = do
  apexToLeft <- expectJust (branchArrow BranchApex BranchLeft)
  leftCover <- expectRight (mkMatchingCover BranchLeft apexToLeft)
  leftEffectiveCover <- expectRight (prepareEffectiveCoverPlan branchSite leftCover)
  leftMatchingFamily <-
    expectRight
      ( mkMatchingFamily
          leftEffectiveCover
          (Vector.singleton (branchStalk [(BranchApex, 7)]))
      )
  leftCompatibleFamily <-
    expectRight
      (certifyMatchingFamilyCompatibility branchCompiledStalkAlgebra branchSite leftMatchingFamily)
  let uniquenessVerdict =
        certifyUniqueAmalgamation
          branchCompiledStalkAlgebra
          branchSite
          separatedCover
          leftCompatibleFamily
          branchCompatibleAmalgamatedStalk
  case uniquenessVerdict of
    Left UniquenessCoverPlanMismatch ->
      pure ()
    Left refusal ->
      assertFailure ("expected cover-plan mismatch refusal, received " <> show refusal)
    Right _ ->
      assertFailure "expected cover-plan mismatch refusal"

assertUniquenessAmalgamatedStalkOutsideUniverse ::
  SeparatedCover BranchSite BranchStalk ->
  CompatibleMatchingFamily BranchSite BranchStalk ->
  Assertion
assertUniquenessAmalgamatedStalkOutsideUniverse separatedCover compatibleFamily =
  let uniquenessVerdict =
        certifyUniqueAmalgamation
          branchCompiledStalkAlgebra
          branchSite
          separatedCover
          compatibleFamily
          branchOutsideUniverseAmalgamatedStalk
   in case uniquenessVerdict of
        Left UniquenessAmalgamatedStalkOutsideUniverse ->
          pure ()
        Left refusal ->
          assertFailure ("expected outside-universe refusal, received " <> show refusal)
        Right _ ->
          assertFailure "expected outside-universe refusal"

assertUniquenessFamilySectionsOutsideUniverse ::
  EffectiveCoverPlan BranchContext BranchMorphism ->
  SeparatedCover BranchSite BranchStalk ->
  Assertion
assertUniquenessFamilySectionsOutsideUniverse effectiveCover separatedCover = do
  let foreignLeftSection = branchStalk [(BranchLeft, 12), (BranchApex, 7)]
      foreignSections =
        Vector.fromList [foreignLeftSection, branchRightCompatibleStalk]
      expectedForeignSlots =
        IntMap.keysSet
          (slotValuesByCoverSource effectiveCover (Map.singleton BranchLeft foreignLeftSection))
  foreignMatchingFamily <- expectRight (mkMatchingFamily effectiveCover foreignSections)
  foreignCompatibleFamily <-
    expectRight
      (certifyMatchingFamilyCompatibility branchCompiledStalkAlgebra branchSite foreignMatchingFamily)
  let uniquenessVerdict =
        certifyUniqueAmalgamation
          branchCompiledStalkAlgebra
          branchSite
          separatedCover
          foreignCompatibleFamily
          branchCompatibleAmalgamatedStalk
  case uniquenessVerdict of
    Left (UniquenessFamilySectionsOutsideUniverse foreignSlots) ->
      foreignSlots @?= expectedForeignSlots
    Left refusal ->
      assertFailure ("expected family-sections-outside-universe refusal, received " <> show refusal)
    Right _ ->
      assertFailure "expected family-sections-outside-universe refusal"

branchRootEffectiveCover :: IO (EffectiveCoverPlan BranchContext BranchMorphism)
branchRootEffectiveCover = do
  let coverValue = branchRootCover
  expectRight (prepareEffectiveCoverPlan branchSite coverValue)

branchCompatibleFamilyFromPlan ::
  EffectiveCoverPlan BranchContext BranchMorphism ->
  IO (CompatibleMatchingFamily BranchSite BranchStalk)
branchCompatibleFamilyFromPlan effectiveCover =
  branchCompatibleFamilyFromPlanWith effectiveCover branchLeftCompatibleStalk branchRightCompatibleStalk

branchVisibleVariantFamilyFromPlan ::
  EffectiveCoverPlan BranchContext BranchMorphism ->
  IO (CompatibleMatchingFamily BranchSite BranchStalk)
branchVisibleVariantFamilyFromPlan effectiveCover =
  branchCompatibleFamilyFromPlanWith effectiveCover branchLeftVisibleVariantStalk branchRightCompatibleStalk

branchCompatibleFamilyFromPlanWith ::
  EffectiveCoverPlan BranchContext BranchMorphism ->
  BranchStalk ->
  BranchStalk ->
  IO (CompatibleMatchingFamily BranchSite BranchStalk)
branchCompatibleFamilyFromPlanWith effectiveCover leftStalk rightStalk = do
  matchingFamily <-
    expectRight
      ( mkMatchingFamily
          effectiveCover
          (Vector.fromList [leftStalk, rightStalk])
      )
  expectRight (certifyMatchingFamilyCompatibility branchCompiledStalkAlgebra branchSite matchingFamily)

branchSeparatedCoverFixture ::
  EffectiveCoverPlan BranchContext BranchMorphism ->
  IO (SeparatedCover BranchSite BranchStalk)
branchSeparatedCoverFixture effectiveCover =
  expectRight
    ( certifySeparatedCover
        branchCompiledStalkAlgebra
        branchSite
        effectiveCover
        (branchSeparatedCoverUniverse effectiveCover)
    )

branchSeparatedCoverUniverse ::
  EffectiveCoverPlan BranchContext BranchMorphism ->
  CoverStalkUniverse BranchStalk
branchSeparatedCoverUniverse effectiveCover =
  CoverStalkUniverse
    { csuTargetStalks =
        [ branchCompatibleAmalgamatedStalk,
          branchVisibleVariantAmalgamatedStalk
        ],
      csuSlotStalks =
        slotValuesByCoverSource
          effectiveCover
          ( Map.fromList
              [ (BranchLeft, [branchLeftCompatibleStalk, branchLeftVisibleVariantStalk]),
                (BranchRight, [branchRightCompatibleStalk])
              ]
          )
    }

branchLeftSlotKeyFor ::
  EffectiveCoverPlan BranchContext BranchMorphism ->
  IO CoverSlotKey
branchLeftSlotKeyFor effectiveCover =
  case IntMap.keys (slotValuesByCoverSource effectiveCover (Map.singleton BranchLeft branchLeftCompatibleStalk)) of
    [slotKeyInt] ->
      pure (CoverSlotKey slotKeyInt)
    slotKeys ->
      assertFailure ("expected one BranchLeft cover slot, received " <> show slotKeys)

branchCompatibleSlotComparands ::
  EffectiveCoverPlan BranchContext BranchMorphism ->
  IntMap.IntMap BranchStalk
branchCompatibleSlotComparands effectiveCover =
  slotValuesByCoverSource
    effectiveCover
    ( Map.fromList
        [ (BranchLeft, branchLeftCompatibleStalk),
          (BranchRight, branchRightCompatibleStalk)
        ]
    )

branchBaseInvisibleTarget :: Int -> BranchStalk
branchBaseInvisibleTarget baseValue =
  branchStalk
    [ (BranchBase, baseValue),
      (BranchLeft, 10),
      (BranchRight, 20),
      (BranchApex, 7)
    ]

branchLeftVisibleVariantStalk :: BranchStalk
branchLeftVisibleVariantStalk =
  branchStalk [(BranchLeft, 11), (BranchApex, 7)]

branchVisibleVariantAmalgamatedStalk :: BranchStalk
branchVisibleVariantAmalgamatedStalk =
  branchStalk [(BranchLeft, 11), (BranchRight, 20), (BranchApex, 7)]

branchOutsideUniverseAmalgamatedStalk :: BranchStalk
branchOutsideUniverseAmalgamatedStalk =
  branchStalk [(BranchLeft, 10), (BranchRight, 21), (BranchApex, 7)]

testOverlapDisagreementIsTyped :: Assertion
testOverlapDisagreementIsTyped = do
  let coverValue = branchRootCover
  matchingFamily <-
    expectRight
      ( mkMatchingFamilyForCover branchSite
          coverValue
          (Vector.fromList [branchLeftCompatibleStalk, branchRightIncompatibleStalk])
      )
  let compatibilityFailures = pairwiseCompatibilityFailures branchCompiledStalkAlgebra branchSite matchingFamily
  case compatibilityFailures of
    [PullbackDisagreement square [BranchCoordinateConflict BranchApex 7 8]] -> do
      cmSource (psToLeft square) @?= BranchApex
      cmSource (psToRight square) @?= BranchApex
    otherFailures ->
      assertFailure ("expected one apex pullback disagreement, received " <> show otherFailures)
  case certifyMatchingFamilyCompatibility branchCompiledStalkAlgebra branchSite matchingFamily of
    Left (PullbackDisagreement _ [BranchCoordinateConflict BranchApex 7 8] :| []) ->
      pure ()
    Left failures ->
      assertFailure ("expected certified obstruction to retain the apex disagreement, received " <> show failures)
    Right _ ->
      assertFailure "expected incompatible matching family witness rejection"
  case amalgamateMatchingFamilyWith branchCompiledStalkAlgebra (gaAmalgamate branchGluingAlgebra) branchSite matchingFamily of
    Left (IncompatibleMatchingFamily (_ :| [])) ->
      pure ()
    Left failure ->
      assertFailure ("expected incompatible matching family, received " <> show failure)
    Right _ ->
      assertFailure "expected incompatible matching family, received successful amalgamation"

data TripleCell
  = TripleGlobal
  | TripleA
  | TripleB
  | TripleC
  | TripleAB
  | TripleAC
  | TripleBC
  | TripleABC
  deriving stock (Eq, Ord, Show)

data TripleStalk = TripleStalk
  { tripleABValue :: !Int,
    tripleACValue :: !Int,
    tripleBCValue :: !Int
  }
  deriving stock (Eq, Show)

data TripleMismatch = TripleMismatch !TripleStalk !TripleStalk
  deriving stock (Eq, Show)

type TripleSite = FiniteMeetSite TripleCell

tripleSiteSpec :: FiniteMeetSiteSpec TripleCell
tripleSiteSpec =
  FiniteMeetSiteSpec
    { fmssCells = TripleGlobal :| [TripleA, TripleB, TripleC, TripleAB, TripleAC, TripleBC, TripleABC],
      fmssRefinements =
        Set.fromList
          [ (TripleA, TripleGlobal),
            (TripleB, TripleGlobal),
            (TripleC, TripleGlobal),
            (TripleAB, TripleA),
            (TripleAB, TripleB),
            (TripleAC, TripleA),
            (TripleAC, TripleC),
            (TripleBC, TripleB),
            (TripleBC, TripleC),
            (TripleABC, TripleAB),
            (TripleABC, TripleAC),
            (TripleABC, TripleBC)
          ],
      fmssCovers = Map.singleton TripleGlobal [TripleA :| [TripleB, TripleC]]
    }

tripleStalkAlgebra :: StalkAlgebra (CompiledRestriction TripleSite) TripleStalk TripleMismatch ()
tripleStalkAlgebra =
  StalkAlgebra
    { saRestrictionKernel = tripleRestrictionKernel,
      saMismatches = \left right -> [TripleMismatch left right | left /= right],
      saMerge = \left _right -> Right left,
      saRepair = const (Left ()),
      saNormalize = id
    }

tripleRestrictionKernel :: CompiledRestriction TripleSite -> StalkRestrictionKernel TripleStalk
tripleRestrictionKernel (CompiledRestriction _site morphism) =
  case cmSource morphism of
    TripleAB ->
      StalkRestrictionMap (\stalk -> TripleStalk (tripleABValue stalk) 0 0)
    TripleAC ->
      StalkRestrictionMap (\stalk -> TripleStalk 0 (tripleACValue stalk) 0)
    TripleBC ->
      StalkRestrictionMap (\stalk -> TripleStalk 0 0 (tripleBCValue stalk))
    _ ->
      StalkRestrictionIdentity

tripleMatchingFamily ::
  Vector TripleStalk ->
  Either String (TripleSite, MatchingFamily TripleSite TripleStalk)
tripleMatchingFamily sections = do
  site <- first show (mkFiniteMeetSite tripleSiteSpec)
  coverValue <- tripleRootCover site
  family <- first show (mkMatchingFamilyForCover site coverValue sections)
  Right (site, family)

tripleRootCover ::
  TripleSite ->
  Either String (CoveringFamily TripleCell (FiniteMeetMorphism TripleCell))
tripleRootCover site = do
  aToGlobal <- tripleMorphism TripleA TripleGlobal
  bToGlobal <- tripleMorphism TripleB TripleGlobal
  cToGlobal <- tripleMorphism TripleC TripleGlobal
  case mkCoveringFamily TripleGlobal (aToGlobal :| [bToGlobal, cToGlobal]) of
    Left failure ->
      Left (show failure)
    Right coverValue ->
      Right coverValue
  where
    tripleMorphism source target =
      case finiteMeetMorphism site source target of
        Nothing ->
          Left ("missing triple morphism " <> show source <> " -> " <> show target)
        Just morphism ->
          Right morphism

tripleCompatibleSections :: Vector TripleStalk
tripleCompatibleSections =
  Vector.fromList
    [ TripleStalk 1 2 0,
      TripleStalk 1 0 3,
      TripleStalk 0 2 3
    ]

tripleFirstObstructedSections :: Vector TripleStalk
tripleFirstObstructedSections =
  Vector.fromList
    [ TripleStalk 10 2 0,
      TripleStalk 11 0 3,
      TripleStalk 0 2 3
    ]

tripleLastObstructedSections :: Vector TripleStalk
tripleLastObstructedSections =
  Vector.fromList
    [ TripleStalk 1 2 0,
      TripleStalk 1 0 30,
      TripleStalk 0 2 31
    ]

tripleEverywhereObstructedSections :: Vector TripleStalk
tripleEverywhereObstructedSections =
  Vector.fromList
    [ TripleStalk 10 20 0,
      TripleStalk 11 0 30,
      TripleStalk 0 21 31
    ]

testMultiOverlapFirstObstruction :: Assertion
testMultiOverlapFirstObstruction = do
  (compatibleSite, compatibleFamily) <- expectRight (tripleMatchingFamily tripleCompatibleSections)
  case certifyMatchingFamilyCompatibilityFirstObstruction tripleStalkAlgebra compatibleSite compatibleFamily of
    Right _compatibleFamily ->
      pure ()
    Left failure ->
      assertFailure ("expected compatible triple family, received " <> show failure)

  (firstSite, firstFamily) <- expectRight (tripleMatchingFamily tripleFirstObstructedSections)
  assertFailureApexes firstSite firstFamily [TripleAB]
  assertFirstFailureApex firstSite firstFamily TripleAB

  (lastSite, lastFamily) <- expectRight (tripleMatchingFamily tripleLastObstructedSections)
  assertFailureApexes lastSite lastFamily [TripleBC]
  assertFirstFailureApex lastSite lastFamily TripleBC

  (everywhereSite, everywhereFamily) <- expectRight (tripleMatchingFamily tripleEverywhereObstructedSections)
  assertFailureApexes everywhereSite everywhereFamily [TripleAB, TripleAC, TripleBC]
  assertFirstFailureApex everywhereSite everywhereFamily TripleAB

assertFailureApexes :: TripleSite -> MatchingFamily TripleSite TripleStalk -> [TripleCell] -> Assertion
assertFailureApexes site matchingFamily expectedApexes =
  failureApexes (pairwiseCompatibilityFailures tripleStalkAlgebra site matchingFamily) @?= expectedApexes

assertFirstFailureApex :: TripleSite -> MatchingFamily TripleSite TripleStalk -> TripleCell -> Assertion
assertFirstFailureApex site matchingFamily expectedApex =
  case certifyMatchingFamilyCompatibilityFirstObstruction tripleStalkAlgebra site matchingFamily of
    Left (PullbackDisagreement square [_mismatch]) ->
      psApex square @?= expectedApex
    Left failure ->
      assertFailure ("expected pullback disagreement, received " <> show failure)
    Right _compatibleFamily ->
      assertFailure "expected incompatible triple family"

failureApexes :: [MatchingFailure TripleCell (FiniteMeetMorphism TripleCell) TripleMismatch] -> [TripleCell]
failureApexes =
  mapMaybe failureApex

failureApex :: MatchingFailure TripleCell (FiniteMeetMorphism TripleCell) TripleMismatch -> Maybe TripleCell
failureApex failure =
  case failure of
    PullbackDisagreement square _mismatches ->
      Just (psApex square)
    MissingPullback _left _right ->
      Nothing
    MissingLocalSection _slot ->
      Nothing

testGrothendieckRestrictionsPreserveParallelLocalFaces :: Assertion
testGrothendieckRestrictionsPreserveParallelLocalFaces = do
  let idempotentMorphismId = FinGeneratorMorphismId (FinGeneratorId 10)
  categoryValue <-
    expectRight
      ( mkFinCat
          (Set.singleton (FinObjectId 0))
          (Map.singleton (FinObjectId 0, FinObjectId 0) [idempotentMorphismId])
          (Map.singleton (idempotentMorphismId, idempotentMorphismId) idempotentMorphismId)
      )
  let siteValue =
        mkGrothendieckSite (SampleSystem categoryValue) 2
      faceMorphisms =
        grothendieckSiteFaceMorphisms siteValue
      endpointPairs =
        Set.fromList
          [ (grothendieckFaceMorphismSource faceValue, grothendieckFaceMorphismTarget faceValue)
          | faceValue <- faceMorphisms
          ]
  Set.size endpointPairs @?= 2
  restrictionIndex <- expectRight (buildGrothendieckRestrictions siteValue)
  restrictionCount restrictionIndex @?= length faceMorphisms

testInterfaceWitnessIdentityParticipatesInCompatibility :: Assertion
testInterfaceWitnessIdentityParticipatesInCompatibility = do
  leftMorphism <- expectRight (mkFinMorphism sampleFinCat (FinGeneratorMorphismId (FinGeneratorId 10)))
  rightMorphism <- expectRight (mkFinMorphism sampleFinCat (FinGeneratorMorphismId (FinGeneratorId 11)))
  stalkMismatches
    interfaceStalkAlgebra
    (interfaceStalkWithWitness (ComposedWitness leftMorphism))
    (interfaceStalkWithWitness (ComposedWitness rightMorphism))
    @?= [WitnessValueMismatch]

interfaceStalkWithWitness :: CompositionWitness SampleSiteTag -> InterfaceStalk SampleSiteTag
interfaceStalkWithWitness witnessValue =
  InterfaceStalk
    { rsBoundNames = Set.empty,
      rsDeletedNames = Set.empty,
      rsCreatedNames = Set.empty,
      rsGuarded = False,
      rsWitness = witnessValue,
      rsCellDimension = 1
    }

testOverlapDisagreementPrunesMatchingFamily :: Assertion
testOverlapDisagreementPrunesMatchingFamily = do
  let coverValue = branchRootCover
  matchingFamily <-
    expectRight
      ( mkMatchingFamilyForCover branchSite
          coverValue
          (Vector.fromList [branchLeftCompatibleStalk, branchRightIncompatibleStalk])
      )
  case matchingFamilyPruningVerdict branchCompiledStalkAlgebra branchSite matchingFamily of
    Rejected (MatchingFamilyIncompatible (PullbackDisagreement _ [BranchCoordinateConflict BranchApex 7 8]) :| []) ->
      pure ()
    otherVerdict ->
      assertFailure ("expected one typed matching-family pruning obstruction, received " <> show otherVerdict)

testUnavailableGluingIsTyped :: Assertion
testUnavailableGluingIsTyped = do
  apexToLeft <- expectJust (branchArrow BranchApex BranchLeft)
  leftCover <- expectRight (mkMatchingCover BranchLeft apexToLeft)
  matchingFamily <-
    expectRight
      (mkMatchingFamilyForCover branchSite leftCover (Vector.singleton (branchStalk [(BranchApex, 7)])))
  case amalgamateMatchingFamilyWith branchCompiledStalkAlgebra (gaAmalgamate branchGluingAlgebra) branchSite matchingFamily of
    Left (GluingObstructed (GluingUnavailable BranchLeft)) ->
      pure ()
    Left failure ->
      assertFailure ("expected typed gluing unavailability, received " <> show failure)
    Right _ ->
      assertFailure "expected typed gluing unavailability, received successful amalgamation"

testUnavailableGluingDoesNotPrune :: Assertion
testUnavailableGluingDoesNotPrune = do
  apexToLeft <- expectJust (branchArrow BranchApex BranchLeft)
  leftCover <- expectRight (mkMatchingCover BranchLeft apexToLeft)
  matchingFamily <-
    expectRight
      (mkMatchingFamilyForCover branchSite leftCover (Vector.singleton (branchStalk [(BranchApex, 7)])))
  matchingFamilyPruningVerdict branchCompiledStalkAlgebra branchSite matchingFamily
    @?= (Accepted () :: ObstructionVerdict (MatchingFamilyPruningObstruction BranchContext BranchMorphism BranchMismatch))

mkMatchingFamilyForCover ::
  Site site =>
  site ->
  CoveringFamily (SiteObject site) (SiteMorphism site) ->
  Vector stalk ->
  Either
    (Either (EffectiveCoverPlanFailure (SiteObject site) (SiteMorphism site)) MatchingFamilyConstructionError)
    (MatchingFamily site stalk)
mkMatchingFamilyForCover site coverValue sections = do
  effectiveCover <-
    case prepareEffectiveCoverPlan site coverValue of
      Left failure ->
        Left (Left failure)
      Right planValue ->
        Right planValue
  case mkMatchingFamily effectiveCover sections of
    Left failure ->
      Left (Right failure)
    Right matchingFamily ->
      Right matchingFamily

slotValuesByCoverSource ::
  Ord obj =>
  EffectiveCoverPlan obj mor ->
  Map obj stalk ->
  IntMap.IntMap stalk
slotValuesByCoverSource effectiveCover sectionsBySource =
  IntMap.fromList
    [ (slotIndex, section)
    | slot <- IntMap.elems (effectiveCoverSlots effectiveCover),
      let slotIndex = unCoverSlotKey (coverSlotKey slot),
      Just section <- [Map.lookup (cmSource (coverSlotArrow slot)) sectionsBySource]
    ]

mkMatchingCover ::
  BranchContext ->
  CheckedMorphism BranchContext BranchMorphism ->
  Either
    (CoverConstructionError BranchContext)
    (CoveringFamily BranchContext BranchMorphism)
mkMatchingCover targetContext arrow =
  mkCoveringFamily targetContext (arrow :| [])

testSitePlansPrecomputeOverlaps :: Assertion
testSitePlansPrecomputeOverlaps = do
  sitePlans <-
    expectRight
      (prepareSitePlans (mkObjectIndex (siteObjects branchSite)) branchSite)
  case siteCoverPlansAt (toEnum 0) sitePlans of
    [coverPlan] ->
      case effectiveCoverOverlapPlans (cpEffectiveCover coverPlan) of
        [overlapPlan] -> do
          cpSourceKeys coverPlan @?= [toEnum 1, toEnum 2]
          psApex (opPullbackSquare overlapPlan) @?= BranchApex
        otherOverlaps ->
          assertFailure ("expected one prepared overlap, received " <> show otherOverlaps)
    otherPlans ->
      assertFailure ("expected one prepared BranchBase cover, received " <> show otherPlans)

testEffectiveCoverPlanSlotsAreDense :: Assertion
testEffectiveCoverPlanSlotsAreDense = do
  let rootCover = branchRootCover
  preparedRoot <- expectRight (prepareEffectiveCoverPlan branchSite rootCover)
  leftToBase <- expectJust (branchArrow BranchLeft BranchBase)
  pulledRoot <- expectRight (pullbackEffectiveCoverPlanAlong branchSite leftToBase preparedRoot)
  sitePlans <- expectRight (prepareSitePlans (mkObjectIndex (siteObjects branchSite)) branchSite)
  repeatedCover <- expectRight repeatedSourceCover
  preparedRepeated <- expectRight (prepareEffectiveCoverPlan repeatedSourceSite repeatedCover)
  traverse_
    assertDenseSlotKeys
    ( [ preparedRoot,
        identityEffectiveCoverPlan branchSite BranchBase,
        pulledRoot
      ]
        <> fmap cpEffectiveCover (IntMap.elems (spCoversById sitePlans))
    )
  assertDenseSlotKeys preparedRepeated
  where
    assertDenseSlotKeys :: EffectiveCoverPlan obj mor -> Assertion
    assertDenseSlotKeys plan =
      fmap unCoverSlotKey (effectiveCoverSlotKeys plan)
        @?= [0 .. effectiveCoverSlotCount plan - 1]

testForeignCoverPlanReportsMissingLocalSection :: Assertion
testForeignCoverPlanReportsMissingLocalSection = do
  sitePlans <- expectRight (prepareSitePlans (mkObjectIndex (siteObjects branchSite)) branchSite)
  rootPlan <-
    case siteCoverPlansAt (toEnum 0) sitePlans of
      [plan] ->
        pure plan
      plans ->
        assertFailure ("expected one prepared BranchBase cover, received " <> show plans)
  apexToLeft <- expectJust (branchArrow BranchApex BranchLeft)
  leftCover <- expectRight (mkMatchingCover BranchLeft apexToLeft)
  leftPlan <- expectRight (prepareEffectiveCoverPlan branchSite leftCover)
  leftFamily <-
    expectRight
      (mkMatchingFamily leftPlan (Vector.singleton (branchStalk [(BranchApex, 7)])))
  case pairwiseCompatibilityFailuresFromPlan branchCompiledStalkAlgebra branchSite rootPlan leftFamily of
    [MissingLocalSection (CoverSlotKey 1)] ->
      pure ()
    failures ->
      assertFailure ("expected the foreign plan's second slot to be missing, received " <> show failures)

testSitePlanMissingPullbackIsTyped :: Assertion
testSitePlanMissingPullbackIsTyped =
  case prepareSitePlans (mkObjectIndex (siteObjects brokenPullbackBranchSite)) brokenPullbackBranchSite of
    Left (SitePlanEffectiveCoverFailed (CoverId 0) (EffectiveCoverPlanMissingPullback leftArrow rightArrow)) -> do
      cmSource leftArrow @?= BranchLeft
      cmSource rightArrow @?= BranchRight
    Left otherError ->
      assertFailure ("expected missing pullback preparation error, received " <> show otherError)
    Right _ ->
      assertFailure "expected missing pullback preparation error, received prepared site plans"

data BrokenPullbackBranchSite = BrokenPullbackBranchSite

brokenPullbackBranchSite :: BrokenPullbackBranchSite
brokenPullbackBranchSite =
  BrokenPullbackBranchSite

instance Site BrokenPullbackBranchSite where
  type SiteObject BrokenPullbackBranchSite = BranchContext
  type SiteMorphism BrokenPullbackBranchSite = BranchMorphism

  siteObjects _ =
    siteObjects branchSite

  siteMorphisms _ =
    siteMorphisms branchSite

  identityAt _ =
    identityAt branchSite

  coversAt _ =
    coversAt branchSite

  composeChecked _ =
    composeChecked branchSite

  pullbackPair _ _ _ =
    Nothing


data RepeatedSourceObject
  = RepeatedOverlap
  | RepeatedSource
  | RepeatedBase
  deriving stock (Eq, Ord, Show)

data RepeatedSourceMorphism
  = RepeatedIdentity RepeatedSourceObject
  | RepeatedEqualizer
  | RepeatedFirst
  | RepeatedSecond
  | RepeatedOverlapToBase
  deriving stock (Eq, Ord, Show)

data RepeatedSourceSite = RepeatedSourceSite
  deriving stock (Eq, Ord, Show)

repeatedSourceSite :: RepeatedSourceSite
repeatedSourceSite =
  RepeatedSourceSite

instance Site RepeatedSourceSite where
  type SiteObject RepeatedSourceSite = RepeatedSourceObject
  type SiteMorphism RepeatedSourceSite = RepeatedSourceMorphism

  siteObjects _ =
    repeatedSourceObjects

  siteMorphisms _ =
    fmap repeatedIdentityAt repeatedSourceObjects
      <> [ repeatedEqualizer,
           repeatedCoverArrow RepeatedFirst,
           repeatedCoverArrow RepeatedSecond,
           repeatedOverlapToBase
         ]

  identityAt _ =
    repeatedIdentityAt

  coversAt _ objectValue =
    [coverValue | objectValue == RepeatedBase, Right coverValue <- [repeatedSourceCover]]

  composeChecked _ outer inner
    | cmSource outer /= cmTarget inner =
        Nothing
    | repeatedIsIdentity outer =
        Just inner
    | repeatedIsIdentity inner =
        Just outer
    | cmWitness inner == RepeatedEqualizer
        && cmWitness outer `elem` [RepeatedFirst, RepeatedSecond] =
        Just repeatedOverlapToBase
    | otherwise =
        Nothing

  pullbackPair _ =
    repeatedPullbackPair

repeatedSourceObjects :: [RepeatedSourceObject]
repeatedSourceObjects =
  [RepeatedOverlap, RepeatedSource, RepeatedBase]

repeatedIdentityAt :: RepeatedSourceObject -> CheckedMorphism RepeatedSourceObject RepeatedSourceMorphism
repeatedIdentityAt objectValue =
  CheckedMorphism
    { cmSource = objectValue,
      cmTarget = objectValue,
      cmWitness = RepeatedIdentity objectValue
    }

repeatedEqualizer :: CheckedMorphism RepeatedSourceObject RepeatedSourceMorphism
repeatedEqualizer =
  CheckedMorphism
    { cmSource = RepeatedOverlap,
      cmTarget = RepeatedSource,
      cmWitness = RepeatedEqualizer
    }

repeatedCoverArrow :: RepeatedSourceMorphism -> CheckedMorphism RepeatedSourceObject RepeatedSourceMorphism
repeatedCoverArrow witness =
  CheckedMorphism
    { cmSource = RepeatedSource,
      cmTarget = RepeatedBase,
      cmWitness = witness
    }

repeatedOverlapToBase :: CheckedMorphism RepeatedSourceObject RepeatedSourceMorphism
repeatedOverlapToBase =
  CheckedMorphism
    { cmSource = RepeatedOverlap,
      cmTarget = RepeatedBase,
      cmWitness = RepeatedOverlapToBase
    }

repeatedIsIdentity :: CheckedMorphism RepeatedSourceObject RepeatedSourceMorphism -> Bool
repeatedIsIdentity morphismValue =
  case cmWitness morphismValue of
    RepeatedIdentity _ -> True
    RepeatedEqualizer -> False
    RepeatedFirst -> False
    RepeatedSecond -> False
    RepeatedOverlapToBase -> False

repeatedPullbackPair ::
  CheckedMorphism RepeatedSourceObject RepeatedSourceMorphism ->
  CheckedMorphism RepeatedSourceObject RepeatedSourceMorphism ->
  Maybe (PullbackSquare RepeatedSourceObject RepeatedSourceMorphism)
repeatedPullbackPair leftMorphism rightMorphism
  | cmTarget leftMorphism /= cmTarget rightMorphism =
      Nothing
  | repeatedIsIdentity leftMorphism =
      repeatedPullbackSquare
        leftMorphism
        rightMorphism
        (cmSource rightMorphism)
        rightMorphism
        (repeatedIdentityAt (cmSource rightMorphism))
  | repeatedIsIdentity rightMorphism =
      repeatedPullbackSquare
        leftMorphism
        rightMorphism
        (cmSource leftMorphism)
        (repeatedIdentityAt (cmSource leftMorphism))
        leftMorphism
  | leftMorphism == rightMorphism =
      repeatedPullbackSquare
        leftMorphism
        rightMorphism
        (cmSource leftMorphism)
        (repeatedIdentityAt (cmSource leftMorphism))
        (repeatedIdentityAt (cmSource rightMorphism))
  | repeatedIsCoverArrow leftMorphism && repeatedIsCoverArrow rightMorphism =
      repeatedPullbackSquare leftMorphism rightMorphism RepeatedOverlap repeatedEqualizer repeatedEqualizer
  | repeatedIsCoverArrow leftMorphism && rightMorphism == repeatedOverlapToBase =
      repeatedPullbackSquare
        leftMorphism
        rightMorphism
        RepeatedOverlap
        repeatedEqualizer
        (repeatedIdentityAt RepeatedOverlap)
  | leftMorphism == repeatedOverlapToBase && repeatedIsCoverArrow rightMorphism =
      repeatedPullbackSquare
        leftMorphism
        rightMorphism
        RepeatedOverlap
        (repeatedIdentityAt RepeatedOverlap)
        repeatedEqualizer
  | otherwise =
      Nothing

repeatedIsCoverArrow :: CheckedMorphism RepeatedSourceObject RepeatedSourceMorphism -> Bool
repeatedIsCoverArrow morphismValue =
  cmWitness morphismValue `elem` [RepeatedFirst, RepeatedSecond]

repeatedPullbackSquare ::
  CheckedMorphism RepeatedSourceObject RepeatedSourceMorphism ->
  CheckedMorphism RepeatedSourceObject RepeatedSourceMorphism ->
  RepeatedSourceObject ->
  CheckedMorphism RepeatedSourceObject RepeatedSourceMorphism ->
  CheckedMorphism RepeatedSourceObject RepeatedSourceMorphism ->
  Maybe (PullbackSquare RepeatedSourceObject RepeatedSourceMorphism)
repeatedPullbackSquare leftMorphism rightMorphism apexObject toLeft toRight =
  Just
    PullbackSquare
      { psLeftBase = leftMorphism,
        psRightBase = rightMorphism,
        psApex = apexObject,
        psToLeft = toLeft,
        psToRight = toRight
      }

repeatedSourceCover :: Either (CoverConstructionError RepeatedSourceObject) (CoveringFamily RepeatedSourceObject RepeatedSourceMorphism)
repeatedSourceCover =
  mkCoveringFamily RepeatedBase (repeatedCoverArrow RepeatedFirst :| [repeatedCoverArrow RepeatedSecond])

repeatedSourceStalkAlgebra :: StalkAlgebra (CompiledRestriction RepeatedSourceSite) Int (Int, Int) ()
repeatedSourceStalkAlgebra =
  StalkAlgebra
    { saRestrictionKernel = const StalkRestrictionIdentity,
      saMismatches = \left right -> [(left, right) | left /= right],
      saMerge = \left _right -> Right left,
      saRepair = const (Left ()),
      saNormalize = id
    }

testRepeatedSourceCoverSlotsDoNotCollapse :: Assertion
testRepeatedSourceCoverSlotsDoNotCollapse = do
  coverValue <- expectRight repeatedSourceCover
  effectiveCover <- expectRight (prepareEffectiveCoverPlan repeatedSourceSite coverValue)
  effectiveCoverSlotSources effectiveCover @?= [RepeatedSource, RepeatedSource]
  case mkMatchingFamily effectiveCover (Vector.singleton 7) :: Either MatchingFamilyConstructionError (MatchingFamily RepeatedSourceSite Int) of
    Left (MatchingFamilyArityMismatch expectedCount actualCount) -> do
      expectedCount @?= 2
      actualCount @?= 1
    Right _ ->
      assertFailure "expected missing second repeated-source slot"
  case mkMatchingFamily effectiveCover (Vector.fromList [7, 8, 9]) :: Either MatchingFamilyConstructionError (MatchingFamily RepeatedSourceSite Int) of
    Left (MatchingFamilyArityMismatch expectedCount actualCount) -> do
      expectedCount @?= 2
      actualCount @?= 3
    Right _ ->
      assertFailure "expected excess repeated-source slot"
  matchingFamily <-
    expectRight
      ( mkMatchingFamily
          effectiveCover
          (Vector.fromList [7, 8])
      )
  case pairwiseCompatibilityFailures repeatedSourceStalkAlgebra repeatedSourceSite matchingFamily of
    [PullbackDisagreement _ [(7, 8)]] ->
      pure ()
    otherFailures ->
      assertFailure ("expected repeated-source slots to retain distinct local values, received " <> show otherFailures)
