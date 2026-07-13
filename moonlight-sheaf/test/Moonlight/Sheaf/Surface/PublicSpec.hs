{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Surface.PublicSpec
  ( tests,
  )
where

import Data.Bifunctor (first)
import Data.IntSet qualified as IntSet
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Sheaf
import Moonlight.Sheaf.Stalk
import Moonlight.Sheaf.Stalk qualified as PublicStalk
import Moonlight.Sheaf.Surface.MiniSiteFixture
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

mergeFromMismatches :: [mismatch] -> stalk -> Either (MergeObstruction mismatch) stalk
mergeFromMismatches mismatches stalkValue =
  maybe (Right stalkValue) Left (mismatchObstruction mismatches)

prepareMiniSite :: (PreparedSite MiniSite -> Assertion) -> Assertion
prepareMiniSite continue =
  case compile (siteSpec MiniSite) of
    Left failure -> assertFailure ("expected public site preparation to succeed, received " <> show failure)
    Right preparedSite -> continue preparedSite

tests :: TestTree
tests =
  testGroup
    "public-api"
    [ testCase "prepare/section/check path keeps version explicit and object index hidden" testPrepareSectionCheck,
      testCase "public restriction morphisms elide categorical identities" testRestrictionMorphismsElideIdentities,
      testCase "compile rejects structurally invalid Site values" testPrepareRejectsBadSite,
      testCase "prepared cover lookup distinguishes unknown objects from empty cover sets" testPreparedCoverLookup,
      testCase "Moonlight.Sheaf.Stalk is the public stalk front door" testStalkFacadeImport,
      testCase "empty assignment is an exact no-op" testEmptyAssignmentNoop,
      testCase "non-empty assignment advances epoch and marks object extent" testNonEmptyAssignmentAdvancesEpoch,
      testCase "repair preserves typed merge obstruction" testRepairObstruction,
      testCase "glue reports typed pullback mismatches through the prepared site" testGluingMismatch,
      testCase "glue uses StalkAlgebra restriction authority" testGluingUsesStalkAlgebraRestriction,
      testCase "obstructed gluing preserves base mismatch evidence" testObstructedGluingPreservesMismatch,
      testCase "repeated-source cover slots retain distinct local sections" testRepeatedSourceCoverSlotsRemainDistinct,
      testCase "matching rejects short and long section vectors with exact arities" testMatchingFamilyArityMismatch,
      testCase "sections retain their prepared owner and transport stays explicit" testSectionOwnerProvenance,
      testCase "gluing algebras remain first-class values under one failure hierarchy" testGluingAlgebrasAreFirstClass,
      testCase "public descent composes matching into glue and certifies the amalgamation" testGoldenPublicDescentGlue,
      testCase "public separatedness resolves a unique amalgamation over the cover-stalk universe" testGoldenPublicSeparatedResolution,
      testCase "cover-stalk universe refuses wrong-arity slot vectors" testCoverStalkUniverseRefusesWrongSlotArity,
      testCase "separatedness refuses families owned by a foreign cover" testCrossCoverWrapperMixingRefused
    ]

testStalkFacadeImport :: Assertion
testStalkFacadeImport = do
  let discreteSmoke ::
        PublicStalk.StalkAlgebra
          ()
          Int
          (PublicStalk.DiscreteMismatch Int)
          (PublicStalk.DiscreteRepairObstruction Int)
      discreteSmoke =
        PublicStalk.discreteStalkAlgebra

      geometricSmoke ::
        PublicStalk.StalkAlgebra
          (PublicStalk.GeometricRestriction () ())
          (PublicStalk.GeometricStalk Int Bool)
          (PublicStalk.GeometricMismatch (PublicStalk.DiscreteMismatch Int) (PublicStalk.DiscreteMismatch Bool))
          ( PublicStalk.GeometricRepairObstruction
              (PublicStalk.DiscreteRepairObstruction Int)
              (PublicStalk.DiscreteRepairObstruction Bool)
          )
      geometricSmoke =
        PublicStalk.geometricStalkAlgebra
          PublicStalk.discreteStalkAlgebra
          PublicStalk.discreteStalkAlgebra

      groupoidSmoke =
        PublicStalk.mkInterfaceStalkGroupoid
          (IntSet.fromList [0, 1])
          (IntMap.fromList [(1, 4)])
          (IntMap.fromList [(0, [(1, 1)]), (1, [(0, 1)])])

  assertEqual
    "discrete facade"
    [PublicStalk.DiscreteMismatch 1 2]
    (PublicStalk.stalkMismatches discreteSmoke 1 2)
  assertEqual
    "geometric facade"
    [PublicStalk.GeometricChartMismatch (PublicStalk.DiscreteMismatch 1 2)]
    ( PublicStalk.stalkMismatches
        geometricSmoke
        (PublicStalk.GeometricStalk 1 True)
        (PublicStalk.GeometricStalk 2 True)
    )
  assertEqual
    "groupoid facade"
    4
    (PublicStalk.maxInterfaceStalkAutomorphismCount groupoidSmoke)

testRestrictionMorphismsElideIdentities :: Assertion
testRestrictionMorphismsElideIdentities =
  assertEqual
    "operational restrictions"
    [(Parent, Child)]
    (fmap (\morphismValue -> (cmSource morphismValue, cmTarget morphismValue)) (siteRestrictionMorphisms MiniSite))

testPrepareSectionCheck :: Assertion
testPrepareSectionCheck =
  prepareMiniSite $ \preparedSite ->
    case section preparedSite (Map.fromList [(Parent, MiniStalk 7), (Child, MiniStalk 7)]) of
      Left failure -> assertFailure ("expected total section to succeed, received " <> show failure)
      Right sectionValue -> do
        assertEqual "stalkAt" (Right (MiniStalk 7)) (stalkAt Child sectionValue)
        assertEqual "entries" (Map.fromList [(Parent, MiniStalk 7), (Child, MiniStalk 7)]) (entries sectionValue)
        assertEqual "check" (Right SectionCertified) (certify miniAlgebra sectionValue)
        case globalSection miniAlgebra sectionValue of
          Left failure -> assertFailure ("expected global section, received " <> show failure)
          Right globalSectionValue ->
            assertEqual "global section underlying value" sectionValue (globalSectionUnderlying globalSectionValue)

testPrepareRejectsBadSite :: Assertion
testPrepareRejectsBadSite =
  case compile (siteSpec BadIdentitySite) of
    Left (SheafSiteLawFailed failures) ->
      assertBool "site law failures are retained" (not (null failures))
    Left failure ->
      assertFailure ("expected site law failure, received " <> show failure)
    Right _ ->
      assertFailure "expected invalid site preparation to fail"

testPreparedCoverLookup :: Assertion
testPreparedCoverLookup =
  prepareMiniSite $ \preparedSite -> do
    assertEqual
      "known object with no declared covers"
      (Right [])
      (preparedCovers preparedSite Parent)
    assertEqual
      "unknown object"
      (Left (PreparedCoversUnknownObject Ghost))
      (preparedCovers preparedSite Ghost)

testEmptyAssignmentNoop :: Assertion
testEmptyAssignmentNoop =
  prepareMiniSite $ \preparedSite -> do
    let sectionValue = tabulateSection preparedSite (const (MiniStalk 0))
    case assign Map.empty sectionValue of
      Left failure -> assertFailure ("expected empty assignment to succeed, received " <> show failure)
      Right nextSection -> do
        assertEqual "section equality" sectionValue nextSection
        assertEqual "epoch" (sectionEpoch sectionValue) (sectionEpoch nextSection)
        assertEqual "extent" (changedObjects sectionValue) (changedObjects nextSection)

testNonEmptyAssignmentAdvancesEpoch :: Assertion
testNonEmptyAssignmentAdvancesEpoch =
  prepareMiniSite $ \preparedSite -> do
    let sectionValue = tabulateSection preparedSite (const (MiniStalk 0))
    case assignOne Parent (MiniStalk 9) sectionValue of
      Left failure -> assertFailure ("expected assignment to succeed, received " <> show failure)
      Right nextSection -> do
        assertBool "epoch advanced" (sectionEpoch nextSection > sectionEpoch sectionValue)
        case changedObjects nextSection of
          ChangedObjects objectKeys ->
            assertBool "extent is nonempty" (not (Set.null objectKeys))
          extent ->
            assertFailure ("expected object extent, received " <> show extent)
        assertEqual "assigned stalk" (Right (MiniStalk 9)) (stalkAt Parent nextSection)

testRepairObstruction :: Assertion
testRepairObstruction =
  prepareMiniSite $ \preparedSite ->
    case partial preparedSite (Map.fromList [(Parent, MiniStalk 1), (Child, MiniStalk 2)]) of
      Left failure -> assertFailure ("expected partial assignment to succeed, received " <> show failure)
      Right assignment ->
        case repair miniAlgebra assignment of
          Left (RepairDomainObstruction Child (DiscreteMergeConflict _)) -> pure ()
          Left failure -> assertFailure ("expected child repair obstruction, received " <> show failure)
          Right result -> assertFailure ("expected repair obstruction, received " <> show result)

data BranchContext
  = BranchBase
  | BranchLeft
  | BranchRight
  | BranchApex
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data BranchMorphism = BranchMorphism BranchContext BranchContext
  deriving stock (Eq, Ord, Show)

data BranchSite = BranchSite
  deriving stock (Eq, Ord, Show)

newtype BranchStalk = BranchStalk (Map BranchContext Int)
  deriving stock (Eq, Show)

branchStalkEntries :: BranchStalk -> Map BranchContext Int
branchStalkEntries (BranchStalk entryMap) =
  entryMap

data BranchMismatch = BranchCoordinateConflict BranchContext Int Int
  deriving stock (Eq, Show)

data BranchGluingFailure = BranchGluingRejected
  deriving stock (Eq, Show)

branchContexts :: [BranchContext]
branchContexts = [minBound .. maxBound]

branchLeq :: BranchContext -> BranchContext -> Bool
branchLeq left right
  | left == right = True
  | left == BranchBase = True
  | right == BranchApex = True
  | otherwise = False

branchJoin :: BranchContext -> BranchContext -> BranchContext
branchJoin left right
  | branchLeq left right = right
  | branchLeq right left = left
  | otherwise = BranchApex

branchArrow :: BranchContext -> BranchContext -> Maybe (CheckedMorphism BranchContext BranchMorphism)
branchArrow source target =
  if branchLeq target source
    then
      Just
        CheckedMorphism
          { cmSource = source,
            cmTarget = target,
            cmWitness = BranchMorphism source target
          }
    else Nothing

checkedBranchArrow :: BranchContext -> BranchContext -> CheckedMorphism BranchContext BranchMorphism
checkedBranchArrow source target =
  case branchArrow source target of
    Just arrow -> arrow
    Nothing -> CheckedMorphism source target (BranchMorphism source target)

branchRootCover :: Either (CoverConstructionError BranchContext) (CoveringFamily BranchContext BranchMorphism)
branchRootCover =
  mkCoveringFamily BranchBase (checkedBranchArrow BranchLeft BranchBase :| [checkedBranchArrow BranchRight BranchBase])

branchRepeatedSourceCover :: Either (CoverConstructionError BranchContext) (CoveringFamily BranchContext BranchMorphism)
branchRepeatedSourceCover =
  mkCoveringFamily BranchBase (checkedBranchArrow BranchLeft BranchBase :| [checkedBranchArrow BranchLeft BranchBase])

instance Site BranchSite where
  type SiteObject BranchSite = BranchContext
  type SiteMorphism BranchSite = BranchMorphism

  siteObjects _ = branchContexts

  siteMorphisms _ = mapMaybe (uncurry branchArrow) ((,) <$> branchContexts <*> branchContexts)

  identityAt _ contextValue = checkedBranchArrow contextValue contextValue

  coversAt _ contextValue =
    case contextValue of
      BranchBase ->
        either (const []) pure branchRootCover
          <> either (const []) pure branchRepeatedSourceCover
      _ -> []

  composeChecked _ outer inner =
    if cmTarget inner == cmSource outer
      then branchArrow (cmSource inner) (cmTarget outer)
      else Nothing

  pullbackPair _ left right =
    if cmTarget left == cmTarget right
      then
        let apex = branchJoin (cmSource left) (cmSource right)
         in Just
              PullbackSquare
                { psLeftBase = left,
                  psRightBase = right,
                  psApex = apex,
                  psToLeft = checkedBranchArrow apex (cmSource left),
                  psToRight = checkedBranchArrow apex (cmSource right)
                }
      else Nothing

branchAlgebra :: StalkAlgebra (CompiledRestriction BranchSite) BranchStalk BranchMismatch ()
branchAlgebra =
  StalkAlgebra
    { saRestrictionKernel = StalkRestrictionMap . restrictBranchStalk,
      saMismatches = branchMismatches,
      saMerge = \left right -> mergeFromMismatches (branchMismatches left right) left,
      saRepair = const (Left ()),
      saNormalize = id
    }

restrictBranchStalk :: CompiledRestriction BranchSite -> BranchStalk -> BranchStalk
restrictBranchStalk restriction (BranchStalk entryMap) =
  BranchStalk
    ( Map.fromList
        ( fmap
            (\value -> (cmSource (restrictionMorphism restriction), value))
            (Map.elems entryMap)
        )
    )

branchMismatches :: BranchStalk -> BranchStalk -> [BranchMismatch]
branchMismatches (BranchStalk left) (BranchStalk right) =
  mapMaybe
    ( \contextValue ->
        case (Map.lookup contextValue left, Map.lookup contextValue right) of
          (Just leftValue, Just rightValue)
            | leftValue /= rightValue -> Just (BranchCoordinateConflict contextValue leftValue rightValue)
          _ -> Nothing
    )
    (Map.keys (Map.intersection left right))

branchGluingAlgebra :: GluingAlgebra BranchSite BranchStalk BranchGluingFailure
branchGluingAlgebra =
  GluingAlgebra
    { gaAmalgamate = \_site compatibleFamily ->
        Right
          ( BranchStalk
              ( Map.unions
                  (fmap branchStalkEntries (Vector.toList (matchingSections (compatibleMatchingFamilyUnderlying compatibleFamily))))
              )
          )
    }

branchRejectingGluingAlgebra :: GluingAlgebra BranchSite BranchStalk BranchGluingFailure
branchRejectingGluingAlgebra =
  GluingAlgebra
    { gaAmalgamate = \_site _compatibleFamily ->
        Left (GluingRejected BranchGluingRejected)
    }

prepareBranchSite :: (PreparedSite BranchSite -> Assertion) -> Assertion
prepareBranchSite continue =
  case compile (siteSpec BranchSite) of
    Left failure -> assertFailure ("expected branch site preparation to succeed, received " <> show failure)
    Right preparedSite -> continue preparedSite

withBranchRootCoverPlan :: PreparedSite BranchSite -> (PreparedCover BranchSite -> Assertion) -> Assertion
withBranchRootCoverPlan preparedSite continue =
  case preparedCovers preparedSite BranchBase of
    Left refusal ->
      assertFailure ("expected known branch root, received " <> show refusal)
    Right coverPlans ->
      case
          filter
            ((== Vector.fromList [BranchLeft, BranchRight]) . preparedCoverSources)
            coverPlans
        of
          [coverPlan] -> continue coverPlan
          matchingCoverPlans ->
            assertFailure
              ("expected one branch root cover, received " <> show (length matchingCoverPlans))

withBranchRepeatedSourceCoverPlan :: PreparedSite BranchSite -> (PreparedCover BranchSite -> Assertion) -> Assertion
withBranchRepeatedSourceCoverPlan preparedSite continue =
  case preparedCovers preparedSite BranchBase of
    Left refusal ->
      assertFailure ("expected known branch root, received " <> show refusal)
    Right coverPlans ->
      case
          filter
            ((== Vector.fromList [BranchLeft, BranchLeft]) . preparedCoverSources)
            coverPlans
        of
          [coverPlan] -> continue coverPlan
          matchingCoverPlans ->
            assertFailure
              ("expected one repeated-source root cover, received " <> show (length matchingCoverPlans))

branchMismatchSections :: PreparedCover BranchSite -> Vector BranchStalk
branchMismatchSections _coverPlan =
  Vector.fromList
    [ BranchStalk (Map.fromList [(BranchApex, 7)]),
      BranchStalk (Map.fromList [(BranchApex, 8)])
    ]

branchAuthoritySections :: PreparedCover BranchSite -> Vector BranchStalk
branchAuthoritySections _coverPlan =
  Vector.fromList
    [ BranchStalk (Map.fromList [(BranchLeft, 7)]),
      BranchStalk (Map.fromList [(BranchRight, 8)])
    ]

branchCompatibleSections :: Vector BranchStalk
branchCompatibleSections =
  Vector.fromList
    [ BranchStalk (Map.fromList [(BranchLeft, 7)]),
      BranchStalk (Map.fromList [(BranchRight, 7)])
    ]

testGluingMismatch :: Assertion
testGluingMismatch =
  prepareBranchSite $ \preparedSite ->
    withBranchRootCoverPlan preparedSite $ \coverPlan ->
      case first CoverMatchingFamilyConstructionFailed (matching coverPlan (branchMismatchSections coverPlan)) >>= glue branchAlgebra branchGluingAlgebra of
        Left (CoverAmalgamationFailed (IncompatibleMatchingFamily (PullbackDisagreement _ [BranchCoordinateConflict BranchApex 7 8] :| []))) -> pure ()
        Left failure -> assertFailure ("expected typed pullback failure, received " <> show failure)
        Right success -> assertFailure ("expected gluing failure, received " <> show (amalgamatedStalk success))

testGluingUsesStalkAlgebraRestriction :: Assertion
testGluingUsesStalkAlgebraRestriction =
  prepareBranchSite $ \preparedSite ->
    withBranchRootCoverPlan preparedSite $ \coverPlan ->
      case first CoverMatchingFamilyConstructionFailed (matching coverPlan (branchAuthoritySections coverPlan)) >>= glue branchAlgebra branchGluingAlgebra of
        Left (CoverAmalgamationFailed (IncompatibleMatchingFamily (PullbackDisagreement _ [BranchCoordinateConflict BranchApex 7 8] :| []))) -> pure ()
        Left failure -> assertFailure ("expected algebra-restricted pullback failure, received " <> show failure)
        Right success -> assertFailure ("expected algebra-restricted gluing failure, received " <> show (amalgamatedStalk success))

testObstructedGluingPreservesMismatch :: Assertion
testObstructedGluingPreservesMismatch =
  prepareBranchSite $ \preparedSite ->
    withBranchRootCoverPlan preparedSite $ \coverPlan ->
      case first CoverMatchingFamilyConstructionFailed (matching coverPlan (branchMismatchSections coverPlan)) >>= glue branchAlgebra branchGluingAlgebra of
        Left (CoverAmalgamationFailed (IncompatibleMatchingFamily (PullbackDisagreement _ [BranchCoordinateConflict BranchApex 7 8] :| []))) -> pure ()
        Left failure -> assertFailure ("expected lifted pullback failure, received " <> show failure)
        Right success -> assertFailure ("expected lifted gluing failure, received " <> show (amalgamatedStalk success))

testRepeatedSourceCoverSlotsRemainDistinct :: Assertion
testRepeatedSourceCoverSlotsRemainDistinct =
  prepareBranchSite $ \preparedSite ->
    withBranchRepeatedSourceCoverPlan preparedSite $ \coverPlan -> do
      let slots = preparedCoverSlots coverPlan
          localSections =
            Vector.fromList
              [ BranchStalk (Map.fromList [(BranchLeft, 7)]),
                BranchStalk (Map.fromList [(BranchLeft, 8)])
              ]
      assertEqual
        "repeated sources remain distinct ordered slots"
        (Vector.fromList [BranchLeft, BranchLeft])
        (Vector.map (cmSource . coverSlotArrow) slots)
      assertEqual "slot count" 2 (Vector.length slots)
      assertEqual
        "slot keys remain distinct"
        2
        (Set.size (Set.fromList (Vector.toList (Vector.map coverSlotKey slots))))
      case matching coverPlan localSections of
        Left failure ->
          assertFailure ("expected repeated-source matching family, received " <> show failure)
        Right matchingFamilyValue ->
          assertEqual
            "matching family retains both slot values"
            localSections
            (matchingSections matchingFamilyValue)
      case first CoverMatchingFamilyConstructionFailed (matching coverPlan localSections) >>= glue branchAlgebra branchGluingAlgebra of
        Left
          ( CoverAmalgamationFailed
              ( IncompatibleMatchingFamily
                  (PullbackDisagreement square [BranchCoordinateConflict BranchLeft 7 8] :| [])
                )
            ) ->
            assertEqual "repeated-source pullback apex" BranchLeft (psApex square)
        Left failure ->
          assertFailure ("expected repeated-source pullback disagreement, received " <> show failure)
        Right success ->
          assertFailure ("expected repeated-source disagreement, received " <> show (amalgamatedStalk success))

testMatchingFamilyArityMismatch :: Assertion
testMatchingFamilyArityMismatch =
  prepareBranchSite $ \preparedSite ->
    withBranchRepeatedSourceCoverPlan preparedSite $ \coverPlan -> do
      case matching coverPlan (Vector.singleton (BranchStalk Map.empty)) of
        Left failure ->
          assertEqual
            "short vector"
            ( MatchingFamilyArityMismatch
                { expectedSectionCount = 2,
                  actualSectionCount = 1
                }
            )
            failure
        Right _ ->
          assertFailure "expected short matching vector to be rejected"
      case matching coverPlan (Vector.replicate 3 (BranchStalk Map.empty)) of
        Left failure ->
          assertEqual
            "long vector"
            ( MatchingFamilyArityMismatch
                { expectedSectionCount = 2,
                  actualSectionCount = 3
                }
            )
            failure
        Right _ ->
          assertFailure "expected long matching vector to be rejected"

testSectionOwnerProvenance :: Assertion
testSectionOwnerProvenance =
  case compile (siteSpec ParentFirstMiniSite) of
    Left failure ->
      assertFailure ("expected parent-first prepared site, received " <> show failure)
    Right parentFirstPreparedSite ->
      case compile (siteSpec ChildFirstMiniSite) of
        Left failure ->
          assertFailure ("expected child-first prepared site, received " <> show failure)
        Right childFirstPreparedSite -> do
          let sourceEntries =
                Map.fromList [(Parent, MiniStalk 7), (Child, MiniStalk 7)]
          parentSection <-
            case section parentFirstPreparedSite sourceEntries of
              Left failure ->
                assertFailure ("expected parent-first section, received " <> show failure)
              Right sectionValue ->
                pure sectionValue
          childSection <-
            case section childFirstPreparedSite sourceEntries of
              Left failure ->
                assertFailure ("expected child-first section, received " <> show failure)
              Right sectionValue ->
                pure sectionValue
          assertBool
            "different prepared owners keep otherwise equal sections distinct"
            (parentSection /= childSection)
          assertEqual "parent-first entries" sourceEntries (entries parentSection)
          assertEqual "child-first entries" sourceEntries (entries childSection)
          assertEqual "parent-first certification" (Right SectionCertified) (certify orderedMiniAlgebra parentSection)
          assertEqual "child-first certification" (Right SectionCertified) (certify orderedMiniAlgebra childSection)
          case globalSection orderedMiniAlgebra parentSection of
            Left failure ->
              assertFailure ("expected parent-first global section, received " <> show failure)
            Right _ -> pure ()
          case globalSection orderedMiniAlgebra childSection of
            Left failure ->
              assertFailure ("expected child-first global section, received " <> show failure)
            Right _ -> pure ()
          editedSection <-
            case assignOne Parent (MiniStalk 11) parentSection of
              Left failure ->
                assertFailure ("expected owner-bound edit, received " <> show failure)
              Right sectionValue ->
                pure sectionValue
          assertEqual "edited owner-bound stalk" (Right (MiniStalk 11)) (stalkAt Parent editedSection)
          let extractedEntries = entries editedSection
          case section childFirstPreparedSite extractedEntries of
            Left failure ->
              assertFailure ("expected explicit reconstruction, received " <> show failure)
            Right transportedSection ->
              assertEqual
                "explicit extraction and reconstruction transports by object"
                extractedEntries
                (entries transportedSection)

testGluingAlgebrasAreFirstClass :: Assertion
testGluingAlgebrasAreFirstClass =
  prepareBranchSite $ \preparedSite ->
    withBranchRootCoverPlan preparedSite $ \coverPlan -> do
      case first CoverMatchingFamilyConstructionFailed (matching coverPlan branchCompatibleSections) >>= glue branchAlgebra branchGluingAlgebra of
        Left failure ->
          assertFailure ("expected accepting gluing algebra, received " <> show failure)
        Right _ -> pure ()
      case first CoverMatchingFamilyConstructionFailed (matching coverPlan branchCompatibleSections) >>= glue branchAlgebra branchRejectingGluingAlgebra of
        Left
          ( CoverAmalgamationFailed
              (GluingObstructed (GluingRejected BranchGluingRejected))
            ) ->
            pure ()
        Left failure ->
          assertFailure ("expected GluingRejected through canonical failure, received " <> show failure)
        Right success ->
          assertFailure ("expected rejecting gluing algebra, received " <> show (amalgamatedStalk success))

testGoldenPublicDescentGlue :: Assertion
testGoldenPublicDescentGlue =
  prepareBranchSite $ \preparedSite ->
    withBranchRootCoverPlan preparedSite $ \coverPlan -> do
      matchingFamilyValue <-
        case matching coverPlan branchCompatibleSections of
          Left failure -> assertFailure ("expected matching family, received " <> show failure)
          Right value -> pure value
      compatibleFamily <-
        case certifyMatching branchAlgebra matchingFamilyValue of
          Left failures -> assertFailure ("expected compatible matching family, received " <> show failures)
          Right value -> pure value
      amalgamation <-
        case glue branchAlgebra branchGluingAlgebra matchingFamilyValue of
          Left failure -> assertFailure ("expected shared-middle amalgamation, received " <> show failure)
          Right value -> pure value
      let expectedStalk = BranchStalk (Map.fromList [(BranchLeft, 7), (BranchRight, 7)])
      assertEqual
        "amalgamated stalk unions compatible slot sections"
        expectedStalk
        (amalgamatedStalk amalgamation)
      case first CoverMatchingFamilyConstructionFailed (matching coverPlan branchCompatibleSections) >>= glue branchAlgebra branchGluingAlgebra of
        Left failure ->
          assertFailure ("expected composed matching/glue amalgamation, received " <> show failure)
        Right composed ->
          assertEqual
            "point-free matching-to-glue composition agrees with the shared middle"
            expectedStalk
            (amalgamatedStalk composed)
      case certifyAmalgamation branchAlgebra compatibleFamily (amalgamatedStalk amalgamation) of
        Left failures ->
          assertFailure ("expected amalgamation locality certificate, received " <> show failures)
        Right certified ->
          assertEqual
            "certified amalgamation agrees with the glued stalk"
            expectedStalk
            (amalgamatedStalk certified)

testGoldenPublicSeparatedResolution :: Assertion
testGoldenPublicSeparatedResolution =
  prepareBranchSite $ \preparedSite ->
    withBranchRootCoverPlan preparedSite $ \coverPlan -> do
      matchingFamilyValue <-
        case matching coverPlan branchCompatibleSections of
          Left failure -> assertFailure ("expected matching family, received " <> show failure)
          Right value -> pure value
      compatibleFamily <-
        case certifyMatching branchAlgebra matchingFamilyValue of
          Left failures -> assertFailure ("expected compatible matching family, received " <> show failures)
          Right value -> pure value
      let targetStalk = BranchStalk (Map.fromList [(BranchLeft, 7), (BranchRight, 7)])
      universe <-
        case
          coverStalkUniverse
            coverPlan
            [targetStalk]
            ( Vector.fromList
                [ [BranchStalk (Map.fromList [(BranchLeft, 7)])],
                  [BranchStalk (Map.fromList [(BranchRight, 7)])]
                ]
            )
        of
          Left shapeError -> assertFailure ("expected cover-stalk universe, received " <> show shapeError)
          Right value -> pure value
      separated <-
        case separatedCover branchAlgebra universe of
          Left refusal -> assertFailure ("expected separated cover, received " <> show refusal)
          Right value -> pure value
      resolved <-
        case resolveUniqueAmalgamation branchAlgebra separated compatibleFamily of
          Left refusal -> assertFailure ("expected resolved unique amalgamation, received " <> show refusal)
          Right value -> pure value
      assertEqual
        "resolution selects the separated target"
        targetStalk
        (amalgamatedStalk (uniqueAmalgamationUnderlying resolved))
      case certifyUniqueAmalgamation branchAlgebra separated compatibleFamily targetStalk of
        Left refusal -> assertFailure ("expected certified unique amalgamation, received " <> show refusal)
        Right certified ->
          assertEqual
            "certified unique amalgamation agrees with resolution"
            targetStalk
            (amalgamatedStalk (uniqueAmalgamationUnderlying certified))
      assertEqual
        "reflexive local equality at the sole target"
        (Right SeparatedStalksEqual)
        (separatedLocalEqualityAt branchAlgebra separated 0 0)

testCoverStalkUniverseRefusesWrongSlotArity :: Assertion
testCoverStalkUniverseRefusesWrongSlotArity =
  prepareBranchSite $ \preparedSite ->
    withBranchRootCoverPlan preparedSite $ \coverPlan ->
      case
        coverStalkUniverse
          coverPlan
          [BranchStalk (Map.fromList [(BranchLeft, 7), (BranchRight, 7)])]
          (Vector.fromList [[BranchStalk (Map.fromList [(BranchLeft, 7)])]])
      of
        Left shapeError ->
          assertEqual
            "universe shape rejects a short slot vector"
            (UniverseShapeError {universeExpectedSlotCount = 2, universeActualSlotCount = 1})
            shapeError
        Right _ ->
          assertFailure "expected a wrong-arity slot vector to be rejected"

testCrossCoverWrapperMixingRefused :: Assertion
testCrossCoverWrapperMixingRefused =
  prepareBranchSite $ \preparedSite ->
    withBranchRootCoverPlan preparedSite $ \rootCoverPlan ->
      withBranchRepeatedSourceCoverPlan preparedSite $ \repeatedCoverPlan -> do
        let targetStalk = BranchStalk (Map.fromList [(BranchLeft, 7), (BranchRight, 7)])
        universe <-
          case
            coverStalkUniverse
              rootCoverPlan
              [targetStalk]
              ( Vector.fromList
                  [ [BranchStalk (Map.fromList [(BranchLeft, 7)])],
                    [BranchStalk (Map.fromList [(BranchRight, 7)])]
                  ]
              )
          of
            Left shapeError -> assertFailure ("expected root cover-stalk universe, received " <> show shapeError)
            Right value -> pure value
        separated <-
          case separatedCover branchAlgebra universe of
            Left refusal -> assertFailure ("expected separated root cover, received " <> show refusal)
            Right value -> pure value
        repeatedMatching <-
          case matching repeatedCoverPlan (Vector.replicate 2 (BranchStalk (Map.fromList [(BranchLeft, 7)]))) of
            Left failure -> assertFailure ("expected repeated-source matching family, received " <> show failure)
            Right value -> pure value
        repeatedCompatible <-
          case certifyMatching branchAlgebra repeatedMatching of
            Left failures -> assertFailure ("expected repeated-source compatible family, received " <> show failures)
            Right value -> pure value
        case resolveUniqueAmalgamation branchAlgebra separated repeatedCompatible of
          Left ResolutionCoverPlanMismatch -> pure ()
          Left refusal -> assertFailure ("expected resolution plan mismatch, received " <> show refusal)
          Right _ -> assertFailure "expected cross-cover resolution to be refused"
        case certifyUniqueAmalgamation branchAlgebra separated repeatedCompatible targetStalk of
          Left UniquenessCoverPlanMismatch -> pure ()
          Left refusal -> assertFailure ("expected uniqueness plan mismatch, received " <> show refusal)
          Right _ -> assertFailure "expected cross-cover uniqueness to be refused"
