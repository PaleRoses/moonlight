{-# LANGUAGE RankNTypes #-}

module Moonlight.Sheaf.Descent.DescentSpec
  ( tests,
  )
where

import Data.Kind (Type)

import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Data.Set qualified as Set
import Moonlight.Core (DenseKey (..))
import Moonlight.Sheaf.Context.Section
  ( ContextClassSection (..),
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    withPreparedContextSiteFromFiniteLattice,
  )
import Moonlight.Sheaf.Descent.Assignment qualified as AssignmentDescent
import Moonlight.Sheaf.Descent.Kernel
  ( CoverDescentKernel (..),
    CoverSearchBudget (..),
    CoverSearchCost (..),
    CoverSearchRefusal (..),
    coverContextAt,
    coverSearchCost,
    coverSearchWithinBudget,
    descentAtCover,
  )
import Moonlight.Sheaf.Descent.Core
  ( DescentOutcome (..),
    collectDescentReport,
  )
import Moonlight.Sheaf.Descent.Quotient qualified as QuotientDescent
import Moonlight.Sheaf.TestFixture.Branch
  ( BranchContext (..),
    branchContextLattice,
  )
import Moonlight.Sheaf.Verdict
  ( ObstructionVerdict,
    SearchVerdict (..),
    Verdict (..),
    decidedSearchVerdict,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    (@?=),
    assertBool,
    testCase,
  )

type TestCoordinate :: Type
data TestCoordinate
  = CoordA
  | CoordB
  deriving stock (Eq, Ord, Show)

type BucketValue :: Type
newtype BucketValue = BucketValue Int
  deriving stock (Eq, Ord, Show)

type TestRep :: Type
newtype TestRep = TestRep Int
  deriving stock (Eq, Ord, Show)

instance DenseKey TestRep where
  encodeDenseKey (TestRep key) = key
  decodeDenseKey = TestRep

tests :: TestTree
tests =
  testGroup
    "descent"
    [ testCase "quotient descent remains satisfied for consistent restriction families" testQuotientDescentSatisfied,
      testCase "quotient descent preserves typed representatives on obstruction" testQuotientDescentPreservesTypedRepresentatives,
      testCase "quotient two-cover descent matches the former binary semantics" testQuotientBinaryEquivalence,
      testCase "assignment descent accepts a compatible branch cover" testAssignmentDescentSatisfiedForCompatibleBranchCover,
      testCase "assignment descent distinguishes compatible local agreement from obstructed branch disagreement" testAssignmentDescentSeparatesAgreementFromObstruction,
      testCase "cover search cost preflights exhaustive descent leaves" testCoverSearchCostPreflight,
      testCase "cover search budget is enforced by the kernel" testCoverSearchBudgetRemainsCallerOwned,
      testCase "vacuous generic cover rejects empty coordinate domains" testVacuousDomainRejectedByGenericKernel,
      testCase "vacuous quotient cover rejects an empty cover section" testVacuousQuotientCoverRejected,
      testCase "vacuity rejects before budget refusal" testVacuityBeatsBudget,
      testCase "collectDescentReport propagates undecided taint and retained obstructions" testCollectDescentReportTaintPropagation,
      testCase "cover context lookup rejects negative coordinates" testCoverContextAtRejectsNegativeCoordinates,
      testCase "section-aware admissibility evidence is preserved on obstruction" testAdmissibilityEvidencePreserved
    ]

unboundedBudget :: CoverSearchBudget
unboundedBudget = CoverSearchBudget Nothing

withBranchSite ::
  (forall owner. PreparedContextSite owner BranchContext -> Assertion) ->
  Assertion
withBranchSite =
  withPreparedContextSiteFromFiniteLattice branchContextLattice

testQuotientDescentSatisfied :: Assertion
testQuotientDescentSatisfied =
  withBranchSite $ \branchSite ->
    let sectionAt :: BranchContext -> ContextClassSection TestRep
        sectionAt contextValue =
          ContextClassSection $
            case contextValue of
              BranchBase -> IntMap.fromList [(0, TestRep 0)]
              BranchLeft -> IntMap.fromList [(0, TestRep 1)]
              BranchRight -> IntMap.fromList [(0, TestRep 1)]
              BranchApex -> IntMap.fromList [(0, TestRep 1)]
        kernel =
          QuotientDescent.DescentKernel
            { QuotientDescent.dkSite = branchSite,
              QuotientDescent.dkMaterializedContexts = [BranchBase, BranchLeft, BranchRight],
              QuotientDescent.dkClassSectionAt = sectionAt
            }
        report = QuotientDescent.fullDescentCheck unboundedBudget kernel
     in assertBool
          "quotient descent should accept a consistent family"
          (QuotientDescent.drSatisfied report)

testQuotientDescentPreservesTypedRepresentatives :: Assertion
testQuotientDescentPreservesTypedRepresentatives =
  withBranchSite $ \branchSite ->
  let sectionAt :: BranchContext -> ContextClassSection TestRep
      sectionAt contextValue =
        ContextClassSection $
          case contextValue of
            BranchBase -> IntMap.fromList [(0, TestRep 0)]
            BranchLeft -> IntMap.fromList [(0, TestRep 1), (2, TestRep 2)]
            BranchRight -> IntMap.fromList [(0, TestRep 1), (2, TestRep 2)]
            BranchApex -> IntMap.fromList [(0, TestRep 0)]
      kernel =
        QuotientDescent.DescentKernel
          { QuotientDescent.dkSite = branchSite,
            QuotientDescent.dkMaterializedContexts = [BranchBase, BranchLeft, BranchRight],
            QuotientDescent.dkClassSectionAt = sectionAt
          }
   in case QuotientDescent.descentAt unboundedBudget kernel BranchBase of
        SearchAccepted ->
          assertBool "expected typed representative obstruction" False
        SearchRejected (QuotientDescent.QuotientDescentObstruction _contextValue _coverElements obstructedTuples :| []) ->
          obstructedTuples
            @?= [IntMap.fromList [(0, TestRep 2), (1, TestRep 2)]]
        SearchRejected (_lookupObstruction :| []) ->
          assertBool "expected tuple-level quotient descent obstruction" False
        SearchRejected (_ :| _ : _) ->
          assertBool "expected one obstruction for the branch cover" False
        SearchUndecided {} ->
          assertBool "unbounded quotient descent must decide the branch cover" False

testQuotientBinaryEquivalence :: Assertion
testQuotientBinaryEquivalence =
  withBranchSite $ \branchSite ->
  let sectionAt :: BranchContext -> ContextClassSection TestRep
      sectionAt contextValue =
        ContextClassSection $
          case contextValue of
            BranchBase -> IntMap.fromList [(0, TestRep 0)]
            BranchLeft -> IntMap.fromList [(0, TestRep 1), (2, TestRep 2)]
            BranchRight -> IntMap.fromList [(0, TestRep 1), (2, TestRep 2)]
            BranchApex -> IntMap.fromList [(0, TestRep 0)]
      kernel =
        QuotientDescent.DescentKernel
          { QuotientDescent.dkSite = branchSite,
            QuotientDescent.dkMaterializedContexts = [BranchBase, BranchLeft, BranchRight],
            QuotientDescent.dkClassSectionAt = sectionAt
          }
   in QuotientDescent.descentAt unboundedBudget kernel BranchBase
        @?= decidedSearchVerdict (oldBinaryDescentForFixture kernel BranchBase)

testAssignmentDescentSatisfiedForCompatibleBranchCover :: Assertion
testAssignmentDescentSatisfiedForCompatibleBranchCover =
  let sectionAt :: BranchContext -> Map TestCoordinate BucketValue
      sectionAt _ =
        Map.fromList [(CoordA, BucketValue 1), (CoordB, BucketValue 2)]
      kernel :: AssignmentDescent.DescentKernel BranchContext (Map TestCoordinate BucketValue) TestCoordinate BucketValue () ()
      kernel =
        AssignmentDescent.DescentKernel
          { AssignmentDescent.dkCoverOf =
              \case
                BranchBase -> [BranchLeft, BranchRight]
                _ -> [],
            AssignmentDescent.dkMaterializedContexts = [BranchBase, BranchLeft, BranchRight],
            AssignmentDescent.dkSectionAt = sectionAt,
            AssignmentDescent.dkAssignmentOf = id,
            AssignmentDescent.dkAdmissibility = AssignmentDescent.trivialAdmissibility
          }
      report = AssignmentDescent.fullDescentCheck unboundedBudget kernel
   in do
        AssignmentDescent.descentAt unboundedBudget kernel BranchBase @?= SearchAccepted
        AssignmentDescent.drSatisfied report @?= True

testAssignmentDescentSeparatesAgreementFromObstruction :: Assertion
testAssignmentDescentSeparatesAgreementFromObstruction = do
  let compatibleKernel =
        branchAssignmentKernel
          (const (Map.fromList [(BranchApex, 7)]))
      obstructedKernel =
        branchAssignmentKernel
          ( \case
              BranchRight -> Map.fromList [(BranchApex, 8)]
              _ -> Map.fromList [(BranchApex, 7)]
          )
  AssignmentDescent.descentAt unboundedBudget compatibleKernel BranchBase @?= SearchAccepted
  case AssignmentDescent.descentAt unboundedBudget obstructedKernel BranchBase of
    SearchRejected (AssignmentDescent.DescentConflictObstruction obstruction :| []) -> do
      AssignmentDescent.doContext obstruction @?= BranchBase
      AssignmentDescent.doCoverElements obstruction @?= [BranchLeft, BranchRight]
      assertBool
        "branch disagreement should be recorded as an actual descent obstruction"
        (not (null (AssignmentDescent.doObstructedAssignments obstruction)))
    SearchRejected (_ :| []) ->
      assertBool "expected assignment conflict obstruction" False
    otherVerdict ->
      assertBool ("expected obstructed branch descent, received " <> show otherVerdict) False

branchAssignmentKernel ::
  (BranchContext -> Map BranchContext Int) ->
  AssignmentDescent.DescentKernel BranchContext (Map BranchContext Int) BranchContext Int () ()
branchAssignmentKernel sectionAt =
  AssignmentDescent.DescentKernel
    { AssignmentDescent.dkCoverOf =
        \case
          BranchBase -> [BranchLeft, BranchRight]
          _ -> [],
      AssignmentDescent.dkMaterializedContexts = [BranchBase, BranchLeft, BranchRight],
      AssignmentDescent.dkSectionAt = sectionAt,
      AssignmentDescent.dkAssignmentOf = id,
      AssignmentDescent.dkAdmissibility = AssignmentDescent.trivialAdmissibility
    }

testCoverSearchCostPreflight :: Assertion
testCoverSearchCostPreflight =
  let searchCost =
        coverSearchCost
          (bucketCoverKernel [] (const False) (const []) (\_ _ _ -> ()))
          BranchBase
          branchPairCover
   in do
        cscCoordinates searchCost @?= [CoordB, CoordA]
        cscAssignmentUpperBound searchCost @?= 6
        coverSearchWithinBudget (CoverSearchBudget (Just 6)) searchCost @?= True
        coverSearchWithinBudget (CoverSearchBudget (Just 5)) searchCost @?= False

testCoverSearchBudgetRemainsCallerOwned :: Assertion
testCoverSearchBudgetRemainsCallerOwned =
  let kernel =
        bucketCoverKernel
          [BranchBase]
          (const True)
          (\assignments -> ["obstructed=" <> show (length assignments)])
          (\_ _ coordinates -> "vacuous=" <> show coordinates)
      expectedCost =
        CoverSearchCost
          { cscCoordinates = [CoordB, CoordA],
            cscDomainSizes = Map.fromList [(CoordA, 3), (CoordB, 2)],
            cscAssignmentUpperBound = 6
          }
   in do
        coverSearchCost kernel BranchBase branchPairCover @?= expectedCost
        coverSearchWithinBudget (CoverSearchBudget (Just 5)) expectedCost @?= False
        descentAtCover (CoverSearchBudget (Just 5)) kernel BranchBase
          @?= SearchUndecided (CoverSearchBudgetExceeded (CoverSearchBudget (Just 5)) expectedCost :| []) []
        descentAtCover unboundedBudget kernel BranchBase
          @?= SearchRejected ("obstructed=6" :| [])

testVacuousDomainRejectedByGenericKernel :: Assertion
testVacuousDomainRejectedByGenericKernel =
  let kernel =
        bucketCoverKernelWithDomain
          vacuousBucketDomain
          [BranchBase]
          (const True)
          (const [])
          (\_ _ coordinates -> coordinates)
   in descentAtCover unboundedBudget kernel BranchBase
        @?= SearchRejected ((CoordA :| []) :| [])

testVacuousQuotientCoverRejected :: Assertion
testVacuousQuotientCoverRejected =
  withBranchSite $ \branchSite ->
  let sectionAt :: BranchContext -> ContextClassSection TestRep
      sectionAt contextValue =
        ContextClassSection $
          case contextValue of
            BranchBase -> IntMap.fromList [(0, TestRep 0)]
            BranchLeft -> IntMap.fromList [(0, TestRep 1)]
            BranchRight -> IntMap.empty
            BranchApex -> IntMap.fromList [(0, TestRep 1)]
      kernel =
        QuotientDescent.DescentKernel
          { QuotientDescent.dkSite = branchSite,
            QuotientDescent.dkMaterializedContexts = [BranchBase, BranchLeft, BranchRight],
            QuotientDescent.dkClassSectionAt = sectionAt
          }
   in QuotientDescent.descentAt unboundedBudget kernel BranchBase
        @?= SearchRejected
          ( QuotientDescent.DescentVacuousCoverObstruction
              BranchBase
              [BranchLeft, BranchRight]
              (1 :| [])
              :| []
          )

testVacuityBeatsBudget :: Assertion
testVacuityBeatsBudget =
  let kernel =
        bucketCoverKernelWithDomain
          vacuousBucketDomain
          [BranchBase]
          (const True)
          (const [])
          (\_ _ coordinates -> coordinates)
   in descentAtCover (CoverSearchBudget (Just 0)) kernel BranchBase
        @?= SearchRejected ((CoordA :| []) :| [])

testCollectDescentReportTaintPropagation :: Assertion
testCollectDescentReportTaintPropagation =
  let verdictAt :: Int -> SearchVerdict String String
      verdictAt contextValue =
        case contextValue of
          1 -> SearchRejected ("obstruction" :| [])
          2 -> SearchUndecided ("budget" :| []) ["partial"]
          _ -> SearchAccepted
      report =
        collectDescentReport [0, 1, 2] (const True) verdictAt
   in do
        AssignmentDescent.drOutcome report @?= DescentUndecided
        AssignmentDescent.drSatisfied report @?= False
        AssignmentDescent.drRefusals report @?= ["budget"]
        AssignmentDescent.drObstructions report @?= ["obstruction", "partial"]
        AssignmentDescent.drObstructionCount report @?= 2

testCoverContextAtRejectsNegativeCoordinates :: Assertion
testCoverContextAtRejectsNegativeCoordinates =
  coverContextAt (-1) branchPairCover @?= Nothing

branchPairCover :: [BranchContext]
branchPairCover = [BranchLeft, BranchRight]

bucketCoverKernel ::
  [BranchContext] ->
  (Map TestCoordinate BucketValue -> Bool) ->
  ([Map TestCoordinate BucketValue] -> [obstruction]) ->
  (BranchContext -> [BranchContext] -> NonEmpty TestCoordinate -> obstruction) ->
  CoverDescentKernel BranchContext TestCoordinate BucketValue obstruction
bucketCoverKernel =
  bucketCoverKernelWithDomain bucketDomain

bucketCoverKernelWithDomain ::
  (TestCoordinate -> [BucketValue]) ->
  [BranchContext] ->
  (Map TestCoordinate BucketValue -> Bool) ->
  ([Map TestCoordinate BucketValue] -> [obstruction]) ->
  (BranchContext -> [BranchContext] -> NonEmpty TestCoordinate -> obstruction) ->
  CoverDescentKernel BranchContext TestCoordinate BucketValue obstruction
bucketCoverKernelWithDomain domainAt materializedContexts tupleObstructed obstructions vacuousObstruction =
  CoverDescentKernel
    { cdkMaterializedContexts = materializedContexts,
      cdkCoverOf = \case
        BranchBase -> branchPairCover
        _ -> [],
      cdkCoordinates = \_ _ -> [CoordA, CoordB],
      cdkDomainAt = \_ _ -> domainAt,
      cdkCompatible = \_ _ _ _ _ _ -> True,
      cdkTupleObstructed = \_ _ -> tupleObstructed,
      cdkObstructions = \_ _ -> obstructions,
      cdkVacuousObstruction = vacuousObstruction
    }

bucketDomain :: TestCoordinate -> [BucketValue]
bucketDomain CoordA = fmap BucketValue [1, 2, 3]
bucketDomain CoordB = fmap BucketValue [10, 20]

vacuousBucketDomain :: TestCoordinate -> [BucketValue]
vacuousBucketDomain CoordA = []
vacuousBucketDomain CoordB = bucketDomain CoordB

testAdmissibilityEvidencePreserved :: Assertion
testAdmissibilityEvidencePreserved =
  let sectionAt :: BranchContext -> Map TestCoordinate BucketValue
      sectionAt _ =
        Map.fromList [(CoordA, BucketValue 0)]
      admissibility :: BranchContext -> Map TestCoordinate BucketValue -> BranchContext -> Map TestCoordinate BucketValue -> AssignmentDescent.CompatibilityEvidence (Set.Set String) (Sum Double)
      admissibility leftContext _ rightContext _ =
        case (leftContext, rightContext) of
          (BranchBase, BranchLeft) ->
            AssignmentDescent.compatibleEvidence
              (Set.singleton "parent-left")
              (Sum 0.4)
          (BranchBase, BranchRight) ->
            AssignmentDescent.compatibleEvidence
              (Set.singleton "parent-right")
              (Sum 0.6)
          (BranchLeft, BranchRight) ->
            AssignmentDescent.incompatibleEvidence
              (Set.singleton "seam-gap")
              (Sum 1.2)
          _ ->
            AssignmentDescent.compatibleEvidence Set.empty mempty
      kernel :: AssignmentDescent.DescentKernel BranchContext (Map TestCoordinate BucketValue) TestCoordinate BucketValue (Set.Set String) (Sum Double)
      kernel =
        AssignmentDescent.DescentKernel
          { AssignmentDescent.dkCoverOf =
              \case
                BranchBase -> [BranchLeft, BranchRight]
                _ -> [],
            AssignmentDescent.dkMaterializedContexts = [BranchBase, BranchLeft, BranchRight],
            AssignmentDescent.dkSectionAt = sectionAt,
            AssignmentDescent.dkAssignmentOf = id,
            AssignmentDescent.dkAdmissibility = admissibility
          }
      report = AssignmentDescent.fullDescentCheck unboundedBudget kernel
   in case AssignmentDescent.drObstructions report of
        AssignmentDescent.DescentConflictObstruction obstructionValue : _ -> do
          assertBool
            "custom admissibility should obstruct the family even when assignments agree"
            (not (AssignmentDescent.drSatisfied report))
          Map.lookup BranchLeft (AssignmentDescent.doParentAdmissibility obstructionValue)
            @?= Just (AssignmentDescent.compatibleEvidence (Set.singleton "parent-left") (Sum 0.4))
          Map.lookup (BranchLeft, BranchRight) (AssignmentDescent.doPairAdmissibility obstructionValue)
            @?= Just (AssignmentDescent.incompatibleEvidence (Set.singleton "seam-gap") (Sum 1.2))
        otherObstructions ->
          assertBool ("expected assignment conflict obstruction, received " <> show otherObstructions) False

type RepSet :: Type -> Type
type RepSet rep = Set.Set rep

type BinaryBuckets :: Type -> Type
type BinaryBuckets rep = Map rep (Set.Set rep)

type BinaryImage :: Type -> Type
type BinaryImage rep = Map rep (Map rep (Set.Set rep))

oldBinaryDescentForFixture ::
  DenseKey rep =>
  QuotientDescent.DescentKernel owner BranchContext rep ->
  BranchContext ->
  ObstructionVerdict (QuotientDescent.QuotientDescentObstruction BranchContext rep)
oldBinaryDescentForFixture kernel parentContext =
  oldBinaryDescent kernel parentContext BranchLeft BranchRight BranchBase

oldBinaryDescent ::
  DenseKey rep =>
  QuotientDescent.DescentKernel owner BranchContext rep ->
  BranchContext ->
  BranchContext ->
  BranchContext ->
  BranchContext ->
  ObstructionVerdict (QuotientDescent.QuotientDescentObstruction BranchContext rep)
oldBinaryDescent kernel parentContext leftContext rightContext meetContext =
  let parentClasses = QuotientDescent.dkClassSectionAt kernel parentContext
      leftClasses = QuotientDescent.dkClassSectionAt kernel leftContext
      rightClasses = QuotientDescent.dkClassSectionAt kernel rightContext
      meetClasses = QuotientDescent.dkClassSectionAt kernel meetContext
      toMeet = oldRestrictClassIdWith meetClasses . oldRestrictClassIdWith leftClasses
      toLeft = oldRestrictClassIdWith leftClasses
      toRight = oldRestrictClassIdWith rightClasses
      imageRelation = oldImage2 toMeet toLeft toRight (oldRepsOf parentClasses)
      leftBuckets = oldBucketBy toMeet (oldRepsOf leftClasses)
      rightBuckets =
        oldBucketBy
          (oldRestrictClassIdWith meetClasses . oldRestrictClassIdWith rightClasses)
          (oldRepsOf rightClasses)
      obstructedTuples =
        Map.foldlWithKey'
          (missingTuplesAtMeet imageRelation rightBuckets)
          []
          leftBuckets
   in case obstructedTuples of
        [] -> Accepted ()
        tuples ->
          Rejected
            ( QuotientDescent.QuotientDescentObstruction
                parentContext
                [leftContext, rightContext]
                tuples
                :| []
            )

oldRepsOf :: Ord rep => ContextClassSection rep -> RepSet rep
oldRepsOf =
  Set.fromList . IntMap.elems . ccsEntries

oldRestrictClassIdWith :: DenseKey rep => ContextClassSection rep -> rep -> rep
oldRestrictClassIdWith targetClasses classId =
  IntMap.findWithDefault classId (encodeDenseKey classId) (ccsEntries targetClasses)

oldBucketBy :: Ord rep => (rep -> rep) -> RepSet rep -> BinaryBuckets rep
oldBucketBy toMeet =
  Map.fromListWith Set.union
    . fmap (\representative -> (toMeet representative, Set.singleton representative))
    . Set.toAscList

oldImage2 ::
  Ord rep =>
  (rep -> rep) ->
  (rep -> rep) ->
  (rep -> rep) ->
  RepSet rep ->
  BinaryImage rep
oldImage2 toMeet toLeft toRight =
  Map.fromListWith
    (Map.unionWith Set.union)
    . fmap
      ( \topRepresentative ->
          ( toMeet topRepresentative,
            Map.singleton (toLeft topRepresentative) (Set.singleton (toRight topRepresentative))
          )
      )
    . Set.toAscList

missingTuplesAtMeet ::
  Ord rep =>
  BinaryImage rep ->
  BinaryBuckets rep ->
  [IntMap.IntMap rep] ->
  rep ->
  Set.Set rep ->
  [IntMap.IntMap rep]
missingTuplesAtMeet imageRelation rightBuckets accumulatedTuples meetKey leftBucket =
  let rightBucket = Map.findWithDefault Set.empty meetKey rightBuckets
      relationAtMeet = Map.findWithDefault Map.empty meetKey imageRelation
      missingTuples =
        [ IntMap.fromList [(0, leftRepresentative), (1, rightRepresentative)]
        | leftRepresentative <- reverse (Set.toAscList leftBucket),
          rightRepresentative <-
            reverse
              ( Set.toAscList
                  ( Set.difference
                      rightBucket
                      (Map.findWithDefault Set.empty leftRepresentative relationAtMeet)
                  )
              )
        ]
   in missingTuples <> accumulatedTuples
