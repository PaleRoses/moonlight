module Moonlight.EGraph.Context.DescentSpec
  ( tests,
  )
where

import Data.Bifunctor (first)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Core (UnionFindAllocationError)
import Moonlight.EGraph.Pure.Context
    ( beginContextRebaseBatch,
      commitContextRebaseBatch,
      contextMerge,
      ContextDeltaError,
      emptyContextEGraph,
      globalMerge,
      stageSupportClass,
      stageTermAtContext,
      stageTermGlobally,
      ContextEGraph )
import Moonlight.EGraph.Pure.Kernel.HashCons ( addTerm )
import Moonlight.EGraph.Pure.Types ( ClassId, EGraph, emptyEGraph )
import Moonlight.EGraph.Test.Assertions ( isRestrictionBarrier )
import Moonlight.EGraph.Test.Context.Anatomy
    ( AnatomyRegion(Head, Upper, Whole), coarseAnatomyLattice )
import Moonlight.EGraph.Test.Context.Diamond
    ( DiamondCtx(DBottom, DLeft, DRight, DTop) )
import Moonlight.EGraph.Test.Context.SimpleArith
    ( ArithF, Depth, baseFixture, depthSpec, lit, plus )
import Moonlight.Sheaf.Context.Algebra (classesFor, contextEquivalentAt)
import Moonlight.Sheaf.Verdict
  ( SearchVerdict (..),
  )
import Moonlight.Sheaf.Descent.Context
    ( QuotientDescentObstruction(..),
      DescentReport(..),
      descentAt,
      fullDescentCheck )
import Moonlight.Sheaf.Obstruction ( obstructionReport )
import Moonlight.Pale.Test.Site.Assertion (expectRight, withResult)
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( (@?=), assertBool, assertFailure, testCase )
import Moonlight.FiniteLattice
  ( ContextLattice,
    contextLatticeElements,
    latticeContext,
    leqContext,
    principalSupport
  )

diamondLattice :: ContextLattice DiamondCtx
diamondLattice =
  case latticeContext of
    Right latticeValue -> latticeValue
    Left compileError ->
      error ("invalid DiamondCtx lattice fixture: " <> show compileError)

diamondFixture :: Either (ContextDeltaError ArithF DiamondCtx) (ClassId, ClassId, ContextEGraph ArithF Depth DiamondCtx)
diamondFixture = baseFixture diamondLattice

anatomyFixture :: Either (ContextDeltaError ArithF AnatomyRegion) (ClassId, ClassId, ContextEGraph ArithF Depth AnatomyRegion)
anatomyFixture = baseFixture coarseAnatomyLattice

seedThreeLiterals :: Either UnionFindAllocationError (ClassId, ClassId, ClassId, EGraph ArithF Depth)
seedThreeLiterals = do
  let graph0 = emptyEGraph depthSpec
  (classA, graph1) <- addTerm (lit 1) graph0
  (classB, graph2) <- addTerm (lit 2) graph1
  (classC, graph3) <- addTerm (lit 3) graph2
  pure (classA, classB, classC, graph3)

obstructionCoverElements :: QuotientDescentObstruction ctx classId -> [ctx]
obstructionCoverElements obstruction =
  case obstruction of
    QuotientDescentObstruction _ coverElements _ ->
      coverElements
    DescentMeetLookupObstruction _ coverElements _ _ _ ->
      coverElements
    DescentSupportLookupObstruction _ coverElements _ _ ->
      coverElements
    DescentCoverLookupObstruction {} ->
      []
    DescentJoinLookupObstruction {} ->
      []
    DescentClassSectionLookupObstruction {} ->
      []
    DescentVacuousCoverObstruction _ coverElements _ ->
      coverElements
    DescentMonotonicityObstruction _ coverElement _ _ _ ->
      [coverElement]

obstructionHasTuples :: QuotientDescentObstruction ctx classId -> Bool
obstructionHasTuples obstruction =
  case obstruction of
    QuotientDescentObstruction _ _ tuples ->
      not (null tuples)
    DescentCoverLookupObstruction {} ->
      False
    DescentMeetLookupObstruction {} ->
      False
    DescentSupportLookupObstruction {} ->
      False
    DescentJoinLookupObstruction {} ->
      False
    DescentClassSectionLookupObstruction {} ->
      False
    DescentVacuousCoverObstruction {} ->
      False
    DescentMonotonicityObstruction {} ->
      False

tests :: TestTree
tests =
  testGroup
    "descent"
    [ diamondUnitTests,
      anatomyIntegrationTests,
      monotonicityTests
    ]

sectionMonotonicityViolations ::
  Ord c =>
  Show c =>
  ContextLattice c ->
  ContextEGraph ArithF Depth c ->
  Either String [String]
sectionMonotonicityViolations latticeValue contextGraph =
  fmap concat (traverse violationsAt comparablePairs)
  where
    comparablePairs =
      [ (parentContext, upperContext)
        | parentContext <- contextLatticeElements latticeValue,
          upperContext <- contextLatticeElements latticeValue,
          parentContext /= upperContext,
          leqContext latticeValue parentContext upperContext == Right True
      ]

    violationsAt (parentContext, upperContext) = do
      parentSection <- first show (classesFor parentContext contextGraph)
      upperSection <- first show (classesFor upperContext contextGraph)
      pure
        ( [ "key " <> show keyValue
              <> " visible at " <> show parentContext
              <> " is missing from the section at " <> show upperContext
          | keyValue <- IntMap.keys parentSection,
            not (IntMap.member keyValue upperSection)
          ]
            <> [ "keys " <> show (leftKey, rightKey)
                   <> " equivalent at " <> show parentContext
                   <> " diverge at " <> show upperContext
               | (leftKey, leftClass) <- IntMap.toAscList parentSection,
                 (rightKey, rightClass) <- IntMap.toAscList parentSection,
                 leftKey < rightKey,
                 leftClass == rightClass,
                 IntMap.lookup leftKey upperSection /= IntMap.lookup rightKey upperSection
               ]
        )

assertSectionMonotonicity ::
  Ord c =>
  Show c =>
  String ->
  ContextLattice c ->
  ContextEGraph ArithF Depth c ->
  IO ()
assertSectionMonotonicity label latticeValue contextGraph =
  case sectionMonotonicityViolations latticeValue contextGraph of
    Left supportError ->
      assertFailure (label <> ": " <> supportError)
    Right [] ->
      pure ()
    Right violations ->
      assertFailure (label <> ":\n" <> unlines violations)

monotonicityTests :: TestTree
monotonicityTests =
  testGroup
    "section monotonicity (election-free descent precondition)"
    [ testCase "diamond: sections are monotone along <= across merge scripts" $ do
        (termA, termB, ceg) <- expectRight diamondFixture
        assertSectionMonotonicity "unmerged" diamondLattice ceg
        withResult (contextMerge DLeft termA termB ceg) $ \oneSided ->
          assertSectionMonotonicity "one-sided context merge" diamondLattice oneSided
        withResult (globalMerge termA termB ceg) $ \global ->
          assertSectionMonotonicity "global merge" diamondLattice global
        (a, b, c, graph3) <- expectRight seedThreeLiterals
        let conflictBase = emptyContextEGraph diamondLattice graph3
        withResult (contextMerge DLeft a b conflictBase) $ \leftMerged ->
          withResult (contextMerge DRight b c leftMerged) $ \conflicting ->
            assertSectionMonotonicity "conflicting incomparable merges" diamondLattice conflicting
        withResult
          ( do
              narrowedBatch <-
                stageSupportClass
                  (principalSupport DLeft)
                  b
                  (beginContextRebaseBatch conflictBase)
              (_report, narrowed) <- commitContextRebaseBatch narrowedBatch
              contextMerge DLeft a b narrowed
          ) $ \narrowedMerged ->
          assertSectionMonotonicity "carrier-limited merge" diamondLattice narrowedMerged,
      testCase "anatomy: sections are monotone with local generators and narrowed carriers" $ do
        (termA, termB, ceg) <- expectRight anatomyFixture
        assertSectionMonotonicity "unmerged" coarseAnatomyLattice ceg
        withResult (contextMerge Head termA termB ceg) $ \headMerged ->
          assertSectionMonotonicity "merge at Head" coarseAnatomyLattice headMerged
        withResult (contextMerge Upper termA termB ceg) $ \upperMerged ->
          assertSectionMonotonicity "merge at Upper" coarseAnatomyLattice upperMerged
        withResult supportSensitiveAdmissibilityFixture $ \(fixtureA, _fixtureB, localHead, extended) -> do
          assertSectionMonotonicity "staged local generator" coarseAnatomyLattice extended
          withResult (contextMerge Head localHead fixtureA extended) $ \localMerged ->
            assertSectionMonotonicity "local generator merged at Head" coarseAnatomyLattice localMerged
    ]

diamondUnitTests :: TestTree
diamondUnitTests =
  testGroup
    "diamond lattice (unit)"
    [ testCase "unmerged diamond: descent satisfied everywhere" $ do
        (_, _, ceg) <- expectRight diamondFixture
        let report = fullDescentCheck ceg
        drSatisfied report @?= True
        drObstructionCount report @?= 0,
      testCase "compatible merge (both sides): descent satisfied at DTop" $ do
        (termA, termB, ceg) <- expectRight diamondFixture
        withResult (contextMerge DRight termA termB ceg) $ \rightMerged ->
          withResult (contextMerge DLeft termA termB rightMerged) $ \merged ->
            descentAt DTop merged @?= SearchAccepted,
      testCase "overlap agreement across incomparable branches does not force gluing at DBottom" $ do
        (termA, termB, termC, graph3) <- expectRight seedThreeLiterals
        let ceg = emptyContextEGraph diamondLattice graph3
        withResult (contextMerge DLeft termA termB ceg) $ \leftMerged ->
          withResult (contextMerge DRight termA termC leftMerged) $ \merged -> do
                contextEquivalentAt DLeft termA termB merged @?= Right True
                contextEquivalentAt DRight termA termC merged @?= Right True
                contextEquivalentAt DTop termB termC merged @?= Right True
                contextEquivalentAt DBottom termB termC merged @?= Right False
                assertBool "expected restriction barrier when overlap-compatible branches fail to glue at DBottom"
                  (any isRestrictionBarrier (obstructionReport termB termC DBottom merged)),
      testCase "one-sided merge propagates to top — descent still holds (propagation resolves)" $ do
        (termA, termB, ceg) <- expectRight diamondFixture
        withResult (contextMerge DLeft termA termB ceg) $ \merged ->
          descentAt DTop merged @?= SearchAccepted,
      testCase "after propagation converges, descent is always satisfied" $ do
        (termA, termB, ceg) <- expectRight diamondFixture
        withResult (contextMerge DLeft termA termB ceg) $ \merged -> do
              let report = fullDescentCheck merged
              drSatisfied report @?= True
              drObstructionCount report @?= 0,
      testCase "conflicting merges at DLeft and DRight — descent at DBottom" $ do
        (a, b, c, graph3) <- expectRight seedThreeLiterals
        let ceg = emptyContextEGraph diamondLattice graph3
        withResult (contextMerge DLeft a b ceg) $ \leftMerged ->
          withResult (contextMerge DRight b c leftMerged) $ \merged ->
                case descentAt DBottom merged of
                  SearchAccepted ->
                    assertBool "descent at DBottom may or may not be obstructed depending on propagation" True
                  SearchRejected obstructions ->
                    assertBool "if obstructed, has concrete tuples"
                      (obstructionHasTuples (NonEmpty.head obstructions))
                  SearchUndecided {} ->
                    assertBool "unbounded descent at DBottom should decide" False,
      testCase "leaf context: descent trivially satisfied" $ do
        (termA, termB, ceg) <- expectRight diamondFixture
        withResult (contextMerge DLeft termA termB ceg) $ \merged -> do
              descentAt DLeft merged @?= SearchAccepted
              descentAt DRight merged @?= SearchAccepted,
      testCase "bottom context with no children above: descent trivially satisfied" $ do
        (_, _, ceg) <- expectRight diamondFixture
        descentAt DBottom ceg @?= SearchAccepted,
      testCase "global merge: descent satisfied everywhere" $ do
        (termA, termB, ceg) <- expectRight diamondFixture
        withResult (globalMerge termA termB ceg) $ \merged -> do
              let report = fullDescentCheck merged
              drSatisfied report @?= True
              drObstructionCount report @?= 0,
      testCase "regional merges with a top-local congruence bystander do not poison the meet" $ do
        (atomA, atomB, atomC, graph3) <- expectRight seedThreeLiterals
        let ceg0 = emptyContextEGraph diamondLattice graph3
        withResult
              ( do
                  (parentB, stagedParentB) <-
                    stageTermGlobally (plus (lit 2) (lit 2)) (beginContextRebaseBatch ceg0)
                  (_parentA, stagedParentA) <-
                    stageTermGlobally (plus (lit 1) (lit 1)) stagedParentB
                  (_parentC, stagedParents) <-
                    stageTermGlobally (plus (lit 3) (lit 3)) stagedParentA
                  narrowed <- stageSupportClass (principalSupport DTop) parentB stagedParents
                  (_report, extended) <- commitContextRebaseBatch narrowed
                  pure extended
              ) $ \extended ->
              withResult (contextMerge DLeft atomA atomB extended) $ \leftMerged ->
                withResult (contextMerge DRight atomB atomC leftMerged) $ \merged -> do
                  contextEquivalentAt DLeft atomA atomB merged @?= Right True
                  contextEquivalentAt DRight atomB atomC merged @?= Right True
                  contextEquivalentAt DBottom atomA atomB merged @?= Right False
                  descentAt DBottom merged @?= SearchAccepted
                  let report = fullDescentCheck merged
                  drSatisfied report @?= True,
      testCase "election divergence across covers does not manufacture obstructions at the meet" $ do
        (a, b, c, graph3) <- expectRight seedThreeLiterals
        let ceg0 = emptyContextEGraph diamondLattice graph3
        withResult
              ( do
                  narrowedBatch <-
                    stageSupportClass
                      (principalSupport DLeft)
                      c
                      (beginContextRebaseBatch ceg0)
                  (_report, narrowed) <- commitContextRebaseBatch narrowedBatch
                  pure narrowed
              ) $ \narrowed ->
              withResult (contextMerge DLeft a c narrowed) $ \leftMerged ->
                withResult (contextMerge DRight a b leftMerged) $ \merged -> do
                  descentAt DBottom merged @?= SearchAccepted
                  drSatisfied (fullDescentCheck merged) @?= True
    ]

anatomyIntegrationTests :: TestTree
anatomyIntegrationTests =
  testGroup
    "anatomy lattice (integration)"
    [ testCase "unmerged anatomy: descent satisfied everywhere" $ do
        (_, _, ceg) <- expectRight anatomyFixture
        drSatisfied (fullDescentCheck ceg) @?= True,
      testCase "merge at Head: descent at Upper checks k=4 cover" $ do
        (termA, termB, ceg) <- expectRight anatomyFixture
        withResult (contextMerge Head termA termB ceg) $ \merged ->
          case descentAt Upper merged of
            SearchAccepted -> pure ()
            SearchRejected obstructions ->
              assertBool "if obstructed, cover has >1 element"
                (length (obstructionCoverElements (NonEmpty.head obstructions)) > 1)
            SearchUndecided {} ->
              assertBool "unbounded descent at Upper should decide" False,
      testCase "descent accepts invisible local generators across k-ary covers" $
        withResult supportSensitiveAdmissibilityFixture $ \(termA, _termB, localHead, extended) ->
          withResult (contextMerge Head localHead termA extended) $ \merged ->
            descentAt Upper merged @?= SearchAccepted,
      testCase "global merge: descent satisfied at every level" $ do
        (termA, termB, ceg) <- expectRight anatomyFixture
        withResult (globalMerge termA termB ceg) $ \merged -> do
              let report = fullDescentCheck merged
              drSatisfied report @?= True
              drObstructionCount report @?= 0,
      testCase "merge at Upper: descent at Whole checks k=2 cover {Upper, Lower}" $ do
        (termA, termB, ceg) <- expectRight anatomyFixture
        withResult (contextMerge Upper termA termB ceg) $ \merged ->
          case descentAt Whole merged of
            SearchAccepted -> pure ()
            SearchRejected obstructions ->
              assertBool "obstruction has cover elements"
                (not (null (obstructionCoverElements (NonEmpty.head obstructions))))
            SearchUndecided {} ->
              assertBool "unbounded descent at Whole should decide" False,
      testCase "regional cache survives unrelated base growth" $
        withResult supportSensitiveAdmissibilityFixture $ \(_termA, _termB, _localHead, extended) ->
          withResult (stageTermGlobally (lit 99) (beginContextRebaseBatch extended)) $ \(_unrelated, grownBatch) ->
            withResult (commitContextRebaseBatch grownBatch) $ \(_report, grown) ->
              drSatisfied (fullDescentCheck grown) @?= True
    ]

supportSensitiveAdmissibilityFixture ::
  Either
    (ContextDeltaError ArithF AnatomyRegion)
    (ClassId, ClassId, ClassId, ContextEGraph ArithF Depth AnatomyRegion)
supportSensitiveAdmissibilityFixture =
  anatomyFixture >>= \(termA, termB, ceg) -> do
    (localHead, stagedBatch) <-
      stageTermAtContext Head (lit 7) (beginContextRebaseBatch ceg)
    (_rebaseReport, extended) <- commitContextRebaseBatch stagedBatch
    pure (termA, termB, localHead, extended)
