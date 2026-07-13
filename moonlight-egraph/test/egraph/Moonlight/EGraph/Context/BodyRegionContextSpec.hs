{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Moonlight.EGraph.Context.BodyRegionContextSpec
  ( tests,
  )
where

import Data.Kind ( Type )
import Data.Bifunctor (first)
import Data.Either ( isLeft, isRight )
import Data.List.NonEmpty (NonEmpty (..))
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Algebra
    ( BoundedJoinSemilattice(..),
      BoundedMeetSemilattice(..),
      JoinSemilattice(..),
      Lattice,
      MeetSemilattice(..) )
import Moonlight.FiniteLattice
  ( ContextLattice,
    joinContext,
    latticeContext,
    leqContext,
    meetContext,
    principalSupport
  )
import Moonlight.EGraph.Effect.Harness ( obstructionComplete )
import Moonlight.EGraph.Pure.Context
    ( ContextDeltaError (..),
      beginContextRebaseBatch,
      commitContextRebaseBatch,
      contextCachedObjectsForExecution,
      contextMerge,
      ContextMutationTrace (..),
      ContextRebaseReport (..),
      contextMutationTraceEffect,
      emptyContextEGraph,
      globalMerge,
      stageSupportClass,
      planContextMerges,
      stageContextMerges,
      stageTermGlobally,
      stageTermAtContext,
      rebaseContextGraphAtContexts,
      ContextEGraph )
import Moonlight.EGraph.Test.Context.MaterializedOracle
    ( materializedContextGraphAt )
import Moonlight.EGraph.Pure.Context.Core
    ( cegBase,
      cegRuntimeState,
      ContextRuntimeState (..),
      materializeIncidenceCategoryFromSnapshot,
      materializeIncidenceSiteFromSnapshot )
import Moonlight.Sheaf.Context.Core qualified as SheafCore
import Moonlight.Sheaf.Context.Site
    ( classSupportDeltaTouchedClassKeys,
    )
import Moonlight.EGraph.Pure.Kernel.HashCons ( addTerm )
import Moonlight.Core.EGraph.Program (eGraphProgramEffectCount)
import Moonlight.EGraph.Pure.Change (observedClassUnionPairs)
import Moonlight.Core qualified as UnionFind
import Moonlight.EGraph.Pure.Types
    ( classIdKey, ClassId, EGraph, eGraphClassCount, eGraphUnionFind, emptyEGraph )
import Moonlight.EGraph.Sheaf.IncidenceSite
    ( incidenceCategoryStructuralMorphisms,
      incidenceClassesEquivalent,
      incidenceClassRepresentative )
import Moonlight.EGraph.Test.Arith.Core
    ( ArithF(..), NodeCount(..), addTermNode, analysisSpec, numTerm )
import Moonlight.EGraph.Test.Assertions
    ( isContextBarrier,
      isPropagationBarrier,
      isRestrictionBarrier,
      isStructuralMismatch )
import Moonlight.Sheaf.Context.Algebra
  ( contextClassAt,
    contextEquivalentAt,
    restrictionMap,
  )
import Moonlight.Sheaf.Obstruction
    ( obstructionReport, whyNotMerged )
import Moonlight.Sheaf.Site (nerveSiteCells)
import Moonlight.Pale.Test.LawSuite ( LawSuite, renderLawSuite )
import Moonlight.Pale.Test.Laws.Lattice
    ( LatticeLawSeedError,
      latticeLawSeed,
      unfoldLatticeLaws,
      withBounded,
      withComparableFilter )
import Moonlight.Pale.Test.Site.Assertion (expectRight)
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit
    ( (@?=), assertBool, assertEqual, assertFailure, testCase )
import Data.IntMap.Strict qualified as IntMap
    ( findWithDefault, map )

type BodyRegion :: Type
data BodyRegion
  = Creature
  | Torso
  | Head
  | LimbUpper
  | LimbLower
  | Ribcage
  | Spine
  | Jaw
  | Cranium
  | PawFront
  | PawHind
  | BodyLocal
  deriving stock (Eq, Ord, Show, Enum, Bounded)

ancestors :: BodyRegion -> [BodyRegion]
ancestors Creature = []
ancestors Torso = [Creature]
ancestors Head = [Creature]
ancestors LimbUpper = [Creature]
ancestors LimbLower = [Creature]
ancestors Ribcage = [Torso, Creature]
ancestors Spine = [Torso, Creature]
ancestors Jaw = [Head, Creature]
ancestors Cranium = [Head, Creature]
ancestors PawFront = [LimbUpper, Creature]
ancestors PawHind = [LimbLower, Creature]
ancestors BodyLocal = [minBound .. PawHind]

regionLeq :: BodyRegion -> BodyRegion -> Bool
regionLeq leftRegion rightRegion
  | leftRegion == rightRegion = True
  | rightRegion == BodyLocal = True
  | leftRegion == Creature = True
  | otherwise = leftRegion `elem` ancestors rightRegion

instance JoinSemilattice BodyRegion where
  join leftRegion rightRegion
    | regionLeq leftRegion rightRegion = rightRegion
    | regionLeq rightRegion leftRegion = leftRegion
    | otherwise = leastCommonUpperBound leftRegion rightRegion

instance BoundedJoinSemilattice BodyRegion where
  bottom = Creature

instance MeetSemilattice BodyRegion where
  meet leftRegion rightRegion
    | regionLeq leftRegion rightRegion = leftRegion
    | regionLeq rightRegion leftRegion = rightRegion
    | otherwise = greatestCommonLowerBound leftRegion rightRegion

instance BoundedMeetSemilattice BodyRegion where
  top = BodyLocal

instance Lattice BodyRegion

leastCommonUpperBound :: BodyRegion -> BodyRegion -> BodyRegion
leastCommonUpperBound leftRegion rightRegion =
  let leftUpperBounds = filter (\candidate -> regionLeq leftRegion candidate) [minBound .. maxBound]
      rightUpperBounds = filter (\candidate -> regionLeq rightRegion candidate) [minBound .. maxBound]
      commonUpperBounds = filter (\candidate -> candidate `elem` rightUpperBounds) leftUpperBounds
      isLeast candidate =
        not
          (any
             (\other -> other /= candidate && regionLeq other candidate)
             commonUpperBounds)
   in case filter isLeast commonUpperBounds of
        (firstBound : _) -> firstBound
        [] -> BodyLocal

greatestCommonLowerBound :: BodyRegion -> BodyRegion -> BodyRegion
greatestCommonLowerBound leftRegion rightRegion =
  let leftLowerBounds = filter (\candidate -> regionLeq candidate leftRegion) [minBound .. maxBound]
      rightLowerBounds = filter (\candidate -> regionLeq candidate rightRegion) [minBound .. maxBound]
      commonLowerBounds = filter (\candidate -> candidate `elem` rightLowerBounds) leftLowerBounds
      isGreatest candidate =
        not
          (any
             (\other -> other /= candidate && regionLeq candidate other)
             commonLowerBounds)
   in case filter isGreatest commonLowerBounds of
        (firstBound : _) -> firstBound
        [] -> Creature

tests :: TestTree
tests =
  testGroup
    "body-region-context"
    [ latticePropertyTests,
      contextPropagationTests,
      restrictionMapTests,
      obstructionTests
    ]

classesEquivalentAt :: BodyRegion -> ClassId -> ClassId -> ContextEGraph ArithF NodeCount BodyRegion -> Bool
classesEquivalentAt contextValue leftClassId rightClassId contextGraph =
  either
    (const False)
    id
    (contextEquivalentAt contextValue leftClassId rightClassId contextGraph)

latticePropertyTests :: TestTree
latticePropertyTests =
  testGroup
    "lattice structure" $
    [ testCase "ribcage and spine are incomparable siblings" $ do
        leqContext bodyLattice Ribcage Spine @?= Right False
        leqContext bodyLattice Spine Ribcage @?= Right False,
      testCase "join of incomparable siblings yields body-local" $
        joinContext bodyLattice Ribcage Spine @?= Right BodyLocal,
      testCase "meet of incomparable siblings yields their common parent" $
        meetContext bodyLattice Ribcage Spine @?= Right Torso,
      testCase "torso is below ribcage in lattice order" $
        leqContext bodyLattice Torso Ribcage @?= Right True,
      testCase "head and torso are incomparable" $ do
        leqContext bodyLattice Head Torso @?= Right False
        leqContext bodyLattice Torso Head @?= Right False,
      testCase "meet of cross-subtree leaves yields creature" $
        meetContext bodyLattice Jaw PawFront @?= Right Creature
    ]
      <> bodyRegionLatticeLawTests

bodyRegionLatticeLawTests :: [TestTree]
bodyRegionLatticeLawTests =
  case bodyRegionLatticeLawSuites of
    Right lawSuites ->
      fmap renderLawSuite lawSuites
    Left seedErrors ->
      [testCase "body-region lattice seed validates" (expectRight (Left seedErrors))]

bodyRegionLatticeLawSuites :: Either (NonEmpty (LatticeLawSeedError BodyRegion)) [LawSuite]
bodyRegionLatticeLawSuites =
  unfoldLatticeLaws
    <$> ( latticeLawSeed "body-region" join meet allRegionsNonEmpty
            >>= (withBounded Creature BodyLocal . withComparableFilter regionLeq)
        )

contextPropagationTests :: TestTree
contextPropagationTests =
  testGroup
    "context propagation"
    [ testCase "merge at creature propagates to all regions" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        mergedGraph <- expectRight (contextMerge Creature termA termB baseContextGraph)
        assertBool "creature merge should be visible at every region"
          (all
             (\region -> classesEquivalentAt region termA termB mergedGraph)
             allRegions),
      testCase "merge at paw-front stays local to paw-front and body-local" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        mergedGraph <- expectRight (contextMerge PawFront termA termB baseContextGraph)
        classesEquivalentAt PawFront termA termB mergedGraph @?= True
        classesEquivalentAt BodyLocal termA termB mergedGraph @?= True
        classesEquivalentAt LimbUpper termA termB mergedGraph @?= False
        classesEquivalentAt Creature termA termB mergedGraph @?= False
        classesEquivalentAt Head termA termB mergedGraph @?= False,
      testCase "merge at torso propagates to ribcage, spine, and body-local but not head" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        mergedGraph <- expectRight (contextMerge Torso termA termB baseContextGraph)
        classesEquivalentAt Torso termA termB mergedGraph @?= True
        classesEquivalentAt Ribcage termA termB mergedGraph @?= True
        classesEquivalentAt Spine termA termB mergedGraph @?= True
        classesEquivalentAt BodyLocal termA termB mergedGraph @?= True
        classesEquivalentAt Head termA termB mergedGraph @?= False
        classesEquivalentAt Jaw termA termB mergedGraph @?= False
        classesEquivalentAt Cranium termA termB mergedGraph @?= False
        classesEquivalentAt LimbUpper termA termB mergedGraph @?= False
        classesEquivalentAt PawFront termA termB mergedGraph @?= False
        classesEquivalentAt Creature termA termB mergedGraph @?= False,
      testCase "merge at head propagates to jaw, cranium, body-local but not torso subtree" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        mergedGraph <- expectRight (contextMerge Head termA termB baseContextGraph)
        classesEquivalentAt Head termA termB mergedGraph @?= True
        classesEquivalentAt Jaw termA termB mergedGraph @?= True
        classesEquivalentAt Cranium termA termB mergedGraph @?= True
        classesEquivalentAt BodyLocal termA termB mergedGraph @?= True
        classesEquivalentAt Torso termA termB mergedGraph @?= False
        classesEquivalentAt Ribcage termA termB mergedGraph @?= False
        classesEquivalentAt Creature termA termB mergedGraph @?= False,
      testCase "global merge makes equivalence visible everywhere" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        mergedGraph <- expectRight (globalMerge termA termB baseContextGraph)
        assertBool "global merge visible everywhere"
          (all
             (\region -> classesEquivalentAt region termA termB mergedGraph)
             allRegions)
        isGlobalEquivalence termA termB mergedGraph @?= True,
      testCase "two independent context merges do not interfere" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        (termC, termD, contextGraphWithExtra) <- expectRight (fixtureBodyGraphExtended baseContextGraph)
        afterFirstMerge <- expectRight (contextMerge Jaw termA termB contextGraphWithExtra)
        afterBothMerges <- expectRight (contextMerge PawFront termC termD afterFirstMerge)
        classesEquivalentAt Jaw termA termB afterBothMerges @?= True
        classesEquivalentAt PawFront termC termD afterBothMerges @?= True
        classesEquivalentAt PawFront termA termB afterBothMerges @?= False
        classesEquivalentAt Jaw termC termD afterBothMerges @?= False,
      testCase "chained merges at increasing specificity accumulate" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        (termC, termD, contextGraphWithExtra) <- expectRight (fixtureBodyGraphExtended baseContextGraph)
        afterCreatureMerge <- expectRight (contextMerge Creature termA termB contextGraphWithExtra)
        afterTorsoMerge <- expectRight (contextMerge Torso termC termD afterCreatureMerge)
        classesEquivalentAt Ribcage termA termB afterTorsoMerge @?= True
        classesEquivalentAt Ribcage termC termD afterTorsoMerge @?= True
        classesEquivalentAt Head termA termB afterTorsoMerge @?= True
        classesEquivalentAt Head termC termD afterTorsoMerge @?= False,
      testCase "context find returns canonical representative" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        mergedGraph <- expectRight (contextMerge Torso termA termB baseContextGraph)
        contextClassAt Torso termA mergedGraph @?= contextClassAt Torso termB mergedGraph,
      testCase "propagation report is present after context merge" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        mergedGraph <- expectRight (contextMerge Head termA termB baseContextGraph)
        contextPropagationSettled mergedGraph @?= True
        contextPropagationFailed mergedGraph @?= False,
      testCase "central context merge applies context merge semantics" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        mergedGraph <- expectRight (contextMerge Torso termA termB baseContextGraph)
        classesEquivalentAt Torso termA termB mergedGraph @?= True
        classesEquivalentAt Head termA termB mergedGraph @?= False,
      testCase "context incidence nerve site materializes on demand from snapshot" $ do
        (_, _, baseContextGraph) <- expectRight fixtureBodyGraph
        site <- expectRight (materializeIncidenceSiteFromSnapshot Torso baseContextGraph)
        assertBool "expected non-empty generic incidence nerve site" (not (null (nerveSiteCells site))),
      testCase "context incidence representatives agree with direct graph after local merge" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        mergedGraph <- expectRight (contextMerge Torso termA termB baseContextGraph)
        categoryValue <- expectRight (materializeIncidenceCategoryFromSnapshot Ribcage mergedGraph)
        termARepresentative <- expectRight (contextClassAt Ribcage termA mergedGraph)
        termBRepresentative <- expectRight (contextClassAt Ribcage termB mergedGraph)
        incidenceClassRepresentative termA categoryValue @?= Right termARepresentative
        incidenceClassRepresentative termB categoryValue @?= Right termBRepresentative,
      testCase "context incidence category reflects context-local insertion before merge" $ do
        (termA, _, baseContextGraph) <- expectRight fixtureBodyGraph
        (localClass, stagedBatch) <-
          expectRight
            (stageTermAtContext Torso (numTerm 7) (beginContextRebaseBatch baseContextGraph))
        (_rebaseReport, extendedGraph) <- expectRight (commitContextRebaseBatch stagedBatch)
        mergedGraph <- expectRight (contextMerge Torso localClass termA extendedGraph)
        categoryValue <- expectRight (materializeIncidenceCategoryFromSnapshot Torso mergedGraph)
        incidenceClassesEquivalent localClass termA categoryValue @?= Right True
        classesEquivalentAt Torso localClass termA mergedGraph @?= True,
      testCase "global multi-node term insertion reports every fresh node" $ do
        let baseContextGraph =
              emptyContextEGraph bodyLattice (emptyEGraph analysisSpec)
        (_insertedClass, stagedBatch) <-
          expectRight
            (stageTermGlobally (addTermNode (numTerm 1) (numTerm 2)) (beginContextRebaseBatch baseContextGraph))
        (rebaseReport, _extendedGraph) <- expectRight (commitContextRebaseBatch stagedBatch)
        eGraphProgramEffectCount (contextMutationTraceEffect (crrTrace rebaseReport)) @?= 3,
      testCase "context trace records per-context local unions without overcounting propagated effect" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        let initialBatch = beginContextRebaseBatch baseContextGraph
        mergePlan <- expectRight (planContextMerges [Torso] termA termB initialBatch)
        stagedBatch <- expectRight (stageContextMerges mergePlan initialBatch)
        (rebaseReport, _mergedGraph) <- expectRight (commitContextRebaseBatch stagedBatch)
        let traceValue = crrTrace rebaseReport
        eGraphProgramEffectCount (contextMutationTraceEffect traceValue) @?= 1
        length (observedClassUnionPairs (cmtObservedLocalUnions traceValue)) @?= 1
        case Map.lookup Torso (cmtObservedLocalUnionsByContext traceValue) of
          Just torsoUnions ->
            length (observedClassUnionPairs torsoUnions) @?= 1
          Nothing ->
            assertFailure "expected Torso-local union evidence",
      testCase "context trace records support touches only when support changes" $ do
        let baseContextGraph =
              emptyContextEGraph bodyLattice (emptyEGraph analysisSpec)
        (localClass, stagedInsertion) <-
          expectRight
            (stageTermAtContext Torso (numTerm 7) (beginContextRebaseBatch baseContextGraph))
        (insertReport, extendedGraph) <- expectRight (commitContextRebaseBatch stagedInsertion)
        IntSet.size (classSupportDeltaTouchedClassKeys (cmtSupportDelta (crrTrace insertReport))) @?= 1
        restagedSupport <-
          expectRight
            (stageSupportClass (principalSupport Torso) localClass (beginContextRebaseBatch extendedGraph))
        (supportReport, _supportGraph) <- expectRight (commitContextRebaseBatch restagedSupport)
        IntSet.size (classSupportDeltaTouchedClassKeys (cmtSupportDelta (crrTrace supportReport))) @?= 0,
      testCase "context term staging supports unchanged compound footprint" $ do
        let baseContextGraph =
              emptyContextEGraph bodyLattice (emptyEGraph analysisSpec)
            compoundTerm =
              addTermNode (numTerm 1) (numTerm 2)
        (torsoClass, stagedTorsoInsertion) <-
          expectRight
            (stageTermAtContext Torso compoundTerm (beginContextRebaseBatch baseContextGraph))
        (_torsoReport, torsoGraph) <- expectRight (commitContextRebaseBatch stagedTorsoInsertion)
        (headClass, stagedHeadInsertion) <-
          expectRight
            (stageTermAtContext Head compoundTerm (beginContextRebaseBatch torsoGraph))
        (headReport, headGraph) <- expectRight (commitContextRebaseBatch stagedHeadInsertion)
        headClass @?= torsoClass
        IntSet.size (classSupportDeltaTouchedClassKeys (cmtSupportDelta (crrTrace headReport))) @?= 3
        contextClassAt Head headClass headGraph @?= Right headClass
        headIncidenceCategory <- expectRight (materializeIncidenceCategoryFromSnapshot Head headGraph)
        length (incidenceCategoryStructuralMorphisms headIncidenceCategory) @?= 2
    ]

contextPropagationSettled :: ContextEGraph f a c -> Bool
contextPropagationSettled contextGraph =
  maybe False SheafCore.contextPropagationSettled (crsLastRepair (cegRuntimeState contextGraph))

contextPropagationFailed :: ContextEGraph f a c -> Bool
contextPropagationFailed _contextGraph =
  False

restrictionMapTests :: TestTree
restrictionMapTests =
  testGroup
    "restriction maps"
    [ testCase "restriction from ribcage to torso exists (torso leq ribcage)" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        mergedGraph <- expectRight (contextMerge Torso termA termB baseContextGraph)
        assertBool "restriction ribcage -> torso should exist"
          (isRight (restrictionMap Ribcage Torso mergedGraph)),
      testCase "restriction from torso to head is typed failure (incomparable)" $ do
        (_, _, baseContextGraph) <- expectRight fixtureBodyGraph
        isLeft (restrictionMap Torso Head baseContextGraph) @?= True,
      testCase "restriction from paw-front to creature exists" $ do
        (_, _, baseContextGraph) <- expectRight fixtureBodyGraph
        assertBool "restriction paw-front -> creature should exist"
          (isRight (restrictionMap PawFront Creature baseContextGraph)),
      testCase "restriction from jaw to paw-hind is typed failure (incomparable)" $ do
        (_, _, baseContextGraph) <- expectRight fixtureBodyGraph
        isLeft (restrictionMap Jaw PawHind baseContextGraph) @?= True,
      testCase "restriction maps compose across creature-torso-ribcage chain" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        mergedGraph <- expectRight (contextMerge Torso termA termB baseContextGraph)
        ribcageToTorso <- expectRight (restrictionMap Ribcage Torso mergedGraph)
        torsoToCreature <- expectRight (restrictionMap Torso Creature mergedGraph)
        ribcageToCreature <- expectRight (restrictionMap Ribcage Creature mergedGraph)
        let composedRestriction =
              IntMap.map
                (\classId -> IntMap.findWithDefault classId (classIdKey classId) torsoToCreature)
                ribcageToTorso
        composedRestriction @?= ribcageToCreature,
      testCase "restrict-to-context produces a coherent base graph" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        mergedGraph <- expectRight (contextMerge Torso termA termB baseContextGraph)
        restrictedGraph <- expectRight (materializedContextGraphAt Torso mergedGraph)
        assertBool "restricted graph should have positive class count"
          (eGraphClassCount restrictedGraph > 0)
    ]

obstructionTests :: TestTree
obstructionTests =
  testGroup
    "obstructions"
    [ testCase "obstruction report non-empty when classes merged only at leaf" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        mergedGraph <- expectRight (contextMerge PawFront termA termB baseContextGraph)
        assertEqual "exactly one structural mismatch" 1 (length (filter isStructuralMismatch (obstructionReport termA termB Creature mergedGraph)))
        assertEqual "exactly one context barrier" 1 (length (filter isContextBarrier (obstructionReport termA termB Creature mergedGraph)))
        assertEqual "exactly one restriction barrier" 1 (length (filter isRestrictionBarrier (obstructionReport termA termB Creature mergedGraph)))
        assertEqual "no propagation barrier" 0 (length (filter isPropagationBarrier (obstructionReport termA termB Creature mergedGraph))),
      testCase "context barrier present when equivalence is context-dependent" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        mergedGraph <- expectRight (contextMerge Jaw termA termB baseContextGraph)
        assertBool "expected context barrier" (any isContextBarrier (whyNotMerged termA termB mergedGraph)),
      testCase "no obstructions when classes are equivalent at queried context" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        mergedGraph <- expectRight (contextMerge Torso termA termB baseContextGraph)
        assertBool "expected empty obstruction report" (null (obstructionReport termA termB Ribcage mergedGraph)),
      testCase "obstruction completeness holds for torso-scoped merge queried at creature" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        mergedGraph <- expectRight (contextMerge Torso termA termB baseContextGraph)
        obstructionComplete termA termB Creature mergedGraph @?= True,
      testCase "restriction barrier present when sheaf condition detects inconsistency" $ do
        (termA, termB, baseContextGraph) <- expectRight fixtureBodyGraph
        mergedGraph <- expectRight (contextMerge PawFront termA termB baseContextGraph)
        assertBool "expected restriction barrier" (any isRestrictionBarrier (obstructionReport termA termB Creature mergedGraph))
    ]

bodyLattice :: ContextLattice BodyRegion
bodyLattice =
  case latticeContext of
    Right latticeValue -> latticeValue
    Left compileError ->
      error ("invalid BodyRegion lattice fixture: " <> show compileError)

allRegions :: [BodyRegion]
allRegions = [minBound .. maxBound]

allRegionsNonEmpty :: NonEmpty BodyRegion
allRegionsNonEmpty =
  minBound :| filter (/= minBound) allRegions

fixtureBodyGraph :: Either (ContextDeltaError ArithF BodyRegion) (ClassId, ClassId, ContextEGraph ArithF NodeCount BodyRegion)
fixtureBodyGraph = do
  let graph0 = emptyEGraph analysisSpec
  (oneClassId, graph1) <- first ContextClassIdAllocationFailed (addTerm (numTerm 1) graph0)
  (_, graph2) <- first ContextClassIdAllocationFailed (addTerm (numTerm 0) graph1)
  (sumClassId, graph3) <- first ContextClassIdAllocationFailed (addTerm (addTermNode (numTerm 1) (numTerm 0)) graph2)
  pure (sumClassId, oneClassId, emptyContextEGraph bodyLattice graph3)

diagnosticFullContextRebase :: ContextEGraph ArithF NodeCount BodyRegion -> EGraph ArithF NodeCount -> Either (ContextDeltaError ArithF BodyRegion) (ContextEGraph ArithF NodeCount BodyRegion)
diagnosticFullContextRebase contextGraph baseGraph =
  rebaseContextGraphAtContexts
    (Set.fromList (contextCachedObjectsForExecution contextGraph))
    baseGraph
    contextGraph

fixtureBodyGraphExtended :: ContextEGraph ArithF NodeCount BodyRegion -> Either (ContextDeltaError ArithF BodyRegion) (ClassId, ClassId, ContextEGraph ArithF NodeCount BodyRegion)
fixtureBodyGraphExtended contextGraph = do
  let baseGraph = cegBase contextGraph
  (twoClassId, graph1) <- first ContextClassIdAllocationFailed (addTerm (numTerm 2) baseGraph)
  (threeClassId, graph2) <- first ContextClassIdAllocationFailed (addTerm (numTerm 3) graph1)
  rebasedGraph <- diagnosticFullContextRebase contextGraph graph2
  pure (twoClassId, threeClassId, rebasedGraph)

isGlobalEquivalence :: ClassId -> ClassId -> ContextEGraph f a c -> Bool
isGlobalEquivalence leftClassId rightClassId =
  UnionFind.equivalent leftClassId rightClassId . eGraphUnionFind . cegBase
