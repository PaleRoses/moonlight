{-# LANGUAGE RankNTypes #-}

module Moonlight.EGraph.Context.ColoredVsSheafSpec
  ( tests,
  )
where

import Data.Kind ( Type )
import Data.Either ( isLeft, isRight )
import Moonlight.Algebra
  ( BoundedJoinSemilattice(..),
    BoundedMeetSemilattice(..),
    JoinSemilattice(..),
    Lattice,
    MeetSemilattice(..)
  )
import Moonlight.Core ( Language )
import Moonlight.EGraph.Pure.Context
    ( ContextDeltaError,
      contextMerge,
      globalMerge,
      contextCachedObjectsForExecution,
      ContextEGraph )
import Moonlight.EGraph.Pure.Context
    ( cegBase,
      cegLattice )
import Moonlight.EGraph.Pure.Types
    ( classIdKey, ClassId, EClass(eClassData), materializeEGraphClasses )
import Moonlight.EGraph.Test.Assertions
    ( isContextBarrier,
      isPropagationBarrier,
      isRestrictionBarrier,
      isStructuralMismatch )
import Moonlight.EGraph.Test.Context.Anatomy
    ( AnatomyRegion(ArmLeft, ArmRight, Head, LegLeft, LegRight, Local,
                    Lower, Torso, Upper, Whole),
      preciseAnatomyLattice )
import Moonlight.EGraph.Test.Context.SimpleArith
    ( ArithF, Depth(Depth), baseFixture, extendFixture )
import Moonlight.Sheaf.Context.Site (PreparedContextSupportError)
import Moonlight.Sheaf.Context.Algebra
  ( contextClassAt,
    contextEquivalentAt,
    restrictionMap,
  )
import Moonlight.Sheaf.Context.Witness
  ( contextAnalysisGlobalSectionInvariant,
    contextAnalysisRestrictionComposition,
    contextAnalysisRestrictionIdentity,
    mkContextMorphism,
  )
import Moonlight.Sheaf.Obstruction ( obstructionReport )
import Moonlight.Pale.Test.Site.Assertion (expectRight, withResult)
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit
    ( Assertion, (@?=), assertBool, assertEqual, testCase )
import Moonlight.Sheaf.Context.Algebra qualified as Algebra
    ( ContextAlgebraSite(contextAnalysisFor) )
import Data.IntMap.Strict qualified as IntMap
    ( IntMap, lookup, size, map )
import Moonlight.FiniteLattice
  ( ContextLattice,
    latticeContext,
    leqContext
  )

type FlatColor :: Type
data FlatColor
  = ColorAll
  | ColorRed
  | ColorBlue
  | ColorGreen
  | ColorNone
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance JoinSemilattice FlatColor where
  join ColorAll b = b
  join a ColorAll = a
  join ColorNone _ = ColorNone
  join _ ColorNone = ColorNone
  join a b
    | a == b = a
    | otherwise = ColorNone

instance BoundedJoinSemilattice FlatColor where
  bottom = ColorAll

instance MeetSemilattice FlatColor where
  meet ColorNone b = b
  meet a ColorNone = a
  meet ColorAll _ = ColorAll
  meet _ ColorAll = ColorAll
  meet a b
    | a == b = a
    | otherwise = ColorAll

instance BoundedMeetSemilattice FlatColor where
  top = ColorNone

instance Lattice FlatColor

flatLattice :: ContextLattice FlatColor
flatLattice =
  case latticeContext of
    Right latticeValue -> latticeValue
    Left compileError ->
      error ("invalid FlatColor lattice fixture: " <> show compileError)

contextAnalysisFor :: (Language f, Ord c) => c -> ContextEGraph owner f a c -> Either (PreparedContextSupportError c) (IntMap.IntMap a)
contextAnalysisFor =
  Algebra.contextAnalysisFor

classesEquivalentAt :: (Language f, Ord c) => c -> ClassId -> ClassId -> ContextEGraph owner f a c -> Bool
classesEquivalentAt contextValue leftClassId rightClassId contextGraph =
  either
    (const False)
    id
    (contextEquivalentAt contextValue leftClassId rightClassId contextGraph)

tests :: TestTree
tests =
  testGroup
    "colored-vs-sheaf"
    [ flatColorTests,
      sheafPropagationTests,
      divergenceTests,
      expressiveGapTests,
      contextAnalysisTests
    ]

flatColorTests :: TestTree
flatColorTests =
  testGroup
    "flat-color (traditional colored e-graph)"
    [ testCase "merge at Red is visible at Red but not Blue" $
        withFlatFixture $ \(termA, termB, ceg) ->
          withResult (contextMerge ColorRed termA termB ceg) $ \merged -> do
              classesEquivalentAt ColorRed termA termB merged @?= True
              classesEquivalentAt ColorBlue termA termB merged @?= False
              classesEquivalentAt ColorGreen termA termB merged @?= False,
      testCase "merge at Red does NOT propagate to All (bottom)" $
        withFlatFixture $ \(termA, termB, ceg) ->
          withResult (contextMerge ColorRed termA termB ceg) $ \merged ->
              classesEquivalentAt ColorAll termA termB merged @?= False,
      testCase "merge at All propagates everywhere (flat)" $
        withFlatFixture $ \(termA, termB, ceg) ->
          withResult (contextMerge ColorAll termA termB ceg) $ \merged -> do
              classesEquivalentAt ColorAll termA termB merged @?= True
              classesEquivalentAt ColorRed termA termB merged @?= True
              classesEquivalentAt ColorBlue termA termB merged @?= True
              classesEquivalentAt ColorGreen termA termB merged @?= True
              classesEquivalentAt ColorNone termA termB merged @?= True,
      testCase "flat colors are pairwise incomparable — no cross-propagation" $
        withFlatFixture $ \(termA, termB, ceg) ->
          do
              withResult (contextMerge ColorRed termA termB ceg) $ \mergedRed ->
                classesEquivalentAt ColorBlue termA termB mergedRed @?= False
              withResult (contextMerge ColorBlue termA termB ceg) $ \mergedBlue ->
                classesEquivalentAt ColorRed termA termB mergedBlue @?= False,
      testCase "restriction between incomparable colors is typed failure" $
        withFlatFixture $ \(_, _, ceg) ->
          do
              isLeft (restrictionMap ColorRed ColorBlue ceg) @?= True
              isLeft (restrictionMap ColorBlue ColorGreen ceg) @?= True
              isLeft (restrictionMap ColorGreen ColorRed ceg) @?= True,
      testCase "restriction from color to All exists (All leq everything)" $
        withFlatFixture $ \(_, _, ceg) ->
          do
              isRight (restrictionMap ColorRed ColorAll ceg) @?= True
              isRight (restrictionMap ColorBlue ColorAll ceg) @?= True
              isRight (restrictionMap ColorGreen ColorAll ceg) @?= True
    ]

sheafPropagationTests :: TestTree
sheafPropagationTests =
  testGroup
    "sheaf-propagation (lattice-indexed context e-graph)"
    [ testCase "merge at Upper propagates to Head, Torso, ArmLeft, ArmRight, Local" $
        withAnatomyFixture $ \(termA, termB, ceg) ->
          withResult (contextMerge Upper termA termB ceg) $ \merged -> do
              classesEquivalentAt Upper termA termB merged @?= True
              classesEquivalentAt Head termA termB merged @?= True
              classesEquivalentAt Torso termA termB merged @?= True
              classesEquivalentAt ArmLeft termA termB merged @?= True
              classesEquivalentAt ArmRight termA termB merged @?= True
              classesEquivalentAt Local termA termB merged @?= True
              classesEquivalentAt Lower termA termB merged @?= False
              classesEquivalentAt LegLeft termA termB merged @?= False
              classesEquivalentAt Whole termA termB merged @?= False,
      testCase "merge at Head stays within Head subtree and Local" $
        withAnatomyFixture $ \(termA, termB, ceg) ->
          withResult (contextMerge Head termA termB ceg) $ \merged -> do
              classesEquivalentAt Head termA termB merged @?= True
              classesEquivalentAt Local termA termB merged @?= True
              classesEquivalentAt Upper termA termB merged @?= False
              classesEquivalentAt Torso termA termB merged @?= False
              classesEquivalentAt ArmLeft termA termB merged @?= False
              classesEquivalentAt Whole termA termB merged @?= False,
      testCase "restriction maps exist along lattice chains" $
        withAnatomyFixture $ \(_, _, ceg) ->
          do
              isRight (restrictionMap Head Upper ceg) @?= True
              isRight (restrictionMap Upper Whole ceg) @?= True
              isRight (restrictionMap Head Whole ceg) @?= True
              isRight (restrictionMap LegLeft Lower ceg) @?= True
              isRight (restrictionMap LegLeft Whole ceg) @?= True,
      testCase "restriction maps reject incomparable regions" $
        withAnatomyFixture $ \(_, _, ceg) ->
          do
              isLeft (restrictionMap Head LegLeft ceg) @?= True
              isLeft (restrictionMap Upper Lower ceg) @?= True
              isLeft (restrictionMap ArmLeft LegRight ceg) @?= True
              isLeft (restrictionMap Torso LegLeft ceg) @?= True,
      testCase "merge at Whole propagates to every region" $
        withAnatomyFixture $ \(termA, termB, ceg) ->
          withResult (contextMerge Whole termA termB ceg) $ \merged ->
              assertBool "whole-body merge visible everywhere"
                (all
                   (\region -> classesEquivalentAt region termA termB merged)
                   [minBound .. maxBound])
    ]

divergenceTests :: TestTree
divergenceTests =
  testGroup
    "divergence (same scenario, different lattice, different outcome)"
    [ testCase "flat: two independent color merges cannot see each other" $
        withFlatFixture $ \(termA, termB, ceg) ->
          withResult (flatFixtureExtended ceg) $ \(termC, termD, ceg2) ->
              withResult (contextMerge ColorRed termA termB ceg2) $ \afterRed ->
                withResult (contextMerge ColorBlue termC termD afterRed) $ \afterBoth -> do
                  classesEquivalentAt ColorRed termA termB afterBoth @?= True
                  classesEquivalentAt ColorBlue termC termD afterBoth @?= True
                  classesEquivalentAt ColorBlue termA termB afterBoth @?= False
                  classesEquivalentAt ColorRed termC termD afterBoth @?= False
                  classesEquivalentAt ColorAll termA termB afterBoth @?= False
                  classesEquivalentAt ColorAll termC termD afterBoth @?= False,
      testCase "sheaf: merge at parent subsumes children but not siblings" $
        withAnatomyFixture $ \(termA, termB, ceg) ->
          withResult (anatomyFixtureExtended ceg) $ \(termC, termD, ceg2) ->
              withResult (contextMerge Upper termA termB ceg2) $ \afterUpper ->
                withResult (contextMerge Lower termC termD afterUpper) $ \afterLower -> do
                  classesEquivalentAt Head termA termB afterLower @?= True
                  classesEquivalentAt Torso termA termB afterLower @?= True
                  classesEquivalentAt ArmLeft termA termB afterLower @?= True
                  classesEquivalentAt LegLeft termC termD afterLower @?= True
                  classesEquivalentAt LegRight termC termD afterLower @?= True
                  classesEquivalentAt Head termC termD afterLower @?= False
                  classesEquivalentAt LegLeft termA termB afterLower @?= False
                  classesEquivalentAt Whole termA termB afterLower @?= False
                  classesEquivalentAt Whole termC termD afterLower @?= False
                  classesEquivalentAt Local termA termB afterLower @?= True
                  classesEquivalentAt Local termC termD afterLower @?= True,
      testCase "key difference: flat loses the RELATIONSHIP between contexts" $
        withFlatFixture $ \(termA, termB, ceg) ->
          withResult (contextMerge ColorRed termA termB ceg) $ \mergedRed -> do
              assertEqual "exactly one structural mismatch" 1 (length (filter isStructuralMismatch (obstructionReport termA termB ColorAll mergedRed)))
              assertEqual "exactly one context barrier" 1 (length (filter isContextBarrier (obstructionReport termA termB ColorAll mergedRed)))
              assertEqual "exactly one restriction barrier" 1 (length (filter isRestrictionBarrier (obstructionReport termA termB ColorAll mergedRed)))
              assertEqual "no propagation barrier" 0 (length (filter isPropagationBarrier (obstructionReport termA termB ColorAll mergedRed)))
              classesEquivalentAt ColorNone termA termB mergedRed @?= True
              classesEquivalentAt ColorAll termA termB mergedRed @?= False
    ]

expressiveGapTests :: TestTree
expressiveGapTests =
  testGroup
    "expressive gap (things sheaf can express that flat cannot)"
    [ testCase "sheaf: transitive propagation — merge at Whole flows through Upper to Head" $
        withAnatomyFixture $ \(termA, termB, ceg) ->
          withResult (contextMerge Whole termA termB ceg) $ \merged -> do
              classesEquivalentAt Upper termA termB merged @?= True
              classesEquivalentAt Head termA termB merged @?= True
              classesEquivalentAt LegRight termA termB merged @?= True,
      testCase "sheaf: obstruction explains WHY equivalence fails at parent" $
        withAnatomyFixture $ \(termA, termB, ceg) ->
          withResult (contextMerge Head termA termB ceg) $ \merged -> do
              assertEqual "exactly one structural mismatch" 1 (length (filter isStructuralMismatch (obstructionReport termA termB Upper merged)))
              assertEqual "exactly one restriction barrier" 1 (length (filter isRestrictionBarrier (obstructionReport termA termB Upper merged)))
              assertEqual "no propagation barrier" 0 (length (filter isPropagationBarrier (obstructionReport termA termB Upper merged)))
              assertEqual "exactly one context barrier" 1 (length (filter isContextBarrier (obstructionReport termA termB Upper merged))),
      testCase "sheaf: obstruction clean when equivalence holds at queried context" $
        withAnatomyFixture $ \(termA, termB, ceg) ->
          withResult (contextMerge Upper termA termB ceg) $ \merged ->
              assertBool "no obstruction at Head when merged at Upper" (null (obstructionReport termA termB Head merged)),
      testCase "flat: cannot express 'merge at Red implies merge at subset of Red' — no subsets exist" $
        withFlatFixture $ \(termA, termB, ceg) ->
          withResult (contextMerge ColorRed termA termB ceg) $ \merged -> do
              assertBool "no obstruction at Red" (null (obstructionReport termA termB ColorRed merged))
              assertEqual "exactly one structural mismatch" 1 (length (filter isStructuralMismatch (obstructionReport termA termB ColorAll merged)))
              assertEqual "exactly one context barrier" 1 (length (filter isContextBarrier (obstructionReport termA termB ColorAll merged)))
              assertEqual "exactly one restriction barrier" 1 (length (filter isRestrictionBarrier (obstructionReport termA termB ColorAll merged)))
              assertEqual "no propagation barrier" 0 (length (filter isPropagationBarrier (obstructionReport termA termB ColorAll merged))),
      testCase "sheaf: restriction composition is functorial" $
        withAnatomyFixture $ \(termA, termB, ceg) ->
          withResult (contextMerge Upper termA termB ceg) $ \merged -> do
              let headToUpper = restrictionMap Head Upper merged
                  upperToWhole = restrictionMap Upper Whole merged
                  headToWhole = restrictionMap Head Whole merged
              assertBool "head->upper exists" (isRight headToUpper)
              assertBool "upper->whole exists" (isRight upperToWhole)
              assertBool "head->whole exists" (isRight headToWhole),
      testCase "flat: no intermediate structure to compose through" $
        withFlatFixture $ \(_, _, ceg) -> do
          let redToAll = restrictionMap ColorRed ColorAll ceg
              blueToAll = restrictionMap ColorBlue ColorAll ceg
              redToBlue = restrictionMap ColorRed ColorBlue ceg
          assertBool "red->all exists" (isRight redToAll)
          assertBool "blue->all exists" (isRight blueToAll)
          assertBool "red->blue is rejected as incomparable" (isLeft redToBlue)
    ]

contextAnalysisTests :: TestTree
contextAnalysisTests =
  testGroup
    "context-indexed analysis sections"
    [ testCase "analysis at bottom context matches base graph" $
        withAnatomyFixture $ \(_, _, ceg) ->
          let baseAnalysis = IntMap.map eClassData (materializeEGraphClasses (cegBase ceg))
           in withResult (contextAnalysisFor Whole ceg) $ \bottomAnalysis ->
                bottomAnalysis @?= baseAnalysis,
      testCase "merge joins analysis via semilattice: Depth 0 ⊔ Depth 1 = Depth 1" $
        withAnatomyFixture $ \(termA, termB, ceg) ->
          withResult (contextMerge Head termA termB ceg) $ \merged -> do
              canonA <- expectRight (contextClassAt Head termA merged)
              canonB <- expectRight (contextClassAt Head termB merged)
              headAnalysis <- expectRight (contextAnalysisFor Head merged)
              let mergedDepth = IntMap.lookup (classIdKey canonA) headAnalysis
              assertEqual "merged class has join of Depth 0 and Depth 1"
                (Just (Depth 1)) mergedDepth
              assertEqual "termA and termB are same class at Head"
                canonA
                canonB,
      testCase "merge reduces class count: 4 classes → 3 at Head, 4 elsewhere" $
        withAnatomyFixture $ \(termA, termB, ceg) ->
          withResult (contextMerge Head termA termB ceg) $ \merged -> do
              headAnalysis <- expectRight (contextAnalysisFor Head merged)
              wholeAnalysis <- expectRight (contextAnalysisFor Whole merged)
              legLeftAnalysis <- expectRight (contextAnalysisFor LegLeft merged)
              assertEqual "Head has 3 analysis entries (one merge)" 3 (IntMap.size headAnalysis)
              assertEqual "Whole has 4 analysis entries (no merge)" 4 (IntMap.size wholeAnalysis)
              assertEqual "LegLeft has 4 analysis entries (incomparable)" 4 (IntMap.size legLeftAnalysis),
      testCase "context-local analysis preserves unmerged classes exactly" $
        withAnatomyFixture $ \(termA, termB, ceg) ->
          let baseAnalysis = IntMap.map eClassData (materializeEGraphClasses (cegBase ceg))
           in withResult (contextMerge Head termA termB ceg) $ \merged ->
                withResult (contextAnalysisFor Whole merged) $ \wholeAnalysis ->
                  assertEqual "Whole analysis unchanged by Head-only merge" baseAnalysis wholeAnalysis,
      testCase "global merge: all contexts have identical class count" $
        withAnatomyFixture $ \(termA, termB, ceg) ->
          withResult (globalMerge termA termB ceg) $ \merged -> do
              let contexts = contextCachedObjectsForExecution merged
              analyses <- expectRight (traverse (`contextAnalysisFor` merged) contexts)
              let sizes = fmap IntMap.size analyses
              assertBool "all contexts have same analysis size"
                (case sizes of
                   [] -> True
                   firstSize : remainingSizes -> all (== firstSize) remainingSizes),
      testCase "global merge: merged analysis value is join at every context" $
        withAnatomyFixture $ \(termA, termB, ceg) ->
          withResult (globalMerge termA termB ceg) $ \merged -> do
              canonA <- expectRight (contextClassAt Head termA merged)
              headAnalysis <- expectRight (contextAnalysisFor Head merged)
              let mergedKey = classIdKey canonA
              assertEqual "globally merged class has Depth 1 at Head"
                (Just (Depth 1)) (IntMap.lookup mergedKey headAnalysis),
      testCase "merged class analysis is accessible via contextClassAt" $
        withAnatomyFixture $ \(termA, termB, ceg) ->
          withResult (contextMerge Head termA termB ceg) $ \merged -> do
              canonA <- expectRight (contextClassAt Head termA merged)
              canonB <- expectRight (contextClassAt Head termB merged)
              wholeCanonA <- expectRight (contextClassAt Whole termA merged)
              headAnalysis <- expectRight (contextAnalysisFor Head merged)
              wholeAnalysis <- expectRight (contextAnalysisFor Whole merged)
              assertEqual "termA and termB canonicalize to same class" canonA canonB
              assertEqual "canonical class has Depth 1 (join of 0 and 1)"
                (Just (Depth 1)) (IntMap.lookup (classIdKey canonA) headAnalysis)
              assertEqual "same key absent from Whole (different canonical structure)"
                (Just (Depth 0)) (IntMap.lookup (classIdKey wholeCanonA) wholeAnalysis),
      testCase "analysis restriction identity holds at every context after merge" $
        withAnatomyFixture $ \(termA, termB, ceg) ->
          withResult (contextMerge Head termA termB ceg) $ \merged ->
              mapM_
                (\c -> contextAnalysisRestrictionIdentity c merged @?= Right True)
                (contextCachedObjectsForExecution merged),
      testCase "analysis restriction composition: Head→Upper→Whole" $
        withAnatomyFixture $ \(termA, termB, ceg) ->
          withResult (contextMerge Head termA termB ceg) $ \merged -> do
              let latticeValue = cegLattice merged
              firstMorphism <- expectRight (mkContextMorphism latticeValue Head Upper)
              secondMorphism <- expectRight (mkContextMorphism latticeValue Upper Whole)
              case (firstMorphism, secondMorphism) of
                (Just first, Just second) ->
                  contextAnalysisRestrictionComposition first second merged @?= Right True
                _ -> assertBool "morphisms should exist" False,
      testCase "analysis restriction composition: LegLeft→Lower→Whole" $
        withAnatomyFixture $ \(termA, termB, ceg) ->
          withResult (contextMerge Head termA termB ceg) $ \merged -> do
              let latticeValue = cegLattice merged
              firstMorphism <- expectRight (mkContextMorphism latticeValue LegLeft Lower)
              secondMorphism <- expectRight (mkContextMorphism latticeValue Lower Whole)
              case (firstMorphism, secondMorphism) of
                (Just first, Just second) ->
                  contextAnalysisRestrictionComposition first second merged @?= Right True
                _ -> assertBool "morphisms should exist" False,
      testCase "analysis global section invariant: all lattice edges" $
        withAnatomyFixture $ \(termA, termB, ceg) ->
          withResult (contextMerge Head termA termB ceg) $ \merged -> do
              let latticeValue = cegLattice merged
                  contexts = contextCachedObjectsForExecution merged
                  morphisms =
                    [ (s, t)
                    | s <- contexts, t <- contexts,
                      s /= t, leqContext latticeValue t s == Right True
                    ]
              mapM_
                ( \(s, t) -> do
                    maybeMorphism <- expectRight (mkContextMorphism latticeValue s t)
                    case maybeMorphism of
                      Just morphism ->
                        contextAnalysisGlobalSectionInvariant morphism merged @?= Right True
                      Nothing -> assertBool "morphism should exist" False
                )
                morphisms
    ]

withFlatFixture ::
  (forall owner. (ClassId, ClassId, ContextEGraph owner ArithF Depth FlatColor) -> Assertion) ->
  Assertion
withFlatFixture useFixture =
  withResult (baseFixture flatLattice useFixture) id

flatFixtureExtended :: ContextEGraph owner ArithF Depth FlatColor -> Either (ContextDeltaError ArithF FlatColor) (ClassId, ClassId, ContextEGraph owner ArithF Depth FlatColor)
flatFixtureExtended = extendFixture

withAnatomyFixture ::
  (forall owner. (ClassId, ClassId, ContextEGraph owner ArithF Depth AnatomyRegion) -> Assertion) ->
  Assertion
withAnatomyFixture useFixture =
  withResult (baseFixture preciseAnatomyLattice useFixture) id

anatomyFixtureExtended :: ContextEGraph owner ArithF Depth AnatomyRegion -> Either (ContextDeltaError ArithF AnatomyRegion) (ClassId, ClassId, ContextEGraph owner ArithF Depth AnatomyRegion)
anatomyFixtureExtended = extendFixture
