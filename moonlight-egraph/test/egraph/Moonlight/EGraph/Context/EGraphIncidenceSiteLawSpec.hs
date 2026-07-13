{-# LANGUAGE TypeApplications #-}

module Moonlight.EGraph.Context.EGraphIncidenceSiteLawSpec
  ( tests,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (find)
import Moonlight.Category
  ( Category (..),
    FiniteComposableCategory (..),
    composeMor,
  )
import Moonlight.Core qualified as UnionFind
import Moonlight.EGraph.Pure.Context
  ( ContextDeltaError (..),
    ContextEGraph,
    contextMerge,
    emptyContextEGraph,
    materializeIncidenceCategoryFromSnapshot,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    ENode (..),
    emptyEGraph,
  )
import Moonlight.EGraph.Sheaf.IncidenceSite
  ( EGraphIncidenceArrow (..),
    EGraphIncidenceCategory,
    EGraphIncidenceCategoryError,
    EGraphIncidenceMorphism,
    eimSource,
    eimTarget,
    eimWitness,
    EGraphIncidenceObject (..),
    egraphIncidenceCategoryFromSnapshot,
    egraphIncidenceNerveSite,
    incidenceClassRepresentative,
    incidenceCategoryStructuralMorphisms,
    structuralPathSource,
    structuralPathTarget,
  )
import Moonlight.EGraph.Test.Arith.Core
  ( ArithF (..),
    NodeCount (..),
    addTermNode,
    analysisSpec,
    numTerm,
  )
import Moonlight.EGraph.Test.Context.ThreeLevel (Scope (..))
import Moonlight.Sheaf.Context.Algebra (classesFor)
import Moonlight.Sheaf.Site
  ( nerveSiteCells,
    siteCellsAtDimension,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck qualified as QC
import Moonlight.FiniteLattice
  ( ContextLattice,
    latticeContext
  )

class0, class1, class2, class3 :: ClassId
class0 = ClassId 0
class1 = ClassId 1
class2 = ClassId 2
class3 = ClassId 3

addNode, mulNode, negNode :: ENode ArithF
addNode = ENode (Add class0 class1)
mulNode = ENode (Mul class0 class1)
negNode = ENode (Neg class2)

tests :: TestTree
tests =
  testGroup
    "EGraphIncidenceCategory"
    [ testCase "identity laws hold over enumerated incidence morphisms" identityLawTest,
      testCase "structural composition builds path morphisms" structuralPathCompositionTest,
      testCase "composition is associative over the finite incidence category" associativityLawTest,
      testCase "mkNerveSite exposes class objects and incidence arrows as nerve cells" incidenceNerveCellsTest,
      QC.testProperty "PerContextUnionFindIsolation compares on-demand incidence categories to direct context graphs" perContextUnionFindIsolationProperty
    ]

identityLawTest :: Assertion
identityLawTest = do
  categoryValue <- expectRight (nestedCategory [])
  let morphisms = enumerateMorphisms categoryValue
      leftIdentityHolds morphismValue =
        case identity categoryValue (eimTarget morphismValue) of
          Right identityMorphism ->
            case composeMor categoryValue identityMorphism morphismValue of
              Right compositeMorphism ->
                compositeMorphism == morphismValue
              Left _ ->
                False
          Left _ ->
            False
      rightIdentityHolds morphismValue =
        case identity categoryValue (eimSource morphismValue) of
          Right identityMorphism ->
            case composeMor categoryValue morphismValue identityMorphism of
              Right compositeMorphism ->
                compositeMorphism == morphismValue
              Left _ ->
                False
          Left _ ->
            False
  assertBool "left identity failed" (all leftIdentityHolds morphisms)
  assertBool "right identity failed" (all rightIdentityHolds morphisms)

structuralPathCompositionTest :: Assertion
structuralPathCompositionTest = do
  categoryValue <- expectRight (nestedCategory [])
  outerMorphism <-
    expectJust
      "expected class2-to-class3 incidence morphism"
      (findStructuralMorphism (IncidenceClassObject class2) (IncidenceClassObject class3) categoryValue)
  innerMorphism <-
    expectJust
      "expected class0-to-class2 incidence morphism"
      (findStructuralMorphism (IncidenceClassObject class0) (IncidenceClassObject class2) categoryValue)
  case compose categoryValue outerMorphism innerMorphism of
    Right (compositeMorphism, _compositor) -> do
      eimSource compositeMorphism @?= eimSource innerMorphism
      eimTarget compositeMorphism @?= eimTarget outerMorphism
      case eimWitness compositeMorphism of
        IncidenceStructuralPathArrow witnesses -> do
          structuralPathSource witnesses @?= IncidenceClassObject class0
          structuralPathTarget witnesses @?= IncidenceClassObject class3
        otherWitness ->
          assertFailure ("expected structural path, got " <> show otherWitness)
    Left errorValue ->
      assertFailure ("expected structural incidence composition, got " <> show errorValue)

associativityLawTest :: Assertion
associativityLawTest = do
  categoryValue <- expectRight (nestedCategory [])
  let morphisms = enumerateMorphisms categoryValue
      triples = composableTriples morphisms
      associativityHolds (h, g, f) =
        (composeOnly categoryValue h g >>= \hg -> composeOnly categoryValue hg f)
          == (composeOnly categoryValue g f >>= \gf -> composeOnly categoryValue h gf)
  assertBool "expected at least one composable incidence triple" (not (null triples))
  assertBool "associativity failed" (all associativityHolds triples)

incidenceNerveCellsTest :: Assertion
incidenceNerveCellsTest = do
  categoryValue <- expectRight (nestedCategory [])
  let siteValue = egraphIncidenceNerveSite categoryValue 2
  length (siteCellsAtDimension siteValue 0) @?= 5
  assertBool "expected primitive incidence 1-cells" (length (siteCellsAtDimension siteValue 1) >= 5)
  assertBool "expected composed incidence 2-cells" (not (null (siteCellsAtDimension siteValue 2)))
  assertBool "expected materialized generic nerve cells" (not (null (nerveSiteCells siteValue)))

composeOnly ::
  EGraphIncidenceCategory ArithF ->
  EGraphIncidenceMorphism ArithF ->
  EGraphIncidenceMorphism ArithF ->
  Maybe (EGraphIncidenceMorphism ArithF)
composeOnly categoryValue leftMorphism rightMorphism =
  either (const Nothing) (Just . fst) $
    compose categoryValue leftMorphism rightMorphism

composableTriples ::
  [EGraphIncidenceMorphism ArithF] ->
  [(EGraphIncidenceMorphism ArithF, EGraphIncidenceMorphism ArithF, EGraphIncidenceMorphism ArithF)]
composableTriples morphisms =
  [ (h, g, f)
    | f <- morphisms,
      g <- morphisms,
      h <- morphisms,
      eimTarget f == eimSource g,
      eimTarget g == eimSource h
  ]

findStructuralMorphism ::
  EGraphIncidenceObject ArithF ->
  EGraphIncidenceObject ArithF ->
  EGraphIncidenceCategory ArithF ->
  Maybe (EGraphIncidenceMorphism ArithF)
findStructuralMorphism sourceObject targetObject =
  find
    ( \morphismValue ->
        eimSource morphismValue == sourceObject
          && eimTarget morphismValue == targetObject
    )
    . incidenceCategoryStructuralMorphisms

nestedCategory :: [ENode ArithF] -> Either (EGraphIncidenceCategoryError ArithF) (EGraphIncidenceCategory ArithF)
nestedCategory extraClassTwoNodes =
  egraphIncidenceCategoryFromSnapshot
    UnionFind.emptyUnionFind
    ( IntMap.fromList
        [ (0, []),
          (1, []),
          (2, addNode : extraClassTwoNodes),
          (3, [negNode]),
          (4, [mulNode])
        ]
    )

perContextUnionFindIsolationProperty :: [Scope] -> QC.Property
perContextUnionFindIsolationProperty scopes =
  case fixtureContext of
    Left contextError -> QC.counterexample (show contextError) False
    Right (sumClassId, oneClassId, contextGraph0) ->
      case foldM (applyScopeMerge sumClassId oneClassId) contextGraph0 (take 24 scopes) of
        Left contextError -> QC.counterexample (show contextError) False
        Right contextGraph ->
          QC.conjoin (fmap (contextCanonicalMapsAgree contextGraph) [GlobalCtx, ModuleCtx, LocalCtx])

contextCanonicalMapsAgree :: ContextEGraph ArithF NodeCount Scope -> Scope -> QC.Property
contextCanonicalMapsAgree contextGraph scope =
  case
      ( materializeIncidenceCategoryFromSnapshot scope contextGraph,
        classesFor scope contextGraph
      )
    of
      (Left siteError, _) -> QC.counterexample (show siteError) False
      (_, Left supportError) -> QC.counterexample (show supportError) False
      (Right categoryValue, Right directCanonical) ->
        let visibleKeys = IntMap.keysSet directCanonical
            incidenceCanonical = incidenceRepresentativeMap visibleKeys categoryValue
         in QC.counterexample (show (scope, directCanonical, incidenceCanonical)) (Right directCanonical == incidenceCanonical)

applyScopeMerge :: ClassId -> ClassId -> ContextEGraph ArithF NodeCount Scope -> Scope -> Either (ContextDeltaError ArithF Scope) (ContextEGraph ArithF NodeCount Scope)
applyScopeMerge sumClassId oneClassId contextGraph scope =
  contextMerge scope sumClassId oneClassId contextGraph

incidenceRepresentativeMap ::
  IntSet.IntSet ->
  EGraphIncidenceCategory ArithF ->
  Either (EGraphIncidenceCategoryError ArithF) (IntMap ClassId)
incidenceRepresentativeMap visibleKeys categoryValue =
  IntMap.fromAscList
    <$> traverse
      ( \classKey ->
          (,) classKey
            <$> incidenceClassRepresentative (ClassId classKey) categoryValue
      )
      (IntSet.toAscList visibleKeys)

fixtureContext :: Either (ContextDeltaError ArithF Scope) (ClassId, ClassId, ContextEGraph ArithF NodeCount Scope)
fixtureContext = do
  let graph0 = emptyEGraph analysisSpec
  (oneClassId, graph1) <- first ContextClassIdAllocationFailed (addTerm (numTerm 1) graph0)
  (_, graph2) <- first ContextClassIdAllocationFailed (addTerm (numTerm 0) graph1)
  (sumClassId, graph3) <- first ContextClassIdAllocationFailed (addTerm (addTermNode (numTerm 1) (numTerm 0)) graph2)
  pure (sumClassId, oneClassId, emptyContextEGraph incidenceScopeLattice graph3)

incidenceScopeLattice :: ContextLattice Scope
incidenceScopeLattice =
  case latticeContext of
    Right latticeValue -> latticeValue
    Left compileError ->
      error ("invalid incidence Scope lattice fixture: " <> show compileError)

expectRight :: Show error => Either error value -> IO value
expectRight result =
  case result of
    Right value -> pure value
    Left errorValue -> assertFailure (show errorValue)

expectJust :: String -> Maybe value -> IO value
expectJust failureMessage result =
  case result of
    Just value -> pure value
    Nothing -> assertFailure failureMessage
