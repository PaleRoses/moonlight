{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Homology.Examples.H1CyclicGroupSpec
  ( tests,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.Cosheaf.Chain
  ( CosheafChainCell,
    PreparedFiniteCosheafChain,
    cosheafChainBasisIndexOf,
    cosheafChainCellKey,
    cosheafChainCellNerveChain,
    cosheafNerveChainMorphisms,
  )
import Moonlight.Cosheaf.Homology
  ( cosheafIntegralHomology,
    liftCosheafRepresentative,
  )
import Moonlight.Cosheaf.Test.Fixture.ConstantSingleton
  ( SingletonCostalk,
    constantSingletonCosheaf,
  )
import Moonlight.Cosheaf.Test.Fixture.Representative
  ( RepresentativeBuildFailure,
    boundaryOfRepresentative,
    findUniqueCellAtDegree,
    liftedWitnessSupportKeys,
    representativeFromCells,
    representativeVector,
  )
import Moonlight.Cosheaf.Test.Homology.Expect
  ( expectRight,
    shouldHaveCellCounts,
    shouldHaveHomologyGroup,
  )
import Moonlight.Cosheaf.Test.Homology.Performance
  ( HomologyExampleCounterExpectations (..),
    TimedAction (..),
    assertHomologyExampleCounters,
    homologyExampleCounters,
    planCellTotal,
    timedActionWith,
    witnessSupportSize,
  )
import Moonlight.Cosheaf.Test.Support
  ( prepareFullFiniteCosheafChain,
  )
import Moonlight.Cosheaf.Test.Site.CyclicGroup
  ( CyclicGroupMorphism (..),
    CyclicGroupObject,
    CyclicGroupSite,
    cyclicGroupSite,
  )
import Moonlight.Homology
  ( HomologicalDegree (..)
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )

oneDegree :: HomologicalDegree
oneDegree =
  HomologicalDegree 1

twoDegree :: HomologicalDegree
twoDegree =
  HomologicalDegree 2

tests :: TestTree
tests =
  testGroup
    "cyclic-group cosheaf H1 torsion examples"
    [ testCase "C2 has d2(g,g)=2g and H1 torsion Z/2" testC2Torsion,
      testCase "C3 retains nonidentity inner composites and H1 torsion Z/3" testC3Torsion
    ]

testC2Torsion :: Assertion
testC2Torsion = do
  timedPlan <- timedActionWith (planCellTotal [0, 1, 2]) (prepareCyclicPlan 2 2)
  let plan = timedActionValue timedPlan
  shouldHaveCellCounts plan [(0, 1), (1, 1), (2, 1)]
  timedGroups <- timedActionWith length (expectRight (cosheafIntegralHomology plan))
  let groups = timedActionValue timedGroups
  shouldHaveHomologyGroup groups 0 1 []
  shouldHaveHomologyGroup groups 1 0 [2]
  shouldHaveHomologyGroup groups 2 0 []
  g <- expectRight (cellByExponents oneDegree [1] plan)
  gg <- expectRight (cellByExponents twoDegree [1, 1] plan)
  assertBoundaryEquals
    "d(g,g) = 2g"
    plan
    twoDegree
    [(1, gg)]
    oneDegree
    [(2, g)]
  gRepresentative <- expectRight (representativeFromCells oneDegree [(1, g)] plan)
  assertEqual
    "d(g) is zero in the one-object category"
    Map.empty
    (boundaryOfRepresentative plan gRepresentative)
  timedWitness <- timedActionWith witnessSupportSize (expectRight (liftCosheafRepresentative oneDegree plan gRepresentative))
  let witness = timedActionValue timedWitness
  assertEqual
    "torsion generator lifts to the one nonidentity loop"
    [(1, cosheafChainCellKey g)]
    (liftedWitnessSupportKeys witness)
  assertHomologyExampleCounters
    "C2 cyclic-group homology counters"
    c2CounterExpectations
    (homologyExampleCounters [0, 1, 2] timedPlan timedGroups timedWitness)

testC3Torsion :: Assertion
testC3Torsion = do
  timedPlan <- timedActionWith (planCellTotal [0, 1, 2]) (prepareCyclicPlan 3 2)
  let plan = timedActionValue timedPlan
  shouldHaveCellCounts plan [(0, 1), (1, 2), (2, 4)]
  timedHomologyPlan <- timedActionWith (planCellTotal [0, 1, 2, 3]) (prepareCyclicPlan 3 3)
  timedGroups <- timedActionWith length (expectRight (cosheafIntegralHomology (timedActionValue timedHomologyPlan)))
  let groups = timedActionValue timedGroups
  shouldHaveHomologyGroup groups 0 1 []
  shouldHaveHomologyGroup groups 1 0 [3]
  shouldHaveHomologyGroup groups 2 0 []
  a <- expectRight (cellByExponents oneDegree [1] plan)
  a2 <- expectRight (cellByExponents oneDegree [2] plan)
  aa <- expectRight (cellByExponents twoDegree [1, 1] plan)
  aa2 <- expectRight (cellByExponents twoDegree [1, 2] plan)
  a2a <- expectRight (cellByExponents twoDegree [2, 1] plan)
  a2a2 <- expectRight (cellByExponents twoDegree [2, 2] plan)
  assertBoundaryEquals "d(a,a) = 2a - a²" plan twoDegree [(1, aa)] oneDegree [(2, a), (-1, a2)]
  assertBoundaryEquals "d(a,a²) = a + a²" plan twoDegree [(1, aa2)] oneDegree [(1, a), (1, a2)]
  assertBoundaryEquals "d(a²,a) = a + a²" plan twoDegree [(1, a2a)] oneDegree [(1, a), (1, a2)]
  assertBoundaryEquals "d(a²,a²) = -a + 2a²" plan twoDegree [(1, a2a2)] oneDegree [(-1, a), (2, a2)]
  a2Index <- expectCellIndex oneDegree a2 plan
  aaRepresentative <- expectRight (representativeFromCells twoDegree [(1, aa)] plan)
  assertEqual
    "the (a,a) inner face keeps the nonidentity composite a²"
    (Just (-1))
    (Map.lookup a2Index (boundaryOfRepresentative plan aaRepresentative))
  aRepresentative <- expectRight (representativeFromCells oneDegree [(1, a)] plan)
  timedWitness <- timedActionWith witnessSupportSize (expectRight (liftCosheafRepresentative oneDegree plan aRepresentative))
  let witness = timedActionValue timedWitness
  assertEqual
    "chosen cyclic H1 representative lifts through the cosheaf basis table"
    [(1, cosheafChainCellKey a)]
    (liftedWitnessSupportKeys witness)
  assertHomologyExampleCounters
    "C3 cyclic-group homology counters"
    c3CounterExpectations
    (homologyExampleCounters [0, 1, 2] timedPlan timedGroups timedWitness)

prepareCyclicPlan :: Int -> Int -> IO (PreparedFiniteCosheafChain CyclicGroupSite SingletonCostalk)
prepareCyclicPlan orderValue maxDegreeValue = do
  site <- expectRight (cyclicGroupSite orderValue)
  cosheaf <- expectRight (constantSingletonCosheaf site)
  expectRight (prepareFullFiniteCosheafChain (fromIntegral maxDegreeValue) cosheaf)

cellByExponents ::
  HomologicalDegree ->
  [Int] ->
  PreparedFiniteCosheafChain CyclicGroupSite SingletonCostalk ->
  Either
    (RepresentativeBuildFailure CyclicGroupObject CyclicGroupMorphism SingletonCostalk)
    (CosheafChainCell CyclicGroupObject CyclicGroupMorphism SingletonCostalk)
cellByExponents degreeValue expectedExponents =
  findUniqueCellAtDegree
    degreeValue
    ("cyclic cell " <> show expectedExponents)
    (cellHasExponents expectedExponents)

cellHasExponents :: [Int] -> CosheafChainCell CyclicGroupObject CyclicGroupMorphism value -> Bool
cellHasExponents expectedExponents cellValue =
  fmap (cyclicGroupMorphismExponent . cmWitness) (cosheafNerveChainMorphisms (cosheafChainCellNerveChain cellValue))
    == expectedExponents

assertBoundaryEquals ::
  String ->
  PreparedFiniteCosheafChain CyclicGroupSite SingletonCostalk ->
  HomologicalDegree ->
  [(Integer, CosheafChainCell CyclicGroupObject CyclicGroupMorphism SingletonCostalk)] ->
  HomologicalDegree ->
  [(Integer, CosheafChainCell CyclicGroupObject CyclicGroupMorphism SingletonCostalk)] ->
  Assertion
assertBoundaryEquals label plan sourceDegree sourceCells targetDegree expectedCells = do
  sourceRepresentative <- expectRight (representativeFromCells sourceDegree sourceCells plan)
  expectedRepresentative <- expectRight (representativeFromCells targetDegree expectedCells plan)
  assertEqual label (representativeVector expectedRepresentative) (boundaryOfRepresentative plan sourceRepresentative)

expectCellIndex ::
  HomologicalDegree ->
  CosheafChainCell CyclicGroupObject CyclicGroupMorphism SingletonCostalk ->
  PreparedFiniteCosheafChain CyclicGroupSite SingletonCostalk ->
  IO Int
expectCellIndex degreeValue cellValue plan =
  expectMaybe
    ("cell index at degree " <> show degreeValue <> " for " <> show (cosheafChainCellKey cellValue))
    (cosheafChainBasisIndexOf degreeValue (cosheafChainCellKey cellValue) plan)

expectMaybe :: String -> Maybe value -> IO value
expectMaybe label result =
  case result of
    Just value ->
      pure value
    Nothing ->
      assertFailure ("expected Just: " <> label)

c2CounterExpectations :: HomologyExampleCounterExpectations
c2CounterExpectations =
  HomologyExampleCounterExpectations
    { expectedObjectCount = 1,
      expectedNonidentityMorphismCount = 1,
      expectedCellsByDegree = [(0, 1), (1, 1), (2, 1)],
      expectedBoundaryNonzerosByDegree = [(0, 0), (1, 0), (2, 1)],
      expectedRepresentativeSupportSize = 1
    }

c3CounterExpectations :: HomologyExampleCounterExpectations
c3CounterExpectations =
  HomologyExampleCounterExpectations
    { expectedObjectCount = 1,
      expectedNonidentityMorphismCount = 2,
      expectedCellsByDegree = [(0, 1), (1, 2), (2, 4)],
      expectedBoundaryNonzerosByDegree = [(0, 0), (1, 0), (2, 8)],
      expectedRepresentativeSupportSize = 1
    }
