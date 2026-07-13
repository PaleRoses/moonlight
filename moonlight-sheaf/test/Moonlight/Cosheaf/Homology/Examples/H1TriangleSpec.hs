{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Homology.Examples.H1TriangleSpec
  ( tests,
  )
where

import Data.List (sort)
import Data.Maybe (mapMaybe)
import Moonlight.Cosheaf.Chain
  ( CosheafChainBasisKey,
    CosheafChainCell,
    PreparedFiniteCosheafChain,
    cosheafChainCellCostalkKey,
    cosheafChainCellKey,
    verifyCosheafBoundaryNilpotence,
  )
import Moonlight.Cosheaf.Finite
  ( CostalkKey (..),
  )
import Moonlight.Cosheaf.Homology
  ( LiftedCosheafChainTerm (..),
    chwRepresentativeTerms,
    cosheafIntegralHomology,
    liftCosheafRepresentative,
  )
import Moonlight.Cosheaf.Test.Fixture.ConstantSingleton
  ( SingletonCostalk,
    constantSingletonCosheaf,
  )
import Moonlight.Cosheaf.Test.Fixture.Representative
  ( RepresentativeBuildFailure,
    assertRepresentativeCycle,
    chainCellObjectPath,
    findUniqueCellAtDegree,
    liftedWitnessCellPaths,
    liftedWitnessSupportKeys,
    representativeFromCells,
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
import Moonlight.Cosheaf.Test.Site.FacePoset
  ( Face,
    FaceInclusion,
    FacePosetSite,
    faceEdge,
    facePosetBoundarySite,
    faceVertex,
  )
import Moonlight.Homology
  ( HomologicalDegree (..)
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    testCase,
  )

oneDegree :: HomologicalDegree
oneDegree =
  HomologicalDegree 1

tests :: TestTree
tests =
  testGroup
    "triangle-boundary cosheaf H1 free generator"
    [ testCase "six semantic one-flags lift to a real H1 cycle" testTriangleBoundaryGenerator
    ]

testTriangleBoundaryGenerator :: Assertion
testTriangleBoundaryGenerator = do
  timedPlan <- timedActionWith (planCellTotal [0, 1, 2]) prepareTrianglePlan
  let plan = timedActionValue timedPlan
  shouldHaveCellCounts plan [(0, 6), (1, 6), (2, 0)]
  expectRight (verifyCosheafBoundaryNilpotence plan)
  timedGroups <- timedActionWith length (expectRight (cosheafIntegralHomology plan))
  let groups = timedActionValue timedGroups
  shouldHaveHomologyGroup groups 0 1 []
  shouldHaveHomologyGroup groups 1 1 []
  shouldHaveHomologyGroup groups 2 0 []
  cycleCells <- traverse (resolvePathTerm plan) triangleBoundaryCycleTerms
  representative <- expectRight (representativeFromCells oneDegree cycleCells plan)
  expectRight (assertRepresentativeCycle plan representative)
  timedWitness <- timedActionWith witnessSupportSize (expectRight (liftCosheafRepresentative oneDegree plan representative))
  let witness = timedActionValue timedWitness
  assertEqual
    "lifted support matches the six semantic triangle-boundary flags"
    (sort (fmap supportKeyOfTerm cycleCells))
    (sort (liftedWitnessSupportKeys witness))
  let liftedPaths = fmap snd (liftedWitnessCellPaths witness)
  assertEqual
    "lifted source objects are exactly the triangle vertices, with incidence multiplicity"
    (sort [faceVertex 0, faceVertex 0, faceVertex 1, faceVertex 1, faceVertex 2, faceVertex 2])
    (sort (mapMaybe oneFlagSource liftedPaths))
  assertEqual
    "lifted terminal objects are exactly the triangle edges, with incidence multiplicity"
    (sort [faceEdge 0 1, faceEdge 0 1, faceEdge 1 2, faceEdge 1 2, faceEdge 0 2, faceEdge 0 2])
    (sort (mapMaybe oneFlagTerminal liftedPaths))
  assertEqual
    "constant-singleton fixture leaves no costalk-key ambiguity"
    (replicate 6 (CostalkKey 0))
    (fmap (cosheafChainCellCostalkKey . lcctCell) (chwRepresentativeTerms witness))
  assertHomologyExampleCounters
    "triangle-boundary homology counters"
    triangleCounterExpectations
    (homologyExampleCounters [0, 1, 2] timedPlan timedGroups timedWitness)

prepareTrianglePlan :: IO (PreparedFiniteCosheafChain FacePosetSite SingletonCostalk)
prepareTrianglePlan = do
  site <- expectRight (facePosetBoundarySite 3)
  cosheaf <- expectRight (constantSingletonCosheaf site)
  expectRight (prepareFullFiniteCosheafChain 2 cosheaf)

triangleBoundaryCycleTerms :: [(Integer, [Face])]
triangleBoundaryCycleTerms =
  [ (1, [faceVertex 0, faceEdge 0 1]),
    (-1, [faceVertex 1, faceEdge 0 1]),
    (1, [faceVertex 1, faceEdge 1 2]),
    (-1, [faceVertex 2, faceEdge 1 2]),
    (1, [faceVertex 2, faceEdge 0 2]),
    (-1, [faceVertex 0, faceEdge 0 2])
  ]

resolvePathTerm ::
  PreparedFiniteCosheafChain FacePosetSite SingletonCostalk ->
  (Integer, [Face]) ->
  IO (Integer, CosheafChainCell Face FaceInclusion SingletonCostalk)
resolvePathTerm plan (coefficientValue, pathValue) = do
  cell <- expectRight (flagCellAt oneDegree pathValue plan)
  pure (coefficientValue, cell)

flagCellAt ::
  HomologicalDegree ->
  [Face] ->
  PreparedFiniteCosheafChain FacePosetSite SingletonCostalk ->
  Either
    (RepresentativeBuildFailure Face FaceInclusion SingletonCostalk)
    (CosheafChainCell Face FaceInclusion SingletonCostalk)
flagCellAt degreeValue pathValue =
  findUniqueCellAtDegree
    degreeValue
    ("face flag " <> show pathValue)
    ((== pathValue) . chainCellObjectPath)

supportKeyOfTerm :: (Integer, CosheafChainCell Face FaceInclusion SingletonCostalk) -> (Integer, CosheafChainBasisKey)
supportKeyOfTerm (coefficientValue, cellValue) =
  (coefficientValue, cosheafChainCellKey cellValue)

oneFlagSource :: [Face] -> Maybe Face
oneFlagSource pathValue =
  case pathValue of
    sourceFace : _targetFace : [] ->
      Just sourceFace
    _ ->
      Nothing

oneFlagTerminal :: [Face] -> Maybe Face
oneFlagTerminal pathValue =
  case pathValue of
    _sourceFace : targetFace : [] ->
      Just targetFace
    _ ->
      Nothing

triangleCounterExpectations :: HomologyExampleCounterExpectations
triangleCounterExpectations =
  HomologyExampleCounterExpectations
    { expectedObjectCount = 6,
      expectedNonidentityMorphismCount = 6,
      expectedCellsByDegree = [(0, 6), (1, 6), (2, 0)],
      expectedBoundaryNonzerosByDegree = [(0, 0), (1, 12), (2, 0)],
      expectedRepresentativeSupportSize = 6
    }
