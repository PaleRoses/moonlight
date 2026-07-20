{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Homology.Examples.H2TetrahedronBoundarySpec
  ( tests,
  )
where

import Data.List (permutations, sort, tails)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Cosheaf.Chain
  ( CosheafChainCell,
    PreparedFiniteCosheafChain,
    verifyCosheafBoundaryNilpotence,
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
    faceCardinality,
    faceEdge,
    faceFromVertices,
    facePosetBoundarySite,
    faceTriangle,
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
    assertBool,
    assertEqual,
    testCase,
  )

twoDegree :: HomologicalDegree
twoDegree =
  HomologicalDegree 2

tests :: TestTree
tests =
  testGroup
    "tetrahedron-boundary cosheaf H2 generator"
    [ testCase "oriented barycentric boundary lifts to a 24-flag H2 cycle" testTetrahedronBoundaryGenerator
    ]

testTetrahedronBoundaryGenerator :: Assertion
testTetrahedronBoundaryGenerator = do
  timedPlan <- timedActionWith (planCellTotal [0, 1, 2, 3]) prepareTetrahedronPlan
  let plan = timedActionValue timedPlan
  shouldHaveCellCounts plan [(0, 14), (1, 36), (2, 24), (3, 0)]
  expectRight (verifyCosheafBoundaryNilpotence plan)
  timedGroups <- timedActionWith length (expectRight (cosheafIntegralHomology plan))
  let groups = timedActionValue timedGroups
  shouldHaveHomologyGroup groups 0 1 []
  shouldHaveHomologyGroup groups 1 0 []
  shouldHaveHomologyGroup groups 2 1 []
  shouldHaveHomologyGroup groups 3 0 []
  cycleCells <- traverse (resolvePathTerm plan) tetrahedronBoundaryCycleTerms
  assertEqual "oriented tetrahedron boundary has 24 barycentric flag terms" 24 (length cycleCells)
  assertEqual
    "all barycentric coefficients have unit magnitude"
    (replicate 24 (1 :: Integer))
    (sort (fmap (abs . fst) cycleCells))
  assertEqual
    "each triangular face contributes six signed flags before basis projection"
    expectedTriangularFaceCounts
    (faceContributionCountsFromTerms tetrahedronBoundaryCycleTerms)
  representative <- expectRight (representativeFromCells twoDegree cycleCells plan)
  expectRight (assertRepresentativeCycle plan representative)
  timedWitness <- timedActionWith witnessSupportSize (expectRight (liftCosheafRepresentative twoDegree plan representative))
  let witness = timedActionValue timedWitness
  assertEqual
    "lifted H2 support has exactly 24 concrete cosheaf two-cells"
    24
    (length (chwRepresentativeTerms witness))
  assertBool
    "lifted coefficients remain ±1"
    (all ((== 1) . abs . lcctCoefficient) (chwRepresentativeTerms witness))
  let liftedPaths = fmap snd (liftedWitnessCellPaths witness)
  assertBool
    "every lifted cell is a length-two nerve chain"
    (all ((== Just 2) . nerveDimension) liftedPaths)
  assertBool
    "every lifted flag has shape vertex < edge < face"
    (all ((== Just [1, 2, 3]) . pathCardinalities) liftedPaths)
  assertEqual
    "each triangular face contributes six lifted flags"
    expectedTriangularFaceCounts
    (faceContributionCountsFromPaths liftedPaths)
  assertHomologyExampleCounters
    "tetrahedron-boundary homology counters"
    tetrahedronCounterExpectations
    (homologyExampleCounters [0, 1, 2, 3] timedPlan timedGroups timedWitness)

prepareTetrahedronPlan :: IO (PreparedFiniteCosheafChain FacePosetSite SingletonCostalk)
prepareTetrahedronPlan = do
  site <- expectRight (facePosetBoundarySite 4)
  cosheaf <- expectRight (constantSingletonCosheaf site)
  expectRight (prepareFullFiniteCosheafChain 3 cosheaf)

tetrahedronBoundaryCycleTerms :: [(Integer, [Face])]
tetrahedronBoundaryCycleTerms =
  concatMap orientedFaceTerms tetrahedronBoundaryFaces
  where
    orientedFaceTerms (faceIndexValue, orientedVertices) =
      [ (boundaryOrientation faceIndexValue * orientationValue, flagValue)
      | permutedVertices <- permutations orientedVertices
      , Just orientationValue <- [permutationOrientation orientedVertices permutedVertices]
      , Just flagValue <- [barycentricFlag orientedVertices permutedVertices]
      ]

tetrahedronBoundaryFaces :: [(Int, [Int])]
tetrahedronBoundaryFaces =
  [ (omittedVertex, filter (/= omittedVertex) tetrahedronVertices)
  | omittedVertex <- tetrahedronVertices
  ]

tetrahedronVertices :: [Int]
tetrahedronVertices =
  [0, 1, 2, 3]

boundaryOrientation :: Int -> Integer
boundaryOrientation faceIndexValue =
  if even faceIndexValue
    then 1
    else -1

permutationOrientation :: [Int] -> [Int] -> Maybe Integer
permutationOrientation orientedVertices permutedVertices =
  orientationFromPositions <$> traverse (`Map.lookup` positionsByVertex) permutedVertices
  where
    positionsByVertex =
      Map.fromList (zip orientedVertices [0 :: Int ..])

    orientationFromPositions :: [Int] -> Integer
    orientationFromPositions permutationPositions =
      if even (inversionCount permutationPositions)
        then 1
        else -1

inversionCount :: [Int] -> Int
inversionCount positions =
  length
    [ ()
    | leftValue : rightValues <- tails positions,
      rightValue <- rightValues,
      leftValue > rightValue
    ]

barycentricFlag :: [Int] -> [Int] -> Maybe [Face]
barycentricFlag orientedVertices permutedVertices =
  case firstTwo permutedVertices of
    Just (sourceVertex, edgeTargetVertex) ->
      Just
        [ faceVertex sourceVertex,
          faceEdge sourceVertex edgeTargetVertex,
          faceFromVertices orientedVertices
        ]
    Nothing ->
      Nothing

firstTwo :: [a] -> Maybe (a, a)
firstTwo values =
  case values of
    firstValue : secondValue : _ ->
      Just (firstValue, secondValue)
    _ ->
      Nothing

resolvePathTerm ::
  PreparedFiniteCosheafChain FacePosetSite SingletonCostalk ->
  (Integer, [Face]) ->
  IO (Integer, CosheafChainCell Face FaceInclusion SingletonCostalk)
resolvePathTerm plan (coefficientValue, pathValue) = do
  cell <- expectRight (flagCellAt twoDegree pathValue plan)
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

expectedTriangularFaceCounts :: Map Face Int
expectedTriangularFaceCounts =
  Map.fromList
    [ (faceTriangle 0 1 2, 6),
      (faceTriangle 0 1 3, 6),
      (faceTriangle 0 2 3, 6),
      (faceTriangle 1 2 3, 6)
    ]

faceContributionCountsFromTerms :: [(Integer, [Face])] -> Map Face Int
faceContributionCountsFromTerms terms =
  Map.fromListWith
    (+)
    [ (terminalFaceValue, 1)
    | (_coefficientValue, pathValue) <- terms,
      Just terminalFaceValue <- [twoFlagTerminal pathValue]
    ]

faceContributionCountsFromPaths :: [[Face]] -> Map Face Int
faceContributionCountsFromPaths paths =
  Map.fromListWith
    (+)
    [ (terminalFaceValue, 1)
    | pathValue <- paths,
      Just terminalFaceValue <- [twoFlagTerminal pathValue]
    ]

twoFlagTerminal :: [Face] -> Maybe Face
twoFlagTerminal pathValue =
  case pathValue of
    _vertexFace : _edgeFace : terminalFace : [] ->
      Just terminalFace
    _ ->
      Nothing

pathCardinalities :: [Face] -> Maybe [Int]
pathCardinalities pathValue =
  case pathValue of
    vertexFace : edgeFace : terminalFace : [] ->
      Just (fmap faceCardinality [vertexFace, edgeFace, terminalFace])
    _ ->
      Nothing

nerveDimension :: [Face] -> Maybe Int
nerveDimension pathValue =
  case pathValue of
    [] ->
      Nothing
    _nonemptyPath ->
      Just (length pathValue - 1)

tetrahedronCounterExpectations :: HomologyExampleCounterExpectations
tetrahedronCounterExpectations =
  HomologyExampleCounterExpectations
    { expectedObjectCount = 14,
      expectedNonidentityMorphismCount = 36,
      expectedCellsByDegree = [(0, 14), (1, 36), (2, 24), (3, 0)],
      expectedBoundaryNonzerosByDegree = [(0, 0), (1, 72), (2, 72), (3, 0)],
      expectedRepresentativeSupportSize = 24
    }
