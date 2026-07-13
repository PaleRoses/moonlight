module Moonlight.Cosheaf.Homology.TropicalSpec
  ( tests,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Cosheaf
  ( TropicalBidegree (..),
    TropicalCell (..),
    TropicalCellKey (..),
    TropicalCellularComplex (..),
    TropicalFace (..),
    TropicalHomologyFailure (..),
    TropicalPDegree (..),
    TropicalTangentBasis (..),
    thaGroupsByBidegree,
    tropicalHomology,
  )
import Moonlight.Homology
  ( HomologicalDegree (..),
    HomologyGroup (..),
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

tests :: TestTree
tests =
  testGroup
    "tropical homology"
    [ testCase "tropical point has only H(0,0)" testTropicalPoint,
      testCase "compact tropical circle has genus-one bidegree ranks" testTropicalCircle,
      testCase "F_p is zero when p exceeds tangent rank" testFpAboveRankZero,
      testCase "bad tangent map is a typed obstruction" testBadTangentMapFailure
    ]

testTropicalPoint :: Assertion
testTropicalPoint = do
  artifact <- expectRight (tropicalHomology [TropicalPDegree 0, TropicalPDegree 1] tropicalPoint)
  assertBidegreeRank "H(0,0) point" (TropicalPDegree 0) 0 1 (thaGroupsByBidegree artifact)
  assertBidegreeRank "H(1,0) point" (TropicalPDegree 1) 0 0 (thaGroupsByBidegree artifact)

testTropicalCircle :: Assertion
testTropicalCircle = do
  artifact <- expectRight (tropicalHomology [TropicalPDegree 0, TropicalPDegree 1] tropicalCircle)
  assertBidegreeRank "H(0,0) circle" (TropicalPDegree 0) 0 1 (thaGroupsByBidegree artifact)
  assertBidegreeRank "H(0,1) circle" (TropicalPDegree 0) 1 1 (thaGroupsByBidegree artifact)
  assertBidegreeRank "H(1,0) circle" (TropicalPDegree 1) 0 1 (thaGroupsByBidegree artifact)
  assertBidegreeRank "H(1,1) circle" (TropicalPDegree 1) 1 1 (thaGroupsByBidegree artifact)

testFpAboveRankZero :: Assertion
testFpAboveRankZero = do
  artifact <- expectRight (tropicalHomology [TropicalPDegree 2] tropicalCircle)
  assertBidegreeRank "H(2,0) rank-zero cosheaf" (TropicalPDegree 2) 0 0 (thaGroupsByBidegree artifact)
  assertBidegreeRank "H(2,1) rank-zero cosheaf" (TropicalPDegree 2) 1 0 (thaGroupsByBidegree artifact)

testBadTangentMapFailure :: Assertion
testBadTangentMapFailure =
  case tropicalHomology [TropicalPDegree 1] badTangentMapCircle of
    Left (TropicalFaceTangentMapShapeMismatch _face 1 1 1 [2]) -> pure ()
    Left otherFailure -> assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ -> assertFailure "expected tangent-map shape obstruction"

assertBidegreeRank ::
  String ->
  TropicalPDegree ->
  Int ->
  Int ->
  Map TropicalBidegree (HomologyGroup coefficient) ->
  Assertion
assertBidegreeRank label pDegree qDegree expectedRank groups =
  case Map.lookup (TropicalBidegree pDegree (HomologicalDegree qDegree)) groups of
    Nothing -> assertFailure (label <> ": missing group")
    Just groupValue -> assertEqual label expectedRank (freeRank groupValue)

expectRight :: Show failure => Either failure value -> IO value
expectRight result =
  case result of
    Right value -> pure value
    Left failure -> assertFailure ("unexpected failure: " <> show failure)

tropicalPoint :: TropicalCellularComplex
tropicalPoint =
  TropicalCellularComplex
    { tropicalCells = Map.fromList [(tropicalCellKey pointCell, pointCell)],
      tropicalFaces = [],
      tropicalTangentBases = Map.fromList [(tropicalCellKey pointCell, TropicalTangentBasis [])]
    }

tropicalCircle :: TropicalCellularComplex
tropicalCircle =
  TropicalCellularComplex
    { tropicalCells = Map.fromList (fmap (\cell -> (tropicalCellKey cell, cell)) [vertex0, vertex1, edgeA, edgeB]),
      tropicalFaces = circleFaces [[1]],
      tropicalTangentBases = Map.fromList (fmap (\cell -> (tropicalCellKey cell, TropicalTangentBasis [[1]])) [vertex0, vertex1, edgeA, edgeB])
    }

badTangentMapCircle :: TropicalCellularComplex
badTangentMapCircle =
  tropicalCircle
    { tropicalFaces = circleFaces [[1, 2]]
    }

circleFaces :: [[Integer]] -> [TropicalFace]
circleFaces tangentMap =
  [ TropicalFace edgeA vertex0 (-1) tangentMap,
    TropicalFace edgeA vertex1 1 tangentMap,
    TropicalFace edgeB vertex0 (-1) tangentMap,
    TropicalFace edgeB vertex1 1 tangentMap
  ]

pointCell, vertex0, vertex1, edgeA, edgeB :: TropicalCell
pointCell = TropicalCell (TropicalCellKey 0) 0
vertex0 = TropicalCell (TropicalCellKey 1) 0
vertex1 = TropicalCell (TropicalCellKey 2) 0
edgeA = TropicalCell (TropicalCellKey 3) 1
edgeB = TropicalCell (TropicalCellKey 4) 1
