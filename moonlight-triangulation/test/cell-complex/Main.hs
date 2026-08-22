module Main (main) where

import Data.Foldable (traverse_)
import Data.Vector qualified as Vector
import Moonlight.Triangulation.CellComplex (DCELComplex, fromExactCellSet)
import Moonlight.Homology.Pure.Topology.CellComplex
  ( CellComplex2D (..),
    CellTypes (..),
    OrientedEdge (..),
    ValidateComplex2D (..),
    eulerCharacteristic,
    isBoundaryEdge,
  )
import Moonlight.Triangulation.BulkLoad (delaunayGeometry)
import Moonlight.Triangulation.CellSet
  ( ExactCellSet,
    closeFaceCellSet,
    exactCellSetEdgeCount,
    exactCellSetFaceCount,
    exactCellSetVertexCount,
  )
import Moonlight.Triangulation.Handles.Iterators.FixedIterators (innerFaces)
import Moonlight.Triangulation.Types (Point (..))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, assertBool, assertFailure, testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "ExactCellSet bridge"
    [ testCase "preserves the admitted cell inventory" preserveCellInventory,
      testCase "preserves downward-closed incidence" preserveClosedIncidence,
      testCase "marks the triangular exterior as absent" preserveExteriorAdjacency
    ]

preserveCellInventory :: Assertion
preserveCellInventory =
  withTriangleComplex $ \cellSet complexValue -> do
    length (vertices complexValue) @?= exactCellSetVertexCount cellSet
    length (edges complexValue) @?= exactCellSetEdgeCount cellSet
    length (faces complexValue) @?= exactCellSetFaceCount cellSet
    (length (vertices complexValue), length (edges complexValue), length (faces complexValue))
      @?= (3, 3, 1)
    eulerCharacteristic complexValue @?= 1
    validateComplex complexValue @?= []

preserveClosedIncidence :: Assertion
preserveClosedIncidence =
  withTriangleComplex $ \_ complexValue -> do
    traverse_ (assertSelectedEdgeEndpoints complexValue) (edges complexValue)
    traverse_ (assertSelectedFaceBoundary complexValue) (faces complexValue)
    fmap (length . edgesAtVertex complexValue) (vertices complexValue) @?= [2, 2, 2]

preserveExteriorAdjacency :: Assertion
preserveExteriorAdjacency =
  withTriangleComplex $ \_ complexValue ->
    assertBool
      "every edge of a single selected triangle has one exterior incident face"
      (all (isBoundaryEdge complexValue) (edges complexValue))

assertSelectedEdgeEndpoints :: DCELComplex -> Edge DCELComplex -> Assertion
assertSelectedEdgeEndpoints complexValue edgeValue = do
  let selectedVertices = vertices complexValue
      (sourceVertex, targetVertex) = edgeBoundary complexValue edgeValue
  assertBool "edge source is selected" (sourceVertex `elem` selectedVertices)
  assertBool "edge target is selected" (targetVertex `elem` selectedVertices)

assertSelectedFaceBoundary :: DCELComplex -> Face DCELComplex -> Assertion
assertSelectedFaceBoundary complexValue faceValue = do
  let selectedEdges = edges complexValue
      boundary = faceBoundary complexValue faceValue
  length boundary @?= 3
  assertBool
    "every oriented boundary edge is selected"
    (all ((`elem` selectedEdges) . orientedEdge) boundary)

withTriangleComplex :: (ExactCellSet -> DCELComplex -> Assertion) -> Assertion
withTriangleComplex assertion =
  case delaunayGeometry trianglePoints of
    Left buildFailure -> assertFailure ("triangle construction failed: " <> show buildFailure)
    Right triangulation ->
      case closeFaceCellSet triangulation (innerFaces triangulation) of
        Left selectionFailure -> assertFailure ("triangle selection failed: " <> show selectionFailure)
        Right cellSet -> assertion cellSet (fromExactCellSet cellSet)

trianglePoints :: Vector.Vector Point
trianglePoints =
  Vector.fromList
    [ Point 0 0,
      Point 2 0,
      Point 0 2
    ]
