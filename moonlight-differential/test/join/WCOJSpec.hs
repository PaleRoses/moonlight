module WCOJSpec
  ( tests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet

import Moonlight.Differential.Join.WCOJ
  ( intBinaryConstraintIndex,
    intBinaryRelationIndexFromList,
    intIndexedJoinProblem,
    intIndexedJoinValidate,
  )
import Moonlight.Differential.Join.WCOJ.Dense.Triangle
  ( TriangleBenchmarkStats (..),
    TriangleCount (..),
    buildDenseTriangleTrie,
    countTrianglesWCOJ,
    triangleBenchmarkStats,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "WCOJ laws"
    [ testCase "Dense triangle WCOJ counts a single triangle" denseTriangleWCOJCountsSingleTriangle,
      testCase "Dense triangle WCOJ counts a bitset-sized clique" denseTriangleWCOJCountsBitsetClique,
      testCase "Dense triangle WCOJ normalizes duplicate reversed loop and negative edges" denseTriangleWCOJNormalizesInput,
      testCase "Dense triangle WCOJ rejects path graphs as non-triangles" denseTriangleWCOJRejectsPathGraph,
      testCase "Dense triangle WCOJ stats expose normalized finite AGM summary" denseTriangleWCOJStatsExposeNormalizedFiniteSummary,
      testCase "Int indexed WCOJ rejects out-of-universe validation" intIndexedWCOJRejectsOutOfUniverseValidation
    ]

intIndexedWCOJRejectsOutOfUniverseValidation :: IO ()
intIndexedWCOJRejectsOutOfUniverseValidation =
  assertBool
    "validation must reject assignments outside the indexed problem universe"
    (not (intIndexedJoinValidate problem (IntMap.fromList [(0, 0), (1, 99)])))
  where
    problem =
      intIndexedJoinProblem
        (IntSet.singleton 0)
        [intBinaryConstraintIndex 0 1 (intBinaryRelationIndexFromList [(0, 99)])]

denseTriangleWCOJCountsSingleTriangle :: IO ()
denseTriangleWCOJCountsSingleTriangle =
  assertEqual
    "3-cycle should produce one triangle"
    TriangleCount
      { tcTriangles = 1,
        tcIntersectionSteps = 2
      }
    (countTrianglesWCOJ (buildDenseTriangleTrie triangleEdges))

denseTriangleWCOJCountsBitsetClique :: IO ()
denseTriangleWCOJCountsBitsetClique =
  assertEqual
    "bitset-sized clique should count every vertex triple"
    (choose3 cliqueSize)
    (tcTriangles (countTrianglesWCOJ (buildDenseTriangleTrie (cliqueEdges cliqueSize))))
  where
    cliqueSize =
      65

denseTriangleWCOJNormalizesInput :: IO ()
denseTriangleWCOJNormalizesInput =
  assertEqual
    "normalization should erase reversed duplicates, loops, and negative edges"
    (countTrianglesWCOJ (buildDenseTriangleTrie triangleEdges))
    (countTrianglesWCOJ (buildDenseTriangleTrie noisyTriangleEdges))

denseTriangleWCOJRejectsPathGraph :: IO ()
denseTriangleWCOJRejectsPathGraph =
  assertEqual
    "path graph has no closed triangle"
    0
    (tcTriangles (countTrianglesWCOJ (buildDenseTriangleTrie [(0, 1), (1, 2)])))

denseTriangleWCOJStatsExposeNormalizedFiniteSummary :: IO ()
denseTriangleWCOJStatsExposeNormalizedFiniteSummary = do
  let stats =
        triangleBenchmarkStats (buildDenseTriangleTrie noisyTriangleEdges)
  assertEqual "normalized edge count" 3 (tbsEdges stats)
  assertEqual "normalized vertex count" 3 (tbsVertices stats)
  assertEqual "triangle count" 1 (tbsTriangles stats)
  assertBool "AGM bound should be positive" (tbsAgmBound stats > 0.0)
  assertBool "work ratio should be finite" (not (isNaN (tbsWorkToAgm stats) || isInfinite (tbsWorkToAgm stats)))

triangleEdges :: [(Int, Int)]
triangleEdges =
  [(0, 1), (1, 2), (0, 2)]

noisyTriangleEdges :: [(Int, Int)]
noisyTriangleEdges =
  [ (0, 1),
    (1, 0),
    (1, 2),
    (2, 1),
    (0, 2),
    (2, 0),
    (2, 2),
    (-1, 0),
    (3, -1)
  ]

cliqueEdges :: Int -> [(Int, Int)]
cliqueEdges vertexCount =
  [ (leftVertex, rightVertex)
  | leftVertex <- [0 .. vertexCount - 1],
    rightVertex <- [leftVertex + 1 .. vertexCount - 1]
  ]

choose3 :: Int -> Int
choose3 value =
  (value * (value - 1) * (value - 2)) `quot` 6
