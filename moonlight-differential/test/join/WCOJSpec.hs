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
  ( DenseTriangleAllocation (..),
    DenseTriangleBuildError (..),
    TriangleBenchmarkStats (..),
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
    assertFailure,
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
      testCase "Dense triangle WCOJ preserves declared isolated vertices" denseTriangleWCOJPreservesDeclaredIsolatedVertices,
      testCase "Dense triangle WCOJ rejects surviving out-of-domain endpoints" denseTriangleWCOJRejectsOutOfDomainEndpoints,
      testCase "Dense triangle WCOJ rejects invalid and overflowing capacities before allocation" denseTriangleWCOJRejectsOverflowingCapacities,
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
    ( Right
        ( TriangleCount
            { tcTriangles = 1,
              tcIntersectionSteps = 2
            }
        )
    )
    (fmap countTrianglesWCOJ (buildDenseTriangleTrie 3 triangleEdges))

denseTriangleWCOJCountsBitsetClique :: IO ()
denseTriangleWCOJCountsBitsetClique =
  assertEqual
    "bitset-sized clique should count every vertex triple"
    (Right (choose3 cliqueSize))
    (fmap (tcTriangles . countTrianglesWCOJ) (buildDenseTriangleTrie cliqueSize (cliqueEdges cliqueSize)))
  where
    cliqueSize =
      65

denseTriangleWCOJNormalizesInput :: IO ()
denseTriangleWCOJNormalizesInput =
  assertEqual
    "normalization should erase reversed duplicates, loops, and negative edges"
    (fmap countTrianglesWCOJ (buildDenseTriangleTrie 3 triangleEdges))
    (fmap countTrianglesWCOJ (buildDenseTriangleTrie 3 noisyTriangleEdges))

denseTriangleWCOJRejectsPathGraph :: IO ()
denseTriangleWCOJRejectsPathGraph =
  assertEqual
    "path graph has no closed triangle"
    (Right 0)
    (fmap (tcTriangles . countTrianglesWCOJ) (buildDenseTriangleTrie 3 [(0, 1), (1, 2)]))

denseTriangleWCOJPreservesDeclaredIsolatedVertices :: IO ()
denseTriangleWCOJPreservesDeclaredIsolatedVertices =
  assertEqual
    "declared vertices remain observable even when no edge mentions them"
    (Right (0, 5, 0))
    ( fmap
        (\stats -> (tbsEdges stats, tbsVertices stats, tbsTriangles stats))
        (triangleBenchmarkStats <$> buildDenseTriangleTrie 5 [])
    )

denseTriangleWCOJRejectsOutOfDomainEndpoints :: IO ()
denseTriangleWCOJRejectsOutOfDomainEndpoints =
  assertEqual
    "a sparse label outside the declared dense domain must fail before allocation"
    (Left (DenseTriangleEndpointOutOfRange 3 (0, 1_000_000_000)))
    (buildDenseTriangleTrie 3 [(0, 1_000_000_000)])

denseTriangleWCOJRejectsOverflowingCapacities :: IO ()
denseTriangleWCOJRejectsOverflowingCapacities = do
  assertEqual
    "negative vertex count"
    (Left (DenseTriangleNegativeVertexCount (-1)))
    (buildDenseTriangleTrie (-1) [])
  assertEqual
    "offset count must not wrap at maxBound"
    (Left (DenseTriangleCapacityAdditionOverflow DenseTriangleOffsetVector maxBound 1))
    (buildDenseTriangleTrie maxBound [])
  assertEqual
    "adjacency product must fail before requesting a vector"
    ( Left
        ( DenseTriangleCapacityProductOverflow
            DenseTriangleAdjacencyBits
            overflowingVertexCount
            overflowingWordCount
        )
    )
    (buildDenseTriangleTrie overflowingVertexCount [])
  where
    overflowingVertexCount =
      maxBound `quot` 64
    overflowingWordCount =
      ((overflowingVertexCount - 1) `quot` 64) + 1

denseTriangleWCOJStatsExposeNormalizedFiniteSummary :: IO ()
denseTriangleWCOJStatsExposeNormalizedFiniteSummary = do
  case triangleBenchmarkStats <$> buildDenseTriangleTrie 4 noisyTriangleEdges of
    Left buildError ->
      assertFailure ("lawful triangle fixture rejected: " <> show buildError)
    Right stats -> do
      assertEqual "normalized edge count" 3 (tbsEdges stats)
      assertEqual "declared vertex count" 4 (tbsVertices stats)
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
