module LocalTopologySpec
  ( tests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Moonlight.Graph
  ( LocalTopologyError (..),
    buildLocalAdjFromChildren,
    closedStarAdj,
    countLocalEdges,
    cyclicCellsFromChildren,
    localEdges,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=), testCase)
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "LocalTopology"
    [ testCase "detects self-loops as cyclic SCCs" testSelfLoopCycle,
      testCase "detects only vertices inside directed cycles" testDirectedCycleCells,
      testCase "rejects zero child multiplicity" testZeroMultiplicity,
      testCase "rejects negative child multiplicity" testNegativeMultiplicity,
      testCase "derives parents and retains parent-only and child-only vertices" testDerivedAdjacency,
      QC.testProperty "edge counts equal materialized edge multiplicity" propEdgeCountAgrees
    ]

testSelfLoopCycle :: IO ()
testSelfLoopCycle =
  cyclicCellsFromChildren (IntMap.singleton 7 (IntMap.singleton 7 1))
    @?= Right (IntSet.singleton 7)

testDirectedCycleCells :: IO ()
testDirectedCycleCells =
  cyclicCellsFromChildren
    ( IntMap.fromList
        [ (1, IntMap.singleton 2 1),
          (2, IntMap.singleton 3 1),
          (3, IntMap.singleton 1 1),
          (4, IntMap.singleton 5 1),
          (5, IntMap.empty)
        ]
    )
    @?= Right (IntSet.fromList [1, 2, 3])

testZeroMultiplicity :: IO ()
testZeroMultiplicity =
  cyclicCellsFromChildren (IntMap.singleton 1 (IntMap.singleton 2 0))
    @?= Left (NonPositiveChildMultiplicity 1 2 0)

testNegativeMultiplicity :: IO ()
testNegativeMultiplicity =
  cyclicCellsFromChildren (IntMap.singleton 1 (IntMap.singleton 2 (-1)))
    @?= Left (NonPositiveChildMultiplicity 1 2 (-1))

testDerivedAdjacency :: Assertion
testDerivedAdjacency =
  case
      buildLocalAdjFromChildren
        ( IntMap.fromList
            [ (1, IntMap.singleton 2 2),
              (3, IntMap.empty)
            ]
        )
    of
      Left topologyError ->
        assertFailure ("unexpected topology obstruction: " <> show topologyError)
      Right adjacency -> do
        closedStarAdj adjacency 2 @?= IntSet.fromList [1, 2]
        closedStarAdj adjacency 3 @?= IntSet.singleton 3
        localEdges adjacency (IntSet.fromList [1, 2]) @?= [(1, 2), (1, 2)]
        countLocalEdges adjacency (IntSet.fromList [1, 2])
          @?= length (localEdges adjacency (IntSet.fromList [1, 2]))

propEdgeCountAgrees :: [(QC.Small Int, [(QC.Small Int, Int)])] -> [QC.Small Int] -> QC.Property
propEdgeCountAgrees rawChildren rawVertices =
  let children =
        IntMap.fromList
          [ (parentCell, IntMap.fromList [(childCell, 1 + multiplicity `mod` 4) | (QC.Small childCell, multiplicity) <- childEntries])
          | (QC.Small parentCell, childEntries) <- rawChildren
          ]
      vertices = IntSet.fromList [cell | QC.Small cell <- rawVertices]
   in case buildLocalAdjFromChildren children of
        Left topologyError ->
          QC.counterexample ("positive fixture was rejected: " <> show topologyError) False
        Right adjacency ->
          countLocalEdges adjacency vertices QC.=== length (localEdges adjacency vertices)
