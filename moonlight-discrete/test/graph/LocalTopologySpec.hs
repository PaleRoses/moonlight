module LocalTopologySpec
  ( tests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Graph (cyclicCellsFromChildren, cyclicCellsFromChildrenInt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "LocalTopology"
    [ testCase "detects self-loops as cyclic SCCs" testSelfLoopCycle,
      testCase "detects only vertices inside directed cycles" testDirectedCycleCells,
      testCase "preserves IntMap cycle detection" testIntCycleCells
    ]

testSelfLoopCycle :: IO ()
testSelfLoopCycle =
  cyclicCellsFromChildren (Map.singleton (7 :: Int) (Map.singleton 7 1))
    @?= Set.singleton 7

testDirectedCycleCells :: IO ()
testDirectedCycleCells =
  cyclicCellsFromChildren
    ( Map.fromList
        [ (1 :: Int, Map.singleton 2 1),
          (2, Map.singleton 3 1),
          (3, Map.singleton 1 1),
          (4, Map.singleton 5 1),
          (5, Map.empty)
        ]
    )
    @?= Set.fromList [1, 2, 3]

testIntCycleCells :: IO ()
testIntCycleCells =
  cyclicCellsFromChildrenInt
    ( IntMap.fromList
        [ (1, IntMap.singleton 2 1),
          (2, IntMap.singleton 1 1),
          (3, IntMap.empty)
        ]
    )
    @?= IntSet.fromList [1, 2]
