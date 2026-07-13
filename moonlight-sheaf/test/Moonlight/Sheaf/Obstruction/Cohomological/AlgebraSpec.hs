module Moonlight.Sheaf.Obstruction.Cohomological.AlgebraSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Moonlight.Sheaf.Obstruction.Cohomological.Algebra
  ( nerveFromAdjacency,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Types
  ( CoverNerve (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "cohomological algebra"
    [ testCase "nerveFromAdjacency keeps deterministic local cycle-basis semantics" testNerveCycleBasis
    ]

testNerveCycleBasis :: IO ()
testNerveCycleBasis =
  let triangleNeighbors :: Int -> Set.Set Int
      triangleNeighbors vertexValue =
        case vertexValue of
          1 -> Set.fromList [2, 3]
          2 -> Set.fromList [1, 3]
          3 -> Set.fromList [1, 2]
          _ -> Set.empty
      nerveValue = nerveFromAdjacency [1 :: Int, 2, 3] triangleNeighbors
   in cnFundamentalCycles nerveValue @?= [2 :| [1, 3]]
