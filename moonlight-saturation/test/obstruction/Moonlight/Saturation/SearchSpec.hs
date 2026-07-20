module Moonlight.Saturation.SearchSpec
  ( searchTests,
  )
where

import Moonlight.Saturation.Obstruction.Cohomological.Search
import Moonlight.Saturation.Test.ObstructionFixture
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

searchTests :: TestTree
searchTests =
  testGroup
    "feasible-family search"
    [ searchCase ExhaustiveSearch 4,
      searchCase LowerBoundPrunedSearch 12
    ]

searchCase :: SearchProfile -> Int -> TestTree
searchCase profile contextCount =
  testCase (show profile <> " chooses the exact minimum family") $
    let input = searchInput profile contextCount
        expectedSections = expectedSearchSections input
     in fmap
          (\family -> (ffsCost family, ffsChosenSections family))
          (chooseMinimumFeasibleFamily (feasibleFamilySearch input))
          @?= Just (sum expectedSections, expectedSections)
