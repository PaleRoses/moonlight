module ArcConsistencySpec
  ( tests,
  )
where

import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Constraint.Pure.ArcConsistency
  ( ac3,
    buildCompatWith,
    compatibilityTableFromList,
    searchSatisfying,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    testCase,
  )

type SearchCoord :: Type
data SearchCoord
  = LeftCoord
  | RightCoord
  deriving stock (Eq, Ord, Show)

type SearchValue :: Type
data SearchValue
  = SearchOne
  | SearchTwo
  deriving stock (Eq, Ord, Show)

tests :: TestTree
tests =
  testGroup
    "arc consistency"
    [ testCase "ac3 prunes typed domains" testAc3PrunesTypedDomains,
      testCase "searchSatisfying enumerates surviving assignments" testSearchSatisfyingEnumeratesAssignments,
      testCase "reversed construction preserves directional compatibility" testReversedConstruction
    ]

testAc3PrunesTypedDomains :: Assertion
testAc3PrunesTypedDomains =
  let domains =
        Map.fromList
          [ (LeftCoord, Set.fromList [SearchOne]),
            (RightCoord, Set.fromList [SearchOne, SearchTwo])
          ]
      compatibilityTable =
        compatibilityTableFromList
          [ ( (LeftCoord, RightCoord),
              buildCompatWith
                (==)
                (Map.findWithDefault Set.empty LeftCoord domains)
                (Map.findWithDefault Set.empty RightCoord domains)
            )
          ]
      prunedDomains = ac3 compatibilityTable domains
   in assertEqual
        "typed AC3 should retain only the equality-compatible right value"
        (Set.fromList [SearchOne])
        (Map.findWithDefault Set.empty RightCoord prunedDomains)

testSearchSatisfyingEnumeratesAssignments :: Assertion
testSearchSatisfyingEnumeratesAssignments =
  let domains =
        Map.fromList
          [ (LeftCoord, Set.fromList [SearchOne, SearchTwo]),
            (RightCoord, Set.fromList [SearchOne, SearchTwo])
          ]
      compatibilityTable =
        compatibilityTableFromList
          [ ( (LeftCoord, RightCoord),
              buildCompatWith
                (==)
                (Map.findWithDefault Set.empty LeftCoord domains)
                (Map.findWithDefault Set.empty RightCoord domains)
            )
          ]
      satisfyingAssignments =
        searchSatisfying
          [LeftCoord, RightCoord]
          domains
          compatibilityTable
          (const True)
   in assertEqual
        "search should enumerate all compatible assignments"
        [[SearchOne, SearchOne], [SearchTwo, SearchTwo]]
        satisfyingAssignments

testReversedConstruction :: Assertion
testReversedConstruction =
  let domains =
        Map.fromList
          [ (LeftCoord, Set.fromList [SearchOne, SearchTwo]),
            (RightCoord, Set.fromList [SearchOne, SearchTwo])
          ]
      compatibilityTable =
        compatibilityTableFromList
          [ ( (RightCoord, LeftCoord),
              buildCompatWith
                (>)
                (Map.findWithDefault Set.empty RightCoord domains)
                (Map.findWithDefault Set.empty LeftCoord domains)
            )
          ]
      satisfyingAssignments =
        searchSatisfying
          [LeftCoord, RightCoord]
          domains
          compatibilityTable
          (const True)
   in assertEqual
        "canonical storage must transpose a relation supplied in reverse coordinate order"
        [[SearchOne, SearchTwo]]
        satisfyingAssignments
