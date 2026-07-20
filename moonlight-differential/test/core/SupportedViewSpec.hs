{-# LANGUAGE DerivingStrategies #-}

module SupportedViewSpec
  ( tests,
  )
where

import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Monoid
  ( Sum (..),
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Differential.Operator.SupportedView
  ( Contribution (..),
    SupportedView,
    ViewChange,
    buildSupportedView,
    supportedViewAdvance,
    supportedViewKeys,
    supportedViewKeysForCells,
    supportedViewValueAt,
    viewChangeAfter,
    viewChangeBefore,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertEqual,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "supported view"
    (fmap fixtureTest fixtures)

type Cell = Int

type Key = Int

type Value = Sum Int

type TestContribution = Contribution Cell Value

type TestView = SupportedView Cell Key Value

type ChangeEndpoints = Map Key (Maybe Value, Maybe Value)

data Fixture = Fixture
  { fixtureName :: !String,
    fixtureInitialContributions :: !(Map Key [TestContribution]),
    fixtureDirtyCells :: !(Set Cell),
    fixtureFreshContributions :: !(Map Key [TestContribution])
  }
  deriving stock (Eq, Show)

fixtureTest :: Fixture -> TestTree
fixtureTest fixture =
  testCase (fixtureName fixture) $
    let view0 = buildSupportedView (fixtureInitialContributions fixture)
        (view1, changes) =
          supportedViewAdvance
            (fixtureDirtyCells fixture)
            (fixtureFreshContributions fixture)
            view0
        expectedContributions = oracleExpectedContributions fixture
        expectedView = buildSupportedView expectedContributions
        valueKeys = Set.union (supportedViewKeys view1) (Map.keysSet expectedContributions)
        mentionedCells = fixtureMentionedCells fixture
     in do
          assertEqual
            "advanced values match rebuild oracle"
            (viewValues valueKeys expectedView)
            (viewValues valueKeys view1)
          assertEqual
            "advanced keys match nonempty oracle rows"
            (Map.keysSet expectedContributions)
            (supportedViewKeys view1)
          assertEqual
            "inverted support index matches surviving supports"
            (oracleKeysByCell mentionedCells expectedContributions)
            (actualKeysByCell mentionedCells view1)
          assertEqual
            "changes are exactly changed before/after endpoints"
            (expectedChangeEndpoints view0 view1 changes)
            (actualChangeEndpoints changes)

viewValues :: Set Key -> TestView -> Map Key Value
viewValues keys view =
  Map.fromSet (`supportedViewValueAt` view) keys

oracleExpectedContributions :: Fixture -> Map Key [TestContribution]
oracleExpectedContributions fixture =
  Map.filter
    (not . null)
    ( Map.unionWith
        (<>)
        (Map.map (filter (contributionSurvives (fixtureDirtyCells fixture))) (fixtureInitialContributions fixture))
        (fixtureFreshContributions fixture)
    )

contributionSurvives :: Set Cell -> TestContribution -> Bool
contributionSurvives dirtyCells contribution =
  Set.disjoint (contributionSupport contribution) dirtyCells

oracleKeysByCell :: Set Cell -> Map Key [TestContribution] -> Map Cell (Set Key)
oracleKeysByCell cells contributions =
  Map.fromSet (`oracleKeysForCell` contributions) cells

oracleKeysForCell :: Cell -> Map Key [TestContribution] -> Set Key
oracleKeysForCell cell contributions =
  Map.keysSet
    ( Map.filter
        (any (Set.member cell . contributionSupport))
        contributions
    )

actualKeysByCell :: Set Cell -> TestView -> Map Cell (Set Key)
actualKeysByCell cells view =
  Map.fromSet
    (\cell -> supportedViewKeysForCells (Set.singleton cell) view)
    cells

expectedChangeEndpoints :: TestView -> TestView -> Map Key (ViewChange Value) -> ChangeEndpoints
expectedChangeEndpoints view0 view1 changes =
  Map.filter
    (uncurry (/=))
    ( Map.fromSet
        (\key -> (viewMaybeValue key view0, viewMaybeValue key view1))
        ( Set.unions
            [ supportedViewKeys view0,
              supportedViewKeys view1,
              Map.keysSet changes
            ]
        )
    )

actualChangeEndpoints :: Map Key (ViewChange Value) -> ChangeEndpoints
actualChangeEndpoints =
  fmap (\change -> (viewChangeBefore change, viewChangeAfter change))

viewMaybeValue :: Key -> TestView -> Maybe Value
viewMaybeValue key view =
  if Set.member key (supportedViewKeys view)
    then Just (supportedViewValueAt key view)
    else Nothing

fixtureMentionedCells :: Fixture -> Set Cell
fixtureMentionedCells fixture =
  Set.unions
    [ fixtureDirtyCells fixture,
      contributionsCells (fixtureInitialContributions fixture),
      contributionsCells (fixtureFreshContributions fixture)
    ]

contributionsCells :: Map Key [TestContribution] -> Set Cell
contributionsCells contributions =
  Set.unions
    [ contributionSupport contribution
      | contribution <- concat (Map.elems contributions)
    ]

fixtures :: [Fixture]
fixtures =
  [ insertOnlyIntoEmptyView,
    dirtyCellUpdatesSurvivingKey,
    dirtyCellRemovesSoleContribution,
    freshContributionReaddsEmptiedKey,
    netUnchangedReaddIsNotAChange,
    twoKeysShareDirtyCell,
    emptyDirtyCellsWithFreshContributions,
    oneDirtyCellRemovesMultiCellContribution
  ]

insertOnlyIntoEmptyView :: Fixture
insertOnlyIntoEmptyView =
  Fixture
    { fixtureName = "insert-only advance into empty view",
      fixtureInitialContributions = Map.empty,
      fixtureDirtyCells = Set.empty,
      fixtureFreshContributions =
        Map.fromList
          [ (1, [contribution 7 [10]]),
            (2, [contribution 3 [20]])
          ]
    }

dirtyCellUpdatesSurvivingKey :: Fixture
dirtyCellUpdatesSurvivingKey =
  Fixture
    { fixtureName = "dirty cell removes one contribution while key survives",
      fixtureInitialContributions =
        Map.fromList
          [ (1, [contribution 5 [1], contribution 7 [2]])
          ],
      fixtureDirtyCells = Set.fromList [1],
      fixtureFreshContributions = Map.empty
    }

dirtyCellRemovesSoleContribution :: Fixture
dirtyCellRemovesSoleContribution =
  Fixture
    { fixtureName = "dirty cell removes the sole contribution of a key",
      fixtureInitialContributions =
        Map.fromList
          [ (1, [contribution 5 [1]])
          ],
      fixtureDirtyCells = Set.fromList [1],
      fixtureFreshContributions = Map.empty
    }

freshContributionReaddsEmptiedKey :: Fixture
freshContributionReaddsEmptiedKey =
  Fixture
    { fixtureName = "fresh contribution re-adds a key emptied by dirt",
      fixtureInitialContributions =
        Map.fromList
          [ (1, [contribution 5 [1]])
          ],
      fixtureDirtyCells = Set.fromList [1],
      fixtureFreshContributions =
        Map.fromList
          [ (1, [contribution 11 [2]])
          ]
    }

netUnchangedReaddIsNotAChange :: Fixture
netUnchangedReaddIsNotAChange =
  Fixture
    { fixtureName = "same value re-add is absent from changes",
      fixtureInitialContributions =
        Map.fromList
          [ (1, [contribution 5 [1]])
          ],
      fixtureDirtyCells = Set.fromList [1],
      fixtureFreshContributions =
        Map.fromList
          [ (1, [contribution 5 [2]])
          ]
    }

twoKeysShareDirtyCell :: Fixture
twoKeysShareDirtyCell =
  Fixture
    { fixtureName = "two keys sharing a dirty cell are both invalidated",
      fixtureInitialContributions =
        Map.fromList
          [ (1, [contribution 2 [1]]),
            (2, [contribution 3 [1], contribution 4 [2]])
          ],
      fixtureDirtyCells = Set.fromList [1],
      fixtureFreshContributions = Map.empty
    }

emptyDirtyCellsWithFreshContributions :: Fixture
emptyDirtyCellsWithFreshContributions =
  Fixture
    { fixtureName = "empty dirty cells still accept fresh contributions",
      fixtureInitialContributions =
        Map.fromList
          [ (1, [contribution 4 [1]])
          ],
      fixtureDirtyCells = Set.empty,
      fixtureFreshContributions =
        Map.fromList
          [ (1, [contribution 6 [3]]),
            (2, [contribution 8 [2]])
          ]
    }

oneDirtyCellRemovesMultiCellContribution :: Fixture
oneDirtyCellRemovesMultiCellContribution =
  Fixture
    { fixtureName = "one dirty cell removes a contribution supported by two cells",
      fixtureInitialContributions =
        Map.fromList
          [ (1, [contribution 9 [1, 2], contribution 1 [3]])
          ],
      fixtureDirtyCells = Set.fromList [2],
      fixtureFreshContributions = Map.empty
    }

contribution :: Int -> [Cell] -> TestContribution
contribution value cells =
  Contribution
    { contributionValue = Sum value,
      contributionSupport = Set.fromList cells
    }
