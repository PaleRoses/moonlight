{-# LANGUAGE DerivingStrategies #-}

module WCOJDeltaSpec
  ( tests,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.List
  ( sort,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Differential.Index.RowIdSet
  ( RowIdSet,
    rowIdSetFromIntSetCanonical,
  )
import Moonlight.Differential.Index.RowSet
  ( RowSet,
    rowSetFromIntSetCanonical,
    rowSetToIntSet,
  )
import Moonlight.Differential.Join.WCOJ
  ( Env,
    Slot,
  )
import Moonlight.Differential.Join.WCOJ.Delta
  ( DeltaJoinConstraint (..),
    DeltaJoinProblem,
    DeltaJoinSource (..),
    deltaWCOJLeaves,
    mkDeltaJoinProblem,
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
    "delta WCOJ"
    (fmap fixtureTest fixtures)

type NormalizedLeaf = ([(Slot, Int)], [(Int, [Int])])

type RowKey = (Slot, Int)

data Fixture = Fixture
  { fixtureName :: !String,
    fixtureSlots :: ![Slot],
    fixtureSources :: ![FixtureSource],
    fixtureConstraints :: ![FixtureConstraint]
  }
  deriving stock (Eq, Show)

data FixtureSource = FixtureSource
  { fixtureSourceRows :: ![FixtureRow],
    fixtureSourceDirtyRows :: !IntSet
  }
  deriving stock (Eq, Show)

data FixtureConstraint = FixtureConstraint
  { fixtureConstraintRows :: ![FixtureRow]
  }
  deriving stock (Eq, Show)

data FixtureRow = FixtureRow
  { fixtureRowId :: !Int,
    fixtureRowValues :: !(IntMap Int)
  }
  deriving stock (Eq, Show)

fixtureTest :: Fixture -> TestTree
fixtureTest fixture =
  testCase (fixtureName fixture) $
    assertEqual
      "dirty-restricted leaves match brute-force oracle"
      (oracleLeaves fixture)
      (actualLeaves fixture)

actualLeaves :: Fixture -> [NormalizedLeaf]
actualLeaves fixture =
  sort
    [ (IntMap.toAscList env, normalizeSupports supports)
      | (env, supports) <- deltaWCOJLeaves (fixtureProblem fixture)
    ]

oracleLeaves :: Fixture -> [NormalizedLeaf]
oracleLeaves fixture =
  sort
    [ (IntMap.toAscList assignment, normalizeOracleSupports sourceSupports)
      | assignment <- totalAssignments (fixtureSlots fixture) (observedSlotValues (fixtureSources fixture)),
        let sourceSupports = fmap (sourceRowsAgreeing assignment) (fixtureSources fixture),
        all (not . IntSet.null) sourceSupports,
        any (sourceHasDirtyWitness assignment) (fixtureSources fixture),
        all (not . IntSet.null . constraintRowsAgreeing assignment) (fixtureConstraints fixture)
    ]

normalizeSupports :: IntMap RowSet -> [(Int, [Int])]
normalizeSupports supports =
  [ (sourceId, IntSet.toAscList (rowSetToIntSet rows))
    | (sourceId, rows) <- IntMap.toAscList supports
  ]

normalizeOracleSupports :: [IntSet] -> [(Int, [Int])]
normalizeOracleSupports supports =
  zip [0 ..] (fmap IntSet.toAscList supports)

fixtureProblem :: Fixture -> DeltaJoinProblem
fixtureProblem fixture =
  mkDeltaJoinProblem
    (fixtureSlots fixture)
    (fmap deltaSourceFromFixture (fixtureSources fixture))
    (fmap deltaConstraintFromFixture (fixtureConstraints fixture))

deltaSourceFromFixture :: FixtureSource -> DeltaJoinSource
deltaSourceFromFixture source =
  DeltaJoinSource
    { deltaSourceRows = rowSetFromRows (fixtureSourceRows source),
      deltaSourceDirtyRows = rowSetFromIntSetCanonical (fixtureSourceDirtyRows source),
      deltaSourceValueIndex = valueIndexFromRows (fixtureSourceRows source),
      deltaSourceValueAt = sourceValueAt (fixtureSourceRows source)
    }

deltaConstraintFromFixture :: FixtureConstraint -> DeltaJoinConstraint
deltaConstraintFromFixture constraint =
  DeltaJoinConstraint
    { deltaConstraintRows = rowSetFromRows (fixtureConstraintRows constraint),
      deltaConstraintValueIndex = valueIndexFromRows (fixtureConstraintRows constraint)
    }

rowSetFromRows :: [FixtureRow] -> RowSet
rowSetFromRows rows =
  rowSetFromIntSetCanonical (IntSet.fromList (fmap fixtureRowId rows))

sourceValueAt :: [FixtureRow] -> Slot -> Int -> Maybe Int
sourceValueAt rows =
  let values = rowValueMap rows
   in \slot rowId -> Map.lookup (slot, rowId) values

rowValueMap :: [FixtureRow] -> Map RowKey Int
rowValueMap rows =
  Map.fromList
    [ ((slot, fixtureRowId row), value)
      | row <- rows,
        (slot, value) <- IntMap.toAscList (fixtureRowValues row)
    ]

valueIndexFromRows :: [FixtureRow] -> IntMap (IntMap RowIdSet)
valueIndexFromRows =
  fmap (fmap rowIdSetFromIntSetCanonical) . rawValueIndexFromRows

rawValueIndexFromRows :: [FixtureRow] -> IntMap (IntMap IntSet)
rawValueIndexFromRows rows =
  IntMap.unionsWith
    (IntMap.unionWith IntSet.union)
    (fmap rowValueIndex rows)

rowValueIndex :: FixtureRow -> IntMap (IntMap IntSet)
rowValueIndex row =
  IntMap.fromListWith
    (IntMap.unionWith IntSet.union)
    [ (slot, IntMap.singleton value (IntSet.singleton (fixtureRowId row)))
      | (slot, value) <- IntMap.toAscList (fixtureRowValues row)
    ]

observedSlotValues :: [FixtureSource] -> IntMap IntSet
observedSlotValues sources =
  IntMap.unionsWith
    IntSet.union
    [ IntMap.map IntSet.singleton (fixtureRowValues row)
      | source <- sources,
        row <- fixtureSourceRows source
    ]

totalAssignments :: [Slot] -> IntMap IntSet -> [Env Int]
totalAssignments slots observed =
  fmap IntMap.fromList (sequenceA (fmap slotBindings slots))
  where
    slotBindings slot =
      fmap
        (\value -> (slot, value))
        (maybe [] IntSet.toAscList (IntMap.lookup slot observed))

sourceRowsAgreeing :: Env Int -> FixtureSource -> IntSet
sourceRowsAgreeing assignment source =
  agreeingRowIds assignment (fixtureSourceRows source)

constraintRowsAgreeing :: Env Int -> FixtureConstraint -> IntSet
constraintRowsAgreeing assignment constraint =
  agreeingRowIds assignment (fixtureConstraintRows constraint)

sourceHasDirtyWitness :: Env Int -> FixtureSource -> Bool
sourceHasDirtyWitness assignment source =
  not
    ( IntSet.null
        (IntSet.intersection (sourceRowsAgreeing assignment source) (fixtureSourceDirtyRows source))
    )

agreeingRowIds :: Env Int -> [FixtureRow] -> IntSet
agreeingRowIds assignment rows =
  IntSet.fromList
    [ fixtureRowId row
      | row <- rows,
        rowAgrees assignment row
    ]

rowAgrees :: Env Int -> FixtureRow -> Bool
rowAgrees assignment row =
  all
    (\(slot, value) -> IntMap.lookup slot assignment == Just value)
    (IntMap.toAscList (fixtureRowValues row))

fixtures :: [Fixture]
fixtures =
  [ triangleOneDirtyRow,
    triangleTwoDirtySources,
    emptyDirtySets,
    dirtyMarkPrunesSomeFullLeaves,
    constraintPrunesOtherwiseValidLeaves,
    emptySourceRows,
    dirtyRestrictedDomain
  ]

slotA :: Slot
slotA =
  0

slotB :: Slot
slotB =
  1

slotC :: Slot
slotC =
  2

triangleSlots :: [Slot]
triangleSlots =
  [slotA, slotB, slotC]

triangleOneDirtyRow :: Fixture
triangleOneDirtyRow =
  Fixture
    { fixtureName = "triangle join keeps the leaf supported by one dirty row",
      fixtureSlots = triangleSlots,
      fixtureSources =
        [ source [(0, [(slotA, 1), (slotB, 10)])] [0],
          source [(0, [(slotB, 10), (slotC, 100)])] [],
          source [(0, [(slotA, 1), (slotC, 100)])] []
        ],
      fixtureConstraints = []
    }

triangleTwoDirtySources :: Fixture
triangleTwoDirtySources =
  Fixture
    { fixtureName = "triangle join accepts witnesses from two dirty sources",
      fixtureSlots = triangleSlots,
      fixtureSources =
        [ source
            [ (0, [(slotA, 1), (slotB, 10)]),
              (1, [(slotA, 2), (slotB, 20)])
            ]
            [0],
          source
            [ (0, [(slotB, 10), (slotC, 100)]),
              (1, [(slotB, 20), (slotC, 200)])
            ]
            [1],
          source
            [ (0, [(slotA, 1), (slotC, 100)]),
              (1, [(slotA, 2), (slotC, 200)])
            ]
            []
        ],
      fixtureConstraints = []
    }

emptyDirtySets :: Fixture
emptyDirtySets =
  Fixture
    { fixtureName = "empty dirty sets yield no leaves",
      fixtureSlots = triangleSlots,
      fixtureSources =
        [ source [(0, [(slotA, 1), (slotB, 10)])] [],
          source [(0, [(slotB, 10), (slotC, 100)])] [],
          source [(0, [(slotA, 1), (slotC, 100)])] []
        ],
      fixtureConstraints = []
    }

dirtyMarkPrunesSomeFullLeaves :: Fixture
dirtyMarkPrunesSomeFullLeaves =
  Fixture
    { fixtureName = "dirty mark kills some but not all full join leaves",
      fixtureSlots = triangleSlots,
      fixtureSources =
        [ source
            [ (0, [(slotA, 1), (slotB, 10)]),
              (1, [(slotA, 2), (slotB, 20)]),
              (2, [(slotA, 3), (slotB, 30)])
            ]
            [],
          source
            [ (0, [(slotB, 10), (slotC, 100)]),
              (1, [(slotB, 20), (slotC, 200)]),
              (2, [(slotB, 30), (slotC, 300)])
            ]
            [],
          source
            [ (0, [(slotA, 1), (slotC, 100)]),
              (1, [(slotA, 2), (slotC, 200)]),
              (2, [(slotA, 3), (slotC, 300)])
            ]
            [0, 2]
        ],
      fixtureConstraints = []
    }

constraintPrunesOtherwiseValidLeaves :: Fixture
constraintPrunesOtherwiseValidLeaves =
  Fixture
    { fixtureName = "constraint prunes otherwise valid leaves",
      fixtureSlots = triangleSlots,
      fixtureSources =
        [ source
            [ (0, [(slotA, 1), (slotB, 10)]),
              (1, [(slotA, 2), (slotB, 20)])
            ]
            [0, 1],
          source
            [ (0, [(slotB, 10), (slotC, 100)]),
              (1, [(slotB, 20), (slotC, 200)])
            ]
            []
        ],
      fixtureConstraints =
        [ constraint
            [ (0, [(slotA, 1), (slotC, 100)])
            ]
        ]
    }

emptySourceRows :: Fixture
emptySourceRows =
  Fixture
    { fixtureName = "one empty source yields no leaves",
      fixtureSlots = [slotA, slotB],
      fixtureSources =
        [ source [] [],
          source [(0, [(slotA, 1), (slotB, 10)])] [0]
        ],
      fixtureConstraints = []
    }

dirtyRestrictedDomain :: Fixture
dirtyRestrictedDomain =
  Fixture
    { fixtureName = "dirty-restricted domain uses dirty values when every live dirty source touches the slot",
      fixtureSlots = triangleSlots,
      fixtureSources =
        [ source
            [ (0, [(slotA, 1), (slotB, 10)]),
              (1, [(slotA, 2), (slotB, 20)]),
              (2, [(slotA, 3), (slotB, 30)])
            ]
            [0],
          source
            [ (0, [(slotA, 1), (slotC, 100)]),
              (1, [(slotA, 2), (slotC, 200)]),
              (2, [(slotA, 3), (slotC, 300)])
            ]
            [0]
        ],
      fixtureConstraints = []
    }

source :: [(Int, [(Slot, Int)])] -> [Int] -> FixtureSource
source rows dirtyRows =
  FixtureSource
    { fixtureSourceRows = fmap row rows,
      fixtureSourceDirtyRows = IntSet.fromList dirtyRows
    }

constraint :: [(Int, [(Slot, Int)])] -> FixtureConstraint
constraint rows =
  FixtureConstraint
    { fixtureConstraintRows = fmap row rows
    }

row :: (Int, [(Slot, Int)]) -> FixtureRow
row (rowId, values) =
  FixtureRow
    { fixtureRowId = rowId,
      fixtureRowValues = IntMap.fromList values
    }
