module Moonlight.Flow.Carrier.Boundary.CoverageAtomRowsSpec
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Test.Moonlight.Flow.Carrier.Boundary.Coverage
  ( coverCoverageForAtomRows,
    coverRowsFromChildren,
    restrictionCoverageForAtomSchemas,
    sensitiveSlotsFromSchemas,
  )
import Moonlight.Flow.Carrier.Core.Coverage
  ( CoverageFact (..),
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Differential.Row.Block
import Moonlight.Flow.Model.RowIdentity
  ( rowBlockIdentityForAtom,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)
import Moonlight.Flow.Storage.Relation

tests :: TestTree
tests =
  testGroup
    "coverage"
    [ testCase "sensitive slots count repeated join variables" $
        sensitiveSlotsFromSchemas schemasJoinChain
          @?= IntMap.keysSet (IntMap.singleton 1 ())

    , testCase "restriction exact when atom rows restrict exactly and shared slots stay injective" $ do
        source <- expectRight sourceJoinChain
        target <- expectRight restrictedTargetJoinChainInjective
        restrictionCoverageForAtomSchemas
          schemasJoinChain
          injectiveTargetClasses
          source
          target
          @?= ExactRestricted

    , testCase "restriction lower bound when quotient merges a shared-slot domain" $ do
        source <- expectRight sourceJoinChain
        target <- expectRight restrictedTargetJoinChainMerged
        restrictionCoverageForAtomSchemas
          schemasJoinChain
          mergedTargetClasses
          source
          target
          @?= LowerBound

    , testCase "restriction lower bound when target atom relation has extra live rows" $ do
        source <- expectRight sourceJoinChain
        target <- expectRight extraTargetJoinChain
        restrictionCoverageForAtomSchemas
          schemasJoinChain
          injectiveTargetClasses
          source
          target
          @?= LowerBound

    , testCase "cover exact when compatible child rows reconstruct the parent relation" $ do
        parent <- expectRight parentRelExact
        leftChild <- expectRight leftChildRel
        rightChild <- expectRight rightChildRel
        coverCoverageForAtomRows
          16
          parentClasses
          meetMaps
          coverSchema
          parent
          [("L", leftChild), ("R", rightChild)]
          @?= ExactAmalgamated

        coverRowsFromChildren
          parentClasses
          meetMaps
          [("L", leftChild), ("R", rightChild)]
          @?= HashSet.fromList [row [1, 10], row [2, 20]]

    , testCase "cover lower bound when compatible child rows generate parent rows absent from the parent relation" $ do
        parent <- expectRight parentRelMissing
        leftChild <- expectRight leftChildRel
        rightChild <- expectRight rightChildRel
        coverCoverageForAtomRows
          16
          parentClasses
          meetMaps
          coverSchema
          parent
          [("L", leftChild), ("R", rightChild)]
          @?= LowerBound

    , testCase "cover lower bound when the compatibility product exceeds the configured cap" $ do
        parent <- expectRight parentRelExact
        leftChild <- expectRight leftChildRel
        rightChild <- expectRight rightChildRel
        coverCoverageForAtomRows
          1
          parentClasses
          meetMaps
          coverSchema
          parent
          [("L", leftChild), ("R", rightChild)]
          @?= LowerBound
    ]

expectRight :: Show obstruction => Either obstruction value -> IO value
expectRight result =
  case result of
    Right value ->
      pure value
    Left obstruction ->
      assertFailure ("unexpected row build obstruction in test: " <> show obstruction)

slot :: Int -> SlotId
slot =
  mkSlotId

row :: [Int] -> RowTupleKey
row = tupleKeyFromInts

relation :: [Int] -> [[Int]] -> Either RowBuildError (RowBlock 'Canonical)
relation schema rows =
  atomRowsFromTupleKeys
    (relationIdentity schema)
    (Vector.fromList (fmap mkSlotId schema))
    (fmap row rows)

relations :: [(Int, Either RowBuildError (RowBlock 'Canonical))] -> Either RowBuildError (IntMap (RowBlock 'Canonical))
relations =
  fmap IntMap.fromList . traverse relationEntry
  where
    relationEntry ::
      (Int, Either RowBuildError (RowBlock 'Canonical)) ->
      Either RowBuildError (Int, RowBlock 'Canonical)
    relationEntry (atomKey, relationValue) =
      fmap (\atomRelation -> (atomKey, atomRelation)) relationValue

relationIdentity :: [Int] -> RowBlockIdentity
relationIdentity schema =
  rowBlockIdentityForAtom
    0
    0
    0
    (mkAtomId (foldl (\acc slotKey -> acc * 167 + slotKey) 1 schema))
    0

classes :: [(Int, Int)] -> IntMap Int
classes =
  IntMap.fromList . fmap (\(leftKey, rightKey) -> (leftKey, rightKey))

schemasJoinChain :: IntMap (Vector.Vector SlotId)
schemasJoinChain =
  IntMap.fromList
    [ (0, Vector.fromList [slot 0, slot 1]),
      (1, Vector.fromList [slot 1, slot 2])
    ]

sourceJoinChain :: Either RowBuildError (IntMap (RowBlock 'Canonical))
sourceJoinChain =
  relations
    [ (0, relation [0, 1] [[1, 10], [2, 20]]),
      (1, relation [1, 2] [[10, 30], [20, 40]])
    ]

injectiveTargetClasses :: IntMap Int
injectiveTargetClasses =
  classes
    [ (1, 1),
      (2, 2),
      (10, 110),
      (20, 120),
      (30, 130),
      (40, 140)
    ]

mergedTargetClasses :: IntMap Int
mergedTargetClasses =
  classes
    [ (1, 1),
      (2, 2),
      (10, 100),
      (20, 100),
      (30, 130),
      (40, 140)
    ]

restrictedTargetJoinChainInjective :: Either RowBuildError (IntMap (RowBlock 'Canonical))
restrictedTargetJoinChainInjective =
  relations
    [ (0, relation [0, 1] [[1, 110], [2, 120]]),
      (1, relation [1, 2] [[110, 130], [120, 140]])
    ]

restrictedTargetJoinChainMerged :: Either RowBuildError (IntMap (RowBlock 'Canonical))
restrictedTargetJoinChainMerged =
  relations
    [ (0, relation [0, 1] [[1, 100], [2, 100]]),
      (1, relation [1, 2] [[100, 130], [100, 140]])
    ]

extraTargetJoinChain :: Either RowBuildError (IntMap (RowBlock 'Canonical))
extraTargetJoinChain =
  relations
    [ (0, relation [0, 1] [[1, 110], [2, 120]]),
      (1, relation [1, 2] [[110, 130], [120, 140], [110, 140]])
    ]

coverSchema :: Vector.Vector SlotId
coverSchema =
  Vector.fromList [slot 0, slot 1]

parentClasses :: IntMap Int
parentClasses =
  classes
    [ (11, 1),
      (12, 2),
      (21, 1),
      (22, 2),
      (101, 10),
      (102, 20),
      (201, 10),
      (202, 20)
    ]

meetMaps :: Map (String, String) (IntMap Int)
meetMaps =
  Map.fromList
    [(("L", "R"), parentClasses)]

leftChildRel :: Either RowBuildError (RowBlock 'Canonical)
leftChildRel =
  relation [0, 1] [[11, 101], [12, 102]]

rightChildRel :: Either RowBuildError (RowBlock 'Canonical)
rightChildRel =
  relation [0, 1] [[21, 201], [22, 202]]

parentRelExact :: Either RowBuildError (RowBlock 'Canonical)
parentRelExact =
  relation [0, 1] [[1, 10], [2, 20]]

parentRelMissing :: Either RowBuildError (RowBlock 'Canonical)
parentRelMissing =
  relation [0, 1] [[1, 10]]
