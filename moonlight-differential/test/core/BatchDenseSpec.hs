{-# LANGUAGE DerivingStrategies #-}

module BatchDenseSpec
  ( tests,
  )
where

import Moonlight.Delta.Frontier
  ( frontierPoints,
    upperFrontierPoints,
  )
import Moonlight.Differential.Algebra.ZSet qualified as ZSet
import Moonlight.Differential.Batch
  ( Batch,
    BatchMergeFuel (..),
    BatchMergeWork (..),
    batchCoverNull,
    batchCoverPlan,
    batchLower,
    batchMergeDone,
    batchNull,
    batchRowCount,
    batchToUpdates,
    batchUpper,
    beginBatchMerge,
    emptyBatch,
    finishBatchMerge,
    foldBatch,
    foldBatchKey,
    foldBatchKeyRows,
    fromUpdates,
    fromUpdatesDense,
    mergeBatch,
    mergeBatches,
    workBatchMerge,
    workBatchMergeMeasured,
  )
import Moonlight.Differential.Update
  ( Update (..),
  )
import Numeric.Natural
  ( Natural,
  )
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    chooseInt,
    conjoin,
    counterexample,
    frequency,
    oneof,
    shrinkList,
    suchThat,
    vectorOf,
    (===),
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.QuickCheck
  ( testProperty,
  )

type TestUpdate = Update Int Int Int Int

type TestBatch = Batch Int Int Int Int

boxed :: [TestUpdate] -> TestBatch
boxed =
  fromUpdates

dense :: [TestUpdate] -> TestBatch
dense =
  fromUpdatesDense

tests :: TestTree
tests =
  testGroup
    "batch dense arm"
    [ testProperty "dense arm equals the boxed batch of the same updates" denseEqualsBoxedBatch,
      testProperty "dense arm agrees on every observation door" denseAgreesOnObservations,
      testProperty "merge agrees across all dense/boxed arm mixes" denseMergeAgreesAcrossArms,
      testProperty "mergeBatches agrees across mixed-arm covers" denseMergeBatchesAgrees,
      testProperty "fueled merger drains to the direct merge for all arm mixes" denseFueledMergeAgrees,
      testProperty "dense arm coheres with the batch semigroup and monoid" denseSemigroupCoheres,
      testProperty "cover plan and cover null agree between arms" denseCoverPlanAgrees
    ]

denseEqualsBoxedBatch :: UpdateSeq -> Property
denseEqualsBoxedBatch (UpdateSeq us) =
  conjoin
    [ dense us === boxed us,
      compare (dense us) (boxed us) === EQ
    ]

denseAgreesOnObservations :: UpdateSeq -> Property
denseAgreesOnObservations (UpdateSeq us) =
  let denseBatch = dense us
      boxedBatch = boxed us
   in conjoin
        [ batchToUpdates denseBatch === batchToUpdates boxedBatch,
          foldedCells denseBatch === foldedCells boxedBatch,
          collectKeyRows denseBatch === collectKeyRows boxedBatch,
          batchRowCount denseBatch === batchRowCount boxedBatch,
          batchNull denseBatch === batchNull boxedBatch,
          frontierPoints (batchLower denseBatch) === frontierPoints (batchLower boxedBatch),
          upperFrontierPoints (batchUpper denseBatch) === upperFrontierPoints (batchUpper boxedBatch),
          conjoin
            [ foldedKey key denseBatch === foldedKey key boxedBatch
              | key <- observedKeys
            ]
        ]
  where
    observedKeys =
      999 : [0 .. 15]

denseMergeAgreesAcrossArms :: UpdateSeq -> UpdateSeq -> Property
denseMergeAgreesAcrossArms (UpdateSeq a) (UpdateSeq b) =
  let denseA = dense a
      boxedA = boxed a
      denseB = dense b
      boxedB = boxed b
      oracle = mergeBatch boxedA boxedB
   in conjoin
        [ mergeBatch denseA denseB === oracle,
          mergeBatch denseA boxedB === oracle,
          mergeBatch boxedA denseB === oracle,
          mergeBatch boxedA boxedB === oracle
        ]

denseMergeBatchesAgrees :: BatchCover -> Property
denseMergeBatchesAgrees (BatchCover members) =
  let denseBatches = fmap dense members
      boxedBatches = fmap boxed members
      mixedBatches = zipWith pickArm [0 :: Int ..] members
      pickArm index member =
        if even index
          then dense member
          else boxed member
   in conjoin
        [ mergeBatches denseBatches === mergeBatches boxedBatches,
          mergeBatches mixedBatches === mergeBatches boxedBatches
        ]

denseFueledMergeAgrees :: UpdateSeq -> UpdateSeq -> Property
denseFueledMergeAgrees (UpdateSeq a) (UpdateSeq b) =
  conjoin
    [ conjoin
        [ drainMerge fuel left right === drainMerge fuel (boxed a) (boxed b)
          | fuel <- fuels,
            (left, right) <- armMixes a b
        ],
      -- mergeBatch short-circuits a null input and keeps the survivor's
      -- frontiers; the fueled path always merges frontiers, so cross-path
      -- equality is row-level unless both inputs are non-null.
      conjoin
        [ batchToUpdates (drainMerge fuel left right) === batchToUpdates (mergeBatch left right)
          | fuel <- fuels,
            (left, right) <- armMixes a b
        ],
      conjoin
        [ drainMerge fuel left right === mergeBatch left right
          | fuel <- fuels,
            (left, right) <- armMixes a b,
            not (batchNull left || batchNull right)
        ],
      conjoin
        [ counterexample
            "measured merger consumed more fuel than requested"
            (measuredWithinRequest fuel left right)
          | fuel <- fuels,
            (left, right) <- armMixes a b
        ]
    ]
  where
    fuels =
      [1, 3, 7] :: [Natural]

denseSemigroupCoheres :: UpdateSeq -> UpdateSeq -> Property
denseSemigroupCoheres (UpdateSeq a) (UpdateSeq b) =
  conjoin
    [ (dense a <> dense b) === (boxed a <> boxed b),
      dense [] === (emptyBatch :: TestBatch),
      dense [] === (mempty :: TestBatch)
    ]

denseCoverPlanAgrees :: BatchCover -> Property
denseCoverPlanAgrees (BatchCover members) =
  let denseBatches = fmap dense members
      boxedBatches = fmap boxed members
   in conjoin
        [ batchCoverPlan denseBatches === batchCoverPlan boxedBatches,
          batchCoverNull denseBatches === batchCoverNull boxedBatches
        ]

armMixes :: [TestUpdate] -> [TestUpdate] -> [(TestBatch, TestBatch)]
armMixes a b =
  [ (dense a, dense b),
    (dense a, boxed b),
    (boxed a, dense b),
    (boxed a, boxed b)
  ]

drainMerge :: Natural -> TestBatch -> TestBatch -> TestBatch
drainMerge fuel left right =
  finishBatchMerge (drive (beginBatchMerge left right))
  where
    drive merger
      | batchMergeDone merger =
          merger
      | otherwise =
          drive (workBatchMerge (BatchMergeFuel fuel) merger)

measuredWithinRequest :: Natural -> TestBatch -> TestBatch -> Bool
measuredWithinRequest fuel left right =
  drive (beginBatchMerge left right)
  where
    drive merger
      | batchMergeDone merger =
          True
      | otherwise =
          let work = workBatchMergeMeasured (BatchMergeFuel fuel) merger
           in batchMergeFuelConsumed work <= fuel
                && drive (batchMergeWorkMerger work)

foldedCells :: TestBatch -> [(Int, Int, Int, Int)]
foldedCells =
  reverse . foldBatch (\acc time key val weight -> (time, key, val, weight) : acc) []

foldedKey :: Int -> TestBatch -> [(Int, Int, Int)]
foldedKey key =
  reverse . foldBatchKey key (\acc time val weight -> (time, val, weight) : acc) []

collectKeyRows :: TestBatch -> [(Int, [(ZSet.Timed Int Int, Int)])]
collectKeyRows =
  reverse . foldBatchKeyRows (\acc key rows -> (key, ZSet.zsetToAscList rows) : acc) []

newtype UpdateSeq = UpdateSeq [TestUpdate]
  deriving stock (Show)

instance Arbitrary UpdateSeq where
  arbitrary =
    UpdateSeq <$> genUpdates
  shrink (UpdateSeq us) =
    UpdateSeq <$> shrinkList (const []) us

newtype BatchCover = BatchCover [[TestUpdate]]
  deriving stock (Show)

instance Arbitrary BatchCover where
  arbitrary =
    BatchCover <$> oneof [genMixedCover, genDisjointBandedCover]
  shrink (BatchCover members) =
    BatchCover <$> shrinkList (const []) members

genUpdates :: Gen [TestUpdate]
genUpdates =
  frequency
    [ (3, chooseInt (0, 40) >>= genUpdatesFromEvents),
      (1, chooseInt (160, 300) >>= genUpdatesFromEvents)
    ]

genUpdatesFromEvents :: Int -> Gen [TestUpdate]
genUpdatesFromEvents eventCount =
  concat <$> vectorOf eventCount genUpdateEvent

genUpdateEvent :: Gen [TestUpdate]
genUpdateEvent =
  frequency
    [ (4, genSingleCell),
      (2, genDuplicateCell),
      (2, genCancelingCell),
      (1, genZeroCell)
    ]

genSingleCell :: Gen [TestUpdate]
genSingleCell = do
  cell <- genCell
  weight <- genWeight
  pure [updateOf cell weight]

genDuplicateCell :: Gen [TestUpdate]
genDuplicateCell = do
  cell <- genCell
  first <- genWeight
  second <- genWeight `suchThat` (/= first)
  pure [updateOf cell first, updateOf cell second]

genCancelingCell :: Gen [TestUpdate]
genCancelingCell = do
  cell <- genCell
  weight <- genNonZeroWeight
  pure [updateOf cell weight, updateOf cell (negate weight)]

genZeroCell :: Gen [TestUpdate]
genZeroCell = do
  cell <- genCell
  pure [updateOf cell 0]

genMixedCover :: Gen [[TestUpdate]]
genMixedCover = do
  memberCount <- chooseInt (3, 6)
  vectorOf memberCount genCoverMember

genCoverMember :: Gen [TestUpdate]
genCoverMember =
  frequency
    [ (1, pure []),
      (4, genTinyUpdates),
      (2, chooseInt (0, 24) >>= genUpdatesFromEvents)
    ]

genTinyUpdates :: Gen [TestUpdate]
genTinyUpdates = do
  rowCount <- chooseInt (0, 8)
  vectorOf rowCount (updateOf <$> genCell <*> genNonZeroWeight)

genDisjointBandedCover :: Gen [[TestUpdate]]
genDisjointBandedCover = do
  memberCount <- chooseInt (3, 6)
  traverse genBandMember [0 .. memberCount - 1]

genBandMember :: Int -> Gen [TestUpdate]
genBandMember band = do
  rowCount <- chooseInt (1, 6)
  vectorOf rowCount $ do
    key <- chooseInt (0, 7)
    val <- chooseInt (0, 15)
    time <- chooseInt (0, 15)
    weight <- genNonZeroWeight
    pure (Update time (band * 16 + key) val weight)

genCell :: Gen (Int, Int, Int)
genCell =
  (,,) <$> chooseInt (0, 15) <*> chooseInt (0, 15) <*> chooseInt (0, 15)

genWeight :: Gen Int
genWeight =
  chooseInt (-3, 3)

genNonZeroWeight :: Gen Int
genNonZeroWeight =
  genWeight `suchThat` (/= 0)

updateOf :: (Int, Int, Int) -> Int -> TestUpdate
updateOf (time, key, val) weight =
  Update time key val weight
