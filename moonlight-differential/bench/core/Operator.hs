module Operator where

import Control.DeepSeq (NFData (..))
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Moonlight.Differential.Algebra.ZSet qualified as ZSet
import Moonlight.Differential.Arrangement (Arrangement, arrangeByKey, foldArrangement, foldArrangementKey)
import Common (eitherShow, valueAt, weightAt)
import WCOJ (pathGraphEdges)
import Moonlight.Differential.Collection qualified as Collection
import Moonlight.Differential.Operator.Aggregate qualified as Aggregate
import Moonlight.Differential.Operator.Fixpoint
import Moonlight.Differential.Operator.Join (joinIndexed)
import Moonlight.Differential.Operator.Linear qualified as Linear
import Moonlight.Differential.Trace (traceFromUpdates)
import Moonlight.Differential.Update (Update (..))

type OperatorZSet = ZSet.ZSet Int Int
type OperatorIndexedZSet = ZSet.IndexedZSet Int Int Int
type OperatorCollection = Collection.Collection Int Int
type OperatorRightCollection = Collection.Collection (Int, Char) Int

newtype PreparedOperatorZSet = PreparedOperatorZSet OperatorZSet

instance NFData PreparedOperatorZSet where
  rnf (PreparedOperatorZSet rows) =
    length (ZSet.zsetToAscList rows) `seq` ()

data PreparedCollectionEdslComparison = PreparedCollectionEdslComparison
  { preparedRawCollectionLeft :: !OperatorZSet,
    preparedRawCollectionRight :: !(ZSet.ZSet (Int, Char) Int),
    preparedEdslCollectionLeft :: !OperatorCollection,
    preparedEdslCollectionRight :: !OperatorRightCollection
  }

instance NFData PreparedCollectionEdslComparison where
  rnf preparedCase =
    length (ZSet.zsetToAscList (preparedRawCollectionLeft preparedCase))
      `seq` length (ZSet.zsetToAscList (preparedRawCollectionRight preparedCase))
      `seq` length (Collection.collectionToAscList (preparedEdslCollectionLeft preparedCase))
      `seq` length (Collection.collectionToAscList (preparedEdslCollectionRight preparedCase))
      `seq` ()

data PreparedDistinctCase = PreparedDistinctCase
  { preparedDistinctIntegrated :: !OperatorZSet,
    preparedDistinctDelta :: !OperatorZSet
  }

instance NFData PreparedDistinctCase where
  rnf preparedCase =
    length (ZSet.zsetToAscList (preparedDistinctIntegrated preparedCase))
      `seq` length (ZSet.zsetToAscList (preparedDistinctDelta preparedCase))
      `seq` ()

data PreparedGroupViewCase = PreparedGroupViewCase
  { preparedGroupViewInitial :: !OperatorIndexedZSet,
    preparedGroupViewDelta :: !OperatorIndexedZSet
  }

instance NFData PreparedGroupViewCase where
  rnf preparedCase =
    ZSet.indexedZSetCellCount (preparedGroupViewInitial preparedCase)
      `seq` ZSet.indexedZSetCellCount (preparedGroupViewDelta preparedCase)
      `seq` ()

data PreparedFixpointCase = PreparedFixpointCase
  { preparedFixpointLimit :: !Int,
    preparedFixpointSeed :: !OperatorZSet
  }

instance NFData PreparedFixpointCase where
  rnf preparedCase =
    preparedFixpointLimit preparedCase
      `seq` length (ZSet.zsetToAscList (preparedFixpointSeed preparedCase))
      `seq` ()

data PreparedArrangedFixpointCase = PreparedArrangedFixpointCase
  { preparedArrangedFixpointLimit :: !Int,
    preparedArrangedFixpointSeed :: !OperatorZSet,
    preparedArrangedFixpointEdges :: !(Arrangement Int Int Int Int)
  }

instance NFData PreparedArrangedFixpointCase where
  rnf preparedCase =
    preparedArrangedFixpointLimit preparedCase
      `seq` length (ZSet.zsetToAscList (preparedArrangedFixpointSeed preparedCase))
      `seq` arrangementWeight (preparedArrangedFixpointEdges preparedCase)
      `seq` ()

operatorSizes :: [Int]
operatorSizes =
  [512, 2048]

operatorZSetCase :: Int -> PreparedOperatorZSet
operatorZSetCase size =
  PreparedOperatorZSet
    (ZSet.zsetFromList (fmap (\index -> (index, weightAt index)) [0 .. size - 1]))

collectionEdslComparisonCase :: Int -> PreparedCollectionEdslComparison
collectionEdslComparisonCase size =
  PreparedCollectionEdslComparison
    { preparedRawCollectionLeft = ZSet.zsetFromList leftRows,
      preparedRawCollectionRight = ZSet.zsetFromList rightRows,
      preparedEdslCollectionLeft = Collection.collectionFromList leftRows,
      preparedEdslCollectionRight = Collection.collectionFromList rightRows
    }
  where
    leftRows =
      fmap (\index -> (index, weightAt index)) [0 .. size - 1]

    rightRows =
      fmap (\index -> ((index, valueAt index), weightAt (index + size))) [0 .. size - 1]

operatorDistinctCase :: Int -> PreparedDistinctCase
operatorDistinctCase size =
  PreparedDistinctCase
    { preparedDistinctIntegrated =
        ZSet.zsetFromList (fmap (\index -> (index, 1 :: Int)) [0 .. size - 1]),
      preparedDistinctDelta =
        ZSet.zsetFromList
          ( fmap
              ( \index ->
                  if even index
                    then (index, negate (1 :: Int))
                    else (index + size, 1 :: Int)
              )
              [0 .. size - 1]
          )
    }

operatorGroupViewCase :: Int -> PreparedGroupViewCase
operatorGroupViewCase size =
  PreparedGroupViewCase
    { preparedGroupViewInitial =
        ZSet.indexedZSetFromList
          (fmap (\index -> (index `mod` 64, index, weightAt index)) [0 .. size - 1]),
      preparedGroupViewDelta =
        ZSet.indexedZSetFromList
          (fmap (\index -> (index `mod` 64, index + size, weightAt (index + size))) [0 .. size - 1])
    }

fixpointPathCase :: Int -> PreparedFixpointCase
fixpointPathCase size =
  PreparedFixpointCase
    { preparedFixpointLimit = size,
      preparedFixpointSeed = ZSet.zsetSingleton 0 (1 :: Int)
    }

arrangedFixpointPathCase :: Int -> PreparedArrangedFixpointCase
arrangedFixpointPathCase size =
  PreparedArrangedFixpointCase
    { preparedArrangedFixpointLimit = size,
      preparedArrangedFixpointSeed = ZSet.zsetSingleton 0 (1 :: Int),
      preparedArrangedFixpointEdges =
        arrangeByKey
          ( traceFromUpdates
              ( fmap
                  ( \(source, target) ->
                      Update
                        { updateTime = 0,
                          updateKey = source,
                          updateVal = target,
                          updateWeight = 1 :: Int
                        }
                  )
                  (pathGraphEdges size)
              )
          )
    }

operatorLinearPipelineWeight :: PreparedOperatorZSet -> Int
operatorLinearPipelineWeight (PreparedOperatorZSet rows) =
  zsetWeightSum (Linear.filterZSet even (Linear.mapZSet (+ 1) rows))

operatorIndexCountWeight :: PreparedOperatorZSet -> Int
operatorIndexCountWeight (PreparedOperatorZSet rows) =
  zsetWeightSum (Aggregate.countByKey (Linear.indexBy (`mod` 64) rows))

rawMapFilterWeight :: PreparedCollectionEdslComparison -> Int
rawMapFilterWeight preparedCase =
  zsetWeightSum (Linear.filterZSet even (Linear.mapZSet (+ 1) (preparedRawCollectionLeft preparedCase)))

edslMapFilterWeight :: PreparedCollectionEdslComparison -> Int
edslMapFilterWeight preparedCase =
  collectionWeightSum
    ( Collection.filterCollection
        even
        (Collection.mapCollection (+ 1) (preparedEdslCollectionLeft preparedCase))
    )

rawFlatMapWeight :: PreparedCollectionEdslComparison -> Int
rawFlatMapWeight preparedCase =
  zsetWeightSum
    ( ZSet.zsetFold
        collectOutputs
        ZSet.zsetEmpty
        (preparedRawCollectionLeft preparedCase)
    )
  where
    collectOutputs :: ZSet.ZSet Int Int -> Int -> Int -> ZSet.ZSet Int Int
    collectOutputs acc value weight =
      ZSet.zsetInsert
        (value + 10)
        weight
        (ZSet.zsetInsert value weight acc)

edslFlatMapWeight :: PreparedCollectionEdslComparison -> Int
edslFlatMapWeight preparedCase =
  collectionWeightSum
    ( Collection.flatMapCollection
        (\value -> [value, value + 10])
        (preparedEdslCollectionLeft preparedCase)
    )

rawGroupAlgebraWeight :: PreparedCollectionEdslComparison -> Int
rawGroupAlgebraWeight preparedCase =
  zsetWeightSum
    ( ZSet.zsetDifference
        (preparedRawCollectionLeft preparedCase <> mapped)
        mapped
    )
    + zsetWeightSum
      (preparedRawCollectionLeft preparedCase <> ZSet.zsetNegate (preparedRawCollectionLeft preparedCase))
  where
    mapped =
      Linear.mapZSet (+ 1) (preparedRawCollectionLeft preparedCase)

edslGroupAlgebraWeight :: PreparedCollectionEdslComparison -> Int
edslGroupAlgebraWeight preparedCase =
  collectionWeightSum
    ( Collection.differenceCollections
        (Collection.concatCollections (preparedEdslCollectionLeft preparedCase) mapped)
        mapped
    )
    + collectionWeightSum
      ( Collection.concatCollections
          (preparedEdslCollectionLeft preparedCase)
          (Collection.negateCollection (preparedEdslCollectionLeft preparedCase))
      )
  where
    mapped =
      Collection.mapCollection (+ 1) (preparedEdslCollectionLeft preparedCase)

rawIndexCountWeight :: PreparedCollectionEdslComparison -> Int
rawIndexCountWeight preparedCase =
  zsetWeightSum (Aggregate.countByKey (Linear.indexBy (`mod` 64) (preparedRawCollectionLeft preparedCase)))

edslIndexCountWeight :: PreparedCollectionEdslComparison -> Int
edslIndexCountWeight preparedCase =
  collectionWeightSum
    ( Collection.countCollectionByKey
        (Collection.indexCollectionBy (`mod` 64) (preparedEdslCollectionLeft preparedCase))
    )

rawIndexDeindexWeight :: PreparedCollectionEdslComparison -> Int
rawIndexDeindexWeight preparedCase =
  zsetWeightSum
    ( ZSet.indexedZSetFold
        collectKeyRows
        ZSet.zsetEmpty
        (Linear.indexBy (`mod` 64) (preparedRawCollectionLeft preparedCase))
    )
  where
    collectKeyRows :: ZSet.ZSet (Int, Int) Int -> Int -> ZSet.ZSet Int Int -> ZSet.ZSet (Int, Int) Int
    collectKeyRows acc key rows =
      ZSet.zsetFold
        (\indexedRows value weight -> ZSet.zsetInsert (key, value) weight indexedRows)
        acc
        rows

edslIndexDeindexWeight :: PreparedCollectionEdslComparison -> Int
edslIndexDeindexWeight preparedCase =
  collectionWeightSum
    ( Collection.deindexCollection
        (,)
        (Collection.indexCollectionBy (`mod` 64) (preparedEdslCollectionLeft preparedCase))
    )

rawJoinWeight :: PreparedCollectionEdslComparison -> Int
rawJoinWeight preparedCase =
  zsetWeightSum
    ( joinIndexed
        (Linear.indexBy id (preparedRawCollectionLeft preparedCase))
        (Linear.indexBy fst (preparedRawCollectionRight preparedCase))
    )

edslJoinWeight :: PreparedCollectionEdslComparison -> Int
edslJoinWeight preparedCase =
  collectionWeightSum
    ( Collection.joinCollections
        (Collection.indexCollectionBy id (preparedEdslCollectionLeft preparedCase))
        (Collection.indexCollectionBy fst (preparedEdslCollectionRight preparedCase))
    )

operatorDistinctDeltaWeight :: PreparedDistinctCase -> Int
operatorDistinctDeltaWeight preparedCase =
  zsetWeightSum
    ( Aggregate.distinctDelta
        (preparedDistinctIntegrated preparedCase)
        (preparedDistinctDelta preparedCase)
    )

operatorGroupViewAdvanceWeight :: PreparedGroupViewCase -> Int
operatorGroupViewAdvanceWeight preparedCase =
  let (_changes, advancedView) =
        Aggregate.groupViewAdvance
          indexedGroupWeight
          (preparedGroupViewDelta preparedCase)
          (Aggregate.mkGroupView indexedGroupWeight (preparedGroupViewInitial preparedCase))
   in Map.size (Aggregate.groupViewReduced advancedView)
        + ZSet.indexedZSetCellCount (Aggregate.groupViewIntegrated advancedView)

semiNaivePathWeight :: PreparedFixpointCase -> Either String Int
semiNaivePathWeight preparedCase =
  fmap
    (length . ZSet.zsetToAscList)
    ( eitherShow
        (semiNaiveFixpoint (SemiNaiveBudget (fromIntegral (preparedFixpointLimit preparedCase + 1))) step (preparedFixpointSeed preparedCase))
    )
  where
    step frontier =
      ZSet.zsetFold
        ( \acc node _weight ->
            if node + 1 < preparedFixpointLimit preparedCase
              then ZSet.zsetInsert (node + 1) (1 :: Int) acc
              else acc
        )
        ZSet.zsetEmpty
        frontier

arrangedSemiNaivePathWeight :: PreparedArrangedFixpointCase -> Either String Int
arrangedSemiNaivePathWeight preparedCase =
  fmap
    (length . ZSet.zsetToAscList)
    ( eitherShow
        (semiNaiveFixpoint (SemiNaiveBudget (fromIntegral (preparedArrangedFixpointLimit preparedCase + 1))) step (preparedArrangedFixpointSeed preparedCase))
    )
  where
    step frontier =
      ZSet.zsetFold
        ( \acc source weight ->
            if weight > 0
              then foldArrangementKey source insertArrangedPathTarget acc (preparedArrangedFixpointEdges preparedCase)
              else acc
        )
        ZSet.zsetEmpty
        frontier

insertArrangedPathTarget :: ZSet.ZSet Int Int -> Int -> Int -> Int -> ZSet.ZSet Int Int
insertArrangedPathTarget acc _time target weight =
  if weight > 0
    then ZSet.zsetInsert target 1 acc
    else acc

arrangementWeight :: Arrangement Int Int Int Int -> Int
arrangementWeight =
  foldArrangement (\acc time key val weight -> acc + time + key + val + weight) 0

zsetWeightSum :: ZSet.ZSet value Int -> Int
zsetWeightSum =
  ZSet.zsetFold (\acc _value weight -> acc + weight) 0

collectionWeightSum :: Collection.Collection value Int -> Int
collectionWeightSum =
  Foldable.foldl'
    (\acc (_value, weight) -> acc + weight)
    0
    . Collection.collectionToAscList


indexedGroupWeight :: ZSet.ZSet value Int -> Int
indexedGroupWeight =
  zsetWeightSum
