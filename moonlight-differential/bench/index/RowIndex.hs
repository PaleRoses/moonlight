module RowIndex where

import Control.DeepSeq (NFData (..))
import Control.Monad.Trans.State.Strict (evalStateT, execStateT)
import Data.Foldable qualified as Foldable
import Data.Functor.Identity (Identity (..))
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Common (eitherShow, weightAt)
import Moonlight.Differential.Context.RowsCache
import Moonlight.Differential.Index.IndexedRows
import Moonlight.Differential.Index.RowId
import Moonlight.Differential.Index.RowIdSet
import Moonlight.Differential.Index.RowSet
import Test.Tasty.Bench (Benchmark, bench, env, nf)

newtype PreparedRowIds = PreparedRowIds [RowId]

instance NFData PreparedRowIds where
  rnf (PreparedRowIds rowIds) =
    length rowIds `seq` ()

newtype PreparedRowIdSetPair = PreparedRowIdSetPair (RowIdSet, RowIdSet)

instance NFData PreparedRowIdSetPair where
  rnf (PreparedRowIdSetPair (leftSet, rightSet)) =
    rowIdSetSize leftSet `seq` rowIdSetSize rightSet `seq` ()

newtype PreparedRowSetPair = PreparedRowSetPair (RowSet, RowSet)

instance NFData PreparedRowSetPair where
  rnf (PreparedRowSetPair (leftSet, rightSet)) =
    rowSetSize leftSet `seq` rowSetSize rightSet `seq` ()

data PreparedRowIdSetIntersection = PreparedRowIdSetIntersection !RowIdSet !RowSet

instance NFData PreparedRowIdSetIntersection where
  rnf (PreparedRowIdSetIntersection rowIds rows) =
    rowIdSetSize rowIds `seq` rowSetSize rows `seq` ()

newtype PreparedRowSet = PreparedRowSet RowSet

instance NFData PreparedRowSet where
  rnf (PreparedRowSet rows) =
    rowSetSize rows `seq` ()

data PreparedRowSetTransition
  = PreparedRowSetInsertTransition !RowSet [RowId]
  | PreparedRowSetDeleteTransition !RowSet [RowId]

instance NFData PreparedRowSetTransition where
  rnf transition =
    case transition of
      PreparedRowSetInsertTransition rows rowIds ->
        rowSetSize rows `seq` length rowIds `seq` ()
      PreparedRowSetDeleteTransition rows rowIds ->
        rowSetSize rows `seq` length rowIds `seq` ()

rowIndexSizes :: [Int]
rowIndexSizes =
  [32, 512, 2048]

rowIdsForSize :: Int -> PreparedRowIds
rowIdsForSize size =
  PreparedRowIds (mapMaybe (either (const Nothing) Just . mkRowId) [0 .. size - 1])

rowIdSetInsertFoldWeight :: PreparedRowIds -> Int
rowIdSetInsertFoldWeight (PreparedRowIds rowIds) =
  rowIdSetFoldl'
    (\acc _rowId -> acc + 1)
    0
    (Foldable.foldl' (flip rowIdSetInsert) (rowIdSetFromList []) rowIds)

rowIdSetMemberSweepWeight :: PreparedRowIds -> Int
rowIdSetMemberSweepWeight (PreparedRowIds rowIds) =
  let rowSetValue =
        rowIdSetFromList rowIds
   in length (filter (`rowIdSetMember` rowSetValue) rowIds)

rowIdSetUnionCase :: Int -> PreparedRowIdSetPair
rowIdSetUnionCase size =
  let PreparedRowIds leftIds =
        rowIdsForSize size
      PreparedRowIds rightIds =
        rowIdsForSize (size * 2)
   in PreparedRowIdSetPair (rowIdSetFromList leftIds, rowIdSetFromList rightIds)

rowIdSetUnionWeight :: PreparedRowIdSetPair -> Int
rowIdSetUnionWeight (PreparedRowIdSetPair (leftSet, rightSet)) =
  rowIdSetSize (rowIdSetUnion leftSet rightSet)

rowSetInsertFoldWeight :: PreparedRowIds -> Int
rowSetInsertFoldWeight (PreparedRowIds rowIds) =
  rowSetFoldl'
    (\acc _rowId -> acc + 1)
    0
    (Foldable.foldl' (flip rowSetInsert) (rowSetFromIntSetWithUniverse 0 IntSet.empty) rowIds)

rowSetUnionCase :: Int -> PreparedRowSetPair
rowSetUnionCase size =
  PreparedRowSetPair
    ( rowSetFromIntSetWithUniverse (size * 2) (IntSet.fromList [0, 2 .. size * 2 - 1]),
      rowSetFromIntSetWithUniverse (size * 2) (IntSet.fromList [1, 3 .. size * 2 - 1])
    )

rowSetUnionIntersectionWeight :: PreparedRowSetPair -> Int
rowSetUnionIntersectionWeight (PreparedRowSetPair (leftSet, rightSet)) =
  rowSetSize (rowSetUnion leftSet rightSet) + rowSetSize (rowSetIntersection leftSet rightSet)

rowSetIntersectsWitnessCase :: Int -> PreparedRowSetPair
rowSetIntersectsWitnessCase size =
  PreparedRowSetPair
    ( rowSetFromIntSetWithUniverse (size * 16) (IntSet.fromDistinctAscList [0, 2 .. size * 2]),
      rowSetFromIntSetWithUniverse (size * 16) (IntSet.insert 0 (IntSet.fromDistinctAscList [size * 4 + 1, size * 4 + 3 .. size * 6]))
    )

rowSetIntersectsNoWitnessCase :: Int -> PreparedRowSetPair
rowSetIntersectsNoWitnessCase size =
  PreparedRowSetPair
    ( rowSetFromIntSetWithUniverse (size * 16) (IntSet.fromDistinctAscList [0, 2 .. size * 2]),
      rowSetFromIntSetWithUniverse (size * 16) (IntSet.fromDistinctAscList [1, 3 .. size * 2 + 1])
    )

rowSetIntersectsLateWitnessCase :: Int -> PreparedRowSetPair
rowSetIntersectsLateWitnessCase size =
  PreparedRowSetPair
    ( rowSetFromIntSetWithUniverse (size * 16) (IntSet.fromDistinctAscList [0, 2 .. size * 2]),
      rowSetFromIntSetWithUniverse (size * 16) (IntSet.insert (size * 2) (IntSet.fromDistinctAscList [1, 3 .. size * 2 - 1]))
    )

rowSetIntersectsWeight :: PreparedRowSetPair -> Int
rowSetIntersectsWeight (PreparedRowSetPair (leftSet, rightSet)) =
  if rowSetIntersects leftSet rightSet then 1 else 0

rowIdSetIntersectsWitnessCase :: Int -> PreparedRowIdSetIntersection
rowIdSetIntersectsWitnessCase size =
  PreparedRowIdSetIntersection
    (rowIdSetFromList (rowIdsBetween 0 size))
    (rowSetFromIntSetWithUniverse (size * 16) (IntSet.insert 0 (IntSet.fromDistinctAscList [size * 4 + 1, size * 4 + 3 .. size * 6])))

rowIdSetIntersectsNoWitnessCase :: Int -> PreparedRowIdSetIntersection
rowIdSetIntersectsNoWitnessCase size =
  PreparedRowIdSetIntersection
    (rowIdSetFromList (rowIdsBetween 0 size))
    (rowSetFromIntSetWithUniverse (size * 16) (IntSet.fromDistinctAscList [size * 4 .. size * 5]))

rowIdSetIntersectsLateWitnessCase :: Int -> PreparedRowIdSetIntersection
rowIdSetIntersectsLateWitnessCase size =
  PreparedRowIdSetIntersection
    (rowIdSetFromList (rowIdsBetween 0 size))
    (rowSetFromIntSetWithUniverse (size * 16) (IntSet.singleton (size - 1)))

rowIdSetIntersectsWeight :: PreparedRowIdSetIntersection -> Int
rowIdSetIntersectsWeight (PreparedRowIdSetIntersection rowIds rows) =
  if rowSetIntersectsRowIdSet rowIds rows then 1 else 0

rowSetDenseCase :: Int -> PreparedRowSet
rowSetDenseCase size =
  PreparedRowSet (rowSetFullRange (max 1024 size))

rowSetMemberFoldWeight :: PreparedRowSet -> Int
rowSetMemberFoldWeight (PreparedRowSet rows) =
  rowSetFoldl'
    (\acc rowId -> if rowSetMember rowId rows then acc + 1 else acc)
    0
    rows

rowSetThresholdBenchmarks :: [Benchmark]
rowSetThresholdBenchmarks =
  [ env (pure smallToSparseTransition) $ \transition ->
      bench "small->sparse insert" (nf rowSetTransitionWeight transition),
    env (pure sparseToDenseTransition) $ \transition ->
      bench "sparse->dense insert" (nf rowSetTransitionWeight transition),
    env (pure denseToSparseTransition) $ \transition ->
      bench "dense->sparse delete" (nf rowSetTransitionWeight transition)
  ]

smallToSparseTransition :: PreparedRowSetTransition
smallToSparseTransition =
  PreparedRowSetInsertTransition
    (rowSetFromIntSetWithUniverse rowSetDenseMinUniverse (IntSet.fromDistinctAscList [0 .. 31]))
    (rowIdsBetween 32 1)

sparseToDenseTransition :: PreparedRowSetTransition
sparseToDenseTransition =
  PreparedRowSetInsertTransition
    (rowSetFromIntSetWithUniverse rowSetDenseMinUniverse (IntSet.fromDistinctAscList [0 .. 126]))
    (rowIdsBetween 127 1)

denseToSparseTransition :: PreparedRowSetTransition
denseToSparseTransition =
  PreparedRowSetDeleteTransition
    (rowSetFullRange rowSetDenseMinUniverse)
    (rowIdsBetween 0 897)

rowSetTransitionWeight :: PreparedRowSetTransition -> Int
rowSetTransitionWeight transition =
  case transition of
    PreparedRowSetInsertTransition rows rowIds ->
      rowSetSize (Foldable.foldl' (flip rowSetInsert) rows rowIds)
    PreparedRowSetDeleteTransition rows rowIds ->
      rowSetSize (Foldable.foldl' (flip rowSetDelete) rows rowIds)

rowIdsBetween :: Int -> Int -> [RowId]
rowIdsBetween start count =
  mapMaybe (either (const Nothing) Just . mkRowId) [start .. start + count - 1]

type BenchIndexedRows = IndexedRows Int (Int, Int) Int

newtype PreparedIndexedRows = PreparedIndexedRows BenchIndexedRows

instance NFData PreparedIndexedRows where
  rnf (PreparedIndexedRows rows) =
    indexedRowsSize rows `seq` ()

data PreparedIndexedRowsDelete = PreparedIndexedRowsDelete
  { preparedIndexedRowsDeleteRows :: !BenchIndexedRows,
    preparedIndexedRowsDeleteKeys :: ![(Int, Int)]
  }

instance NFData PreparedIndexedRowsDelete where
  rnf preparedRows =
    indexedRowsSize (preparedIndexedRowsDeleteRows preparedRows)
      `seq` length (preparedIndexedRowsDeleteKeys preparedRows)
      `seq` ()

indexedRowsPayloads :: Int -> Map.Map (Int, Int) Int
indexedRowsPayloads size =
  -- the (mod 64, mod 17) grid repeats with period 1088; the cycle offset keeps
  -- the keys injective at every size, and the list is not ascending.
  Map.fromList
    (fmap (\index -> ((index `mod` 64, index `mod` 17 + 17 * (index `div` 1088)), weightAt index)) [0 .. size - 1])

indexedRowsSkewedPayloads :: Int -> Map.Map (Int, Int) Int
indexedRowsSkewedPayloads size =
  Map.fromAscList
    (fmap (\index -> ((0, index), weightAt index)) [0 .. size - 1])

indexedRowsBuildWeight :: Map.Map (Int, Int) Int -> Either String Int
indexedRowsBuildWeight payloads =
  fmap indexedRowsSize
    (eitherShow (indexedRowsFromPayloadMap benchIndexedRowFormat indexedLayoutColumns 2 payloads))

indexedRowsInsertFreshWeight :: Map.Map (Int, Int) Int -> Either String Int
indexedRowsInsertFreshWeight payloads =
  fmap indexedRowsSize
    ( eitherShow
        (Foldable.foldlM insertIndexedRowFresh (emptyIndexedRows indexedLayoutColumns 2) (Map.toAscList payloads))
    )

insertIndexedRowFresh ::
  BenchIndexedRows ->
  ((Int, Int), Int) ->
  Either (IndexedRowsInsertError Int (Int, Int)) BenchIndexedRows
insertIndexedRowFresh rows (key, payload) =
  snd <$> indexedRowsInsertFresh benchIndexedRowFormat key payload rows

preparedIndexedRows :: Int -> Either String PreparedIndexedRows
preparedIndexedRows size =
  fmap PreparedIndexedRows
    ( eitherShow
        (indexedRowsFromPayloadMap benchIndexedRowFormat indexedLayoutColumns 2 (indexedRowsPayloads size))
    )

preparedIndexedRowsDeleteSkew :: Int -> Either String PreparedIndexedRowsDelete
preparedIndexedRowsDeleteSkew size =
  fmap
    (\rows -> PreparedIndexedRowsDelete rows (Map.keys payloads))
    ( eitherShow
        (indexedRowsFromPayloadMap benchIndexedRowFormat indexedLayoutColumns 2 payloads)
    )
  where
    payloads =
      indexedRowsSkewedPayloads size

indexedRowsDeleteSkewWeight :: PreparedIndexedRowsDelete -> Either String Int
indexedRowsDeleteSkewWeight preparedRows =
  fmap indexedRowsSize
    ( eitherShow
        (Foldable.foldlM deleteIndexedRow (preparedIndexedRowsDeleteRows preparedRows) (preparedIndexedRowsDeleteKeys preparedRows))
    )

deleteIndexedRow ::
  BenchIndexedRows ->
  (Int, Int) ->
  Either (IndexedRowsDeleteError Int (Int, Int)) BenchIndexedRows
deleteIndexedRow rows key =
  (\(_rowId, _payload, rowsAfterDelete) -> rowsAfterDelete)
    <$> indexedRowsDelete benchIndexedRowFormat key rows

indexedRowsRestrictWeight :: PreparedIndexedRows -> Int
indexedRowsRestrictWeight (PreparedIndexedRows rows) =
  rowSetSize (indexedRowsRestrictLiveRowsByPins rows (IntMap.fromAscList [(0, 7)]))
    + rowSetSize (indexedRowsLiveRowSet rows)

indexedRowsRebuildWeight :: PreparedIndexedRows -> Either String Int
indexedRowsRebuildWeight (PreparedIndexedRows rows) =
  fmap indexedRowsSize (eitherShow (indexedRowsRebuildValueIndex benchIndexedRowFormat rows))

benchIndexedRowFormat :: IndexedRowFormat Int (Int, Int)
benchIndexedRowFormat =
  indexedRowFormat
    (const 2)
    id
    (\_layout (leftValue, rightValue) step initial ->
       Right (step 1 rightValue (step 0 leftValue initial)))

indexedLayoutColumns :: Int -> IntMap.IntMap Int
indexedLayoutColumns width =
  IntMap.fromAscList (fmap (\slot -> (slot, slot)) [0 .. width - 1])

type BenchRowsCache = ContextRowsCache Int Int

newtype PreparedRowsCache = PreparedRowsCache BenchRowsCache

instance NFData PreparedRowsCache where
  rnf (PreparedRowsCache cache) =
    crcCurrentBytes cache `seq` ()

rowsCacheRuntime :: ContextRowsRuntime Identity Int Int
rowsCacheRuntime =
  ContextRowsRuntime
    { crrKeyFor = contextRowsKey 0 0 0,
      crrChooseRestrictionSource =
        \availableContexts target ->
          pure (if Set.member 0 availableContexts && target /= 0 then Just 0 else Nothing),
      crrMaterializeRootRows = pure . (* 2),
      crrDeriveByRestriction = \source target rows -> pure (rows + source + target),
      crrRowsBytes = const 8
    }

rowsCacheHitCase :: Int -> PreparedRowsCache
rowsCacheHitCase size =
  PreparedRowsCache
    ( runIdentity
        ( execStateT
            (Foldable.traverse_ (\context -> insertContextRows rowsCacheRuntime context context) [0 .. size - 1])
            (emptyContextRowsCache (fromIntegral (size * 16)))
        )
    )

rowsCacheMissCase :: Int -> PreparedRowsCache
rowsCacheMissCase _size =
  PreparedRowsCache
    ( runIdentity
        ( execStateT
            (insertContextRows rowsCacheRuntime 0 0)
            (emptyContextRowsCache 8192)
        )
    )

rowsCacheEvictionCase :: Int -> PreparedRowsCache
rowsCacheEvictionCase _size =
  PreparedRowsCache (emptyContextRowsCache 256)

rowsCachePinnedOverBudgetCase :: Int -> PreparedRowsCache
rowsCachePinnedOverBudgetCase _size =
  PreparedRowsCache (emptyContextRowsCache 256)

rowsCacheHitWeight :: PreparedRowsCache -> Int
rowsCacheHitWeight (PreparedRowsCache cache) =
  runIdentity (evalStateT (getContextRows rowsCacheRuntime 255) cache)

rowsCacheMissWeight :: PreparedRowsCache -> Int
rowsCacheMissWeight (PreparedRowsCache cache) =
  runIdentity (evalStateT (getContextRows rowsCacheRuntime 511) cache)

rowsCacheEvictionWeight :: PreparedRowsCache -> Int
rowsCacheEvictionWeight (PreparedRowsCache cache) =
  fromIntegral
    ( crcCurrentBytes
    ( runIdentity
        ( execStateT
            (Foldable.traverse_ (\context -> insertContextRows rowsCacheRuntime context context) [0 .. 511])
            cache
        )
    )
    )

rowsCachePinnedOverBudgetWeight :: PreparedRowsCache -> Int
rowsCachePinnedOverBudgetWeight (PreparedRowsCache cache) =
  fromIntegral
    ( crcCurrentBytes
    ( runIdentity
        ( execStateT
            ( withPinnedContext
                rowsCacheRuntime
                0
                ( Foldable.traverse_
                    (\context -> insertContextRows rowsCacheRuntime context context)
                    [0 .. 511]
                )
            )
            cache

        )
    )
    )

rowsCacheBulkResizeWeight :: PreparedRowsCache -> Int
rowsCacheBulkResizeWeight (PreparedRowsCache cache) =
  Map.size (crcEntries (resizeContextRowsCache 0 cache))
