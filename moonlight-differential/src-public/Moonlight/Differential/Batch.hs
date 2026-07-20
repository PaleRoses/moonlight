{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Batch
  ( Batch,
    BatchMergeFuel (..),
    BatchMerger,
    BatchMergeWork (..),
    BatchCoverPlan (..),
    batchLower,
    batchUpper,
    batchDescription,
    batchRowCount,
    beginBatchMerge,
    workBatchMerge,
    workBatchMergeMeasured,
    batchMergeDone,
    finishBatchMerge,
    emptyBatch,
    singletonBatch,
    fromUpdates,
    fromUpdatesDense,
    mergeBatch,
    mergeBatches,
    batchCoverPlan,
    batchCoverNull,
    foldBatch,
    foldBatchKey,
    foldBatchKeyRows,
    batchToUpdates,
    batchNull,
  )
where

import Control.Monad.ST
  ( ST,
    runST,
  )
import Data.Foldable qualified as Foldable
import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Ord
  ( comparing,
  )
import Data.Vector
  ( Vector,
  )
import Data.Vector qualified as Vector
import Data.Vector.Algorithms.Intro qualified as Intro
import Data.Vector.Mutable qualified as MVector
import Data.Vector.Unboxed qualified as Unboxed
import Data.Vector.Unboxed.Mutable qualified as UnboxedMVector
import Moonlight.Delta.Frontier
  ( Frontier,
    UpperFrontier,
    emptyFrontier,
    emptyUpperFrontier,
    frontierPoints,
    insertPoint,
    insertUpperFrontierPoint,
    mkUpperFrontier,
    upperFrontierPoints,
  )
import Moonlight.Core
  ( PartialOrder,
  )
import Moonlight.Core (AdditiveGroup (..), AdditiveMonoid (..))
import Moonlight.Differential.Algebra.ZSet
  ( Timed (..),
    ZSet,
  )
import Moonlight.Differential.Algebra.ZSet qualified as ZSet
import Moonlight.Differential.Trace.Description
  ( TraceDescription,
    traceDescription,
  )
import Moonlight.Differential.Update
  ( Update (..),
  )
import Numeric.Natural
  ( Natural,
  )

type Batch :: Type -> Type -> Type -> Type -> Type
data Batch time key val weight = Batch
  { batchLower :: !(Frontier time),
    batchUpper :: !(UpperFrontier time),
    batchRows :: !(OrderedBatchRows time key val weight),
    batchRowCount :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show)

type BatchRow :: Type -> Type -> Type -> Type -> Type
data BatchRow time key val weight = BatchRow
  { batchRowTime :: !time,
    batchRowKey :: !key,
    batchRowValue :: !val,
    batchRowWeight :: !weight
  }
  deriving stock (Eq, Ord, Show)

-- Cell layout (key, val, time, weight): the (key, val, time) prefix is the
-- consolidation identity and matches the derived 'OrderedBatchRowCell' order.
type DenseBatchCell :: Type
type DenseBatchCell = (Int, Int, Int, Int)

type OrderedBatchRows :: Type -> Type -> Type -> Type -> Type
data OrderedBatchRows time key val weight where
  OrderedBatchRows ::
    !(Vector (BatchRow time key val weight)) ->
    OrderedBatchRows time key val weight
  -- | Unboxed all-Int arm; built only through 'fromUpdatesDense', absorbs boxed rows on contact, never converts back.
  DenseRows ::
    !(Unboxed.Vector DenseBatchCell) ->
    OrderedBatchRows Int Int Int Int

instance (Eq time, Eq key, Eq val, Eq weight) => Eq (OrderedBatchRows time key val weight) where
  left == right =
    case (left, right) of
      (OrderedBatchRows leftRows, OrderedBatchRows rightRows) ->
        leftRows == rightRows
      (DenseRows leftCells, DenseRows rightCells) ->
        leftCells == rightCells
      (DenseRows leftCells, OrderedBatchRows rightRows) ->
        leftCells == denseBatchCellsFromRows rightRows
      (OrderedBatchRows leftRows, DenseRows rightCells) ->
        denseBatchCellsFromRows leftRows == rightCells

instance (Ord time, Ord key, Ord val, Ord weight) => Ord (OrderedBatchRows time key val weight) where
  compare left right =
    case (left, right) of
      (OrderedBatchRows leftRows, OrderedBatchRows rightRows) ->
        compare leftRows rightRows
      (DenseRows leftCells, DenseRows rightCells) ->
        compareDenseBatchCellsAsRows leftCells rightCells
      (DenseRows leftCells, OrderedBatchRows rightRows) ->
        compareDenseBatchCellsAsRows leftCells (denseBatchCellsFromRows rightRows)
      (OrderedBatchRows leftRows, DenseRows rightCells) ->
        compareDenseBatchCellsAsRows (denseBatchCellsFromRows leftRows) rightCells

instance (Show time, Show key, Show val, Show weight) => Show (OrderedBatchRows time key val weight) where
  showsPrec depth rows =
    case rows of
      OrderedBatchRows vector ->
        showParen (depth > 10) $
          showString "OrderedBatchRows {orderedBatchRowsVector = "
            . showsPrec 0 vector
            . showString "}"
      DenseRows cells ->
        showParen (depth > 10) $
          showString "DenseRows " . showsPrec 11 cells

denseBatchCell :: BatchRow Int Int Int Int -> DenseBatchCell
denseBatchCell row =
  (batchRowKey row, batchRowValue row, batchRowTime row, batchRowWeight row)
{-# INLINE denseBatchCell #-}

denseBatchRow :: DenseBatchCell -> BatchRow Int Int Int Int
denseBatchRow (key, val, time, weight) =
  BatchRow
    { batchRowTime = time,
      batchRowKey = key,
      batchRowValue = val,
      batchRowWeight = weight
    }
{-# INLINE denseBatchRow #-}

denseBatchRowCell :: DenseBatchCell -> OrderedBatchRowCell Int Int Int
denseBatchRowCell (key, val, time, _weight) =
  OrderedBatchRowCell
    { orderedBatchRowCellKey = key,
      orderedBatchRowCellValue = val,
      orderedBatchRowCellTime = time
    }
{-# INLINE denseBatchRowCell #-}

denseBatchCellKey :: DenseBatchCell -> Int
denseBatchCellKey (key, _val, _time, _weight) =
  key
{-# INLINE denseBatchCellKey #-}

compareDenseBatchCell :: DenseBatchCell -> DenseBatchCell -> Ordering
compareDenseBatchCell (leftKey, leftVal, leftTime, _) (rightKey, rightVal, rightTime, _) =
  compare (leftKey, leftVal, leftTime) (rightKey, rightVal, rightTime)
{-# INLINE compareDenseBatchCell #-}

denseBatchCellsFromRows :: Vector (BatchRow Int Int Int Int) -> Unboxed.Vector DenseBatchCell
denseBatchCellsFromRows vector =
  Unboxed.generate (Vector.length vector) (denseBatchCell . Vector.unsafeIndex vector)
{-# INLINE denseBatchCellsFromRows #-}

denseBatchCells :: OrderedBatchRows Int Int Int Int -> Unboxed.Vector DenseBatchCell
denseBatchCells rows =
  case rows of
    DenseRows cells ->
      cells
    OrderedBatchRows vector ->
      denseBatchCellsFromRows vector
{-# INLINE denseBatchCells #-}

compareDenseBatchCellsAsRows :: Unboxed.Vector DenseBatchCell -> Unboxed.Vector DenseBatchCell -> Ordering
compareDenseBatchCellsAsRows left right =
  go 0
  where
    leftLength =
      Unboxed.length left
    rightLength =
      Unboxed.length right
    rowOrder (key, val, time, weight) =
      (time, key, val, weight)
    go index
      | index >= leftLength && index >= rightLength =
          EQ
      | index >= leftLength =
          LT
      | index >= rightLength =
          GT
      | otherwise =
          case compare (rowOrder (Unboxed.unsafeIndex left index)) (rowOrder (Unboxed.unsafeIndex right index)) of
            EQ ->
              go (index + 1)
            ordering ->
              ordering
{-# INLINABLE compareDenseBatchCellsAsRows #-}

-- Source anchor:
--   differential-dataflow/src/trace/implementations/merge_batcher.rs: MergeBatcher
--   differential-dataflow/src/trace/mod.rs: Merger
-- The batch is a sorted/consolidated immutable chunk; merging is a resumable cursor over
-- two chunks, not a Map-backed denotation masquerading as storage.
type OrderedBatchRowCell :: Type -> Type -> Type -> Type
data OrderedBatchRowCell time key val = OrderedBatchRowCell
  { orderedBatchRowCellKey :: !key,
    orderedBatchRowCellValue :: !val,
    orderedBatchRowCellTime :: !time
  }
  deriving stock (Eq, Ord, Show)

type OrderedBatchRowsMerge :: Type -> Type -> Type -> Type -> Type
data OrderedBatchRowsMerge time key val weight = OrderedBatchRowsMerge
  { orderedBatchRowsMergeLeft :: !(Vector (BatchRow time key val weight)),
    orderedBatchRowsMergeLeftOffset :: {-# UNPACK #-} !Int,
    orderedBatchRowsMergeRight :: !(Vector (BatchRow time key val weight)),
    orderedBatchRowsMergeRightOffset :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show)

type DenseBatchCellsMerge :: Type
data DenseBatchCellsMerge = DenseBatchCellsMerge
  { denseBatchCellsMergeLeft :: !(Unboxed.Vector DenseBatchCell),
    denseBatchCellsMergeLeftOffset :: {-# UNPACK #-} !Int,
    denseBatchCellsMergeRight :: !(Unboxed.Vector DenseBatchCell),
    denseBatchCellsMergeRightOffset :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show)

type OrderedBatchRowsMerger :: Type -> Type -> Type -> Type -> Type
data OrderedBatchRowsMerger time key val weight where
  OrderedBatchRowsMerger ::
    !(OrderedBatchRowsMerge time key val weight) ->
    !(Maybe (BatchRow time key val weight)) ->
    ![Vector (BatchRow time key val weight)] ->
    OrderedBatchRowsMerger time key val weight
  DenseRowsMerger ::
    !DenseBatchCellsMerge ->
    !(Maybe DenseBatchCell) ->
    ![Unboxed.Vector DenseBatchCell] ->
    OrderedBatchRowsMerger Int Int Int Int

orderedBatchRowsMergerView ::
  OrderedBatchRowsMerger time key val weight ->
  ( OrderedBatchRowsMerge time key val weight,
    Maybe (BatchRow time key val weight),
    [Vector (BatchRow time key val weight)]
  )
orderedBatchRowsMergerView merger =
  case merger of
    OrderedBatchRowsMerger remaining pending chunksRev ->
      (remaining, pending, chunksRev)
    DenseRowsMerger remaining pending chunksRev ->
      ( OrderedBatchRowsMerge
          { orderedBatchRowsMergeLeft = boxedBatchRowsFromCells (denseBatchCellsMergeLeft remaining),
            orderedBatchRowsMergeLeftOffset = denseBatchCellsMergeLeftOffset remaining,
            orderedBatchRowsMergeRight = boxedBatchRowsFromCells (denseBatchCellsMergeRight remaining),
            orderedBatchRowsMergeRightOffset = denseBatchCellsMergeRightOffset remaining
          },
        fmap denseBatchRow pending,
        fmap boxedBatchRowsFromCells chunksRev
      )
{-# INLINABLE orderedBatchRowsMergerView #-}

boxedBatchRowsFromCells :: Unboxed.Vector DenseBatchCell -> Vector (BatchRow Int Int Int Int)
boxedBatchRowsFromCells cells =
  Vector.generate (Unboxed.length cells) (denseBatchRow . Unboxed.unsafeIndex cells)
{-# INLINABLE boxedBatchRowsFromCells #-}

instance (Eq time, Eq key, Eq val, Eq weight) => Eq (OrderedBatchRowsMerger time key val weight) where
  left == right =
    orderedBatchRowsMergerView left == orderedBatchRowsMergerView right

instance (Ord time, Ord key, Ord val, Ord weight) => Ord (OrderedBatchRowsMerger time key val weight) where
  compare =
    comparing orderedBatchRowsMergerView

instance (Show time, Show key, Show val, Show weight) => Show (OrderedBatchRowsMerger time key val weight) where
  showsPrec depth merger =
    case merger of
      OrderedBatchRowsMerger remaining pending chunksRev ->
        showParen (depth > 10) $
          showString "OrderedBatchRowsMerger {orderedBatchRowsMergerRemaining = "
            . showsPrec 0 remaining
            . showString ", orderedBatchRowsMergerPending = "
            . showsPrec 0 pending
            . showString ", orderedBatchRowsMergerAcceptedChunksRev = "
            . showsPrec 0 chunksRev
            . showString "}"
      DenseRowsMerger remaining pending chunksRev ->
        showParen (depth > 10) $
          showString "DenseRowsMerger "
            . showsPrec 11 remaining
            . showString " "
            . showsPrec 11 pending
            . showString " "
            . showsPrec 11 chunksRev

type FuelWork :: Type -> Type
data FuelWork state = FuelWork
  { fuelWorkConsumed :: !Natural,
    fuelWorkState :: !state
  }

type BatchMergeFuel :: Type
newtype BatchMergeFuel = BatchMergeFuel
  { unBatchMergeFuel :: Natural
  }
  deriving stock (Eq, Ord, Show)

type BatchMerger :: Type -> Type -> Type -> Type -> Type
data BatchMerger time key val weight = BatchMerger
  { batchMergerLower :: !(Frontier time),
    batchMergerUpper :: !(UpperFrontier time),
    batchMergerRows :: !(OrderedBatchRowsMerger time key val weight)
  }
  deriving stock (Eq, Ord, Show)

type BatchMergeWork :: Type -> Type -> Type -> Type -> Type
data BatchMergeWork time key val weight = BatchMergeWork
  { batchMergeFuelConsumed :: !Natural,
    batchMergeWorkMerger :: !(BatchMerger time key val weight)
  }
  deriving stock (Eq, Ord, Show)

type BatchCoverPlan :: Type
data BatchCoverPlan
  = CoverEmpty
  | CoverSingleton
  | CoverStrictlyDisjoint
  | CoverBoundaryOverlap
  | CoverKWayOverlap
  deriving stock (Eq, Ord, Show)

type OrderedBatchRowsCoverPlanState :: Type -> Type -> Type -> Type
data OrderedBatchRowsCoverPlanState time key val = OrderedBatchRowsCoverPlanState
  { orderedBatchRowsCoverPlanLastCell :: !(Maybe (OrderedBatchRowCell time key val)),
    orderedBatchRowsCoverPlanNonEmptyChunks :: {-# UNPACK #-} !Int,
    orderedBatchRowsCoverPlanHasBoundaryOverlap :: !Bool,
    orderedBatchRowsCoverPlanRequiresKWay :: !Bool
  }

type BatchKeyRowsFold :: Type -> Type -> Type -> Type -> Type -> Type
data BatchKeyRowsFold acc time key val weight = BatchKeyRowsFold
  { batchKeyRowsCurrent :: !(Maybe (key, [(Timed time val, weight)])),
    batchKeyRowsAccumulated :: !acc
  }

instance
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Semigroup (Batch time key val weight)
  where
  (<>) =
    mergeBatch

instance
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Monoid (Batch time key val weight)
  where
  mempty =
    emptyBatch

emptyBatch :: Batch time key val weight
emptyBatch =
  Batch
    { batchLower = emptyFrontier,
      batchUpper = emptyUpperFrontier,
      batchRows = orderedBatchRowsEmpty,
      batchRowCount = 0
    }
{-# INLINABLE emptyBatch #-}

beginBatchMerge ::
  (Ord time, PartialOrder time) =>
  Batch time key val weight ->
  Batch time key val weight ->
  BatchMerger time key val weight
beginBatchMerge left right =
  BatchMerger
    { batchMergerLower = mergeLowerFrontier (batchLower left) (batchLower right),
      batchMergerUpper = mergeUpperFrontier (batchUpper left) (batchUpper right),
      batchMergerRows = beginOrderedBatchRowsMerge (batchRows left) (batchRows right)
    }
{-# INLINE beginBatchMerge #-}

workBatchMerge ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  BatchMergeFuel ->
  BatchMerger time key val weight ->
  BatchMerger time key val weight
workBatchMerge fuel merger =
  batchMergeWorkMerger (workBatchMergeMeasured fuel merger)
{-# INLINE workBatchMerge #-}

workBatchMergeMeasured ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  BatchMergeFuel ->
  BatchMerger time key val weight ->
  BatchMergeWork time key val weight
workBatchMergeMeasured fuel merger =
  BatchMergeWork
    { batchMergeFuelConsumed = fuelWorkConsumed work,
      batchMergeWorkMerger =
        merger
          { batchMergerRows =
              fuelWorkState work
          }
    }
  where
    work =
      workOrderedBatchRowsMergeMeasured fuel (batchMergerRows merger)
{-# INLINE workBatchMergeMeasured #-}

batchMergeDone :: BatchMerger time key val weight -> Bool
batchMergeDone =
  orderedBatchRowsMergerDone . batchMergerRows
{-# INLINE batchMergeDone #-}

finishBatchMerge ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  BatchMerger time key val weight ->
  Batch time key val weight
finishBatchMerge merger =
  batchFromRows
    (batchMergerLower merger)
    (batchMergerUpper merger)
    (finishOrderedBatchRowsMerge (batchMergerRows merger))
{-# INLINE finishBatchMerge #-}

batchDescription :: (Ord time, PartialOrder time) => Batch time key val weight -> TraceDescription time
batchDescription batch =
  traceDescription (batchLower batch) (batchUpper batch) (mkUpperFrontier (frontierPoints (batchLower batch)))
{-# INLINE batchDescription #-}

singletonBatch ::
  (Ord time, PartialOrder time, Eq weight, AdditiveGroup weight) =>
  Update time key val weight ->
  Batch time key val weight
singletonBatch updateValue
  | updateWeight updateValue == zero =
      emptyBatch
  | otherwise =
      Batch
        { batchLower = insertPoint updateTimeValue emptyFrontier,
          batchUpper = insertUpperFrontierPoint updateTimeValue emptyUpperFrontier,
          batchRows =
            orderedBatchRowsSingleton
              BatchRow
                { batchRowTime = updateTimeValue,
                  batchRowKey = updateKey updateValue,
                  batchRowValue = updateVal updateValue,
                  batchRowWeight = updateWeight updateValue
                },
          batchRowCount = 1
        }
  where
    updateTimeValue =
      updateTime updateValue
{-# INLINABLE singletonBatch #-}

fromUpdates ::
  (Foldable updates, Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  updates (Update time key val weight) ->
  Batch time key val weight
fromUpdates updates =
  let construction =
        Foldable.foldl' collectBatchConstruction emptyBatchConstruction updates
   in batchFromRows
        (batchConstructionLower construction)
        (batchConstructionUpper construction)
        (orderedBatchRowsFromRows (batchConstructionRows construction))
{-# INLINABLE fromUpdates #-}

fromUpdatesDense ::
  Foldable updates =>
  updates (Update Int Int Int Int) ->
  Batch Int Int Int Int
fromUpdatesDense updates =
  let construction =
        Foldable.foldl' collectBatchConstruction emptyBatchConstruction updates
   in batchFromRows
        (batchConstructionLower construction)
        (batchConstructionUpper construction)
        (denseOrderedBatchRowsFromRows (batchConstructionRows construction))
{-# INLINABLE fromUpdatesDense #-}

mergeBatch ::
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Batch time key val weight ->
  Batch time key val weight ->
  Batch time key val weight
mergeBatch left right =
  case (batchNull left, batchNull right) of
    (True, _) ->
      right
    (_, True) ->
      left
    _ ->
      batchFromRows
        (mergeLowerFrontier (batchLower left) (batchLower right))
        (mergeUpperFrontier (batchUpper left) (batchUpper right))
        (mergeOrderedBatchRowsPair (batchRows left) (batchRows right))
{-# INLINABLE mergeBatch #-}

mergeOrderedBatchRowsPair ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  OrderedBatchRows time key val weight ->
  OrderedBatchRows time key val weight ->
  OrderedBatchRows time key val weight
mergeOrderedBatchRowsPair left right =
  case (left, right) of
    (OrderedBatchRows leftRows, OrderedBatchRows rightRows) ->
      OrderedBatchRows (mergeSortedBatchRows leftRows rightRows)
    (DenseRows leftCells, DenseRows rightCells) ->
      DenseRows (denseMergeSortedBatchCells leftCells rightCells)
    (DenseRows leftCells, OrderedBatchRows rightRows) ->
      DenseRows (denseMergeSortedBatchCells leftCells (denseBatchCellsFromRows rightRows))
    (OrderedBatchRows leftRows, DenseRows rightCells) ->
      DenseRows (denseMergeSortedBatchCells (denseBatchCellsFromRows leftRows) rightCells)
{-# INLINE mergeOrderedBatchRowsPair #-}

mergeBatches ::
  (Foldable batches, Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  batches (Batch time key val weight) ->
  Batch time key val weight
mergeBatches batches =
  batchFromRows
    (batchMergeLower construction)
    (batchMergeUpper construction)
    (mergeOrderedBatchRowsCover (fmap batchRows (reverse (batchMergeBatchesRev construction))))
  where
    construction =
      Foldable.foldl' collectBatchMerge emptyBatchMerge batches
{-# INLINE mergeBatches #-}

batchCoverPlan ::
  (Foldable batches, Ord time, Ord key, Ord val) =>
  batches (Batch time key val weight) ->
  BatchCoverPlan
batchCoverPlan =
  orderedBatchRowsCoverPlan . batchCoverRows
{-# INLINE batchCoverPlan #-}

batchCoverNull ::
  (Foldable batches, Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  batches (Batch time key val weight) ->
  Bool
batchCoverNull =
  orderedBatchRowsCoverNull . batchCoverRows
{-# INLINE batchCoverNull #-}

foldBatch ::
  (acc -> time -> key -> val -> weight -> acc) ->
  acc ->
  Batch time key val weight ->
  acc
foldBatch step initial batch =
  foldOrderedBatchRows
    ( \acc row ->
        step
          acc
          (batchRowTime row)
          (batchRowKey row)
          (batchRowValue row)
          (batchRowWeight row)
    )
    initial
    (batchRows batch)
{-# INLINE foldBatch #-}

foldBatchKey ::
  Ord key =>
  key ->
  (acc -> time -> val -> weight -> acc) ->
  acc ->
  Batch time key val weight ->
  acc
foldBatchKey key step initial batch =
  foldOrderedBatchRowsKeyRange
    key
    ( \acc row ->
        step acc (batchRowTime row) (batchRowValue row) (batchRowWeight row)
    )
    initial
    (batchRows batch)
{-# INLINE foldBatchKey #-}

foldBatchKeyRows ::
  (Eq key, Ord time, Ord val, Eq weight, AdditiveGroup weight) =>
  (acc -> key -> ZSet (Timed time val) weight -> acc) ->
  acc ->
  Batch time key val weight ->
  acc
foldBatchKeyRows step initial batch =
  finishBatchKeyRows
    step
    ( foldOrderedBatchRows
        (collectBatchKeyRows step)
        (BatchKeyRowsFold Nothing initial)
        (batchRows batch)
    )
{-# INLINE foldBatchKeyRows #-}

batchToUpdates :: Batch time key val weight -> [Update time key val weight]
batchToUpdates batch =
  case batchRows batch of
    OrderedBatchRows vector ->
      fmap batchRowToUpdate (Vector.toList vector)
    DenseRows cells ->
      fmap (batchRowToUpdate . denseBatchRow) (Unboxed.toList cells)
{-# INLINABLE batchToUpdates #-}

batchRowToUpdate :: BatchRow time key val weight -> Update time key val weight
batchRowToUpdate row =
  Update
    { updateTime = batchRowTime row,
      updateKey = batchRowKey row,
      updateVal = batchRowValue row,
      updateWeight = batchRowWeight row
    }
{-# INLINE batchRowToUpdate #-}

batchNull :: Batch time key val weight -> Bool
batchNull =
  (== 0) . batchRowCount
{-# INLINABLE batchNull #-}

orderedBatchRowsEmpty :: OrderedBatchRows time key val weight
orderedBatchRowsEmpty =
  OrderedBatchRows Vector.empty
{-# INLINE orderedBatchRowsEmpty #-}

orderedBatchRowsSingleton :: BatchRow time key val weight -> OrderedBatchRows time key val weight
orderedBatchRowsSingleton =
  OrderedBatchRows . Vector.singleton
{-# INLINE orderedBatchRowsSingleton #-}

orderedBatchRowsLength :: OrderedBatchRows time key val weight -> Int
orderedBatchRowsLength rows =
  case rows of
    OrderedBatchRows vector ->
      Vector.length vector
    DenseRows cells ->
      Unboxed.length cells
{-# INLINE orderedBatchRowsLength #-}

-- Source anchor:
--   feldera/crates/dbsp/src/trace.rs: Batcher / Builder
--   differential-dataflow/src/trace/implementations/merge_batcher.rs: MergeSorter
-- Unordered update ingestion is builder-owned consolidation, split by chunk
-- size (both regimes raced at referent workloads). A small streaming chunk
-- (at most 'smallIngestChunkLength' rows) sorts and collapses in place: no
-- tree, no per-row allocation, cache-resident (~x2 over the tree at the
-- external referent's 64-row step batches). A large chunk consolidates
-- through a cell-keyed tree: its cardinality is bounded by distinct cells,
-- which large ingest keeps far below row count, so the tree stays small and
-- builds each cell once while a full-length sort pays n log n equal-cell
-- comparisons (tree x1.4-2.7 over sort at the package's low-cardinality
-- lanes).
orderedBatchRowsFromRows ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  [BatchRow time key val weight] ->
  OrderedBatchRows time key val weight
orderedBatchRowsFromRows rows =
  case rows of
    [] ->
      orderedBatchRowsEmpty
    [row]
      | batchRowWeight row == zero ->
          orderedBatchRowsEmpty
      | otherwise ->
          orderedBatchRowsSingleton row
    _
      | length rows <= smallIngestChunkLength ->
          OrderedBatchRows
            ( collapseSortedBatchRows
                ( Vector.modify
                    (Intro.sortBy compareBatchRow)
                    (Vector.fromList rows)
                )
            )
      | otherwise ->
          orderedBatchRowsFromCellMap (Foldable.foldl' collectOrderedBatchRowMap Map.empty rows)
{-# INLINE orderedBatchRowsFromRows #-}

denseOrderedBatchRowsFromRows ::
  [BatchRow Int Int Int Int] ->
  OrderedBatchRows Int Int Int Int
denseOrderedBatchRowsFromRows rows =
  case rows of
    [] ->
      DenseRows Unboxed.empty
    [row]
      | batchRowWeight row == 0 ->
          DenseRows Unboxed.empty
      | otherwise ->
          DenseRows (Unboxed.singleton (denseBatchCell row))
    _
      | length rows <= smallIngestChunkLength ->
          DenseRows
            ( denseCollapseSortedBatchCells
                ( Unboxed.modify
                    (Intro.sortBy compareDenseBatchCell)
                    (Unboxed.fromList (fmap denseBatchCell rows))
                )
            )
      | otherwise ->
          DenseRows (denseBatchCellsFromCellMap (Foldable.foldl' collectOrderedBatchRowMap Map.empty rows))
{-# INLINE denseOrderedBatchRowsFromRows #-}

smallIngestChunkLength :: Int
smallIngestChunkLength = 128

consolidateBatchRowChunks ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  [Vector (BatchRow time key val weight)] ->
  Vector (BatchRow time key val weight)
consolidateBatchRowChunks chunks =
  batchRowsVectorFromCellMap
    (Foldable.foldl' (Vector.foldl' collectOrderedBatchRowMap) Map.empty chunks)
{-# INLINABLE consolidateBatchRowChunks #-}

denseConsolidateBatchCellChunks ::
  [Unboxed.Vector DenseBatchCell] ->
  Unboxed.Vector DenseBatchCell
denseConsolidateBatchCellChunks chunks =
  denseBatchCellsFromCellMap
    (Foldable.foldl' (Unboxed.foldl' collectDenseBatchCellMap) Map.empty chunks)
{-# INLINABLE denseConsolidateBatchCellChunks #-}

collectDenseBatchCellMap ::
  Map (OrderedBatchRowCell Int Int Int) Int ->
  DenseBatchCell ->
  Map (OrderedBatchRowCell Int Int Int) Int
collectDenseBatchCellMap cells cell@(_, _, _, weight)
  | weight == 0 =
      cells
  | otherwise =
      Map.insertWith
        (+)
        (denseBatchRowCell cell)
        weight
        cells
{-# INLINE collectDenseBatchCellMap #-}

collectOrderedBatchRowMap ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Map (OrderedBatchRowCell time key val) weight ->
  BatchRow time key val weight ->
  Map (OrderedBatchRowCell time key val) weight
collectOrderedBatchRowMap rows row
  | batchRowWeight row == zero =
      rows
  | otherwise =
      Map.insertWith
        add
        (batchRowCell row)
        (batchRowWeight row)
        rows
{-# INLINE collectOrderedBatchRowMap #-}

orderedBatchRowsFromCellMap ::
  (Eq weight, AdditiveGroup weight) =>
  Map (OrderedBatchRowCell time key val) weight ->
  OrderedBatchRows time key val weight
orderedBatchRowsFromCellMap =
  OrderedBatchRows . batchRowsVectorFromCellMap
{-# INLINE orderedBatchRowsFromCellMap #-}

batchRowsVectorFromCellMap ::
  (Eq weight, AdditiveGroup weight) =>
  Map (OrderedBatchRowCell time key val) weight ->
  Vector (BatchRow time key val weight)
batchRowsVectorFromCellMap =
  Vector.fromList
    . Map.foldrWithKey emitOrderedBatchRow
      []
{-# INLINE batchRowsVectorFromCellMap #-}

denseBatchCellsFromCellMap ::
  Map (OrderedBatchRowCell Int Int Int) Int ->
  Unboxed.Vector DenseBatchCell
denseBatchCellsFromCellMap cells =
  Unboxed.fromListN (Map.size cells) (Map.foldrWithKey emitDenseBatchCell [] cells)
{-# INLINE denseBatchCellsFromCellMap #-}

emitDenseBatchCell ::
  OrderedBatchRowCell Int Int Int ->
  Int ->
  [DenseBatchCell] ->
  [DenseBatchCell]
emitDenseBatchCell cell weight cells
  | weight == 0 =
      cells
  | otherwise =
      ( orderedBatchRowCellKey cell,
        orderedBatchRowCellValue cell,
        orderedBatchRowCellTime cell,
        weight
      )
        : cells
{-# INLINE emitDenseBatchCell #-}

emitOrderedBatchRow ::
  (Eq weight, AdditiveGroup weight) =>
  OrderedBatchRowCell time key val ->
  weight ->
  [BatchRow time key val weight] ->
  [BatchRow time key val weight]
emitOrderedBatchRow cell weight rows
  | weight == zero =
      rows
  | otherwise =
      BatchRow
        { batchRowTime = orderedBatchRowCellTime cell,
          batchRowKey = orderedBatchRowCellKey cell,
          batchRowValue = orderedBatchRowCellValue cell,
          batchRowWeight = weight
        }
        : rows
{-# INLINE emitOrderedBatchRow #-}

collapseSortedBatchRows ::
  (Eq time, Eq key, Eq val, Eq weight, AdditiveGroup weight) =>
  Vector (BatchRow time key val weight) ->
  Vector (BatchRow time key val weight)
collapseSortedBatchRows rows
  | Vector.length rows < 2 =
      rows
  | otherwise =
      runST $ do
        buffer <- MVector.new (Vector.length rows)
        written <- Vector.foldM' (pushCollapsedBatchRow buffer) 0 rows
        freezeBatchRowBuffer buffer written
{-# INLINE collapseSortedBatchRows #-}

pushCollapsedBatchRow ::
  (Eq time, Eq key, Eq val, Eq weight, AdditiveGroup weight) =>
  MVector.MVector s (BatchRow time key val weight) ->
  Int ->
  BatchRow time key val weight ->
  ST s Int
pushCollapsedBatchRow buffer written row
  | batchRowWeight row == zero =
      pure written
  | written == 0 = do
      MVector.unsafeWrite buffer 0 row
      pure 1
  | otherwise = do
      previous <- MVector.unsafeRead buffer (written - 1)
      if batchRowCell previous == batchRowCell row
        then
          let !mergedWeight = add (batchRowWeight previous) (batchRowWeight row)
           in if mergedWeight == zero
                then pure (written - 1)
                else do
                  MVector.unsafeWrite buffer (written - 1) previous {batchRowWeight = mergedWeight}
                  pure written
        else do
          MVector.unsafeWrite buffer written row
          pure (written + 1)
{-# INLINE pushCollapsedBatchRow #-}

freezeBatchRowBuffer ::
  MVector.MVector s (BatchRow time key val weight) ->
  Int ->
  ST s (Vector (BatchRow time key val weight))
freezeBatchRowBuffer buffer written =
  Vector.force <$> Vector.unsafeFreeze (MVector.slice 0 written buffer)
{-# INLINE freezeBatchRowBuffer #-}

denseCollapseSortedBatchCells :: Unboxed.Vector DenseBatchCell -> Unboxed.Vector DenseBatchCell
denseCollapseSortedBatchCells cells
  | Unboxed.length cells < 2 =
      cells
  | otherwise =
      runST $ do
        buffer <- UnboxedMVector.new (Unboxed.length cells)
        written <- Unboxed.foldM' (densePushCollapsedBatchCell buffer) 0 cells
        denseFreezeBatchCellBuffer buffer written
{-# INLINE denseCollapseSortedBatchCells #-}

densePushCollapsedBatchCell ::
  UnboxedMVector.MVector s DenseBatchCell ->
  Int ->
  DenseBatchCell ->
  ST s Int
densePushCollapsedBatchCell buffer written cell@(key, val, time, weight)
  | weight == 0 =
      pure written
  | written == 0 = do
      UnboxedMVector.unsafeWrite buffer 0 cell
      pure 1
  | otherwise = do
      (previousKey, previousVal, previousTime, previousWeight) <- UnboxedMVector.unsafeRead buffer (written - 1)
      if (previousKey, previousVal, previousTime) == (key, val, time)
        then
          let !mergedWeight = previousWeight + weight
           in if mergedWeight == 0
                then pure (written - 1)
                else do
                  UnboxedMVector.unsafeWrite buffer (written - 1) (previousKey, previousVal, previousTime, mergedWeight)
                  pure written
        else do
          UnboxedMVector.unsafeWrite buffer written cell
          pure (written + 1)
{-# INLINE densePushCollapsedBatchCell #-}

denseFreezeBatchCellBuffer ::
  UnboxedMVector.MVector s DenseBatchCell ->
  Int ->
  ST s (Unboxed.Vector DenseBatchCell)
denseFreezeBatchCellBuffer buffer written =
  Unboxed.force <$> Unboxed.unsafeFreeze (UnboxedMVector.slice 0 written buffer)
{-# INLINE denseFreezeBatchCellBuffer #-}

mergeSortedBatchRows ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Vector (BatchRow time key val weight) ->
  Vector (BatchRow time key val weight) ->
  Vector (BatchRow time key val weight)
mergeSortedBatchRows left right
  | Vector.null left =
      right
  | Vector.null right =
      left
  | otherwise =
      mergeSortedBatchRowsFrom Nothing left 0 right 0
{-# INLINE mergeSortedBatchRows #-}

-- Source anchor:
--   differential-dataflow/src/trace/implementations/merge_batcher.rs: Merger.merge
-- Two-pointer merge into one contiguous buffer, consolidating at the write
-- head. Once one sorted chunk is exhausted only the boundary row can still
-- merge or cancel against the write head; the rest of the surviving chunk is
-- copied in bulk.
mergeSortedBatchRowsFrom ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Maybe (BatchRow time key val weight) ->
  Vector (BatchRow time key val weight) ->
  Int ->
  Vector (BatchRow time key val weight) ->
  Int ->
  Vector (BatchRow time key val weight)
mergeSortedBatchRowsFrom pending left leftOffset right rightOffset =
  Vector.force (mergeSortedBatchRowsFromLoose pending left leftOffset right rightOffset)
{-# INLINE mergeSortedBatchRowsFrom #-}

-- A loose merge freezes the collapsed prefix without trimming the buffer;
-- only results retained beyond the enclosing merge tree may keep it.
mergeSortedBatchRowsFromLoose ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Maybe (BatchRow time key val weight) ->
  Vector (BatchRow time key val weight) ->
  Int ->
  Vector (BatchRow time key val weight) ->
  Int ->
  Vector (BatchRow time key val weight)
mergeSortedBatchRowsFromLoose pending left leftOffset right rightOffset =
  runST $ do
    buffer <- MVector.new (leftLength - leftOffset + rightLength - rightOffset + 1)
    seeded <- maybe (pure 0) (pushCollapsedBatchRow buffer 0) pending
    let go leftIndex rightIndex written
          | leftIndex >= leftLength =
              drain right rightIndex written
          | rightIndex >= rightLength =
              drain left leftIndex written
          | otherwise =
              let leftRow = Vector.unsafeIndex left leftIndex
                  rightRow = Vector.unsafeIndex right rightIndex
               in if compareBatchRow leftRow rightRow /= GT
                    then pushCollapsedBatchRow buffer written leftRow >>= go (leftIndex + 1) rightIndex
                    else pushCollapsedBatchRow buffer written rightRow >>= go leftIndex (rightIndex + 1)
        drain source sourceIndex written
          | sourceIndex >= Vector.length source =
              pure written
          | otherwise = do
              boundary <- pushCollapsedBatchRow buffer written (Vector.unsafeIndex source sourceIndex)
              let rest = Vector.length source - (sourceIndex + 1)
              Vector.unsafeCopy (MVector.slice boundary rest buffer) (Vector.slice (sourceIndex + 1) rest source)
              pure (boundary + rest)
    written <- go leftOffset rightOffset seeded
    Vector.unsafeFreeze (MVector.slice 0 written buffer)
  where
    leftLength =
      Vector.length left
    rightLength =
      Vector.length right
{-# INLINABLE mergeSortedBatchRowsFromLoose #-}

denseMergeSortedBatchCells ::
  Unboxed.Vector DenseBatchCell ->
  Unboxed.Vector DenseBatchCell ->
  Unboxed.Vector DenseBatchCell
denseMergeSortedBatchCells left right
  | Unboxed.null left =
      right
  | Unboxed.null right =
      left
  | otherwise =
      denseMergeSortedBatchCellsFrom Nothing left 0 right 0
{-# INLINE denseMergeSortedBatchCells #-}

denseMergeSortedBatchCellsFrom ::
  Maybe DenseBatchCell ->
  Unboxed.Vector DenseBatchCell ->
  Int ->
  Unboxed.Vector DenseBatchCell ->
  Int ->
  Unboxed.Vector DenseBatchCell
denseMergeSortedBatchCellsFrom pending left leftOffset right rightOffset =
  Unboxed.force (denseMergeSortedBatchCellsFromLoose pending left leftOffset right rightOffset)
{-# INLINE denseMergeSortedBatchCellsFrom #-}

denseMergeSortedBatchCellsFromLoose ::
  Maybe DenseBatchCell ->
  Unboxed.Vector DenseBatchCell ->
  Int ->
  Unboxed.Vector DenseBatchCell ->
  Int ->
  Unboxed.Vector DenseBatchCell
denseMergeSortedBatchCellsFromLoose pending left leftOffset right rightOffset =
  runST $ do
    buffer <- UnboxedMVector.new (leftLength - leftOffset + rightLength - rightOffset + 1)
    seeded <- maybe (pure 0) (densePushCollapsedBatchCell buffer 0) pending
    let go leftIndex rightIndex written
          | leftIndex >= leftLength =
              drain right rightIndex written
          | rightIndex >= rightLength =
              drain left leftIndex written
          | otherwise =
              let leftCell = Unboxed.unsafeIndex left leftIndex
                  rightCell = Unboxed.unsafeIndex right rightIndex
               in if compareDenseBatchCell leftCell rightCell /= GT
                    then densePushCollapsedBatchCell buffer written leftCell >>= go (leftIndex + 1) rightIndex
                    else densePushCollapsedBatchCell buffer written rightCell >>= go leftIndex (rightIndex + 1)
        drain source sourceIndex written
          | sourceIndex >= Unboxed.length source =
              pure written
          | otherwise = do
              boundary <- densePushCollapsedBatchCell buffer written (Unboxed.unsafeIndex source sourceIndex)
              let rest = Unboxed.length source - (sourceIndex + 1)
              Unboxed.unsafeCopy (UnboxedMVector.slice boundary rest buffer) (Unboxed.slice (sourceIndex + 1) rest source)
              pure (boundary + rest)
    written <- go leftOffset rightOffset seeded
    Unboxed.unsafeFreeze (UnboxedMVector.slice 0 written buffer)
  where
    leftLength =
      Unboxed.length left
    rightLength =
      Unboxed.length right
{-# INLINABLE denseMergeSortedBatchCellsFromLoose #-}

-- Source anchor:
--   feldera/crates/dbsp/src/trace/spine_async/list_merger.rs: ListMerger
-- Bulk overlap merge is balanced pairwise two-pointer merging over already
-- sorted chunks (n log k, contiguous writes); it must not walk rows through a
-- heap of cursors one cell at a time. A cover of dust (average chunk at or
-- below 'dustCoverChunkLength') instead flattens and re-enters run
-- consolidation: per-chunk merge overhead dominates any order the tiny
-- chunks carry.
mergeSortedBatchRowsCover ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  [Vector (BatchRow time key val weight)] ->
  Vector (BatchRow time key val weight)
mergeSortedBatchRowsCover chunks =
  case chunks of
    [] ->
      Vector.empty
    [chunk] ->
      chunk
    _
      | dustCoverChunkLength * length chunks >= totalRows ->
          consolidateBatchRowChunks chunks
      | otherwise ->
          Vector.force (mergeSortedBatchRowsCoverLoose chunks)
  where
    totalRows =
      Foldable.foldl' (\count chunk -> count + Vector.length chunk) 0 chunks
{-# INLINABLE mergeSortedBatchRowsCover #-}

dustCoverChunkLength :: Int
dustCoverChunkLength = 8

mergeSortedBatchRowsCoverLoose ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  [Vector (BatchRow time key val weight)] ->
  Vector (BatchRow time key val weight)
mergeSortedBatchRowsCoverLoose chunks =
  case chunks of
    [] ->
      Vector.empty
    [chunk] ->
      chunk
    _ ->
      let (front, back) =
            splitAt (length chunks `div` 2) chunks
       in mergeSortedBatchRowsLoose
            (mergeSortedBatchRowsCoverLoose front)
            (mergeSortedBatchRowsCoverLoose back)
{-# INLINABLE mergeSortedBatchRowsCoverLoose #-}

mergeSortedBatchRowsLoose ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Vector (BatchRow time key val weight) ->
  Vector (BatchRow time key val weight) ->
  Vector (BatchRow time key val weight)
mergeSortedBatchRowsLoose left right
  | Vector.null left =
      right
  | Vector.null right =
      left
  | otherwise =
      mergeSortedBatchRowsFromLoose Nothing left 0 right 0
{-# INLINE mergeSortedBatchRowsLoose #-}

denseMergeSortedBatchCellsCover ::
  [Unboxed.Vector DenseBatchCell] ->
  Unboxed.Vector DenseBatchCell
denseMergeSortedBatchCellsCover chunks =
  case chunks of
    [] ->
      Unboxed.empty
    [chunk] ->
      chunk
    _
      | dustCoverChunkLength * length chunks >= totalRows ->
          denseConsolidateBatchCellChunks chunks
      | otherwise ->
          Unboxed.force (denseMergeSortedBatchCellsCoverLoose chunks)
  where
    totalRows =
      Foldable.foldl' (\count chunk -> count + Unboxed.length chunk) 0 chunks
{-# INLINABLE denseMergeSortedBatchCellsCover #-}

denseMergeSortedBatchCellsCoverLoose ::
  [Unboxed.Vector DenseBatchCell] ->
  Unboxed.Vector DenseBatchCell
denseMergeSortedBatchCellsCoverLoose chunks =
  case chunks of
    [] ->
      Unboxed.empty
    [chunk] ->
      chunk
    _ ->
      let (front, back) =
            splitAt (length chunks `div` 2) chunks
       in denseMergeSortedBatchCellsLoose
            (denseMergeSortedBatchCellsCoverLoose front)
            (denseMergeSortedBatchCellsCoverLoose back)
{-# INLINABLE denseMergeSortedBatchCellsCoverLoose #-}

denseMergeSortedBatchCellsLoose ::
  Unboxed.Vector DenseBatchCell ->
  Unboxed.Vector DenseBatchCell ->
  Unboxed.Vector DenseBatchCell
denseMergeSortedBatchCellsLoose left right
  | Unboxed.null left =
      right
  | Unboxed.null right =
      left
  | otherwise =
      denseMergeSortedBatchCellsFromLoose Nothing left 0 right 0
{-# INLINE denseMergeSortedBatchCellsLoose #-}

beginOrderedBatchRowsMerge ::
  OrderedBatchRows time key val weight ->
  OrderedBatchRows time key val weight ->
  OrderedBatchRowsMerger time key val weight
beginOrderedBatchRowsMerge left right =
  case (left, right) of
    (OrderedBatchRows leftRows, OrderedBatchRows rightRows) ->
      OrderedBatchRowsMerger
        ( OrderedBatchRowsMerge
            { orderedBatchRowsMergeLeft = leftRows,
              orderedBatchRowsMergeLeftOffset = 0,
              orderedBatchRowsMergeRight = rightRows,
              orderedBatchRowsMergeRightOffset = 0
            }
        )
        Nothing
        []
    (DenseRows leftCells, DenseRows rightCells) ->
      denseBegin leftCells rightCells
    (DenseRows leftCells, OrderedBatchRows rightRows) ->
      denseBegin leftCells (denseBatchCellsFromRows rightRows)
    (OrderedBatchRows leftRows, DenseRows rightCells) ->
      denseBegin (denseBatchCellsFromRows leftRows) rightCells
  where
    denseBegin ::
      Unboxed.Vector DenseBatchCell ->
      Unboxed.Vector DenseBatchCell ->
      OrderedBatchRowsMerger Int Int Int Int
    denseBegin leftCells rightCells =
      DenseRowsMerger
        ( DenseBatchCellsMerge
            { denseBatchCellsMergeLeft = leftCells,
              denseBatchCellsMergeLeftOffset = 0,
              denseBatchCellsMergeRight = rightCells,
              denseBatchCellsMergeRightOffset = 0
            }
        )
        Nothing
        []
{-# INLINE beginOrderedBatchRowsMerge #-}

workOrderedBatchRowsMergeMeasured ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  BatchMergeFuel ->
  OrderedBatchRowsMerger time key val weight ->
  FuelWork (OrderedBatchRowsMerger time key val weight)
workOrderedBatchRowsMergeMeasured (BatchMergeFuel fuel) merger
  | fuel == 0 || orderedBatchRowsMergerDone merger =
      FuelWork
        { fuelWorkConsumed = 0,
          fuelWorkState = merger
        }
  | otherwise =
      case merger of
        OrderedBatchRowsMerger remaining pending chunksRev ->
          let left = orderedBatchRowsMergeLeft remaining
              right = orderedBatchRowsMergeRight remaining
              leftLength = Vector.length left
              rightLength = Vector.length right
              totalRemaining =
                (leftLength - orderedBatchRowsMergeLeftOffset remaining)
                  + (rightLength - orderedBatchRowsMergeRightOffset remaining)
              budget :: Int
              budget = fromIntegral (min fuel (fromIntegral totalRemaining))
           in runST $ do
                buffer <- MVector.new (budget + 1)
                seeded <- maybe (pure 0) (pushCollapsedBatchRow buffer 0) pending
                let go pops leftIndex rightIndex written
                      | pops >= budget || (leftIndex >= leftLength && rightIndex >= rightLength) =
                          pure (leftIndex, rightIndex, written)
                      | leftIndex >= leftLength =
                          pushCollapsedBatchRow buffer written (Vector.unsafeIndex right rightIndex)
                            >>= go (pops + 1) leftIndex (rightIndex + 1)
                      | rightIndex >= rightLength =
                          pushCollapsedBatchRow buffer written (Vector.unsafeIndex left leftIndex)
                            >>= go (pops + 1) (leftIndex + 1) rightIndex
                      | otherwise =
                          let leftRow = Vector.unsafeIndex left leftIndex
                              rightRow = Vector.unsafeIndex right rightIndex
                           in if compareBatchRow leftRow rightRow /= GT
                                then pushCollapsedBatchRow buffer written leftRow >>= go (pops + 1) (leftIndex + 1) rightIndex
                                else pushCollapsedBatchRow buffer written rightRow >>= go (pops + 1) leftIndex (rightIndex + 1)
                (leftIndex, rightIndex, written) <-
                  go 0 (orderedBatchRowsMergeLeftOffset remaining) (orderedBatchRowsMergeRightOffset remaining) seeded
                (chunk, nextPending) <-
                  if written == 0
                    then pure (Vector.empty, Nothing)
                    else do
                      lastRow <- MVector.unsafeRead buffer (written - 1)
                      frozen <- freezeBatchRowBuffer buffer (written - 1)
                      pure (frozen, Just lastRow)
                let consumed =
                      (leftIndex - orderedBatchRowsMergeLeftOffset remaining)
                        + (rightIndex - orderedBatchRowsMergeRightOffset remaining)
                pure
                  FuelWork
                    { fuelWorkConsumed = fromIntegral consumed,
                      fuelWorkState =
                        OrderedBatchRowsMerger
                          ( remaining
                              { orderedBatchRowsMergeLeftOffset = leftIndex,
                                orderedBatchRowsMergeRightOffset = rightIndex
                              }
                          )
                          nextPending
                          (if Vector.null chunk then chunksRev else chunk : chunksRev)
                    }
        DenseRowsMerger remaining pending chunksRev ->
          let left = denseBatchCellsMergeLeft remaining
              right = denseBatchCellsMergeRight remaining
              leftLength = Unboxed.length left
              rightLength = Unboxed.length right
              totalRemaining =
                (leftLength - denseBatchCellsMergeLeftOffset remaining)
                  + (rightLength - denseBatchCellsMergeRightOffset remaining)
              budget :: Int
              budget = fromIntegral (min fuel (fromIntegral totalRemaining))
           in runST $ do
                buffer <- UnboxedMVector.new (budget + 1)
                seeded <- maybe (pure 0) (densePushCollapsedBatchCell buffer 0) pending
                let go pops leftIndex rightIndex written
                      | pops >= budget || (leftIndex >= leftLength && rightIndex >= rightLength) =
                          pure (leftIndex, rightIndex, written)
                      | leftIndex >= leftLength =
                          densePushCollapsedBatchCell buffer written (Unboxed.unsafeIndex right rightIndex)
                            >>= go (pops + 1) leftIndex (rightIndex + 1)
                      | rightIndex >= rightLength =
                          densePushCollapsedBatchCell buffer written (Unboxed.unsafeIndex left leftIndex)
                            >>= go (pops + 1) (leftIndex + 1) rightIndex
                      | otherwise =
                          let leftCell = Unboxed.unsafeIndex left leftIndex
                              rightCell = Unboxed.unsafeIndex right rightIndex
                           in if compareDenseBatchCell leftCell rightCell /= GT
                                then densePushCollapsedBatchCell buffer written leftCell >>= go (pops + 1) (leftIndex + 1) rightIndex
                                else densePushCollapsedBatchCell buffer written rightCell >>= go (pops + 1) leftIndex (rightIndex + 1)
                (leftIndex, rightIndex, written) <-
                  go 0 (denseBatchCellsMergeLeftOffset remaining) (denseBatchCellsMergeRightOffset remaining) seeded
                (chunk, nextPending) <-
                  if written == 0
                    then pure (Unboxed.empty, Nothing)
                    else do
                      lastCell <- UnboxedMVector.unsafeRead buffer (written - 1)
                      frozen <- denseFreezeBatchCellBuffer buffer (written - 1)
                      pure (frozen, Just lastCell)
                let consumed =
                      (leftIndex - denseBatchCellsMergeLeftOffset remaining)
                        + (rightIndex - denseBatchCellsMergeRightOffset remaining)
                pure
                  FuelWork
                    { fuelWorkConsumed = fromIntegral consumed,
                      fuelWorkState =
                        DenseRowsMerger
                          ( remaining
                              { denseBatchCellsMergeLeftOffset = leftIndex,
                                denseBatchCellsMergeRightOffset = rightIndex
                              }
                          )
                          nextPending
                          (if Unboxed.null chunk then chunksRev else chunk : chunksRev)
                    }
{-# INLINABLE workOrderedBatchRowsMergeMeasured #-}

orderedBatchRowsMergerDone :: OrderedBatchRowsMerger time key val weight -> Bool
orderedBatchRowsMergerDone merger =
  case merger of
    OrderedBatchRowsMerger remaining _pending _chunksRev ->
      orderedBatchRowsMergeLeftOffset remaining >= Vector.length (orderedBatchRowsMergeLeft remaining)
        && orderedBatchRowsMergeRightOffset remaining >= Vector.length (orderedBatchRowsMergeRight remaining)
    DenseRowsMerger remaining _pending _chunksRev ->
      denseBatchCellsMergeLeftOffset remaining >= Unboxed.length (denseBatchCellsMergeLeft remaining)
        && denseBatchCellsMergeRightOffset remaining >= Unboxed.length (denseBatchCellsMergeRight remaining)
{-# INLINE orderedBatchRowsMergerDone #-}

finishOrderedBatchRowsMerge ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  OrderedBatchRowsMerger time key val weight ->
  OrderedBatchRows time key val weight
finishOrderedBatchRowsMerge merger =
  case merger of
    OrderedBatchRowsMerger remaining pending chunksRev ->
      OrderedBatchRows
        ( Vector.concat
            ( reverse
                ( mergeSortedBatchRowsFrom
                    pending
                    (orderedBatchRowsMergeLeft remaining)
                    (orderedBatchRowsMergeLeftOffset remaining)
                    (orderedBatchRowsMergeRight remaining)
                    (orderedBatchRowsMergeRightOffset remaining)
                    : chunksRev
                )
            )
        )
    DenseRowsMerger remaining pending chunksRev ->
      DenseRows
        ( Unboxed.concat
            ( reverse
                ( denseMergeSortedBatchCellsFrom
                    pending
                    (denseBatchCellsMergeLeft remaining)
                    (denseBatchCellsMergeLeftOffset remaining)
                    (denseBatchCellsMergeRight remaining)
                    (denseBatchCellsMergeRightOffset remaining)
                    : chunksRev
                )
            )
        )
{-# INLINE finishOrderedBatchRowsMerge #-}

compareBatchRow ::
  (Ord time, Ord key, Ord val) =>
  BatchRow time key val weight ->
  BatchRow time key val weight ->
  Ordering
compareBatchRow =
  comparing batchRowCell
{-# INLINE compareBatchRow #-}

batchRowCell :: BatchRow time key val weight -> OrderedBatchRowCell time key val
batchRowCell row =
  OrderedBatchRowCell
    { orderedBatchRowCellKey = batchRowKey row,
      orderedBatchRowCellValue = batchRowValue row,
      orderedBatchRowCellTime = batchRowTime row
    }
{-# INLINE batchRowCell #-}

foldOrderedBatchRows ::
  (acc -> BatchRow time key val weight -> acc) ->
  acc ->
  OrderedBatchRows time key val weight ->
  acc
foldOrderedBatchRows step initial rows =
  case rows of
    OrderedBatchRows vector ->
      Vector.foldl' step initial vector
    DenseRows cells ->
      Unboxed.foldl' (\acc cell -> step acc (denseBatchRow cell)) initial cells
{-# INLINE foldOrderedBatchRows #-}

-- Source anchor:
--   feldera/crates/dbsp/src/trace/cursor/cursor_list.rs: seek_key / step_key
-- Batches are ordered by key/value/time; keyed reads seek to the first matching key
-- and stop at the first non-matching key instead of scanning the whole chunk.
foldOrderedBatchRowsKeyRange ::
  Ord key =>
  key ->
  (acc -> BatchRow time key val weight -> acc) ->
  acc ->
  OrderedBatchRows time key val weight ->
  acc
foldOrderedBatchRowsKeyRange key step initial rows =
  case rows of
    OrderedBatchRows vector ->
      foldOrderedBatchRowsFrom
        (lowerBoundBatchRowKey key 0 (Vector.length vector) vector)
        key
        step
        initial
        vector
    DenseRows cells ->
      denseFoldBatchCellsFrom
        (denseLowerBoundBatchCellKey key 0 (Unboxed.length cells) cells)
        key
        step
        initial
        cells
{-# INLINE foldOrderedBatchRowsKeyRange #-}

foldOrderedBatchRowsFrom ::
  Ord key =>
  Int ->
  key ->
  (acc -> BatchRow time key val weight -> acc) ->
  acc ->
  Vector (BatchRow time key val weight) ->
  acc
foldOrderedBatchRowsFrom offset key step initial rows =
  case rows Vector.!? offset of
    Nothing ->
      initial
    Just row
      | batchRowKey row == key ->
          let next =
                step initial row
           in next `seq` foldOrderedBatchRowsFrom (offset + 1) key step next rows
      | otherwise ->
          initial
{-# INLINE foldOrderedBatchRowsFrom #-}

denseFoldBatchCellsFrom ::
  Int ->
  Int ->
  (acc -> BatchRow Int Int Int Int -> acc) ->
  acc ->
  Unboxed.Vector DenseBatchCell ->
  acc
denseFoldBatchCellsFrom offset key step initial cells =
  case cells Unboxed.!? offset of
    Nothing ->
      initial
    Just cell
      | denseBatchCellKey cell == key ->
          let next =
                step initial (denseBatchRow cell)
           in next `seq` denseFoldBatchCellsFrom (offset + 1) key step next cells
      | otherwise ->
          initial
{-# INLINE denseFoldBatchCellsFrom #-}

denseLowerBoundBatchCellKey ::
  Int ->
  Int ->
  Int ->
  Unboxed.Vector DenseBatchCell ->
  Int
denseLowerBoundBatchCellKey key lower upper cells
  | lower >= upper =
      lower
  | otherwise =
      case cells Unboxed.!? midpoint of
        Nothing ->
          lower
        Just cell
          | denseBatchCellKey cell < key ->
              denseLowerBoundBatchCellKey key (midpoint + 1) upper cells
          | otherwise ->
              denseLowerBoundBatchCellKey key lower midpoint cells
  where
    midpoint =
      lower + ((upper - lower) `quot` 2)
{-# INLINE denseLowerBoundBatchCellKey #-}

lowerBoundBatchRowKey ::
  Ord key =>
  key ->
  Int ->
  Int ->
  Vector (BatchRow time key val weight) ->
  Int
lowerBoundBatchRowKey key lower upper rows
  | lower >= upper =
      lower
  | otherwise =
      case rows Vector.!? midpoint of
        Nothing ->
          lower
        Just row
          | batchRowKey row < key ->
              lowerBoundBatchRowKey key (midpoint + 1) upper rows
          | otherwise ->
              lowerBoundBatchRowKey key lower midpoint rows
  where
    midpoint =
      lower + ((upper - lower) `quot` 2)
{-# INLINE lowerBoundBatchRowKey #-}

collectBatchKeyRows ::
  (Eq key, Ord time, Ord val, Eq weight, AdditiveGroup weight) =>
  (acc -> key -> ZSet (Timed time val) weight -> acc) ->
  BatchKeyRowsFold acc time key val weight ->
  BatchRow time key val weight ->
  BatchKeyRowsFold acc time key val weight
collectBatchKeyRows step foldState row =
  case batchKeyRowsCurrent foldState of
    Nothing ->
      foldState {batchKeyRowsCurrent = Just (batchRowKey row, [timedRow])}
    Just (key, rows)
      | key == batchRowKey row ->
          foldState {batchKeyRowsCurrent = Just (key, timedRow : rows)}
      | otherwise ->
          BatchKeyRowsFold
            { batchKeyRowsCurrent = Just (batchRowKey row, [timedRow]),
              batchKeyRowsAccumulated = emitBatchKeyRows step key rows (batchKeyRowsAccumulated foldState)
            }
  where
    timedRow =
      ( Timed
          { timedTime = batchRowTime row,
            timedValue = batchRowValue row
          },
        batchRowWeight row
      )
{-# INLINE collectBatchKeyRows #-}

finishBatchKeyRows ::
  (Ord time, Ord val, Eq weight, AdditiveGroup weight) =>
  (acc -> key -> ZSet (Timed time val) weight -> acc) ->
  BatchKeyRowsFold acc time key val weight ->
  acc
finishBatchKeyRows step foldState =
  case batchKeyRowsCurrent foldState of
    Nothing ->
      batchKeyRowsAccumulated foldState
    Just (key, rows) ->
      emitBatchKeyRows step key rows (batchKeyRowsAccumulated foldState)
{-# INLINE finishBatchKeyRows #-}

emitBatchKeyRows ::
  (Ord time, Ord val, Eq weight, AdditiveGroup weight) =>
  (acc -> key -> ZSet (Timed time val) weight -> acc) ->
  key ->
  [(Timed time val, weight)] ->
  acc ->
  acc
emitBatchKeyRows step key rows acc =
  step acc key (ZSet.zsetFromList rows)
{-# INLINE emitBatchKeyRows #-}

type BatchConstruction :: Type -> Type -> Type -> Type -> Type
data BatchConstruction time key val weight = BatchConstruction
  { batchConstructionLower :: !(Frontier time),
    batchConstructionUpper :: !(UpperFrontier time),
    batchConstructionRows :: ![BatchRow time key val weight]
  }

type BatchMerge :: Type -> Type -> Type -> Type -> Type
data BatchMerge time key val weight = BatchMerge
  { batchMergeLower :: !(Frontier time),
    batchMergeUpper :: !(UpperFrontier time),
    batchMergeBatchesRev :: ![Batch time key val weight]
  }

emptyBatchConstruction :: BatchConstruction time key val weight
emptyBatchConstruction =
  BatchConstruction
    { batchConstructionLower = emptyFrontier,
      batchConstructionUpper = emptyUpperFrontier,
      batchConstructionRows = []
    }
{-# INLINABLE emptyBatchConstruction #-}

emptyBatchMerge :: BatchMerge time key val weight
emptyBatchMerge =
  BatchMerge
    { batchMergeLower = emptyFrontier,
      batchMergeUpper = emptyUpperFrontier,
      batchMergeBatchesRev = []
    }
{-# INLINABLE emptyBatchMerge #-}

collectBatchConstruction ::
  (Eq weight, AdditiveGroup weight, Ord time, PartialOrder time) =>
  BatchConstruction time key val weight ->
  Update time key val weight ->
  BatchConstruction time key val weight
collectBatchConstruction construction updateValue
  | updateWeight updateValue == zero =
      construction
  | otherwise =
      BatchConstruction
        { batchConstructionLower = insertPoint updateTimeValue (batchConstructionLower construction),
          batchConstructionUpper = insertUpperFrontierPoint updateTimeValue (batchConstructionUpper construction),
          batchConstructionRows =
            BatchRow
              { batchRowTime = updateTimeValue,
                batchRowKey = updateKey updateValue,
                batchRowValue = updateVal updateValue,
                batchRowWeight = updateWeight updateValue
              }
              : batchConstructionRows construction
        }
  where
    updateTimeValue =
      updateTime updateValue
{-# INLINABLE collectBatchConstruction #-}

collectBatchMerge ::
  (Ord time, PartialOrder time) =>
  BatchMerge time key val weight ->
  Batch time key val weight ->
  BatchMerge time key val weight
collectBatchMerge construction batch =
  BatchMerge
    { batchMergeLower = mergeLowerFrontier (batchMergeLower construction) (batchLower batch),
      batchMergeUpper = mergeUpperFrontier (batchMergeUpper construction) (batchUpper batch),
      batchMergeBatchesRev =
        if batchNull batch
          then batchMergeBatchesRev construction
          else batch : batchMergeBatchesRev construction
    }
{-# INLINABLE collectBatchMerge #-}

-- Source anchor:
--   feldera/crates/dbsp/src/trace.rs: merge_batches
-- DBSP's ListMerger copies sorted non-overlapping tails directly; the local
-- cover keeps that descent law and only falls back to a k-way cursor merger
-- when overlaps actually obstruct gluing.
mergeOrderedBatchRowsCover ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  [OrderedBatchRows time key val weight] ->
  OrderedBatchRows time key val weight
mergeOrderedBatchRowsCover rowsCover =
  case rowsCover of
    [] ->
      orderedBatchRowsEmpty
    [rows] ->
      rows
    _ ->
      mergeOrderedBatchRowsCoverMany rowsCover
{-# INLINE mergeOrderedBatchRowsCover #-}

mergeOrderedBatchRowsCoverMany ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  [OrderedBatchRows time key val weight] ->
  OrderedBatchRows time key val weight
mergeOrderedBatchRowsCoverMany rowsCover =
  case batchRowsCoverChunks rowsCover of
    BoxedCoverChunks chunks ->
      case plan of
        CoverKWayOverlap ->
          OrderedBatchRows (mergeSortedBatchRowsCover chunks)
        CoverBoundaryOverlap ->
          OrderedBatchRows (collapseSortedBatchRows (Vector.concat chunks))
        _strictlyDisjoint ->
          OrderedBatchRows (Vector.concat chunks)
    DenseCoverChunks chunks ->
      case plan of
        CoverKWayOverlap ->
          DenseRows (denseMergeSortedBatchCellsCover chunks)
        CoverBoundaryOverlap ->
          DenseRows (denseCollapseSortedBatchCells (Unboxed.concat chunks))
        _strictlyDisjoint ->
          DenseRows (Unboxed.concat chunks)
  where
    plan =
      orderedBatchRowsCoverPlan rowsCover
{-# INLINE mergeOrderedBatchRowsCoverMany #-}

type BatchRowsCoverChunks :: Type -> Type -> Type -> Type -> Type
data BatchRowsCoverChunks time key val weight where
  BoxedCoverChunks ::
    ![Vector (BatchRow time key val weight)] ->
    BatchRowsCoverChunks time key val weight
  DenseCoverChunks ::
    ![Unboxed.Vector DenseBatchCell] ->
    BatchRowsCoverChunks Int Int Int Int

batchRowsCoverChunks ::
  forall time key val weight.
  [OrderedBatchRows time key val weight] ->
  BatchRowsCoverChunks time key val weight
batchRowsCoverChunks rowsCover =
  go rowsCover []
  where
    go ::
      [OrderedBatchRows time key val weight] ->
      [Vector (BatchRow time key val weight)] ->
      BatchRowsCoverChunks time key val weight
    go remaining boxedRev =
      case remaining of
        [] ->
          BoxedCoverChunks (reverse boxedRev)
        OrderedBatchRows vector : rest ->
          go rest (vector : boxedRev)
        DenseRows _ : _ ->
          DenseCoverChunks (fmap denseBatchCells rowsCover)
{-# INLINABLE batchRowsCoverChunks #-}

batchCoverRows ::
  Foldable batches =>
  batches (Batch time key val weight) ->
  [OrderedBatchRows time key val weight]
batchCoverRows =
  Foldable.foldr collectBatchCoverRow []
{-# INLINE batchCoverRows #-}

collectBatchCoverRow ::
  Batch time key val weight ->
  [OrderedBatchRows time key val weight] ->
  [OrderedBatchRows time key val weight]
collectBatchCoverRow batch rows
  | batchNull batch =
      rows
  | otherwise =
      batchRows batch : rows
{-# INLINE collectBatchCoverRow #-}

orderedBatchRowsCoverPlan ::
  (Ord time, Ord key, Ord val) =>
  [OrderedBatchRows time key val weight] ->
  BatchCoverPlan
orderedBatchRowsCoverPlan =
  finishOrderedBatchRowsCoverPlan
    . Foldable.foldl' collectOrderedBatchRowsCoverPlan emptyOrderedBatchRowsCoverPlanState
{-# INLINE orderedBatchRowsCoverPlan #-}

orderedBatchRowsCoverNull ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  [OrderedBatchRows time key val weight] ->
  Bool
orderedBatchRowsCoverNull rowsCover =
  case orderedBatchRowsCoverPlan rowsCover of
    CoverEmpty ->
      True
    CoverSingleton ->
      False
    CoverStrictlyDisjoint ->
      False
    CoverBoundaryOverlap ->
      orderedBatchRowsNull (mergeOrderedBatchRowsCoverMany rowsCover)
    CoverKWayOverlap ->
      orderedBatchRowsNull (mergeOrderedBatchRowsCoverMany rowsCover)
{-# INLINE orderedBatchRowsCoverNull #-}

orderedBatchRowsNull :: OrderedBatchRows time key val weight -> Bool
orderedBatchRowsNull rows =
  case rows of
    OrderedBatchRows vector ->
      Vector.null vector
    DenseRows cells ->
      Unboxed.null cells
{-# INLINE orderedBatchRowsNull #-}

emptyOrderedBatchRowsCoverPlanState :: OrderedBatchRowsCoverPlanState time key val
emptyOrderedBatchRowsCoverPlanState =
  OrderedBatchRowsCoverPlanState
    { orderedBatchRowsCoverPlanLastCell = Nothing,
      orderedBatchRowsCoverPlanNonEmptyChunks = 0,
      orderedBatchRowsCoverPlanHasBoundaryOverlap = False,
      orderedBatchRowsCoverPlanRequiresKWay = False
    }
{-# INLINE emptyOrderedBatchRowsCoverPlanState #-}

collectOrderedBatchRowsCoverPlan ::
  (Ord time, Ord key, Ord val) =>
  OrderedBatchRowsCoverPlanState time key val ->
  OrderedBatchRows time key val weight ->
  OrderedBatchRowsCoverPlanState time key val
collectOrderedBatchRowsCoverPlan planState rows =
  case orderedBatchRowsBoundary rows of
    Nothing ->
      planState
    Just (firstCell, lastCell) ->
      collectOrderedBatchRowsCoverBoundary firstCell lastCell planState
{-# INLINE collectOrderedBatchRowsCoverPlan #-}

collectOrderedBatchRowsCoverBoundary ::
  (Ord time, Ord key, Ord val) =>
  OrderedBatchRowCell time key val ->
  OrderedBatchRowCell time key val ->
  OrderedBatchRowsCoverPlanState time key val ->
  OrderedBatchRowsCoverPlanState time key val
collectOrderedBatchRowsCoverBoundary firstCell lastCell planState =
  planState
    { orderedBatchRowsCoverPlanLastCell = Just lastCell,
      orderedBatchRowsCoverPlanNonEmptyChunks =
        orderedBatchRowsCoverPlanNonEmptyChunks planState + 1,
      orderedBatchRowsCoverPlanHasBoundaryOverlap =
        orderedBatchRowsCoverPlanHasBoundaryOverlap planState || boundaryOrdering == Just EQ,
      orderedBatchRowsCoverPlanRequiresKWay =
        orderedBatchRowsCoverPlanRequiresKWay planState || boundaryOrdering == Just GT
    }
  where
    boundaryOrdering =
      compare <$> orderedBatchRowsCoverPlanLastCell planState <*> pure firstCell
{-# INLINE collectOrderedBatchRowsCoverBoundary #-}

finishOrderedBatchRowsCoverPlan :: OrderedBatchRowsCoverPlanState time key val -> BatchCoverPlan
finishOrderedBatchRowsCoverPlan planState
  | orderedBatchRowsCoverPlanRequiresKWay planState =
      CoverKWayOverlap
  | orderedBatchRowsCoverPlanNonEmptyChunks planState == 0 =
      CoverEmpty
  | orderedBatchRowsCoverPlanNonEmptyChunks planState == 1 =
      CoverSingleton
  | orderedBatchRowsCoverPlanHasBoundaryOverlap planState =
      CoverBoundaryOverlap
  | otherwise =
      CoverStrictlyDisjoint
{-# INLINE finishOrderedBatchRowsCoverPlan #-}

orderedBatchRowsBoundary ::
  OrderedBatchRows time key val weight ->
  Maybe (OrderedBatchRowCell time key val, OrderedBatchRowCell time key val)
orderedBatchRowsBoundary rows =
  case rows of
    OrderedBatchRows vector -> do
      firstRow <- vector Vector.!? 0
      lastRow <- vector Vector.!? (Vector.length vector - 1)
      pure (batchRowCell firstRow, batchRowCell lastRow)
    DenseRows cells -> do
      firstCell <- cells Unboxed.!? 0
      lastCell <- cells Unboxed.!? (Unboxed.length cells - 1)
      pure (denseBatchRowCell firstCell, denseBatchRowCell lastCell)
{-# INLINE orderedBatchRowsBoundary #-}

batchFromRows ::
  Frontier time ->
  UpperFrontier time ->
  OrderedBatchRows time key val weight ->
  Batch time key val weight
batchFromRows lower upper rows =
  Batch
    { batchLower = lower,
      batchUpper = upper,
      batchRows = rows,
      batchRowCount = orderedBatchRowsLength rows
    }
{-# INLINABLE batchFromRows #-}

mergeLowerFrontier ::
  (Ord time, PartialOrder time) =>
  Frontier time ->
  Frontier time ->
  Frontier time
mergeLowerFrontier left right =
  Foldable.foldl' (flip insertPoint) left (frontierPoints right)
{-# INLINABLE mergeLowerFrontier #-}

mergeUpperFrontier ::
  (Ord time, PartialOrder time) =>
  UpperFrontier time ->
  UpperFrontier time ->
  UpperFrontier time
mergeUpperFrontier left right =
  Foldable.foldl' (flip insertUpperFrontierPoint) left (upperFrontierPoints right)

{-# INLINABLE mergeUpperFrontier #-}
