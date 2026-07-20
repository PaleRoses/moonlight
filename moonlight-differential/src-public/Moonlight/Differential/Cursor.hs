{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Cursor
  ( Cursor,
    CursorCell (..),
    emptyCursor,
    cursorFromZSet,
    cursorToZSet,
    cursorMerge,
    cursorCells,
    cursorNull,
    cursorCellCount,
    foldCursorWithTime,
    foldCursor,
    cursorValueWeights,
  )
where

import Data.Vector
  ( Vector,
  )
import Data.Vector qualified as Vector
import Data.Kind
  ( Type,
  )

import Moonlight.Core (AdditiveGroup (..), AdditiveMonoid (..))
import Moonlight.Differential.Algebra.ZSet
  ( Timed (..),
    ZSet,
  )
import Moonlight.Differential.Algebra.ZSet qualified as ZSet

type CursorCell :: Type -> Type -> Type -> Type
data CursorCell time val weight = CursorCell
  { cursorCellTime :: !time,
    cursorCellValue :: !val,
    cursorCellWeight :: !weight
  }
  deriving stock (Eq, Ord, Show, Read)

type Cursor :: Type -> Type -> Type -> Type
newtype Cursor time val weight = Cursor
  { cursorCellsRaw :: Vector (CursorCell time val weight)
  }
  deriving stock (Eq, Ord, Show)

cursorCells :: Cursor time val weight -> Vector (CursorCell time val weight)
cursorCells =
  cursorCellsRaw
{-# INLINE cursorCells #-}

emptyCursor :: Cursor time val weight
emptyCursor =
  Cursor Vector.empty

cursorFromZSet ::
  ZSet (Timed time val) weight ->
  Cursor time val weight
cursorFromZSet rows =
  Cursor
    ( Vector.fromList
        [ CursorCell
            { cursorCellTime = timedTime timedCell,
              cursorCellValue = timedValue timedCell,
              cursorCellWeight = weight
            }
        | (timedCell, weight) <- ZSet.zsetToAscList rows
        ]
    )

cursorToZSet ::
  (Ord time, Ord val, Eq weight, AdditiveGroup weight) =>
  Cursor time val weight ->
  ZSet (Timed time val) weight
cursorToZSet =
  foldCursorWithTime
    ( \acc time val weight ->
        ZSet.zsetInsert
          Timed
            { timedTime = time,
              timedValue = val
            }
          weight
          acc
    )
    ZSet.zsetEmpty

cursorMerge ::
  (Ord time, Ord val, Eq weight, AdditiveGroup weight) =>
  Cursor time val weight ->
  Cursor time val weight ->
  Cursor time val weight
cursorMerge (Cursor left) (Cursor right)
  | Vector.null left =
      Cursor right
  | Vector.null right =
      Cursor left
  | otherwise =
      Cursor
        ( Vector.mapMaybe
            id
            ( Vector.unfoldr
                nextCursorMergeStep
                (cursorMergeState left right)
            )
        )
{-# INLINE cursorMerge #-}

type CursorMergeState :: Type -> Type -> Type -> Type
data CursorMergeState time val weight = CursorMergeState
  { cursorMergeLeftCells :: !(Vector (CursorCell time val weight)),
    cursorMergeLeftIndex :: {-# UNPACK #-} !Int,
    cursorMergeRightCells :: !(Vector (CursorCell time val weight)),
    cursorMergeRightIndex :: {-# UNPACK #-} !Int
  }

cursorMergeState ::
  Vector (CursorCell time val weight) ->
  Vector (CursorCell time val weight) ->
  CursorMergeState time val weight
cursorMergeState left right =
  CursorMergeState
    { cursorMergeLeftCells = left,
      cursorMergeLeftIndex = 0,
      cursorMergeRightCells = right,
      cursorMergeRightIndex = 0
    }
{-# INLINE cursorMergeState #-}

nextCursorMergeStep ::
  (Ord time, Ord val, Eq weight, AdditiveGroup weight) =>
  CursorMergeState time val weight ->
  Maybe (Maybe (CursorCell time val weight), CursorMergeState time val weight)
nextCursorMergeStep state =
  case (leftCursorMergeCell state, rightCursorMergeCell state) of
    (Nothing, Nothing) ->
      Nothing
    (Just leftCell, Nothing) ->
      Just (Just leftCell, advanceLeftCursorMergeCell state)
    (Nothing, Just rightCell) ->
      Just (Just rightCell, advanceRightCursorMergeCell state)
    (Just leftCell, Just rightCell) ->
      case compareCursorCellPosition leftCell rightCell of
        LT ->
          Just (Just leftCell, advanceLeftCursorMergeCell state)
        GT ->
          Just (Just rightCell, advanceRightCursorMergeCell state)
        EQ ->
          Just
            ( mergeEqualCursorCells leftCell rightCell,
              advanceBothCursorMergeCells state
            )
{-# INLINE nextCursorMergeStep #-}

leftCursorMergeCell :: CursorMergeState time val weight -> Maybe (CursorCell time val weight)
leftCursorMergeCell state =
  cursorMergeLeftCells state Vector.!? cursorMergeLeftIndex state
{-# INLINE leftCursorMergeCell #-}

rightCursorMergeCell :: CursorMergeState time val weight -> Maybe (CursorCell time val weight)
rightCursorMergeCell state =
  cursorMergeRightCells state Vector.!? cursorMergeRightIndex state
{-# INLINE rightCursorMergeCell #-}

advanceLeftCursorMergeCell :: CursorMergeState time val weight -> CursorMergeState time val weight
advanceLeftCursorMergeCell state =
  state {cursorMergeLeftIndex = cursorMergeLeftIndex state + 1}
{-# INLINE advanceLeftCursorMergeCell #-}

advanceRightCursorMergeCell :: CursorMergeState time val weight -> CursorMergeState time val weight
advanceRightCursorMergeCell state =
  state {cursorMergeRightIndex = cursorMergeRightIndex state + 1}
{-# INLINE advanceRightCursorMergeCell #-}

advanceBothCursorMergeCells :: CursorMergeState time val weight -> CursorMergeState time val weight
advanceBothCursorMergeCells =
  advanceRightCursorMergeCell . advanceLeftCursorMergeCell
{-# INLINE advanceBothCursorMergeCells #-}

mergeEqualCursorCells ::
  (Eq weight, AdditiveGroup weight) =>
  CursorCell time val weight ->
  CursorCell time val weight ->
  Maybe (CursorCell time val weight)
mergeEqualCursorCells left right =
  let !mergedWeight =
        add (cursorCellWeight left) (cursorCellWeight right)
   in if mergedWeight == zero
        then Nothing
        else
          Just
            left
              { cursorCellWeight = mergedWeight
              }
{-# INLINE mergeEqualCursorCells #-}

compareCursorCellPosition ::
  (Ord time, Ord val) =>
  CursorCell time val weight ->
  CursorCell time val weight ->
  Ordering
compareCursorCellPosition left right =
  compare (cursorCellValue left) (cursorCellValue right)
    <> compare (cursorCellTime left) (cursorCellTime right)
{-# INLINE compareCursorCellPosition #-}

cursorNull :: Cursor time val weight -> Bool
cursorNull (Cursor rows) =
  Vector.null rows

cursorCellCount :: Cursor time val weight -> Int
cursorCellCount (Cursor rows) =
  Vector.length rows

foldCursorWithTime ::
  (acc -> time -> val -> weight -> acc) ->
  acc ->
  Cursor time val weight ->
  acc
foldCursorWithTime step initial (Cursor rows) =
  Vector.foldl'
    ( \acc cell ->
        step
          acc
          (cursorCellTime cell)
          (cursorCellValue cell)
          (cursorCellWeight cell)
    )
    initial
    rows

foldCursor ::
  (Ord val, Eq weight, AdditiveGroup weight) =>
  (acc -> val -> weight -> acc) ->
  acc ->
  Cursor time val weight ->
  acc
foldCursor step initial cursor =
  ZSet.zsetFold step initial (cursorValueWeights cursor)

cursorValueWeights ::
  (Ord val, Eq weight, AdditiveGroup weight) =>
  Cursor time val weight ->
  ZSet val weight
cursorValueWeights =
  foldCursorWithTime collect ZSet.zsetEmpty
  where
    collect ::
      (Ord val, Eq weight, AdditiveGroup weight) =>
      ZSet val weight ->
      time ->
      val ->
      weight ->
      ZSet val weight
    collect acc _time val weight =
      ZSet.zsetInsert val weight acc
