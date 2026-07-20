{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Index.RowId
  ( RowId,
    RowIdCursor (..),
    RowIdError (..),
    initialRowId,
    mkRowId,
    rowIdInt,
    rowIdCursorFromExclusiveUniverse,
    rowIdCursorExclusiveUniverse,
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Differential.Internal.Index.RowId
  ( RowId (..),
    RowIdCursor (..),
  )

type RowIdError :: Type
data RowIdError
  = NegativeRowId !Int
  | ReservedRowId !Int
  deriving stock (Eq, Ord, Show)

initialRowId :: RowId
initialRowId =
  RowId 0
{-# INLINE initialRowId #-}

mkRowId :: Int -> Either RowIdError RowId
mkRowId value
  | value < 0 =
      Left (NegativeRowId value)
  | value == maxBound =
      Left (ReservedRowId value)
  | otherwise =
      Right (RowId value)
{-# INLINE mkRowId #-}

rowIdInt :: RowId -> Int
rowIdInt (RowId value) =
  value
{-# INLINE rowIdInt #-}

rowIdCursorFromExclusiveUniverse :: Int -> Either RowIdError RowIdCursor
rowIdCursorFromExclusiveUniverse universe
  | universe == maxBound =
      Right RowIdsExhausted
  | otherwise =
      RowIdAvailable <$> mkRowId universe
{-# INLINE rowIdCursorFromExclusiveUniverse #-}

rowIdCursorExclusiveUniverse :: RowIdCursor -> Int
rowIdCursorExclusiveUniverse cursor =
  case cursor of
    RowIdAvailable rowId ->
      rowIdInt rowId
    RowIdsExhausted ->
      maxBound
{-# INLINE rowIdCursorExclusiveUniverse #-}
