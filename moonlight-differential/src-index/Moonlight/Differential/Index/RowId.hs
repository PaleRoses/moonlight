{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Index.RowId
  ( RowId,
    RowIdError (..),
    mkRowId,
    rowIdInt,
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Differential.Internal.Index.RowId
  ( RowId (..),
  )

type RowIdError :: Type
data RowIdError
  = NegativeRowId !Int
  deriving stock (Eq, Ord, Show)

mkRowId :: Int -> Either RowIdError RowId
mkRowId value
  | value < 0 =
      Left (NegativeRowId value)
  | otherwise =
      Right (RowId value)
{-# INLINE mkRowId #-}

rowIdInt :: RowId -> Int
rowIdInt (RowId value) =
  value
{-# INLINE rowIdInt #-}
