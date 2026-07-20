{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Internal.Index.RowIdSet
  ( RowIdSet (..),
  )
where

import Data.IntSet
  ( IntSet,
  )
import Data.Kind
  ( Type,
  )
import Data.Primitive.PrimArray
  ( PrimArray,
  )

type RowIdSet :: Type
data RowIdSet
  = RowIdSetSmall !(PrimArray Int)
  | RowIdSetLarge !IntSet
  deriving stock (Eq, Ord, Show)
