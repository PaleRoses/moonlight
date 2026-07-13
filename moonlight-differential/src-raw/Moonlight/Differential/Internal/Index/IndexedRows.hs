{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Internal.Index.IndexedRows
  ( IndexedRows (..),
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntSet
  ( IntSet,
  )
import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Moonlight.Differential.Internal.Index.RowIdSet
  ( RowIdSet,
  )

type IndexedRows :: Type -> Type -> Type -> Type
data IndexedRows layout key payload = IndexedRows
  { irLayout :: !layout,
    irColIndex :: !(IntMap Int),
    irLiveRows :: !IntSet,
    irKeyByRowId :: !(IntMap key),
    irIdByKey :: !(Map key Int),
    irPayloadByKey :: !(Map key payload),
    irValueIx :: !(IntMap (IntMap RowIdSet)),
    irNextRowId :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)
