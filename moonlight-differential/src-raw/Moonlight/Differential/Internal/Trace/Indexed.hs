{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Internal.Trace.Indexed
  ( IndexedTrace (..),
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.Kind
  ( Type,
  )
import Moonlight.Differential.Internal.Trace.Id
  ( TraceId,
  )

type IndexedTrace :: Type -> Type -> Type
data IndexedTrace entry indexes = IndexedTrace
  { indexedTraceNextIdRaw :: !TraceId,
    indexedTraceEntriesRaw :: !(IntMap entry),
    indexedTraceIndexesRaw :: !indexes
  }
  deriving stock (Eq, Show)
