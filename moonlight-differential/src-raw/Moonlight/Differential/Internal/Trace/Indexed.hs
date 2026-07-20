{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Internal.Trace.Indexed
  ( TraceIdCursor (..),
    IndexedTrace (..),
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

type TraceIdCursor :: Type
data TraceIdCursor
  = TraceIdAvailable !TraceId
  | TraceIdsExhausted
  deriving stock (Eq, Show)

type IndexedTrace :: Type -> Type -> Type
data IndexedTrace entry indexes = IndexedTrace
  { indexedTraceNextIdRaw :: !TraceIdCursor,
    indexedTraceEntriesRaw :: !(IntMap entry),
    indexedTraceIndexesRaw :: !indexes
  }
  deriving stock (Eq, Show)
