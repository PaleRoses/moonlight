{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Internal.Trace.Id
  ( TraceId (..),
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Core
  ( PartialOrder (..),
  )

type TraceId :: Type
newtype TraceId = TraceId
  { unTraceId :: Int
  }
  deriving stock (Eq, Ord, Show)

instance PartialOrder TraceId where
  leq left right =
    unTraceId left <= unTraceId right
