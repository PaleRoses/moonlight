{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Execution.Observe.Provenance.Id
  ( ProvId (..),
    ProvArenaScope,
    initialProvArenaScope,
    nextProvArenaScope,
    ProvenanceObstruction (..),
  )
where

import Data.Kind (Type)

type ProvId :: Type
newtype ProvId = ProvId {unProvId :: Int}
  deriving stock (Eq, Ord, Show)

-- | Identity scope for interpreting 'ProvId' keys.
--
-- Non-moving collection preserves the scope. Moving compaction rewrites the
-- small-integer id namespace and must advance the scope so raw-id caches can
-- reject stale entries.
type ProvArenaScope :: Type
newtype ProvArenaScope = ProvArenaScope {unProvArenaScope :: Int}
  deriving stock (Eq, Ord, Show)

nextProvArenaScope :: ProvArenaScope -> ProvArenaScope
nextProvArenaScope (ProvArenaScope scope) =
  ProvArenaScope (scope + 1)
{-# INLINE nextProvArenaScope #-}

type ProvenanceObstruction :: Type
data ProvenanceObstruction
  = DanglingProvId !ProvId
  | StaleProvIdRemap !ProvId
  | StaleProvSupportMemoScope !ProvArenaScope !ProvArenaScope
  | MovingCompactionRequiresRemapTransaction
  | MissingReachableProvId !ProvId
  | MissingProvIdRemap !ProvId
  deriving stock (Eq, Ord, Show)

initialProvArenaScope :: ProvArenaScope
initialProvArenaScope = ProvArenaScope 0
{-# INLINE initialProvArenaScope #-}
