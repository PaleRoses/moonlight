{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Execution.Observe.Provenance.Types.Internal
  ( ProvId (..),
    ProvArenaScope,
    nextProvArenaScope,
    ProvenanceObstruction (..),
    ProvVal (..),
    ProvNode (..),
    ProvGen (..),
    ProvEntry (..),
    ProvArena,
    paNodes,
    paCons,
    paNext,
    paEpoch,
    paScope,
    emptyProvArena,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Flow.Execution.Observe.Provenance.Args (ProvArgs)
import Moonlight.Flow.Execution.Observe.Provenance.Id
  ( ProvArenaScope,
    ProvId (..),
    ProvenanceObstruction (..),
    initialProvArenaScope,
    nextProvArenaScope,
  )
import Moonlight.Differential.Row.Tuple (RowTupleKey)
import Moonlight.Flow.Plan.Query.Core (AtomId)

type ProvVal :: Type
data ProvVal
  = PVZero
  | PVOne
  | PVRef {-# UNPACK #-} !ProvId
  | PVObstructed !ProvenanceObstruction
  deriving stock (Eq, Ord, Show)

-- | Composite provenance node.
--
-- Atom leaves identify their row by /content/ ('RowTupleKey'), not by a DB-local
-- 'RowId'.  This keeps provenance hash-consing and factor caches coherent
-- across independent 'Store' rebuilds: the same atom row has the
-- same provenance leaf regardless of which DB it was assigned an id in.
-- Callers that need a DB-local row id resolve it via 'rowIdForRow' against
-- the current view.
--
-- Sum/product arguments are stored as a sorted nub list for deterministic
-- hash-cons keys.
type ProvNode :: Type
data ProvNode
  = PNAtom {-# UNPACK #-} !AtomId !RowTupleKey
  | PNSum !ProvArgs
  | PNProd !ProvArgs
  deriving stock (Eq, Ord, Show)

-- | Generation of a 'ProvEntry' — migrates upward under GC pressure:
-- 'GenNursery' → 'GenCached' (after one minor survival) → 'GenStable'
-- (after @pgcStableSurvivals@ majors).
type ProvGen :: Type
data ProvGen
  = GenNursery
  | GenCached
  | GenStable
  deriving stock (Eq, Ord, Show)

type ProvEntry :: Type
data ProvEntry = ProvEntry
  { peNode :: !ProvNode,
    peGen :: !ProvGen,
    peSurvivals :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

type ProvArena :: Type
data ProvArena = ProvArena
  { paNext :: {-# UNPACK #-} !Int,
    -- | Monotone event counter for telemetry and collection history.
    --
    -- Non-moving collection increments this, but preserves raw 'ProvId'
    -- interpretation.
    paEpoch :: {-# UNPACK #-} !Int,
    -- | Moving-id namespace for support memo keys.
    --
    -- This changes only when compaction rewrites ids. It is deliberately not
    -- exported through the public 'Types' module: callers may observe
    -- collection history, not mint id scopes.
    paScope :: {-# UNPACK #-} !ProvArenaScope,
    paNodes :: !(IntMap ProvEntry),
    paCons :: !(Map ProvNode ProvId)
  }
  deriving stock (Eq, Show)

emptyProvArena :: ProvArena
emptyProvArena =
  ProvArena
    { paNext = 0,
      paEpoch = 0,
      paScope = initialProvArenaScope,
      paNodes = IntMap.empty,
      paCons = Map.empty
    }
