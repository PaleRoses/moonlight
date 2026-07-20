{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

-- | The presence-based provenance identity of a factor-output bag as an
-- idempotent commutative monoid, with its arena render.  A contribution's
-- rendered value reads only which reference ids, unit witnesses, and
-- obstructions are present — never their multiplicities — so the fold that
-- materializes a bag is idempotent and the arena interning is a downstream
-- projection, not a participant in view maintenance.
module Moonlight.Flow.Execution.Factor.Contribution.Identity
  ( ProvAccum (..),
    emptyProvAccum,
    provAccumOfValue,
    renderProvAccum,
  )
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Flow.Execution.Observe.Provenance.Arena
  ( internProvWithTelemetry,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Args
  ( provArgsFromSet,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvArena,
    ProvId (..),
    ProvNode (..),
    ProvVal (..),
    ProvenanceObstruction,
  )
import Moonlight.Flow.Execution.Observe.RepairTelemetry
  ( RepairTelemetry,
    RepairTelemetryConfig,
    emptyRepairTelemetry,
  )

data ProvAccum = ProvAccum
  { paValueRefs :: !IntSet,
    paHasUnit :: !Bool,
    paObstructions :: !(Set ProvenanceObstruction)
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup ProvAccum where
  ProvAccum leftRefs leftUnit leftObstructions <> ProvAccum rightRefs rightUnit rightObstructions =
    ProvAccum
      (IntSet.union leftRefs rightRefs)
      (leftUnit || rightUnit)
      (Set.union leftObstructions rightObstructions)
  {-# INLINE (<>) #-}

instance Monoid ProvAccum where
  mempty = ProvAccum IntSet.empty False Set.empty
  {-# INLINE mempty #-}

emptyProvAccum :: ProvAccum
emptyProvAccum = mempty
{-# INLINE emptyProvAccum #-}

provAccumOfValue :: ProvVal -> ProvAccum
provAccumOfValue value =
  case value of
    PVZero -> mempty
    PVOne -> mempty {paHasUnit = True}
    PVRef (ProvId rawId) -> mempty {paValueRefs = IntSet.singleton rawId}
    PVObstructed obstruction -> mempty {paObstructions = Set.singleton obstruction}
{-# INLINE provAccumOfValue #-}

renderProvAccum ::
  RepairTelemetryConfig ->
  ProvArena ->
  ProvAccum ->
  (ProvArena, ProvVal, RepairTelemetry)
renderProvAccum config arena accum =
  case Set.lookupMin (paObstructions accum) of
    Just obstruction ->
      (arena, PVObstructed obstruction, emptyRepairTelemetry)
    Nothing ->
      case IntSet.minView (paValueRefs accum) of
        Nothing ->
          ( arena,
            if paHasUnit accum then PVOne else PVZero,
            emptyRepairTelemetry
          )
        Just (onlyRef, restRefs)
          | IntSet.null restRefs ->
              (arena, PVRef (ProvId onlyRef), emptyRepairTelemetry)
          | otherwise ->
              let !args =
                    provArgsFromSet (IntSet.insert onlyRef restRefs)
                  (!arena1, !pid, !telemetry) =
                    internProvWithTelemetry config (PNSum args) arena
               in (arena1, PVRef pid, telemetry)
{-# INLINE renderProvAccum #-}
