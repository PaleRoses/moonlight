-- | The epoch calculus: version-anchored key transport between endpoint
-- snapshots.
--
-- 'Version' is an opaque, unbounded epoch counter. An 'Endpoint' pairs a
-- version with the result-key universe observed at that instant. An
-- 'EpochDelta' is an abstract partial transport: source keys either descend to
-- target keys or retire, and the carrier owns its target-side dirty set.
--
-- 'ContextProjectionDelta' is the bounded join-semilattice of dirty-key
-- pairs; its 'Data.Semigroup.Semigroup'/'Data.Monoid.Monoid' structure is
-- the seam consumed upstream and is preserved verbatim.
module Moonlight.Delta.Epoch
  ( Version,
    initialVersion,
    nextVersion,
    versionKey,
    versionFromKey,
    ContextProjectionDelta (..),
    emptyContextProjectionDelta,
    dirtyBaseDelta,
    dirtyResultDelta,
    normalizeContextProjectionDelta,
    nullContextProjectionDelta,
    mapContextProjectionDelta,
    ContextView (..),
    viewAt,
    viewWithVersion,
    viewWithSupport,
    viewWithSection,
    mapContextViewKeys,
    contextViewIsCurrent,
    contextViewIsStale,
    EpochKeyed,
    Endpoint (..),
    EpochDelta,
    epochDelta,
    identityDelta,
    DeltaViolation (..),
    sourceEndpointOf,
    targetEndpointOf,
    sourceVersion,
    targetVersion,
    sourceKeys,
    targetKeys,
    transportOverrides,
    freshKeys,
    retiredKeys,
    changedKeysAcrossEpoch,
    transportKeys,
    Transport (..),
    ViewTransportError (..),
    transportView,
    ComposeError (..),
    composeDelta,
  )
where

import Moonlight.Delta.Epoch.Internal.Compose
  ( composeDelta,
  )
import Moonlight.Delta.Epoch.Internal.Construction
  ( epochDelta,
    identityDelta,
  )
import Moonlight.Delta.Epoch.Internal.Projection
  ( ContextProjectionDelta (..),
    dirtyBaseDelta,
    dirtyResultDelta,
    emptyContextProjectionDelta,
    mapContextProjectionDelta,
    normalizeContextProjectionDelta,
    nullContextProjectionDelta,
  )
import Moonlight.Delta.Epoch.Internal.Transport
  ( transportKeys,
    transportView,
  )
import Moonlight.Delta.Epoch.Internal.Types
  ( ComposeError (..),
    EpochDelta,
    DeltaViolation (..),
    Endpoint (..),
    EpochKeyed,
    Transport (..),
    ViewTransportError (..),
    changedKeysAcrossEpoch,
    freshKeys,
    retiredKeys,
    sourceEndpointOf,
    sourceKeys,
    sourceVersion,
    targetEndpointOf,
    targetKeys,
    targetVersion,
    transportOverrides,
  )
import Moonlight.Delta.Epoch.Internal.Version
  ( Version,
    versionFromKey,
    versionKey,
    initialVersion,
    nextVersion,
  )
import Moonlight.Delta.Epoch.Internal.View
  ( ContextView (..),
    contextViewIsCurrent,
    contextViewIsStale,
    mapContextViewKeys,
    viewAt,
    viewWithSection,
    viewWithSupport,
    viewWithVersion,
  )
