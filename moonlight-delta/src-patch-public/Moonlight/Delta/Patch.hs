module Moonlight.Delta.Patch
  ( CellPatch,
    PatchKey,
    PatchValue,
    Patch,
    ApplyError (..),
    ComposeError (..),
    ReplayError (..),
    MerkleDeltaHash,
    MultisetDeltaHash,
    Digest128,
    DeltaHashDigest (..),
    DeltaHashBuildError (..),
    DeltaHashApplyError (..),
    assertAbsent,
    insert,
    delete,
    replace,
    matchCell,
    cellBefore,
    cellAfter,
    -- | Build a cell patch from observable before and after endpoints.
    --
    -- @Nothing@ means the key is absent on that side; @Just value@ means the
    -- key is present with that value.
    cellFromEndpoints,
    mapCell,
    traverseCell,
    empty,
    singleton,
    -- | Build a patch from an authoritative final map of logical rows.
    --
    -- Duplicate keys use the same last-wins semantics as 'Data.Map.Strict.fromList'.
    -- Use 'recordMany' for a temporal edit log whose repeated keys must stitch
    -- through matching before/after boundaries.
    fromList,
    -- | Build from an ascending logical-row list when possible, falling back
    -- to 'fromList' for non-ascending input. Repeated adjacent keys are treated
    -- as authoritative replacement rows, not as a temporal edit sequence.
    fromAscList,
    toAscList,
    lookup,
    mapMaybeWithKey,
    foldWithKey,
    foldWithKey',
    traverseWithKey,
    normalize,
    null,
    size,
    support,
    compose,
    recordApplied,
    -- | Record a batch of applied edits as a temporal edit log.
    --
    -- Repeated keys must form a valid chain: each later before endpoint must
    -- equal the previous after endpoint. Boundary mismatches are reported as
    -- 'ComposeBoundaryMismatch'.
    recordMany,
    invert,
    diff,
    apply,
    replay,
    buildMerkleDeltaHash,
    merkleDeltaHashState,
    merkleDeltaHashDigest,
    applyMerkleDeltaHash,
    buildMultisetDeltaHash,
    multisetDeltaHashState,
    multisetDeltaHashDigest,
    applyMultisetDeltaHash,
  )
where

import Moonlight.Delta.Patch.Internal.Apply
  ( apply,
  )
import Moonlight.Delta.Patch.Internal.Cell
  ( assertAbsent,
    cellAfter,
    cellBefore,
    cellFromEndpoints,
    delete,
    insert,
    mapCell,
    matchCell,
    replace,
    traverseCell,
  )
import Moonlight.Delta.Patch.Internal.Compose.Core
  ( compose,
  )
import Moonlight.Delta.Patch.Internal.Compose.Record
  ( recordApplied,
    recordMany,
  )
import Moonlight.Delta.Patch.Internal.Construction
  ( diff,
    empty,
    foldWithKey,
    foldWithKey',
    fromAscList,
    fromList,
    invert,
    lookup,
    mapMaybeWithKey,
    singleton,
    toAscList,
    traverseWithKey,
  )
import Moonlight.Delta.Patch.Internal.Replay
  ( replay,
  )
import Moonlight.Delta.Patch.Internal.IncrementalDigest
  ( DeltaHashApplyError (..),
    DeltaHashBuildError (..),
    DeltaHashDigest (..),
    Digest128,
  )
import Moonlight.Delta.Patch.Internal.MerkleDeltaHash
  ( MerkleDeltaHash,
    applyMerkleDeltaHash,
    buildMerkleDeltaHash,
    merkleDeltaHashDigest,
    merkleDeltaHashState,
  )
import Moonlight.Delta.Patch.Internal.MultisetDeltaHash
  ( MultisetDeltaHash,
    applyMultisetDeltaHash,
    buildMultisetDeltaHash,
    multisetDeltaHashDigest,
    multisetDeltaHashState,
  )
import Moonlight.Delta.Patch.Internal.Types
  ( ApplyError (..),
    CellPatch,
    ComposeError (..),
    PatchKey,
    PatchValue,
    Patch,
    ReplayError (..),
    normalize,
    null,
    size,
    support,
  )
