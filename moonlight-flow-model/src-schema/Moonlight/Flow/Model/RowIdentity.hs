{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Model.RowIdentity
  ( rowBlockIdentityForAtom,
    rowBlockIdentityForQuery,
  )
where

import Moonlight.Core
  ( AtomId,
    QueryId,
    atomIdKey,
    queryIdKey,
  )
import Moonlight.Differential.Row.Block
  ( RowBlockIdentity (..),
  )

rowBlockIdentityForAtom :: Int -> Int -> Int -> AtomId -> Int -> RowBlockIdentity
rowBlockIdentityForAtom baseRevision overlayEpoch planFingerprint atomId generation =
  RowBlockIdentity
    { rowBlockBaseRevision = baseRevision,
      rowBlockOverlayEpoch = overlayEpoch,
      rowBlockPlanFingerprint = planFingerprint,
      rowBlockEntityKey = atomIdKey atomId,
      rowBlockGeneration = generation
    }
{-# INLINE rowBlockIdentityForAtom #-}

rowBlockIdentityForQuery :: Int -> Int -> Int -> QueryId -> Int -> RowBlockIdentity
rowBlockIdentityForQuery baseRevision overlayEpoch planFingerprint queryId generation =
  RowBlockIdentity
    { rowBlockBaseRevision = baseRevision,
      rowBlockOverlayEpoch = overlayEpoch,
      rowBlockPlanFingerprint = planFingerprint,
      rowBlockEntityKey = queryIdKey queryId,
      rowBlockGeneration = generation
    }
{-# INLINE rowBlockIdentityForQuery #-}
