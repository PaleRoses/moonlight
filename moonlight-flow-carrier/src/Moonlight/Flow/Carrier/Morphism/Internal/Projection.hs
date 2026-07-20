{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Morphism.Internal.Projection
  ( ProjectionError (..),
    projectRowDeltaWithProfile,
    projectRowDeltaExactWithProfile,
    projectRowDeltaExact,
    projectRowDelta,
  )
where

import Data.Bifunctor
  ( first,
  )
import Moonlight.Core
  ( SlotId,
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchChangeMap,
    plainRowPatchFromChangeMap,
    traversePlainRowPatchRowsWith,
  )
import Moonlight.Flow.Plan.Shape.Encode
  ( canonicalSlotWords,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot,
    canonSlotKey,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Model.Schema.Morphism
  ( ProjectionProfile,
    SchemaProjection,
    SchemaProjectionError,
    projectAtomRow,
    projectAtomRowMapExact,
    projectionProfileWith,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )

data ProjectionError
  = ProjectionSchemaError !(SchemaProjectionError SlotId CanonSlot)
  deriving stock (Eq, Ord, Show)

projectRowDeltaWithProfile ::
  StableDigest128 ->
  Bool ->
  Bool ->
  SchemaProjection SlotId CanonSlot ->
  RowDelta ->
  Either ProjectionError (RowDelta, ProjectionProfile CanonSlot)
projectRowDeltaWithProfile coverageDigest boundaryExact sensitiveCollision projection rows = do
  projectedRows <-
    projectRowDelta projection rows
  pure
    ( projectedRows,
      projectionProfileWith
        canonSlotKey
        canonicalSlotWords
        coverageDigest
        boundaryExact
        sensitiveCollision
        projection
    )
{-# INLINE projectRowDeltaWithProfile #-}

projectRowDeltaExactWithProfile ::
  StableDigest128 ->
  Bool ->
  Bool ->
  SchemaProjection SlotId CanonSlot ->
  RowDelta ->
  Either ProjectionError (RowDelta, ProjectionProfile CanonSlot)
projectRowDeltaExactWithProfile coverageDigest boundaryExact sensitiveCollision projection rows = do
  projectedRows <-
    first ProjectionSchemaError $
      projectRowDeltaExact
        projection
        rows
  pure
    ( projectedRows,
      projectionProfileWith
        canonSlotKey
        canonicalSlotWords
        coverageDigest
        boundaryExact
        sensitiveCollision
        projection
    )
{-# INLINE projectRowDeltaExactWithProfile #-}

projectRowDeltaExact ::
  SchemaProjection SlotId CanonSlot ->
  RowDelta ->
  Either (SchemaProjectionError SlotId CanonSlot) RowDelta
projectRowDeltaExact projection delta =
  plainRowPatchFromChangeMap
    <$> projectAtomRowMapExact
      projection
      id
      (plainRowPatchChangeMap delta)
{-# INLINE projectRowDeltaExact #-}

projectRowDelta ::
  SchemaProjection SlotId CanonSlot ->
  RowDelta ->
  Either ProjectionError RowDelta
projectRowDelta projection =
  traversePlainRowPatchRowsWith
    (first ProjectionSchemaError . projectAtomRow projection)
{-# INLINE projectRowDelta #-}
