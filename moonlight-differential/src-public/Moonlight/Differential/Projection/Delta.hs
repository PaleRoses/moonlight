{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Projection.Delta
  ( ProjectionDelta,
    projectionDeltaOps,
    projectionDelta,
    projectionDeltaProjection,
    projectionDeltaWork,
    projectionDeltaInvalidation,
    projectionDeltaWithProjection,
    projectionOnly,
    invalidationOnly,
    bootstrapQueries,
    projectQueries,
    projectQuery,
    pruneQuery,
    restrictQuery,
  )
where

import Data.IntSet (IntSet)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Delta.Epoch qualified as PropagationEpoch
import Moonlight.Differential.Delta
  ( DeltaOps,
    monoidDeltaOps,
  )
import Moonlight.Differential.Projection.Work
  ( ProjectionPhase (..),
    ProjectionWork,
    bootstrapProjection,
    projectionWorkForPhase,
  )

type ProjectionDelta :: Type -> Type -> Type
data ProjectionDelta query invalidation = ProjectionDelta
  { pdProjection :: !(PropagationEpoch.ContextProjectionDelta IntSet),
    pdWork :: !(Map query ProjectionWork),
    pdInvalidation :: !invalidation
  }
  deriving stock (Eq, Show)

projectionDelta ::
  PropagationEpoch.ContextProjectionDelta IntSet ->
  Map query ProjectionWork ->
  invalidation ->
  ProjectionDelta query invalidation
projectionDelta =
  ProjectionDelta
{-# INLINE projectionDelta #-}

projectionDeltaProjection ::
  ProjectionDelta query invalidation ->
  PropagationEpoch.ContextProjectionDelta IntSet
projectionDeltaProjection =
  pdProjection
{-# INLINE projectionDeltaProjection #-}

projectionDeltaWork ::
  ProjectionDelta query invalidation ->
  Map query ProjectionWork
projectionDeltaWork =
  pdWork
{-# INLINE projectionDeltaWork #-}

projectionDeltaInvalidation ::
  ProjectionDelta query invalidation ->
  invalidation
projectionDeltaInvalidation =
  pdInvalidation
{-# INLINE projectionDeltaInvalidation #-}

projectionDeltaWithProjection ::
  PropagationEpoch.ContextProjectionDelta IntSet ->
  ProjectionDelta query invalidation ->
  ProjectionDelta query invalidation
projectionDeltaWithProjection projectionDeltaValue deltaValue =
  deltaValue {pdProjection = projectionDeltaValue}
{-# INLINE projectionDeltaWithProjection #-}

instance (Ord query, Semigroup invalidation) => Semigroup (ProjectionDelta query invalidation) where
  leftDelta <> rightDelta =
    ProjectionDelta
      { pdProjection = pdProjection leftDelta <> pdProjection rightDelta,
        pdWork = Map.unionWith (<>) (pdWork leftDelta) (pdWork rightDelta),
        pdInvalidation = pdInvalidation leftDelta <> pdInvalidation rightDelta
      }

instance (Ord query, Monoid invalidation) => Monoid (ProjectionDelta query invalidation) where
  mempty =
    emptyProjectionDelta

projectionDeltaOps ::
  (Ord query, Monoid invalidation, Eq invalidation) =>
  DeltaOps
    (ProjectionDelta query invalidation)
    (ProjectionDelta query invalidation)
projectionDeltaOps =
  monoidDeltaOps
{-# INLINE projectionDeltaOps #-}

projectionOnly ::
  Monoid invalidation =>
  IntSet ->
  IntSet ->
  ProjectionDelta query invalidation
projectionOnly dirtyBaseKeys dirtyResultKeys =
  emptyProjectionDelta
    { pdProjection =
        PropagationEpoch.ContextProjectionDelta
          dirtyBaseKeys
          dirtyResultKeys
    }
{-# INLINE projectionOnly #-}

invalidationOnly ::
  invalidation ->
  ProjectionDelta query invalidation
invalidationOnly invalidationValue =
  ProjectionDelta
    { pdProjection = PropagationEpoch.emptyContextProjectionDelta,
      pdWork = Map.empty,
      pdInvalidation = invalidationValue
    }
{-# INLINE invalidationOnly #-}

bootstrapQueries ::
  (Ord query, Monoid invalidation) =>
  [query] ->
  ProjectionDelta query invalidation
bootstrapQueries queryIds =
  workDeltaFromList
    [ (queryId, bootstrapProjection)
    | queryId <- queryIds
    ]
{-# INLINE bootstrapQueries #-}

projectQueries ::
  (Ord query, Monoid invalidation) =>
  [query] ->
  IntSet ->
  ProjectionDelta query invalidation
projectQueries queryIds dirtyResults =
  workDeltaFromList
    [ (queryId, projectionWorkForPhase Project dirtyResults)
    | queryId <- queryIds
    ]
{-# INLINE projectQueries #-}

projectQuery ::
  Monoid invalidation =>
  query ->
  IntSet ->
  ProjectionDelta query invalidation
projectQuery queryId dirtyResults =
  singleWorkDelta queryId (projectionWorkForPhase Project dirtyResults)
{-# INLINE projectQuery #-}

pruneQuery ::
  Monoid invalidation =>
  query ->
  IntSet ->
  ProjectionDelta query invalidation
pruneQuery queryId dirtyResults =
  singleWorkDelta queryId (projectionWorkForPhase Prune dirtyResults)
{-# INLINE pruneQuery #-}

restrictQuery ::
  Monoid invalidation =>
  query ->
  IntSet ->
  ProjectionDelta query invalidation
restrictQuery queryId dirtyResults =
  singleWorkDelta queryId (projectionWorkForPhase Restrict dirtyResults)
{-# INLINE restrictQuery #-}

emptyProjectionDelta ::
  Monoid invalidation =>
  ProjectionDelta query invalidation
emptyProjectionDelta =
  ProjectionDelta
    { pdProjection = PropagationEpoch.emptyContextProjectionDelta,
      pdWork = Map.empty,
      pdInvalidation = mempty
    }
{-# INLINE emptyProjectionDelta #-}

singleWorkDelta ::
  Monoid invalidation =>
  query ->
  ProjectionWork ->
  ProjectionDelta query invalidation
singleWorkDelta queryId work =
  emptyProjectionDelta
    { pdWork = Map.singleton queryId work
    }
{-# INLINE singleWorkDelta #-}

workDeltaFromList ::
  (Ord query, Monoid invalidation) =>
  [(query, ProjectionWork)] ->
  ProjectionDelta query invalidation
workDeltaFromList workItems =
  emptyProjectionDelta
    { pdWork = Map.fromListWith (<>) workItems
    }
{-# INLINE workDeltaFromList #-}
