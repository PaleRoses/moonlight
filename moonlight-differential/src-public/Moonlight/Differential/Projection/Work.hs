{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Projection.Work
  ( ProjectionPhase (..),
    ProjectionWork,
    projectionWorkDeltaOps,
    noProjectionWork,
    bootstrapProjection,
    projectionWorkForPhase,
    projectKeys,
    pruneKeys,
    restrictKeys,
    projectionWorkBootstrap,
    projectionWorkDirtyForPhase,
    projectionWorkPhaseIsDirty,
    projectionWorkNeedsExecution,
  )
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Moonlight.Differential.Delta
  ( DeltaOps,
    monoidDeltaOps,
  )

type ProjectionPhase :: Type
data ProjectionPhase
  = Project
  | Prune
  | Restrict
  deriving stock (Eq, Ord, Show, Read)

type ProjectionWork :: Type
data ProjectionWork = ProjectionWork
  { pwBootstrap :: !Bool,
    pwProject :: !IntSet,
    pwPrune :: !IntSet,
    pwRestrict :: !IntSet
  }
  deriving stock (Eq, Show)

instance Semigroup ProjectionWork where
  leftWork <> rightWork =
    ProjectionWork
      { pwBootstrap = pwBootstrap leftWork || pwBootstrap rightWork,
        pwProject = IntSet.union (pwProject leftWork) (pwProject rightWork),
        pwPrune = IntSet.union (pwPrune leftWork) (pwPrune rightWork),
        pwRestrict = IntSet.union (pwRestrict leftWork) (pwRestrict rightWork)
      }

instance Monoid ProjectionWork where
  mempty =
    noProjectionWork

projectionWorkDeltaOps :: DeltaOps ProjectionWork ProjectionWork
projectionWorkDeltaOps =
  monoidDeltaOps
{-# INLINE projectionWorkDeltaOps #-}

noProjectionWork :: ProjectionWork
noProjectionWork =
  ProjectionWork
    { pwBootstrap = False,
      pwProject = IntSet.empty,
      pwPrune = IntSet.empty,
      pwRestrict = IntSet.empty
    }
{-# INLINE noProjectionWork #-}

bootstrapProjection :: ProjectionWork
bootstrapProjection =
  noProjectionWork
    { pwBootstrap = True
    }
{-# INLINE bootstrapProjection #-}

projectionWorkForPhase ::
  ProjectionPhase ->
  IntSet ->
  ProjectionWork
projectionWorkForPhase phase dirtyKeys
  | IntSet.null dirtyKeys =
      noProjectionWork
  | otherwise =
      case phase of
        Project ->
          projectKeys dirtyKeys
        Prune ->
          pruneKeys dirtyKeys
        Restrict ->
          restrictKeys dirtyKeys
{-# INLINE projectionWorkForPhase #-}

projectKeys :: IntSet -> ProjectionWork
projectKeys dirtyKeys =
  noProjectionWork {pwProject = dirtyKeys}
{-# INLINE projectKeys #-}

pruneKeys :: IntSet -> ProjectionWork
pruneKeys dirtyKeys =
  noProjectionWork {pwPrune = dirtyKeys}
{-# INLINE pruneKeys #-}

restrictKeys :: IntSet -> ProjectionWork
restrictKeys dirtyKeys =
  noProjectionWork {pwRestrict = dirtyKeys}
{-# INLINE restrictKeys #-}

projectionWorkBootstrap :: ProjectionWork -> Bool
projectionWorkBootstrap =
  pwBootstrap
{-# INLINE projectionWorkBootstrap #-}

projectionWorkDirtyForPhase ::
  ProjectionPhase ->
  ProjectionWork ->
  IntSet
projectionWorkDirtyForPhase phase work =
  case phase of
    Project ->
      pwProject work
    Prune ->
      pwPrune work
    Restrict ->
      pwRestrict work
{-# INLINE projectionWorkDirtyForPhase #-}

projectionWorkPhaseIsDirty ::
  ProjectionPhase ->
  ProjectionWork ->
  Bool
projectionWorkPhaseIsDirty phase =
  not . IntSet.null . projectionWorkDirtyForPhase phase
{-# INLINE projectionWorkPhaseIsDirty #-}

projectionWorkNeedsExecution ::
  Bool ->
  ProjectionWork ->
  Bool
projectionWorkNeedsExecution currentIsBootstrap work =
  currentIsBootstrap
    || pwBootstrap work
    || not (IntSet.null (pwProject work))
    || not (IntSet.null (pwPrune work))
    || not (IntSet.null (pwRestrict work))
{-# INLINE projectionWorkNeedsExecution #-}
