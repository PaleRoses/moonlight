{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Delta
  ( DeltaOps,
    deltaApply,
    deltaCombine,
    deltaIdentity,
    deltaIsEmpty,
    deltaApplyMany,
    deltaCombineMany,
    monoidDeltaOps,
  )
where

import Data.Foldable qualified as Foldable
import Data.Kind
  ( Type,
  )

type DeltaOps :: Type -> Type -> Type
data DeltaOps section delta = DeltaOps
  { deltaApply :: delta -> section -> section,
    deltaCombine :: delta -> delta -> delta,
    deltaIdentity :: !delta,
    deltaIsEmpty :: delta -> Bool
  }

monoidDeltaOps :: (Monoid delta, Eq delta) => DeltaOps delta delta
monoidDeltaOps =
  DeltaOps
    { deltaApply = \deltaValue sectionValue -> sectionValue <> deltaValue,
      deltaCombine = (<>),
      deltaIdentity = mempty,
      deltaIsEmpty = (== mempty)
    }
{-# INLINE monoidDeltaOps #-}

deltaApplyMany ::
  Foldable f =>
  DeltaOps section delta ->
  f delta ->
  section ->
  section
deltaApplyMany ops deltas section0 =
  Foldable.foldl'
    (\sectionValue deltaValue -> deltaApply ops deltaValue sectionValue)
    section0
    deltas
{-# INLINE deltaApplyMany #-}

deltaCombineMany ::
  Foldable f =>
  DeltaOps section delta ->
  f delta ->
  delta
deltaCombineMany ops =
  Foldable.foldl' (deltaCombine ops) (deltaIdentity ops)
{-# INLINE deltaCombineMany #-}
