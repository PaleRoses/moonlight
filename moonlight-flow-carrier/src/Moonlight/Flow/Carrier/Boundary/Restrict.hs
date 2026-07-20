{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Boundary.Restrict
  ( BoundaryRestrictionError (..),
    restrictRuntimeBoundary,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    RuntimeBoundaryError,
    BoundaryShape (..),
    boundaryShape,
    mkRuntimeBoundary,
    runtimeBoundarySensitiveSlots,
    runtimeBoundarySlotKeys,
  )
import Moonlight.Differential.Row.Tuple

data BoundaryRestrictionError
  = BoundaryRestrictionNegativeRepresentative !Int !RepKey
  | BoundaryRestrictionBoundaryError !RuntimeBoundaryError
  deriving stock (Eq, Ord, Show, Read)

restrictRuntimeBoundary ::
  IntMap RepKey ->
  RuntimeBoundary ->
  Either BoundaryRestrictionError RuntimeBoundary
restrictRuntimeBoundary targetClasses boundary = do
  restrictedSlotKeys <-
    restrictBoundarySlotKeys targetClasses (runtimeBoundarySlotKeys boundary)
  first BoundaryRestrictionBoundaryError $
    mkRuntimeBoundary
      (bsSchema (boundaryShape boundary))
      (runtimeBoundarySensitiveSlots boundary)
      restrictedSlotKeys

restrictBoundarySlotKeys ::
  IntMap RepKey ->
  IntMap IntSet ->
  Either BoundaryRestrictionError (IntMap IntSet)
restrictBoundarySlotKeys targetClasses =
  IntMap.traverseWithKey
    ( \_slotKey representativeKeys ->
        restrictKeySet targetClasses representativeKeys
    )

restrictKeySet ::
  IntMap RepKey ->
  IntSet ->
  Either BoundaryRestrictionError IntSet
restrictKeySet targetClasses representativeKeys =
  IntSet.fromList
    <$> traverse
      (restrictKey targetClasses)
      (IntSet.toAscList representativeKeys)

restrictKey ::
  IntMap RepKey ->
  Int ->
  Either BoundaryRestrictionError Int
restrictKey targetClasses sourceKey =
  let targetRep =
        IntMap.findWithDefault (RepKey sourceKey) sourceKey targetClasses
      targetKey =
        unRepKey targetRep
   in if targetKey < 0
        then Left (BoundaryRestrictionNegativeRepresentative sourceKey targetRep)
        else Right targetKey
