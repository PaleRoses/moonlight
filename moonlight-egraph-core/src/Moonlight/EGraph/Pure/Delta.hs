{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Pure.Delta
  ( ClassUnionPair,
    classUnionPair,
    classUnionPairClasses,
    EGraphEditDelta,
    emptyEGraphEditDelta,
    classUnionDelta,
    classUnionsDelta,
    eGraphEditDeltaNull,
    eGraphEditDeltaClassUnions,
    EGraphRebuildDelta (..),
    eGraphRebuildDeltaTouchedKeys,
  )
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Delta.Normalize
  ( DeltaNormalize (..),
  )
import Moonlight.Delta.Support
  ( DeltaSupport (..),
  )
import Moonlight.Core
  ( ClassId,
  )
import Prelude
  ( Bool,
    Eq,
    Monoid (..),
    Ord (..),
    Read,
    Semigroup (..),
    Show,
    fmap,
    id,
    null,
    otherwise,
    (&&),
    (.),
    (/=),
  )

type ClassUnionPair :: Type
data ClassUnionPair = ClassUnionPair !ClassId !ClassId
  deriving stock (Eq, Ord, Show)

classUnionPair ::
  ClassId ->
  ClassId ->
  ClassUnionPair
classUnionPair leftClassId rightClassId
  | leftClassId <= rightClassId =
      ClassUnionPair leftClassId rightClassId
  | otherwise =
      ClassUnionPair rightClassId leftClassId
{-# INLINE classUnionPair #-}

classUnionPairClasses ::
  ClassUnionPair ->
  (ClassId, ClassId)
classUnionPairClasses (ClassUnionPair leftClassId rightClassId) =
  (leftClassId, rightClassId)
{-# INLINE classUnionPairClasses #-}

type EGraphEditDelta :: Type
data EGraphEditDelta = EGraphEditDelta
  { egedClassUnions :: !(Set ClassUnionPair)
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup EGraphEditDelta where
  left <> right =
    EGraphEditDelta
      { egedClassUnions =
          Set.union (egedClassUnions left) (egedClassUnions right)
      }
  {-# INLINE (<>) #-}

instance Monoid EGraphEditDelta where
  mempty =
    emptyEGraphEditDelta
  {-# INLINE mempty #-}

emptyEGraphEditDelta :: EGraphEditDelta
emptyEGraphEditDelta =
  EGraphEditDelta
    { egedClassUnions = Set.empty
    }
{-# INLINE emptyEGraphEditDelta #-}

classUnionDelta ::
  ClassId ->
  ClassId ->
  EGraphEditDelta
classUnionDelta leftClassId rightClassId =
  classUnionsDelta [(leftClassId, rightClassId)]
{-# INLINE classUnionDelta #-}

classUnionsDelta ::
  [(ClassId, ClassId)] ->
  EGraphEditDelta
classUnionsDelta classUnions =
  EGraphEditDelta
    { egedClassUnions =
        Set.fromList
          [ classUnionPair leftClassId rightClassId
            | (leftClassId, rightClassId) <- classUnions,
              leftClassId /= rightClassId
          ]
    }
{-# INLINE classUnionsDelta #-}

eGraphEditDeltaNull :: EGraphEditDelta -> Bool
eGraphEditDeltaNull =
  null . eGraphEditDeltaClassUnions
{-# INLINE eGraphEditDeltaNull #-}

eGraphEditDeltaClassUnions ::
  EGraphEditDelta ->
  [(ClassId, ClassId)]
eGraphEditDeltaClassUnions =
  fmap classUnionPairClasses . Set.toAscList . egedClassUnions
{-# INLINE eGraphEditDeltaClassUnions #-}

type EGraphRebuildDelta :: Type
data EGraphRebuildDelta = EGraphRebuildDelta
  { erdImpactedClassKeys :: !IntSet,
    erdDirtyResultKeys :: !IntSet,
    erdTopologyClassKeys :: !IntSet
  }
  deriving stock (Eq, Show, Read)

instance Semigroup EGraphRebuildDelta where
  left <> right =
    EGraphRebuildDelta
      (IntSet.union (erdImpactedClassKeys left) (erdImpactedClassKeys right))
      (IntSet.union (erdDirtyResultKeys left) (erdDirtyResultKeys right))
      (IntSet.union (erdTopologyClassKeys left) (erdTopologyClassKeys right))

instance Monoid EGraphRebuildDelta where
  mempty =
    EGraphRebuildDelta IntSet.empty IntSet.empty IntSet.empty

instance DeltaNormalize EGraphRebuildDelta where
  normalizeDelta =
    id

  deltaNull delta =
    IntSet.null (erdImpactedClassKeys delta)
      && IntSet.null (erdDirtyResultKeys delta)
      && IntSet.null (erdTopologyClassKeys delta)

instance DeltaSupport EGraphRebuildDelta where
  type DeltaSupportSet EGraphRebuildDelta = EGraphRebuildDelta

  emptySupport =
    mempty

  deltaSupport =
    normalizeDelta

eGraphRebuildDeltaTouchedKeys :: EGraphRebuildDelta -> IntSet
eGraphRebuildDeltaTouchedKeys rebuildDelta =
  erdImpactedClassKeys rebuildDelta
    <> erdDirtyResultKeys rebuildDelta
    <> erdTopologyClassKeys rebuildDelta
{-# INLINE eGraphRebuildDeltaTouchedKeys #-}
