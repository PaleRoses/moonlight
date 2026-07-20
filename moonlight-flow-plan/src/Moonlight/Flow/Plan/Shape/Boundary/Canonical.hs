{-# LANGUAGE DataKinds #-}

module Moonlight.Flow.Plan.Shape.Boundary.Canonical
  ( canonicalBoundaryDigestEncoder,
    canonicalBoundaryShapeDigest,
    mkCanonicalBoundaryShape,
    canonicalBoundaryPinnedSlots,
    projectCanonicalBoundary,
    restrictCanonicalBoundary,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Flow.Internal.Digest
  ( wordOfInt,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( BoundaryDigestEncoder (..),
    BoundaryShape (..),
    boundaryShapeDigestWith,
    mkBoundary,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Plan.Shape
  ( CanonicalBoundaryShape,
    cbsSchema,
    cbsSensitiveSlots,
    cbsSlotKeys,
  )
import Moonlight.Flow.Plan.Shape.Encode
  ( canonicalSlotWords,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot,
    ProjectionPayload (..),
    RestrictionPayload (..),
    canonSlotKey,
  )

canonicalBoundaryDigestEncoder :: BoundaryDigestEncoder CanonSlot Int
canonicalBoundaryDigestEncoder =
  BoundaryDigestEncoder
    { bdeSalt = 0x626f756e64617279,
      bdeShapeTag = 0x6f756e5368617065,
      bdeListTag = 0x02,
      bdeSetTag = 0x12,
      bdeMapSetTag = 0x13,
      bdeSlotWords = canonicalSlotWords,
      bdeKeyWords = \key -> [wordOfInt key]
    }
{-# INLINE canonicalBoundaryDigestEncoder #-}

canonicalBoundaryShapeDigest ::
  BoundaryShape CanonSlot Int ->
  StableDigest128
canonicalBoundaryShapeDigest =
  boundaryShapeDigestWith canonicalBoundaryDigestEncoder
{-# INLINE canonicalBoundaryShapeDigest #-}

mkCanonicalBoundaryShape ::
  [CanonSlot] ->
  Set CanonSlot ->
  Map CanonSlot (Set Int) ->
  CanonicalBoundaryShape
mkCanonicalBoundaryShape schema sensitiveSlots slotKeys =
  mkBoundary
    canonicalBoundaryShapeDigest
    BoundaryShape
      { bsSchema = schema,
        bsSensitive = sensitiveSlots,
        bsSlotKeys = slotKeys
      }
{-# INLINE mkCanonicalBoundaryShape #-}

canonicalBoundaryPinnedSlots ::
  CanonicalBoundaryShape ->
  IntMap IntSet
canonicalBoundaryPinnedSlots boundary =
  IntMap.fromAscList
    [ (canonSlotKey slot, IntSet.fromAscList (Set.toAscList slotKeys))
    | (slot, slotKeys) <- Map.toAscList (cbsSlotKeys boundary)
    ]
{-# INLINE canonicalBoundaryPinnedSlots #-}

projectCanonicalBoundary ::
  ProjectionPayload ->
  CanonicalBoundaryShape ->
  CanonicalBoundaryShape
projectCanonicalBoundary projection boundary =
  mkCanonicalBoundaryShape
    (ppTargetSchema projection)
    projectedSensitiveSlots
    projectedSlotKeys
  where
    sourceForTarget target =
      IntMap.lookup (canonSlotKey target) (ppSlotMap projection)

    projectedSensitiveSlots =
      Set.fromList
        [ target
        | target <- ppTargetSchema projection,
          Just source <- [sourceForTarget target],
          Set.member source (cbsSensitiveSlots boundary)
        ]

    projectedSlotKeys =
      Map.fromList
        [ (target, slotKeys)
        | target <- ppTargetSchema projection,
          Just source <- [sourceForTarget target],
          Just slotKeys <- [Map.lookup source (cbsSlotKeys boundary)]
        ]
{-# INLINE projectCanonicalBoundary #-}

restrictCanonicalBoundary ::
  RestrictionPayload ->
  CanonicalBoundaryShape ->
  CanonicalBoundaryShape
restrictCanonicalBoundary restriction boundary =
  mkCanonicalBoundaryShape
    (cbsSchema boundary)
    restrictedSensitiveSlots
    restrictedSlotKeys
  where
    slotByKey =
      Map.fromList
        [ (canonSlotKey slot, slot)
        | slot <- cbsSchema boundary
        ]

    pinnedSlotKeys =
      [ (slot, IntSet.toAscList pinnedValues)
      | (slotKey, pinnedValues) <- IntMap.toAscList (rpPinnedSlots restriction),
        Just slot <- [Map.lookup slotKey slotByKey]
      ]

    restrictedSensitiveSlots =
      Set.union
        (cbsSensitiveSlots boundary)
        (Set.fromList (fmap fst pinnedSlotKeys))

    restrictedSlotKeys =
      foldr
        ( \(slot, pinnedValues) slotKeys ->
            Map.insertWith Set.union slot (Set.fromList pinnedValues) slotKeys
        )
        (cbsSlotKeys boundary)
        pinnedSlotKeys
{-# INLINE restrictCanonicalBoundary #-}
