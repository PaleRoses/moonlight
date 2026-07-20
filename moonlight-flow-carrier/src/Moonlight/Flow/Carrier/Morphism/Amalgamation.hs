{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Morphism.Amalgamation
  ( BoundaryCoherenceResult (..),
    BoundaryMergeError (..),
    AmalgamationError (..),
    AmalgamationResult (..),
    checkCarrierBoundaryCoherence,
    mergeCarrierBoundaries,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty
  ( NonEmpty,
  )
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Core
  ( SlotId,
    slotIdKey,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
  )
import Moonlight.Flow.Carrier.Core.Obstruction.Types
  ( CarrierObstructionEvidence,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( BoundaryShape (..),
    RuntimeBoundary,
    RuntimeBoundaryError,
    boundaryCoherence,
    boundaryShape,
    mkRuntimeBoundary,
    runtimeBoundaryDigest,
    runtimeBoundarySensitiveSlots,
    runtimeBoundarySlotKeys,
  )

data BoundaryCoherenceResult
  = CoherentBoundary !RuntimeBoundary
  | IncompatibleBoundary !RuntimeBoundary
  deriving stock (Eq, Ord, Show, Read)

data BoundaryMergeError
  = BoundaryMergeEmpty
  | BoundaryMergeValidationError !RuntimeBoundaryError
  deriving stock (Eq, Ord, Show, Read)

data AmalgamationError ctx carrier prop boundary evidence
  = AmalgamationCarrierMismatch
      !(CarrierAddr ctx carrier prop)
      !(CarrierAddr ctx carrier prop)
  | AmalgamationContextOutsideCover !ctx
  | AmalgamationDuplicateContext !ctx
  | AmalgamationBoundaryMergeError !BoundaryMergeError
  | AmalgamationRowWidthMismatch !ctx ![SlotId] !RowTupleKey
  deriving stock (Eq, Show)

data AmalgamationResult ctx carrier prop boundary evidence
  = ExactAmalgamatedDelta
      !(RelationalCarrierDelta ctx carrier prop boundary evidence)
  | LowerBoundDelta
      !(RelationalCarrierDelta ctx carrier prop boundary evidence)
  | ObstructedAmalgamation
      !(NonEmpty (CarrierObstructionEvidence ctx carrier prop boundary evidence))
  deriving stock (Eq, Show)

checkCarrierBoundaryCoherence ::
  RuntimeBoundary ->
  RuntimeBoundary ->
  BoundaryCoherenceResult
checkCarrierBoundaryCoherence leftBoundary rightBoundary =
  case boundaryCoherence runtimeBoundaryDigest leftBoundary rightBoundary of
    Right overlapBoundary ->
      CoherentBoundary overlapBoundary
    Left conflictBoundary ->
      IncompatibleBoundary conflictBoundary

mergeCarrierBoundaries ::
  NonEmpty RuntimeBoundary ->
  Either BoundaryMergeError RuntimeBoundary
mergeCarrierBoundaries boundaries =
  let schema =
        orderedSlots
          (NonEmpty.toList boundaries >>= bsSchema . boundaryShape)
      sensitiveSlots =
        foldMap runtimeBoundarySensitiveSlots boundaries
      slotKeys =
        IntMap.unionsWith
          IntSet.union
          (NonEmpty.toList (fmap runtimeBoundarySlotKeys boundaries))
   in first BoundaryMergeValidationError $
        mkRuntimeBoundary schema sensitiveSlots slotKeys

orderedSlots :: [SlotId] -> [SlotId]
orderedSlots =
  IntMap.elems
    . IntMap.fromList
    . fmap (\slot -> (slotIdKey slot, slot))
