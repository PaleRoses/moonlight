{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Flow.Model.Delta
  ( AtomPatch,
    atomPatchRows,
    atomPatchFromRowDelta,
    AtomPatchPositiveView (..),
    mkAtomPatch,
    atomPatchPositiveView,

    AtomEvent (..),
    ScopedAtomEvents (..),
    QuotientPatch (..),
  )
where

import Data.IntMap.Strict (IntMap)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( AtomId,
    QueryId,
    QuotientEpoch,
  )
import Moonlight.Differential.Row.Delta
  ( PositiveMultiplicity,
    RowDelta,
    RowDeltaError (..),
    rowDeltaNegativePart,
    rowDeltaPositivePart,
  )
import Moonlight.Delta.Signed
  ( Multiplicity,
    MultiplicityChange,
    multiplicityAsChange,
    multiplicityValue,
    negateMultiplicityChange
  )
import Moonlight.Differential.Row.Patch
  ( EpochTransition (..),
    composePlainRowPatch,
    plainRowPatchFromChangeMap,
    plainRowPatchFromMultiplicityMap,
    positivePlainRowPatchRows
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope (..),
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )

traversePositiveRows ::
  (RowTupleKey -> Multiplicity -> RowDeltaError) ->
  (Multiplicity -> MultiplicityChange) ->
  Map RowTupleKey Multiplicity ->
  Either RowDeltaError (Map RowTupleKey MultiplicityChange)
traversePositiveRows buildError projectMultiplicity rows =
  Map.traverseWithKey
    ( \rowValue multiplicity ->
        if multiplicityValue multiplicity > 0
          then Right (projectMultiplicity multiplicity)
          else Left (buildError rowValue multiplicity)
    )
    (positivePlainRowPatchRows (plainRowPatchFromMultiplicityMap rows))
{-# INLINE traversePositiveRows #-}

type AtomPatch :: Type
data AtomPatch = AtomPatch
  { apRows :: !RowDelta
  }
  deriving stock (Eq, Show)

atomPatchRows :: AtomPatch -> RowDelta
atomPatchRows =
  apRows
{-# INLINE atomPatchRows #-}

atomPatchFromRowDelta :: RowDelta -> AtomPatch
atomPatchFromRowDelta rows =
  AtomPatch
    { apRows = rows
    }
{-# INLINE atomPatchFromRowDelta #-}

type AtomPatchPositiveView :: Type
data AtomPatchPositiveView = AtomPatchPositiveView
  { apvRemoved :: !(Map RowTupleKey PositiveMultiplicity),
    apvInserted :: !(Map RowTupleKey PositiveMultiplicity)
  }
  deriving stock (Eq, Show)

mkAtomPatch ::
  Map RowTupleKey Multiplicity ->
  Map RowTupleKey Multiplicity ->
  Either RowDeltaError AtomPatch
mkAtomPatch removedRows insertedRows = do
  removedPositive <-
    traversePositiveRows NonPositiveRemovedMultiplicity (negateMultiplicityChange . multiplicityAsChange) removedRows
  insertedPositive <-
    traversePositiveRows NonPositiveInsertedMultiplicity multiplicityAsChange insertedRows
  let removedDelta =
        plainRowPatchFromChangeMap
          removedPositive
      insertedDelta =
        plainRowPatchFromChangeMap
          insertedPositive
  pure
    AtomPatch
      { apRows = composePlainRowPatch removedDelta insertedDelta
      }

atomPatchPositiveView :: AtomPatch -> AtomPatchPositiveView
atomPatchPositiveView patch =
  AtomPatchPositiveView
    { apvRemoved = rowDeltaNegativePart (apRows patch),
      apvInserted = rowDeltaPositivePart (apRows patch)
    }

type ScopedAtomEvents :: Type
data ScopedAtomEvents = ScopedAtomEvents
  { saeScope :: !RelationalScope,
    saeAtomScopeByAtom :: !(IntMap RelationalScope),
    saeTouchScopeByAtom :: !(IntMap RelationalScope),
    saeEvents :: ![AtomEvent]
  }
  deriving stock (Eq, Show)

type QuotientPatch :: Type
data QuotientPatch = QuotientPatch
  { qpEpoch :: !(EpochTransition QuotientEpoch),
    qpScope :: !RelationalScope,
    qpAtomScopeByAtom :: !(IntMap RelationalScope),
    qpEvents :: !(IntMap AtomPatch)
  }
  deriving stock (Eq, Show)

type AtomEvent :: Type
data AtomEvent = AtomEvent
  { aeQueryId :: !QueryId,
    aeAtomId :: !AtomId,
    aeRows :: !RowDelta
  }
  deriving stock (Eq, Show)
