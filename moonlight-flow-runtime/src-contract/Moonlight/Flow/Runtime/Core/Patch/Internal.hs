{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Moonlight.Flow.Runtime.Core.Patch.Internal
  ( Patch (..),
    emptyPatch,
    dirtyPatch,
    scopePatch,
    insertRowsById,
    deleteRowsById,
    replaceRowsById,
    patchNull,
    patchAtomCount,
    splitPatchAtomEvents,
    QuotientPatch,
    patchToQuotientPatch,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Moonlight.Core
  ( AtomId,
    QuotientEpoch,
    atomIdKey,
    nextQuotientEpoch,
  )
import Moonlight.Flow.Model.Delta
  ( AtomPatch,
    QuotientPatch (..),
    atomPatchRows
  )
import Moonlight.Flow.Model.Delta
  ( atomPatchFromRowDelta
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  ( EpochTransition (..),
    composePlainRowPatch,
    plainRowPatchFromList,
    plainRowPatchNull
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
    relationalScopeNull,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )

data Patch = Patch
  { patchScope :: !RelationalScope,
    patchEvents :: !(IntMap AtomPatch)
  }
  deriving stock (Eq, Show)

instance Semigroup Patch where
  left <> right =
    normalizePatch
      Patch
        { patchScope =
            patchScope left <> patchScope right,
          patchEvents =
            IntMap.unionWith
              appendAtomPatch
              (patchEvents left)
              (patchEvents right)
        }
  {-# INLINE (<>) #-}

instance Monoid Patch where
  mempty =
    emptyPatch
  {-# INLINE mempty #-}

emptyPatch :: Patch
emptyPatch =
  Patch
    { patchScope = mempty,
      patchEvents = IntMap.empty
    }
{-# INLINE emptyPatch #-}

dirtyPatch :: RelationalScope -> Patch
dirtyPatch scope
  | relationalScopeNull scope =
      emptyPatch
  | otherwise =
      Patch
        { patchScope = scope,
          patchEvents = IntMap.empty
        }
{-# INLINE dirtyPatch #-}

scopePatch :: RelationalScope -> Patch -> Patch
scopePatch scope patch =
  normalizePatch
    patch
      { patchScope =
          patchScope patch <> scope
      }
{-# INLINE scopePatch #-}

insertRowsById :: AtomId -> [RowTupleKey] -> Patch
insertRowsById atomId rows =
  patchFromRows atomId [(rowValue, MultiplicityChange 1) | rowValue <- rows]
{-# INLINE insertRowsById #-}

deleteRowsById :: AtomId -> [RowTupleKey] -> Patch
deleteRowsById atomId rows =
  patchFromRows atomId [(rowValue, MultiplicityChange (-1)) | rowValue <- rows]
{-# INLINE deleteRowsById #-}

replaceRowsById :: AtomId -> [RowTupleKey] -> [RowTupleKey] -> Patch
replaceRowsById atomId oldRows newRows =
  deleteRowsById atomId oldRows <> insertRowsById atomId newRows
{-# INLINE replaceRowsById #-}

patchNull :: Patch -> Bool
patchNull patch =
  IntMap.null (patchEvents patch)
    && relationalScopeNull (patchScope patch)
{-# INLINE patchNull #-}

patchAtomCount :: Patch -> Int
patchAtomCount =
  IntMap.size . patchEvents
{-# INLINE patchAtomCount #-}

splitPatchAtomEvents :: Int -> Patch -> Maybe (Patch, Patch)
splitPatchAtomEvents atomCount patch
  | atomCount <= 0 =
      Nothing
  | otherwise =
      Just
        ( Patch
            { patchScope = patchScope patch,
              patchEvents = selectedEvents
            },
          Patch
            { patchScope = mempty,
              patchEvents = remainingEvents
            }
        )
  where
    (selectedEvents, remainingEvents) =
      splitPatchEventsAscending atomCount (patchEvents patch)
{-# INLINE splitPatchAtomEvents #-}

patchToQuotientPatch :: QuotientEpoch -> Patch -> QuotientPatch
patchToQuotientPatch epochBefore patch =
  QuotientPatch
    { qpEpoch =
        EpochTransition
          { etBefore = epochBefore,
            etAfter = nextQuotientEpoch epochBefore
          },
      qpScope = patchScope patch,
      qpAtomScopeByAtom = IntMap.empty,
      qpEvents = patchEvents patch
    }
{-# INLINE patchToQuotientPatch #-}

patchFromRows :: AtomId -> [(RowTupleKey, MultiplicityChange)] -> Patch
patchFromRows atomId rows =
  normalizePatch
    Patch
      { patchScope = mempty,
        patchEvents =
          IntMap.singleton
            (atomIdKey atomId)
            (atomPatchFromRowDelta (plainRowPatchFromList rows))
      }
{-# INLINE patchFromRows #-}

appendAtomPatch :: AtomPatch -> AtomPatch -> AtomPatch
appendAtomPatch left right =
  atomPatchFromRowDelta
    (composePlainRowPatch (atomPatchRows left) (atomPatchRows right))
{-# INLINE appendAtomPatch #-}

normalizePatch :: Patch -> Patch
normalizePatch patch =
  patch
    { patchEvents =
        IntMap.filter
          (not . plainRowPatchNull . atomPatchRows)
          (patchEvents patch)
    }
{-# INLINE normalizePatch #-}

splitPatchEventsAscending ::
  Int ->
  IntMap atomPatch ->
  (IntMap atomPatch, IntMap atomPatch)
splitPatchEventsAscending atomCount events =
  let (selected, remaining) =
        splitAt atomCount (IntMap.toAscList events)
   in (IntMap.fromDistinctAscList selected, IntMap.fromDistinctAscList remaining)
{-# INLINE splitPatchEventsAscending #-}
