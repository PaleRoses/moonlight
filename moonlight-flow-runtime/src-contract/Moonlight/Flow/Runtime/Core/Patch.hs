module Moonlight.Flow.Runtime.Core.Patch
  ( Patch,
    emptyPatch,
    dirtyPatch,
    scopePatch,
    insertRows,
    deleteRows,
    replaceRows,
  )
where

import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.Flow.Runtime.Core.Patch.Internal
  ( Patch,
    deleteRowsById,
    dirtyPatch,
    emptyPatch,
    insertRowsById,
    replaceRowsById,
    scopePatch,
  )
import Moonlight.Flow.Runtime.Spec.Schema
  ( RuntimeAtom (..),
  )

insertRows :: RuntimeAtom ctx prop -> [RowTupleKey] -> Patch
insertRows =
  insertRowsById . runtimeAtomId
{-# INLINE insertRows #-}

deleteRows :: RuntimeAtom ctx prop -> [RowTupleKey] -> Patch
deleteRows =
  deleteRowsById . runtimeAtomId
{-# INLINE deleteRows #-}

replaceRows ::
  RuntimeAtom ctx prop ->
  [RowTupleKey] ->
  [RowTupleKey] ->
  Patch
replaceRows atom =
  replaceRowsById (runtimeAtomId atom)
{-# INLINE replaceRows #-}
