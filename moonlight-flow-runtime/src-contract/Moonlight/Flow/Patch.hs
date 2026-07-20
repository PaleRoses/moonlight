{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Patch
  ( Patch,
    PatchError (..),
    emptyPatch,
    patch,
    insert,
    delete,
    replace,
    dirtyPatch,
    scopePatch,
  )
where

import Data.Foldable
  ( fold,
    traverse_,
  )
import Moonlight.Core
  ( AtomId,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    tupleKeyWidth,
  )
import Moonlight.Flow.Runtime.Core.Patch
  ( Patch,
    deleteRows,
    dirtyPatch,
    emptyPatch,
    insertRows,
    replaceRows,
    scopePatch,
  )
import Moonlight.Flow.Runtime.Spec.Schema
  ( RuntimeAtom (..),
    RuntimeAtomSchema (..),
  )

data PatchError
  = PatchRowWidthMismatch !AtomId !Int !Int !RowTupleKey
  deriving stock (Eq, Ord, Show)

patch :: Foldable f => f Patch -> Patch
patch =
  fold
{-# INLINE patch #-}

insert :: RuntimeAtom ctx prop -> [RowTupleKey] -> Either PatchError Patch
insert atom rows = do
  validateRowsForAtom atom rows
  pure (insertRows atom rows)
{-# INLINE insert #-}

delete :: RuntimeAtom ctx prop -> [RowTupleKey] -> Either PatchError Patch
delete atom rows = do
  validateRowsForAtom atom rows
  pure (deleteRows atom rows)
{-# INLINE delete #-}

replace :: RuntimeAtom ctx prop -> [RowTupleKey] -> [RowTupleKey] -> Either PatchError Patch
replace atom oldRows newRows = do
  validateRowsForAtom atom oldRows
  validateRowsForAtom atom newRows
  pure (replaceRows atom oldRows newRows)
{-# INLINE replace #-}

validateRowsForAtom :: RuntimeAtom ctx prop -> [RowTupleKey] -> Either PatchError ()
validateRowsForAtom atom rows =
  traverse_ (validateRowWidth atomId expectedWidth) rows
  where
    atomId =
      runtimeAtomId atom

    expectedWidth =
      length (rasColumns (runtimeAtomSchemaDefinition atom))
{-# INLINE validateRowsForAtom #-}

validateRowWidth :: AtomId -> Int -> RowTupleKey -> Either PatchError ()
validateRowWidth atomId expectedWidth row =
  let actualWidth =
        tupleKeyWidth row
   in if actualWidth == expectedWidth
        then Right ()
        else Left (PatchRowWidthMismatch atomId expectedWidth actualWidth row)
{-# INLINE validateRowWidth #-}
