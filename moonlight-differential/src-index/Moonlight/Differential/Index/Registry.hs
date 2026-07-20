{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Index.Registry
  ( IndexedRegistry,
    registryRows,
    registryIndexes,
    RegistryOps (..),
    emptyIndexedRegistry,
    lookupRegistryRow,
    registryRowsAscList,
    registrySize,
    insertRegistryRow,
    insertRegistryRows,
    upsertRegistryRow,
    deleteRegistryRow,
    deleteRegistryRowReturning,
    validateIndexedRegistry,
  )
where

import Data.Foldable qualified as Foldable
import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Differential.Internal.Index.Registry
  ( IndexedRegistry (..),
  )
import Moonlight.Differential.Index.Reverse
  ( finishInvariantErrors,
  )

type RegistryOps :: Type -> Type -> Type -> Type -> Type
data RegistryOps ident row indexes errorValue = RegistryOps
  { registryRowId :: !(row -> ident),
    registryEmptyIndexes :: !indexes,
    registryInsertIndexes :: !(ident -> row -> indexes -> indexes),
    registryDeleteIndexes :: !(ident -> row -> indexes -> indexes),
    registryValidateIndexes :: !(Map ident row -> indexes -> [errorValue])
  }

emptyIndexedRegistry ::
  RegistryOps ident row indexes errorValue ->
  IndexedRegistry ident row indexes
emptyIndexedRegistry ops =
  IndexedRegistry
    { indexedRegistryRowsRaw = Map.empty,
      indexedRegistryIndexesRaw = registryEmptyIndexes ops
    }
{-# INLINE emptyIndexedRegistry #-}

registryRows ::
  IndexedRegistry ident row indexes ->
  Map ident row
registryRows =
  indexedRegistryRowsRaw
{-# INLINE registryRows #-}

registryIndexes ::
  IndexedRegistry ident row indexes ->
  indexes
registryIndexes =
  indexedRegistryIndexesRaw
{-# INLINE registryIndexes #-}

lookupRegistryRow ::
  Ord ident =>
  ident ->
  IndexedRegistry ident row indexes ->
  Maybe row
lookupRegistryRow ident =
  Map.lookup ident . registryRows
{-# INLINE lookupRegistryRow #-}

registryRowsAscList ::
  IndexedRegistry ident row indexes ->
  [(ident, row)]
registryRowsAscList =
  Map.toAscList . registryRows
{-# INLINE registryRowsAscList #-}

registrySize ::
  IndexedRegistry ident row indexes ->
  Int
registrySize =
  Map.size . registryRows
{-# INLINE registrySize #-}

insertRegistryRow ::
  Ord ident =>
  RegistryOps ident row indexes errorValue ->
  row ->
  IndexedRegistry ident row indexes ->
  IndexedRegistry ident row indexes
insertRegistryRow ops row =
  snd . upsertRegistryRow ops row
{-# INLINE insertRegistryRow #-}

insertRegistryRows ::
  (Foldable rows, Ord ident) =>
  RegistryOps ident row indexes errorValue ->
  rows row ->
  IndexedRegistry ident row indexes ->
  IndexedRegistry ident row indexes
insertRegistryRows ops rows registry0 =
  Foldable.foldl' (\registry row -> insertRegistryRow ops row registry) registry0 rows
{-# INLINE insertRegistryRows #-}

upsertRegistryRow ::
  Ord ident =>
  RegistryOps ident row indexes errorValue ->
  row ->
  IndexedRegistry ident row indexes ->
  (Maybe row, IndexedRegistry ident row indexes)
upsertRegistryRow ops row registry0 =
  let ident =
        registryRowId ops row
      (oldRow, registry1) =
        deleteRegistryRowReturning ops ident registry0
      indexes =
        registryInsertIndexes ops ident row (registryIndexes registry1)
   in ( oldRow,
        registry1
          { indexedRegistryRowsRaw = Map.insert ident row (indexedRegistryRowsRaw registry1),
            indexedRegistryIndexesRaw = indexes
          }
      )
{-# INLINE upsertRegistryRow #-}

deleteRegistryRow ::
  Ord ident =>
  RegistryOps ident row indexes errorValue ->
  ident ->
  IndexedRegistry ident row indexes ->
  IndexedRegistry ident row indexes
deleteRegistryRow ops ident =
  snd . deleteRegistryRowReturning ops ident
{-# INLINE deleteRegistryRow #-}

deleteRegistryRowReturning ::
  Ord ident =>
  RegistryOps ident row indexes errorValue ->
  ident ->
  IndexedRegistry ident row indexes ->
  (Maybe row, IndexedRegistry ident row indexes)
deleteRegistryRowReturning ops ident registry =
  case Map.lookup ident (registryRows registry) of
    Nothing ->
      (Nothing, registry)
    Just row ->
      ( Just row,
        registry
          { indexedRegistryRowsRaw = Map.delete ident (indexedRegistryRowsRaw registry),
            indexedRegistryIndexesRaw =
              registryDeleteIndexes
                ops
                ident
                row
                (indexedRegistryIndexesRaw registry)
          }
      )
{-# INLINE deleteRegistryRowReturning #-}

validateIndexedRegistry ::
  Eq ident =>
  RegistryOps ident row indexes errorValue ->
  (ident -> ident -> errorValue) ->
  IndexedRegistry ident row indexes ->
  Either [errorValue] ()
validateIndexedRegistry ops storedIdMismatch registry =
  finishInvariantErrors $
    storedIdErrors
      <> registryValidateIndexes ops (registryRows registry) (registryIndexes registry)
  where
    storedIdErrors =
      [ storedIdMismatch storedId actualId
      | (storedId, row) <- Map.toAscList (registryRows registry),
        let actualId = registryRowId ops row,
        storedId /= actualId
      ]
{-# INLINE validateIndexedRegistry #-}
