module Moonlight.Flow.Execution.Prepared.Base
  ( BasePreparedDB (..),
    BuildBasePreparedDBError (..),
    PatchBasePreparedDBError (..),
    baseStore,
    buildBasePreparedDBFromAtomRows,
    patchBasePreparedDBWithAtomRows,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Vector qualified as Vector
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchFromList,
    plainRowPatchNull
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Storage.Plan
  ( StoragePlanError,
    compileStoragePlan,
    storagePlanFromQueryPlan,
  )
import Moonlight.Flow.Storage.Relation
import Moonlight.Flow.Storage.Store
  ( Store,
    StorageError,
    StoragePatch (..),
    StoragePatchResult (..),
    applyStoragePatch,
    storeFromRelationsWithPlan,
  )

type BasePreparedDB :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data BasePreparedDB compiled output guard tag tuple key = BasePreparedDB
  { bpdPlan :: !(QueryPlan compiled output guard tag tuple key),
    bpdStore :: !Store
  }

type BuildBasePreparedDBError :: Type
data BuildBasePreparedDBError
  = BuildBasePreparedDBRelationBuildError !RelationPatchError
  | BuildBasePreparedDBStoragePlanError !StoragePlanError
  | BuildBasePreparedDBStorageError !StorageError
  deriving stock (Eq, Show)

type PatchBasePreparedDBError :: Type
data PatchBasePreparedDBError
  = PatchBasePreparedDBStoreError !AtomId !StorageError
  deriving stock (Eq, Show)

baseStore ::
  BasePreparedDB compiled output guard tag tuple key ->
  Store
baseStore =
  bpdStore
{-# INLINE baseStore #-}

flattenRowsByResult :: IntMap [RowTupleKey] -> [RowTupleKey]
flattenRowsByResult =
  IntMap.foldr (<>) []
{-# INLINE flattenRowsByResult #-}

preparedRelationFromResultRows ::
  AtomSpec tag tuple key ->
  IntMap [RowTupleKey] ->
  Either RelationPatchError Relation
preparedRelationFromResultRows atomSpec =
  relationFromTupleRows (asColumns atomSpec) . flattenRowsByResult
{-# INLINE preparedRelationFromResultRows #-}

buildBasePreparedDBFromAtomRows ::
  QueryPlan compiled output guard tag tuple key ->
  IntMap (IntMap [RowTupleKey]) ->
  Either BuildBasePreparedDBError (BasePreparedDB compiled output guard tag tuple key)
buildBasePreparedDBFromAtomRows plan rowsByAtom = do
  relations <-
    IntMap.fromList
      <$> traverse
        ( \spec -> do
            relation <-
              first BuildBasePreparedDBRelationBuildError $
                preparedRelationFromResultRows
                  spec
                  (IntMap.findWithDefault IntMap.empty (queryAtomKey (asQueryAtomId spec)) rowsByAtom)
            pure (queryAtomKey (asQueryAtomId spec), relation)
        )
        (Vector.toList (qpAtoms plan))
  compiledStoragePlan <-
    first
      BuildBasePreparedDBStoragePlanError
      (compileStoragePlan (storagePlanFromQueryPlan plan))
  store <-
    first
      BuildBasePreparedDBStorageError
      (storeFromRelationsWithPlan compiledStoragePlan relations)
  pure
    BasePreparedDB
        { bpdPlan = plan,
          bpdStore = store
        }
{-# INLINE buildBasePreparedDBFromAtomRows #-}

rowIdDeltaRows :: RowIdDelta RowTupleKey -> RowDelta
rowIdDeltaRows delta =
  plainRowPatchFromList
    ( [ (row, MultiplicityChange (-1))
        | row <- IntMap.elems (ridDeleted delta)
      ]
        <> [ (row, MultiplicityChange 1)
             | row <- IntMap.elems (ridInserted delta)
           ]
    )
{-# INLINE rowIdDeltaRows #-}

dirtyRowsByResultForAtom ::
  IntMap (IntMap [RowTupleKey]) ->
  IntSet ->
  Int ->
  IntMap [RowTupleKey]
dirtyRowsByResultForAtom dirtyRowsByAtom dirtyResults atomKey =
  IntMap.filterWithKey
    (\resultKey _ -> IntSet.member resultKey dirtyResults)
    (IntMap.findWithDefault IntMap.empty atomKey dirtyRowsByAtom)
{-# INLINE dirtyRowsByResultForAtom #-}

rowDeltaForDirtyResults ::
  IntMap [RowTupleKey] ->
  IntMap [RowTupleKey] ->
  IntSet ->
  RowDelta
rowDeltaForDirtyResults oldRows dirtyRows dirtyResults =
  plainRowPatchFromList
    ( IntSet.foldr
        dirtyResultEntries
        []
        dirtyResults
    )
  where
    dirtyResultEntries :: Int -> [(RowTupleKey, MultiplicityChange)] -> [(RowTupleKey, MultiplicityChange)]
    dirtyResultEntries resultKey entries =
      weightedRows (-1) (IntMap.lookup resultKey oldRows)
        <> weightedRows 1 (IntMap.lookup resultKey dirtyRows)
        <> entries

    weightedRows :: Int -> Maybe [RowTupleKey] -> [(RowTupleKey, MultiplicityChange)]
    weightedRows weight maybeRows =
      case maybeRows of
        Nothing -> []
        Just rows -> fmap (\row -> (row, MultiplicityChange (fromIntegral weight))) rows
{-# INLINE rowDeltaForDirtyResults #-}

patchBasePreparedDBWithAtomRows ::
  IntMap (IntMap [RowTupleKey]) ->
  IntMap (IntMap [RowTupleKey]) ->
  IntSet ->
  BasePreparedDB compiled output guard tag tuple key ->
  Either
    PatchBasePreparedDBError
    (BasePreparedDB compiled output guard tag tuple key, IntMap RowDelta)
patchBasePreparedDBWithAtomRows oldRowsByAtom dirtyRowsByAtom dirtyResults baseDb = do
  let plan =
        bpdPlan baseDb

      step (storeAcc, deltasAcc) atomSpec = do
        let atomId =
              queryAtomAsAtomId (asQueryAtomId atomSpec)
            atomKey =
              queryAtomKey (asQueryAtomId atomSpec)
            oldRows =
              dirtyRowsByResultForAtom
                oldRowsByAtom
                dirtyResults
                atomKey
        let dirtyRows =
              dirtyRowsByResultForAtom
                dirtyRowsByAtom
                dirtyResults
                atomKey
            rowDelta =
              rowDeltaForDirtyResults oldRows dirtyRows dirtyResults
        (patchedStore, rowIdDelta0) <-
          if plainRowPatchNull rowDelta
            then Right (storeAcc, emptyRowIdDelta)
            else
              let storagePatch =
                    StoragePatch
                      { spScope = mempty,
                        spRowsByAtom = IntMap.singleton atomKey rowDelta
                      }
               in first
                    (PatchBasePreparedDBStoreError atomId)
                    ( do
                        patchResult <- applyStoragePatch storagePatch storeAcc
                        pure
                          ( sprStore patchResult,
                            IntMap.findWithDefault
                              emptyRowIdDelta
                              atomKey
                              (sprRowIdDeltas patchResult)
                          )
                    )
        let rowIdDelta =
              normalizeRowIdDelta rowIdDelta0
            semanticDelta =
              rowIdDeltaRows rowIdDelta
            deltasAcc' =
              if plainRowPatchNull semanticDelta
                then deltasAcc
                else IntMap.insert atomKey semanticDelta deltasAcc
        pure (patchedStore, deltasAcc')

  (store1, atomDeltas) <-
    foldM
      step
      (bpdStore baseDb, IntMap.empty)
      (Vector.toList (qpAtoms plan))

  pure
    ( baseDb {bpdStore = store1},
      atomDeltas
    )
{-# INLINE patchBasePreparedDBWithAtomRows #-}
