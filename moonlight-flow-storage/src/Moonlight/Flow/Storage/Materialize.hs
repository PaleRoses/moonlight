{-# LANGUAGE DataKinds #-}

module Moonlight.Flow.Storage.Materialize
  ( relationSupportRows,
    storeSupportRows,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Moonlight.Differential.Row.Block
  ( RowBlock,
    RowBlockIdentity,
    RowBuildError,
    RowState (Canonical),
  )
import Moonlight.Flow.Storage.Relation
  ( Relation,
    relationSupportRows,
  )
import Moonlight.Flow.Storage.Store
  ( Store,
    storeRelations,
  )

storeSupportRows ::
  (Int -> Relation -> RowBlockIdentity) ->
  Store ->
  Either RowBuildError (IntMap (RowBlock 'Canonical))
storeSupportRows metadataFor store =
  IntMap.traverseWithKey materialize (storeRelations store)
  where
    materialize atomKey relation =
      relationSupportRows (metadataFor atomKey relation) relation
{-# INLINE storeSupportRows #-}
