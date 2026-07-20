module Test.Moonlight.Differential.Index.Registry
  ( indexedRegistryFromPartsForValidation,
    indexedRegistryWithIndexesForValidation,
  )
where

import Data.Map.Strict
  ( Map,
  )
import Moonlight.Differential.Index.Registry
  ( IndexedRegistry,
  )
import Moonlight.Differential.Internal.Index.Registry
  ( IndexedRegistry (..),
  )

indexedRegistryFromPartsForValidation ::
  Map ident row ->
  indexes ->
  IndexedRegistry ident row indexes
indexedRegistryFromPartsForValidation rows indexes =
  IndexedRegistry
    { indexedRegistryRowsRaw = rows,
      indexedRegistryIndexesRaw = indexes
    }
{-# INLINE indexedRegistryFromPartsForValidation #-}

indexedRegistryWithIndexesForValidation ::
  indexes ->
  IndexedRegistry ident row indexes ->
  IndexedRegistry ident row indexes
indexedRegistryWithIndexesForValidation indexes registry =
  registry {indexedRegistryIndexesRaw = indexes}
{-# INLINE indexedRegistryWithIndexesForValidation #-}
