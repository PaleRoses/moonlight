{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Internal.Index.Registry
  ( IndexedRegistry (..),
  )
where

import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )

type IndexedRegistry :: Type -> Type -> Type -> Type
data IndexedRegistry ident row indexes = IndexedRegistry
  { indexedRegistryRowsRaw :: !(Map ident row),
    indexedRegistryIndexesRaw :: !indexes
  }
  deriving stock (Eq, Show)
