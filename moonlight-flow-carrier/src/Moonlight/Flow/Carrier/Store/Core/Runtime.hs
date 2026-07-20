{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Store.Core.Runtime
  ( CarrierStoreRuntime (..),
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.FiniteLattice
  ( ContextLattice
  )

type CarrierStoreRuntime :: Type -> Type -> Type
data CarrierStoreRuntime ctx boundary = CarrierStoreRuntime
  { csrContextLattice :: !(ContextLattice ctx),
    csrBoundaryDigest :: !(boundary -> StableDigest128)
  }
