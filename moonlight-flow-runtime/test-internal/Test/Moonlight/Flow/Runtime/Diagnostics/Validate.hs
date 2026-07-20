{-# LANGUAGE DerivingStrategies #-}
module Test.Moonlight.Flow.Runtime.Diagnostics.Validate
  ( RuntimeTraceValidationError (..),
    validateRuntimeTrace,
    validateRuntimeCarrierStores,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Moonlight.Core
  ( BoundaryOps,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    RuntimeEnv (..),
  )
import Moonlight.Flow.Runtime.Carrier.State
  ( runtimeIndexOps,
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( Shard (..),
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierStoreError,
    validateCarrierStore,
  )

data RuntimeTraceValidationError ctx prop boundary evidence
  = RuntimeCarrierStoreValidationFailed
      !Shard
      !(CarrierStoreError ctx Carrier prop boundary evidence)
  deriving stock (Eq, Show)

validateRuntimeTrace ::
  ( Ord ctx,
    Ord prop,
    Eq boundary,
    Eq evidence,
    BoundaryOps boundary
  ) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RuntimeTraceValidationError ctx prop boundary evidence)
    ()
validateRuntimeTrace =
  validateRuntimeCarrierStores

validateRuntimeCarrierStores ::
  ( Ord ctx,
    Ord prop,
    Eq boundary,
    Eq evidence,
    BoundaryOps boundary
  ) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RuntimeTraceValidationError ctx prop boundary evidence)
    ()
validateRuntimeCarrierStores runtime =
  IntMap.foldlWithKey'
    ( \eitherUnit shardKey indexState -> do
        eitherUnit
        case validateCarrierStore (reContextLattice (rdrEnv runtime)) indexState of
          Left replayError ->
            Left (RuntimeCarrierStoreValidationFailed (Shard shardKey) replayError)
          Right () ->
            Right ()
    )
    (Right ())
    (runtimeIndexOps (rdrState runtime))
