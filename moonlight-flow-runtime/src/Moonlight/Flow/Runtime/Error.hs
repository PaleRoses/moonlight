{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
    SubsumptionRegistrationError,
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Differential.Frontier
  ( RuntimeFrontierError,
    RuntimeInvalidCapabilityAdvance,
  )
import Moonlight.Differential.Runtime.Error
  ( RuntimeIllegalCapabilityTransport,
  )
import Moonlight.Differential.Runtime.Schedule
  ( ScheduleError,
  )
import Moonlight.Differential.Time
  ( FrontierStamp,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    RestrictKey,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalRuntimeEpoch,
  )
import Moonlight.Flow.Carrier.Morphism.Result
  ( CarrierMorphismError,
  )
import Moonlight.Flow.Carrier.Reuse
  ( SubsumptionRegistrationError,
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierStoreError,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase,
  )
import Moonlight.Flow.Runtime.Core.Patch.Validation
  ( PatchValidationError,
  )
import Moonlight.Flow.Runtime.Engine.Capability
  ( RelationalCapabilityTransport,
    RelationalCapabilityTransportMissing,
  )
import Moonlight.Flow.Runtime.Time
  ( RuntimeEventTime,
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( RuntimeRoutingError,
    Shard,
  )
import Moonlight.Flow.Runtime.Topology.Site.Types
  ( GeneratedSitePatchError,
  )
import Moonlight.Flow.Runtime.Topology.Validate
  ( RuntimeTopologyValidationError,
  )

type RuntimeError :: Type -> Type -> Type -> Type -> Type -> Type
data RuntimeError ctx prop boundary evidence opErr
  = RuntimeOpFailure !opErr
  | PatchValidation !PatchValidationError
  | RuntimeMissingProjectShard !Shard
  | RuntimeMissingRestrictShard !Shard
  | RuntimeMissingIndexShard !Shard
  | RuntimeOperatorTimeEscape !(RuntimeEventTime ctx) !(RuntimeEventTime ctx)
  | RuntimeCapabilityAdvanceInvalid !(RuntimeInvalidCapabilityAdvance ctx RelationalRuntimeEpoch RelationalPhase)
  | RuntimeCapabilityTransportIllegal
      !( RuntimeIllegalCapabilityTransport
           ctx
           RelationalRuntimeEpoch
           RelationalPhase
           (RelationalCapabilityTransport ctx prop)
       )
  | RuntimeCapabilityTransportMissing !(RelationalCapabilityTransportMissing ctx prop)
  | RuntimeFrontierPendingCompletionInvalid !(RuntimeFrontierError ctx RelationalRuntimeEpoch RelationalPhase)
  | RuntimeSchedulePriorityInvalid !(ScheduleError RelationalPhase)
  | RuntimeFrontierStampOverflow !FrontierStamp
  | RuntimeMissingQueryRoute !QueryId
  | RuntimeMissingProjectRoute !QueryId
  | RuntimeMissingIndexRoute !(CarrierAddr ctx Carrier prop)
  | RuntimeMissingRestrictRoute !(CarrierAddr ctx Carrier prop)
  | RuntimeMissingCurrentCarrier !(CarrierAddr ctx Carrier prop)
  | RuntimeMissingRestrictionProgram !(RestrictKey ctx Carrier prop)
  | RuntimeRestrictionEdgeError !(RestrictKey ctx Carrier prop) !(CarrierMorphismError ctx Carrier prop boundary evidence)
  | RuntimeMissingFactorProgram !QueryId
  | RuntimeFactorCacheCold !QueryId
  | RuntimeFactorCacheIncoherent !QueryId
  | RuntimeGeneratedSitePatchError !(GeneratedSitePatchError ctx prop)
  | RuntimeGeneratedRoutingError !(RuntimeRoutingError ctx prop)
  | RuntimeGeneratedTopologyInvalid ![RuntimeTopologyValidationError ctx prop]
  | RuntimeCompactionError !Shard !(CarrierStoreError ctx Carrier prop boundary evidence)
  | RuntimeFixedPointIterationLimitExceeded !Int
  deriving stock (Eq, Show)
