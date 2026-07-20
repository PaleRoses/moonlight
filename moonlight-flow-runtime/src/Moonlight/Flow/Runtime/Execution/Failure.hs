{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
    RelationalRuntimeOpFailure (..),
  )
where

import Data.Kind
  ( Type,
  )
import Data.List.NonEmpty
  ( NonEmpty,
  )
import Data.Set
  ( Set,
  )
import Moonlight.Core
  ( AtomId,
    QueryId,
    SlotId,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
    SubsumptionWitnessDigest,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Differential.Carrier.Topology
  ( CarrierFamily,
  )
import Moonlight.Flow.Carrier.Core.Obstruction.Types
  ( CarrierObstructionEvidence,
  )
import Moonlight.Flow.Carrier.Engine.Project
  ( CarrierProjectError,
  )
import Moonlight.Flow.Carrier.Morphism.Amalgamation
  ( AmalgamationError,
  )
import Moonlight.Flow.Carrier.Morphism.Result
  ( CarrierMorphismError,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
import Moonlight.Flow.Carrier.Reuse
  ( SubsumptionRegistrationError,
  )
import Moonlight.Flow.Carrier.Reuse.Types
  ( PlanReuseError,
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierStoreError,
  )
import Moonlight.Flow.Execution.Factor.Run
  ( FactorRunError,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Model.Schema.Morphism
  ( SchemaProjectionError,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot,
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( FactorProgramError,
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( Shard,
  )
import Moonlight.Flow.Storage.Plan
  ( StoragePlanError,
  )
import Moonlight.Flow.Storage.Relation
  ( RelationPatchError,
  )
import Moonlight.Flow.Storage.Store
  ( StorageError,
  )

type RelationalRuntimeError :: Type -> Type -> Type -> Type -> Type
type RelationalRuntimeError ctx prop boundary evidence =
  RuntimeError
    ctx
    prop
    boundary
    evidence
    (RelationalRuntimeOpFailure ctx prop boundary evidence)

type RelationalRuntimeOpFailure :: Type -> Type -> Type -> Type -> Type
data RelationalRuntimeOpFailure ctx prop boundary evidence
  = RelationalRuntimeProjectOperatorError !Shard !CarrierProjectError
  | RelationalRuntimeRestrictOperatorError !Shard !(CarrierMorphismError ctx Carrier prop boundary evidence)
  | RelationalRuntimeCarrierStoreOperatorError !Shard !(CarrierStoreError ctx Carrier prop boundary evidence)
  | RelationalRuntimeFactorProgramInvalid !QueryId !FactorProgramError
  | RelationalRuntimeFactorCarrierArrangementObstructed
      !QueryId
      !AtomId
      !(CarrierStoreError ctx Carrier prop boundary evidence)
  | RelationalRuntimeFactorCarrierRelationProjectionFailed
      !QueryId
      !AtomId
      !(CarrierStoreError ctx Carrier prop boundary evidence)
  | RelationalRuntimeFactorStoragePlanFailed !QueryId !StoragePlanError
  | RelationalRuntimeFactorStorageBuildFailed !QueryId !StorageError
  | RelationalRuntimeFactorPreparedRelationPatchFailed !QueryId !AtomId !RelationPatchError
  | RelationalRuntimeFactorStoragePatchFailed !QueryId !StorageError
  | RelationalRuntimeFactorCarrierRepairFailed !QueryId !FactorRunError
  | RelationalRuntimeSubsumptionRegistrationFailed !QueryId !SubsumptionRegistrationError
  | RelationalRuntimePlanReuseInstallFailed !(PlanReuseError ctx prop)
  | RelationalRuntimeEquivalentReuseProjectionFailed !QueryId !SubsumptionWitnessDigest !(SchemaProjectionError SlotId CanonSlot)
  | RelationalRuntimeContainmentReuseProjectionFailed !QueryId !SubsumptionWitnessDigest !(CarrierReuseError ctx prop evidence)
  | RelationalRuntimeSubsumptionProjectionFailed !(CarrierReuseId ctx prop) !(CarrierReuseError ctx prop evidence)
  | RelationalRuntimeSubsumptionTargetMismatch !(CarrierReuseId ctx prop) !(CarrierAddr ctx Carrier prop)
  | RelationalRuntimeMissingCarrierReuse !(CarrierReuseId ctx prop) !(CarrierAddr ctx Carrier prop)
  | RelationalRuntimeDanglingDerivedCarrier !(CarrierAddr ctx Carrier prop) !(CarrierReuseId ctx prop)
  | RelationalRuntimeMissingInstalledReuseMaterialization !(CarrierReuseId ctx prop) !(CarrierAddr ctx Carrier prop)
  | RelationalRuntimeDerivedCarrierMultipleOwners !(CarrierAddr ctx Carrier prop) !(Set (CarrierReuseId ctx prop))
  | RelationalRuntimeAmalgamationError !(CarrierFamily ctx Carrier prop) !(AmalgamationError ctx Carrier prop RuntimeBoundary evidence)
  | RelationalRuntimeAmalgamationObstructed
      !(CarrierFamily ctx Carrier prop)
      !(NonEmpty (CarrierObstructionEvidence ctx Carrier prop RuntimeBoundary evidence))
  deriving stock (Eq, Show)
