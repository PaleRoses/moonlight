{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
module Moonlight.Flow.Carrier.Reuse.Internal.Shape
  ( RequestedFactorShape (..),
    SubsumptionEntry (..),
    SubsumptionRegistrationError (..),
  )
where
import Data.IntSet (IntSet)
import Moonlight.Core (QueryId)
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Coverage (CoverageFact)
import Moonlight.Flow.Execution.Subsumption.FactorShape (FactorShapeError)
import Moonlight.Flow.Model.Schema.Boundary (RuntimeBoundary)
import Moonlight.Flow.Plan.Rewrite (FactorShapeNormalization, PlanReuseShapeKey, PlanSaturationError)
import Moonlight.Flow.Plan.Shape.Term (PlanShape, PlanStage (..))
import Moonlight.Flow.Carrier.Reuse.Internal.Validity (ReuseValidity, ReuseValidityRequest)

data RequestedFactorShape ctx prop = RequestedFactorShape
  { rfsTargetCarrier :: !(CarrierAddr ctx Carrier prop),
    rfsShape :: !(PlanShape 'FactorShape),
    rfsShapeKey :: !PlanReuseShapeKey,
    rfsShapeNormalization :: !FactorShapeNormalization,
    rfsBoundary :: !RuntimeBoundary,
    rfsValidity :: !ReuseValidityRequest
  }
  deriving stock (Eq, Show)

data SubsumptionEntry ctx prop = SubsumptionEntry
  { seShape :: !(PlanShape 'FactorShape),
    seShapeKey :: !PlanReuseShapeKey,
    seShapeNormalization :: !FactorShapeNormalization,
    seCarrier :: !(CarrierAddr ctx Carrier prop),
    seValidity :: !ReuseValidity,
    seBoundary :: !RuntimeBoundary,
    seCoverageHint :: !CoverageFact,
    seDeps :: !IntSet,
    seTopo :: !IntSet
  }
  deriving stock (Eq, Ord, Show)

data SubsumptionRegistrationError
  = SubsumptionRegistrationFactorShapeError !FactorShapeError
  | SubsumptionRegistrationPlanSaturationError !PlanSaturationError
  | SubsumptionRegistrationNormalizationUnstable !Int
  | SubsumptionRegistrationAtomCarrierRejected
  | SubsumptionRegistrationDerivedCarrierRejected !Carrier
  | SubsumptionRegistrationUnexpectedCarrier !Carrier
  | SubsumptionRegistrationQueryMismatch !QueryId !QueryId
  | SubsumptionRegistrationMissingManifestNode !Carrier
  | SubsumptionRegistrationDuplicateCarrierDifferentShape
      !(CarrierAddr () Carrier ())
      !PlanReuseShapeKey
      !PlanReuseShapeKey
  | SubsumptionRegistrationOwnershipDangling
      !(CarrierAddr () Carrier ())
      !PlanReuseShapeKey
  deriving stock (Eq, Ord, Show)
