{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Reuse.Types
  ( PlanReuseRegistrationEntry (..),
    PlanReuseRegistration (..),
    PlanReuseRequest (..),
    CarrierReuseStrategy (..),
    CarrierReuseCandidateGroup (..),
    PlanReuseMiss (..),
    PlanReuseError (..),
    PlanReuseDiagnostics (..),
    PlanReuseInvariantError (..),
    InstalledReuseMaterialization (..),
    ReuseValidityRequest (..),
    PlanReuseStats (..),
  )
where

import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
  ( CarrierReuse,
    CarrierReuseId,
    CoverageProjectionRule,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Materialization
  ( InstalledReuseMaterialization (..),
    MaterializationInvariantError,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Reuse
  ( CarrierReuseRegistryInvariantError,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Shape
  ( SubsumptionIndexInvariantError,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Shape
  ( RequestedFactorShape,
    SubsumptionRegistrationError,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Stats
  ( PlanReuseStats (..),
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Validity
  ( ReuseValidityRequest (..),
  )
import Moonlight.Flow.Execution.Subsumption.FactorShape
  ( FactorShapeManifest,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
  )
import Moonlight.Flow.Plan.Residual
  ( ResidualTheoryRegistry,
  )
import Moonlight.Flow.Plan.Shape
  ( CanonicalizationResult,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( PlanShape,
    PlanStage (..),
  )

data PlanReuseRegistrationEntry ctx prop = PlanReuseRegistrationEntry
  { prreAddr :: !(CarrierAddr ctx Carrier prop),
    prreTime :: !(RelationalCarrierTime ctx),
    prreBoundary :: !RuntimeBoundary,
    prreScope :: !RelationalScope
  }
  deriving stock (Eq, Show)

data PlanReuseRegistration ctx prop = PlanReuseRegistration
  { prrQueryId :: !QueryId,
    prrCanonicalPlan :: !CanonicalizationResult,
    prrFactorManifest :: !FactorShapeManifest,
    prrInputDigest :: !StableDigest128,
    prrEntries :: ![PlanReuseRegistrationEntry ctx prop]
  }

data PlanReuseRequest ctx prop = PlanReuseRequest
  { prqTargetCarrier :: !(CarrierAddr ctx Carrier prop),
    prqShape :: !(PlanShape 'FactorShape),
    prqBoundary :: !RuntimeBoundary,
    prqValidity :: !ReuseValidityRequest,
    prqResidualTheory :: !ResidualTheoryRegistry
  }

data CarrierReuseStrategy
  = ReuseExactEquivalent
  | ReuseExactByCover
  | ReuseLowerBound
  deriving stock (Eq, Ord, Show, Read)

data CarrierReuseCandidateGroup ctx prop = CarrierReuseCandidateGroup
  { crcgStrategy :: !CarrierReuseStrategy,
    crcgRequested :: !(RequestedFactorShape ctx prop),
    crcgCoverageRule :: !CoverageProjectionRule,
    crcgMiss :: !PlanReuseMiss,
    crcgCandidates :: ![CarrierReuse ctx prop]
  }
  deriving stock (Eq, Show)

data PlanReuseMiss
  = ReuseNoReusableShape
  | ReuseExactRejected
  | ReuseCoverRejected
  | ReuseContainmentRejected
  | ReuseModeRejected
  deriving stock (Eq, Ord, Show, Read)

data PlanReuseError ctx prop
  = ReuseNormalizeFailed !SubsumptionRegistrationError
  | ReuseRegisterFailed !SubsumptionRegistrationError
  | ReuseInstallUnknownReuse !(CarrierReuseId ctx prop)
  | ReuseInstallObstructedReuse !(CarrierReuseId ctx prop)
  | ReuseInstallTargetMismatch
      !(CarrierReuseId ctx prop)
      !(CarrierAddr ctx Carrier prop)
      !(CarrierAddr ctx Carrier prop)
  deriving stock (Eq, Show)

data PlanReuseDiagnostics = PlanReuseDiagnostics
  { prdStats :: !PlanReuseStats,
    prdRegisteredShapes :: {-# UNPACK #-} !Int,
    prdRegisteredReuses :: {-# UNPACK #-} !Int,
    prdInstalledMaterializations :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

data PlanReuseInvariantError ctx prop
  = PlanReuseReuseRegistryInvariant !(CarrierReuseRegistryInvariantError ctx prop)
  | PlanReuseSubsumptionInvariant !(SubsumptionIndexInvariantError ctx prop)
  | PlanReuseMaterializationInvariant !(MaterializationInvariantError ctx prop)
  deriving stock (Eq, Show)
