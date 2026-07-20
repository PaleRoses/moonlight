{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Morphism.Config
  ( CarrierReusePolicy (..),
    CarrierMorphismConfig (..),
    defaultCarrierReusePolicy,
    defaultCarrierMorphismConfig,
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Flow.Carrier.Morphism.Restriction
  ( CarrierRestrictionEdgeSpec,
  )
import Moonlight.Flow.Carrier.Morphism.Internal.Reuse
  ( checkedReuseSupportProject,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
  ( CarrierReuse,
    CarrierReuseError,
    CoverageProjectionRule,
    ReuseWitness,
  )
import Moonlight.Flow.Model.Schema.Morphism
  ( BoundaryProjectionProfile,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.FiniteLattice
  ( SupportBasis
  )

type CarrierReusePolicy :: Type -> Type -> Type -> Type
data CarrierReusePolicy ctx prop evidence = CarrierReusePolicy
  { crpEvidenceOf ::
      !( ReuseWitness ctx prop ->
         CoverageProjectionRule ->
         RuntimeBoundary ->
         evidence ->
         Either (CarrierReuseError ctx prop evidence) evidence
       ),
    crpSupportProject ::
      !( ReuseWitness ctx prop ->
         CoverageProjectionRule ->
         BoundaryProjectionProfile ->
         SupportBasis ctx ->
         Either (CarrierReuseError ctx prop evidence) (SupportBasis ctx)
       )
  }

type CarrierMorphismConfig :: Type -> Type -> Type -> Type -> Type
data CarrierMorphismConfig ctx prop classId evidence = CarrierMorphismConfig
  { cmcfgRestrictions ::
      ![CarrierRestrictionEdgeSpec ctx Carrier prop classId],
    cmcfgReuses ::
      ![CarrierReuse ctx prop],
    cmcfgReusePolicy ::
      !(CarrierReusePolicy ctx prop evidence)
  }

defaultCarrierReusePolicy :: CarrierReusePolicy ctx prop evidence
defaultCarrierReusePolicy =
  CarrierReusePolicy
    { crpEvidenceOf =
        \_witness _rule _targetBoundary evidence ->
          Right evidence,
      crpSupportProject =
        checkedReuseSupportProject
    }
{-# INLINE defaultCarrierReusePolicy #-}

defaultCarrierMorphismConfig ::
  CarrierMorphismConfig ctx prop classId evidence
defaultCarrierMorphismConfig =
  CarrierMorphismConfig
    { cmcfgRestrictions = [],
      cmcfgReuses = [],
      cmcfgReusePolicy = defaultCarrierReusePolicy
    }
{-# INLINE defaultCarrierMorphismConfig #-}
