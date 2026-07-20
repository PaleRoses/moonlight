{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Morphism.Compile
  ( CarrierMorphismCompileError (..),
    compileCarrierMorphism,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( DenseKey,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Carrier.Morphism.Config
  ( CarrierMorphismConfig (..),
    CarrierReusePolicy (..),
  )
import Moonlight.Flow.Carrier.Morphism.Core.Program
  ( CarrierMorphismContext,
    CarrierMorphismProgram (..),
    carrierMorphismContextFromRestrictionPrograms,
    installCarrierMorphismPrograms,
  )
import Moonlight.Flow.Carrier.Morphism.Result
  ( CarrierMorphismError (..),
  )
import Moonlight.Flow.Carrier.Morphism.Internal.RestrictionGraph
  ( CarrierRestrictionGraph (..),
    compileCarrierRestrictionGraph,
  )
import Moonlight.Flow.Carrier.Morphism.Internal.Reuse
  ( CarrierReuseOps (..),
    projectCarrierReuse,
  )
import Moonlight.Flow.Carrier.Morphism.Restriction
  ( CarrierRestrictionInstallError,
    CompiledCarrierRestriction,
    ContextRank,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
  ( CarrierReuse (..),
    ReuseWitness (..),
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.FiniteLattice
  ( ContextLattice
  )

data CarrierMorphismCompileError ctx prop classId
  = CarrierMorphismRestrictionCompileError
      !(CarrierRestrictionInstallError ctx Carrier prop classId)
  deriving stock (Eq, Show)

compileCarrierMorphism ::
  (Ord ctx, Ord prop, DenseKey classId) =>
  ContextLattice ctx ->
  ContextRank ctx ->
  CarrierMorphismConfig ctx prop classId evidence ->
  Either
    (CarrierMorphismCompileError ctx prop classId)
    (CarrierMorphismContext ctx Carrier prop RuntimeBoundary evidence)
compileCarrierMorphism latticeValue rankOf configValue = do
  restrictionGraph <-
    first CarrierMorphismRestrictionCompileError $
      compileCarrierRestrictionGraph
        latticeValue
        rankOf
        (cmcfgRestrictions configValue)
  let restrictionContext =
        carrierMorphismContextFromRestrictionPrograms (restrictionPrograms restrictionGraph)
      reuseContext =
        installCarrierMorphismPrograms $
          fmap
            (reuseProgram configValue)
            (cmcfgReuses configValue)
  pure (restrictionContext <> reuseContext)
{-# INLINE compileCarrierMorphism #-}

restrictionPrograms ::
  CarrierRestrictionGraph ctx carrier prop boundary ->
  [CompiledCarrierRestriction ctx carrier prop boundary]
restrictionPrograms =
  concat . Map.elems . crgProgramsBySource
{-# INLINE restrictionPrograms #-}

reuseProgram ::
  (Ord ctx, Ord prop) =>
  CarrierMorphismConfig ctx prop classId evidence ->
  CarrierReuse ctx prop ->
  CarrierMorphismProgram ctx Carrier prop RuntimeBoundary evidence
reuseProgram configValue reuse =
  ReuseProgram
    (rwSourceCarrier (cruWitness reuse))
    ( \eventTime sourceDelta ->
        first
            CarrierMorphismReuseError
            ( projectCarrierReuse
              (reuseOpsAt eventTime (cmcfgReusePolicy configValue))
              reuse
              sourceDelta
          )
    )
{-# INLINE reuseProgram #-}

reuseOpsAt ::
  RelationalCarrierTime ctx ->
  CarrierReusePolicy ctx prop evidence ->
  CarrierReuseOps ctx prop evidence
reuseOpsAt eventTime policy =
  CarrierReuseOps
    { croEventTime = eventTime,
      croEvidenceOf = crpEvidenceOf policy,
      croSupportProject = crpSupportProject policy
    }
{-# INLINE reuseOpsAt #-}
