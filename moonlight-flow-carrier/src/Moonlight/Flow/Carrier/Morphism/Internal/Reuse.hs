module Moonlight.Flow.Carrier.Morphism.Internal.Reuse
  ( CarrierReuseOps (..),
    checkedReuseSupportProject,
    projectCarrierReuse,
  )
where

import Control.Monad
  ( unless,
  )
import Data.Bifunctor
  ( first,
  )
import Data.IntSet qualified as IntSet
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( OriginEvent (OriginSubsumed),
    RelationalOrigin (..),
    originAddParent,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    retimeRelationalCarrierPhase,
  )
import Moonlight.Flow.Carrier.Morphism.Core.Types
  ( CarrierMorphism (..),
    CarrierMorphismPlan (..),
  )
import Moonlight.Flow.Carrier.Morphism.Internal.Apply
  ( applyCarrierMorphism,
  )
import Moonlight.Flow.Carrier.Morphism.Internal.Projection
  ( projectRowDeltaExactWithProfile,
    projectRowDeltaWithProfile,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
  ( CarrierReuse (..),
    CarrierReuseError (..),
    CoverageProjectionRule (..),
    ReuseKind (EquivalentReuse),
    ReuseWitness (..),
    coverageProjectionRuleDigest,
    derivedAddrForReuse,
  )
import Moonlight.Flow.Model.Schema.Morphism
  ( BoundaryProjection (..),
    BoundaryProjectionProfile
      ( bppBoundaryExact,
        bppSensitiveCollision
      ),
    projectRelationalBoundaryWithProfile,
    projectionIsomorphic,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (PhaseSubsumption),
  )
import Moonlight.Flow.Model.Scope
  ( DepsDelta (..),
    RelationalScope (..),
    TopoDelta (..),
    scopeDeps,
    scopeTopo,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.FiniteLattice
  ( SupportBasis
  )
data CarrierReuseOps ctx prop evidence = CarrierReuseOps
  { croEventTime :: !(RelationalCarrierTime ctx),
    croEvidenceOf ::
      !( ReuseWitness ctx prop ->
         CoverageProjectionRule ->
         RuntimeBoundary ->
         evidence ->
         Either (CarrierReuseError ctx prop evidence) evidence
       ),
    croSupportProject ::
      !( ReuseWitness ctx prop ->
         CoverageProjectionRule ->
         BoundaryProjectionProfile ->
         SupportBasis ctx ->
         Either (CarrierReuseError ctx prop evidence) (SupportBasis ctx)
       )
  }

projectCarrierReuse ::
  (Ord ctx, Ord prop) =>
  CarrierReuseOps ctx prop evidence ->
  CarrierReuse ctx prop ->
  RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence ->
  Either
    (CarrierReuseError ctx prop evidence)
    (RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence)
projectCarrierReuse ops reuse =
  applyCarrierMorphism (carrierReuseMorphism ops reuse)
{-# INLINE projectCarrierReuse #-}

carrierReuseMorphism ::
  (Ord ctx, Ord prop) =>
  CarrierReuseOps ctx prop evidence ->
  CarrierReuse ctx prop ->
  CarrierMorphism
    BoundaryProjectionProfile
    ctx
    Carrier
    prop
    RuntimeBoundary
    evidence
    (CarrierReuseError ctx prop evidence)
carrierReuseMorphism ops reuse =
  CarrierMorphism
    { cmPrepare = prepareReuse,
      cmRows = projectRows,
      cmTime = const (retimeRelationalCarrierPhase PhaseSubsumption (croEventTime ops)),
      cmSupport = projectSupport,
      cmEvidence = projectEvidence,
      cmOrigin = subsumedOrigin,
      cmScope = projectedScope
    }
  where
    witness =
      cruWitness reuse

    BoundaryProjection slotProjection =
      rwProjection witness

    prepareReuse sourceSnapshot = do
      (projectedBoundary, boundaryProfile) <-
        first CarrierReuseBoundaryFailed $
          projectRelationalBoundaryWithProfile
            (rwProjection witness)
            (deBoundary sourceSnapshot)

      unless (projectedBoundary == cruTargetBoundary reuse) $
        Left (CarrierReuseBoundaryMismatch (cruTargetBoundary reuse) projectedBoundary)

      targetAddr <-
        projectedAddrForRule ops reuse sourceSnapshot boundaryProfile

      pure
        CarrierMorphismPlan
          { cmpTarget = targetAddr,
            cmpBoundary = cruTargetBoundary reuse,
            cmpProfile = boundaryProfile
          }

    projectRows boundaryProfile rows = do
      case cruCoverageRule reuse of
        PreserveExact -> do
          requireExactProjection PreserveExact boundaryProfile
          fmap fst $
            first CarrierReuseRowsFailed $
              projectRowDeltaExactWithProfile
                (coverageProjectionRuleDigest PreserveExact)
                (bppBoundaryExact boundaryProfile)
                (bppSensitiveCollision boundaryProfile)
                slotProjection
                rows
        ExactByCover -> do
          fmap fst $
            first CarrierReuseRowsFailed $
              projectRowDeltaExactWithProfile
                (coverageProjectionRuleDigest (cruCoverageRule reuse))
                (bppBoundaryExact boundaryProfile)
                (bppSensitiveCollision boundaryProfile)
                slotProjection
                rows
        DowngradeToLowerBound ->
          projectLowerBoundRows boundaryProfile rows
        ObstructProjection tokens ->
          Left (CarrierReuseObstructed tokens)

    projectLowerBoundRows boundaryProfile rows = do
      let coverageDigest =
            coverageProjectionRuleDigest (cruCoverageRule reuse)
      (projectedRows, _rowProjectionProfile) <-
        first CarrierReuseRowsFailed $
          projectRowDeltaWithProfile
            coverageDigest
            (bppBoundaryExact boundaryProfile)
            (bppSensitiveCollision boundaryProfile)
            slotProjection
            rows
      pure projectedRows

    projectSupport boundaryProfile support =
      croSupportProject ops
        witness
        (cruCoverageRule reuse)
        boundaryProfile
        support

    projectEvidence _boundaryProfile evidence =
      croEvidenceOf ops
        witness
        (cruCoverageRule reuse)
        (cruTargetBoundary reuse)
        evidence

    subsumedOrigin sourceOrigin =
      originAddParent
        (rwSourceCarrier witness)
        ( RelationalOrigin
            { roEvent = OriginSubsumed (rwDigest witness),
              roRoute = roRoute sourceOrigin
            }
        )

    projectedScope sourceScope =
      mempty
        { rsDeps = DepsDelta (IntSet.union (scopeDeps sourceScope) (cruWitnessDeps reuse)),
          rsTopo = TopoDelta (IntSet.union (scopeTopo sourceScope) (cruWitnessTopo reuse))
        }
{-# INLINE carrierReuseMorphism #-}

requireExactProjection ::
  CoverageProjectionRule ->
  BoundaryProjectionProfile ->
  Either (CarrierReuseError ctx prop evidence) ()
requireExactProjection rule boundaryProfile =
  unless (bppBoundaryExact boundaryProfile && not (bppSensitiveCollision boundaryProfile)) $
    Left (CarrierReuseExactProjectionNotPreserved rule boundaryProfile)
{-# INLINE requireExactProjection #-}

checkedReuseSupportProject ::
  ReuseWitness ctx prop ->
  CoverageProjectionRule ->
  BoundaryProjectionProfile ->
  SupportBasis ctx ->
  Either (CarrierReuseError ctx prop evidence) (SupportBasis ctx)
checkedReuseSupportProject _witness rule boundaryProfile support =
  case rule of
    PreserveExact -> do
      requireExactProjection rule boundaryProfile
      Right support
    ExactByCover -> do
      Right support
    DowngradeToLowerBound ->
      Right support
    ObstructProjection tokens ->
      Left (CarrierReuseObstructed tokens)
{-# INLINE checkedReuseSupportProject #-}

projectedAddrForRule ::
  CarrierReuseOps ctx prop evidence ->
  CarrierReuse ctx prop ->
  RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence ->
  BoundaryProjectionProfile ->
  Either (CarrierReuseError ctx prop evidence) (CarrierAddr ctx Carrier prop)
projectedAddrForRule _ops reuse _sourceSnapshot boundaryProfile =
  case cruCoverageRule reuse of
    PreserveExact
      | rwKind witness == EquivalentReuse
          || ( projectionIsomorphic slotProjection
                 && bppBoundaryExact boundaryProfile
             ) ->
          Right (rwTargetCarrier witness)
      | otherwise ->
          Left (CarrierReuseAddressPolicyFailed PreserveExact)
    DowngradeToLowerBound ->
      Right (derivedAddrForReuse (rwTargetCarrier witness) witness)
    ExactByCover ->
      Right (rwTargetCarrier witness)
    ObstructProjection tokens ->
      Left (CarrierReuseObstructed tokens)
  where
    witness =
      cruWitness reuse

    BoundaryProjection slotProjection =
      rwProjection witness
{-# INLINE projectedAddrForRule #-}
