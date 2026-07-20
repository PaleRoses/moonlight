{-# LANGUAGE DataKinds #-}

module Moonlight.Flow.Carrier.Reuse.Internal.Resolve
  ( carrierReuseStrategiesForMode,
    planCarrierReuse,
    planCarrierReuseStrategy,
    selectRequestedCarrierReuseCandidates,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Maybe
  ( mapMaybe,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
  ( CarrierReuse (..),
    CoverageProjectionRule (..),
    ReuseWitness,
    carrierReuseFromWitness,
  )
import Moonlight.Flow.Carrier.Reuse.Config
  ( ReuseConfig (..),
    ReuseMode (..),
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Shape
  ( lookupContainmentCandidates,
    lookupEquivalentFactorShape,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Proof.Subsumption
  ( verifyContainmentReuse,
    verifySemanticEquivalentReuse,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Shape
  ( RequestedFactorShape (..),
    SubsumptionEntry (..),
  )
import Moonlight.Flow.Carrier.Reuse.Internal.State.Normalize
  ( normalizeRequestedFactorShape,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.State.Types
  ( PlanReuseState (..),
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Validity
  ( ReuseValidityRequest (..),
  )
import Moonlight.Flow.Carrier.Reuse.Types
  ( CarrierReuseCandidateGroup (..),
    CarrierReuseStrategy (..),
    PlanReuseError (..),
    PlanReuseMiss (..),
    PlanReuseRequest (..),
  )
import Moonlight.Flow.Plan.Residual
  ( ResidualTheoryRegistry,
  )

carrierReuseStrategiesForMode :: ReuseMode -> [CarrierReuseStrategy]
carrierReuseStrategiesForMode reuseMode =
  case reuseMode of
    ExactOnly ->
      [ReuseExactEquivalent]
    ExactOrCover ->
      [ReuseExactEquivalent, ReuseExactByCover]
    ExactOrContainment ->
      [ReuseExactEquivalent, ReuseExactByCover, ReuseLowerBound]
    ContainmentOnly ->
      [ReuseLowerBound]
{-# INLINE carrierReuseStrategiesForMode #-}

planCarrierReuse ::
  (Ord ctx, Ord prop) =>
  ReuseConfig ->
  PlanReuseRequest ctx prop ->
  PlanReuseState ctx prop ->
  Either (PlanReuseError ctx prop) (PlanReuseState ctx prop, [CarrierReuseCandidateGroup ctx prop])
planCarrierReuse config request state0 = do
  (state1, normalizedRequest) <-
    normalizePlanReuseRequest request state0
  pure
    ( state1,
      fmap
        (planCarrierReuseStrategyGroup config (prqResidualTheory request) normalizedRequest state1)
        (carrierReuseStrategiesForMode (rcMode config))
    )
{-# INLINE planCarrierReuse #-}

planCarrierReuseStrategy ::
  (Ord ctx, Ord prop) =>
  ReuseConfig ->
  CarrierReuseStrategy ->
  PlanReuseRequest ctx prop ->
  PlanReuseState ctx prop ->
  Either (PlanReuseError ctx prop) (PlanReuseState ctx prop, CarrierReuseCandidateGroup ctx prop)
planCarrierReuseStrategy config strategy request state0 = do
  (state1, normalizedRequest) <-
    normalizePlanReuseRequest request state0
  pure
    ( state1,
      planCarrierReuseStrategyGroup
        config
        (prqResidualTheory request)
        normalizedRequest
        state1
        strategy
    )
{-# INLINE planCarrierReuseStrategy #-}

normalizePlanReuseRequest ::
  (Ord ctx, Ord prop) =>
  PlanReuseRequest ctx prop ->
  PlanReuseState ctx prop ->
  Either (PlanReuseError ctx prop) (PlanReuseState ctx prop, RequestedFactorShape ctx prop)
normalizePlanReuseRequest request =
  first ReuseNormalizeFailed
    . normalizeRequestedFactorShape
      (prqTargetCarrier request)
      (prqShape request)
      (prqBoundary request)
      (prqValidity request)
{-# INLINE normalizePlanReuseRequest #-}

planCarrierReuseStrategyGroup ::
  (Ord ctx, Ord prop) =>
  ReuseConfig ->
  ResidualTheoryRegistry ->
  RequestedFactorShape ctx prop ->
  PlanReuseState ctx prop ->
  CarrierReuseStrategy ->
  CarrierReuseCandidateGroup ctx prop
planCarrierReuseStrategyGroup config residualTheory normalizedRequest state strategy =
  CarrierReuseCandidateGroup
    { crcgStrategy = strategy,
      crcgRequested = normalizedRequest,
      crcgCoverageRule = strategyCoverageRule strategy,
      crcgMiss = strategyMiss strategy,
      crcgCandidates =
        selectStrategyReuseCandidates
          config
          residualTheory
          normalizedRequest
          state
          strategy
    }
{-# INLINE planCarrierReuseStrategyGroup #-}

strategyCoverageRule :: CarrierReuseStrategy -> CoverageProjectionRule
strategyCoverageRule strategy =
  case strategy of
    ReuseExactEquivalent ->
      PreserveExact
    ReuseExactByCover ->
      DowngradeToLowerBound
    ReuseLowerBound ->
      DowngradeToLowerBound
{-# INLINE strategyCoverageRule #-}

strategyMiss :: CarrierReuseStrategy -> PlanReuseMiss
strategyMiss strategy =
  case strategy of
    ReuseExactEquivalent ->
      ReuseExactRejected
    ReuseExactByCover ->
      ReuseCoverRejected
    ReuseLowerBound ->
      ReuseContainmentRejected
{-# INLINE strategyMiss #-}

selectStrategyReuseCandidates ::
  (Ord ctx, Ord prop) =>
  ReuseConfig ->
  ResidualTheoryRegistry ->
  RequestedFactorShape ctx prop ->
  PlanReuseState ctx prop ->
  CarrierReuseStrategy ->
  [CarrierReuse ctx prop]
selectStrategyReuseCandidates config residualTheory normalizedRequest state strategy =
  case strategy of
    ReuseExactEquivalent ->
      exactReuseCandidates residualTheory normalizedRequest state
    ReuseExactByCover ->
      containmentReuseCandidates config DowngradeToLowerBound residualTheory normalizedRequest state
    ReuseLowerBound ->
      containmentReuseCandidates config DowngradeToLowerBound residualTheory normalizedRequest state
{-# INLINE selectStrategyReuseCandidates #-}

selectRequestedCarrierReuseCandidates ::
  (Ord ctx, Ord prop) =>
  ReuseConfig ->
  CoverageProjectionRule ->
  ResidualTheoryRegistry ->
  RequestedFactorShape ctx prop ->
  PlanReuseState ctx prop ->
  [CarrierReuse ctx prop]
selectRequestedCarrierReuseCandidates config coverageRule residualTheory normalizedRequest state =
  case coverageRule of
    PreserveExact ->
      exactReuseCandidates residualTheory normalizedRequest state
    DowngradeToLowerBound ->
      containmentReuseCandidates config coverageRule residualTheory normalizedRequest state
    ExactByCover ->
      containmentReuseCandidates config coverageRule residualTheory normalizedRequest state
    ObstructProjection {} ->
      []
{-# INLINE selectRequestedCarrierReuseCandidates #-}

exactReuseCandidates ::
  (Ord ctx, Ord prop) =>
  ResidualTheoryRegistry ->
  RequestedFactorShape ctx prop ->
  PlanReuseState ctx prop ->
  [CarrierReuse ctx prop]
exactReuseCandidates residualTheory normalizedRequest state =
  verifiedReuseCandidates
    PreserveExact
    normalizedRequest
    (verifySemanticEquivalentReuse residualTheory normalizedRequest)
    (lookupEquivalentFactorShape normalizedRequest (prsSubsumptionIndex state))
{-# INLINE exactReuseCandidates #-}

containmentReuseCandidates ::
  (Ord ctx, Ord prop) =>
  ReuseConfig ->
  CoverageProjectionRule ->
  ResidualTheoryRegistry ->
  RequestedFactorShape ctx prop ->
  PlanReuseState ctx prop ->
  [CarrierReuse ctx prop]
containmentReuseCandidates config coverageRule residualTheory normalizedRequest state =
  verifiedReuseCandidates
    coverageRule
    normalizedRequest
    (verifyContainmentReuse residualTheory normalizedRequest)
    ( take (max 0 (rcMaxContainmentCandidates config)) $
        filter
          ((/= rfsTargetCarrier normalizedRequest) . seCarrier)
          $
        lookupContainmentCandidates
          residualTheory
          (prsSubsumptionIndex state)
          normalizedRequest
    )
{-# INLINE containmentReuseCandidates #-}

verifiedReuseCandidates ::
  CoverageProjectionRule ->
  RequestedFactorShape ctx prop ->
  (SubsumptionEntry ctx prop -> Either err (ReuseWitness ctx prop)) ->
  [SubsumptionEntry ctx prop] ->
  [CarrierReuse ctx prop]
verifiedReuseCandidates rule request verify entries =
  mapMaybe verifyOne entries
  where
    verifyOne entry =
      case verify entry of
        Right witness ->
          Just
            ( carrierReuseFromWitness
                rule
                (rfsBoundary request)
                (rvrViewDigest (rfsValidity request))
                (seDeps entry)
                (seTopo entry)
                witness
            )
        Left _ ->
          Nothing
{-# INLINE verifiedReuseCandidates #-}
