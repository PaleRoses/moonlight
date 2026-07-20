{-# LANGUAGE RankNTypes #-}

module Moonlight.Sheaf.Obstruction.Cohomological.Evidence.Certification
  ( CachePolicy (..),
    EnvironmentCacheKey (..),
    environmentFingerprintFromCachePolicy,
    RegionCarrierPlan,
    regionCarrierPlanFromList,
    carrierPlanItems,
    SectionCertificationAlgebra (..),
    mkSectionCertificationAlgebraWithCachePolicy,
    mkSectionCertificationAlgebraWithCapabilitiesAndCachePolicy,
  )
where

import Data.Kind (Type)
import Moonlight.Sheaf.Obstruction.Cohomological.Core.Carrier
  ( RegionCarrierPlan,
    carrierPlanItems,
    regionCarrierPlanFromList,
  )
import Moonlight.Sheaf.Verdict
  ( Verdict (..),
  )
type CachePolicy :: Type
data CachePolicy
  = SharedAcrossEnvironments
  | EnvironmentScoped !EnvironmentCacheKey
  | DoNotCache
  deriving stock (Eq, Ord, Show, Read)

type EnvironmentCacheKey :: Type
newtype EnvironmentCacheKey = EnvironmentCacheKey
  { unEnvironmentCacheKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

environmentFingerprintFromCachePolicy :: CachePolicy -> Maybe Int
environmentFingerprintFromCachePolicy cachePolicy =
  case cachePolicy of
    SharedAcrossEnvironments -> Nothing
    EnvironmentScoped environmentFingerprint -> Just (unEnvironmentCacheKey environmentFingerprint)
    DoNotCache -> Nothing

type SectionCertificationAlgebra :: (Type -> Type) -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data SectionCertificationAlgebra request query occurrence guard region candidate capability kernelFailure = SectionCertificationAlgebra
  { socCollectOccurrences :: query -> [occurrence],
    socRegionCarrierPlan ::
      forall runtime.
      request runtime ->
      query ->
      RegionCarrierPlan region,
    socRefineRegion ::
      forall runtime.
      request runtime ->
      query ->
      region ->
      [region],
    socOccurrenceDomain ::
      forall runtime.
      request runtime ->
      occurrence ->
      region ->
      candidate,
    socGuardDomain ::
      forall runtime.
      request runtime ->
      guard ->
      region ->
      candidate,
    socCapabilityEnvironment ::
      forall runtime.
      request runtime ->
      region ->
      [occurrence] ->
      [guard] ->
      capability,
    socKernelVerdict ::
      forall runtime.
      request runtime ->
      region ->
      Verdict () kernelFailure,
    socPatternFingerprint :: query -> Int,
    socQueryCachePolicy ::
      forall runtime.
      request runtime ->
      CachePolicy,
    socEnvironmentFingerprint ::
      forall runtime.
      request runtime ->
      Maybe Int
  }

mkSectionCertificationAlgebraWithCachePolicy ::
  capability ->
  (query -> [occurrence]) ->
  (forall runtime. request runtime -> query -> RegionCarrierPlan region) ->
  (forall runtime. request runtime -> query -> region -> [region]) ->
  (forall runtime. request runtime -> occurrence -> region -> candidate) ->
  (forall runtime. request runtime -> guard -> region -> candidate) ->
  (query -> Int) ->
  (forall runtime. request runtime -> CachePolicy) ->
  SectionCertificationAlgebra request query occurrence guard region candidate capability ()
mkSectionCertificationAlgebraWithCachePolicy defaultCapability collectOccurrences enumerateRegions refineRegion occurrenceDomain guardDomain = mkSectionCertificationAlgebraWithCapabilitiesAndCachePolicy
    collectOccurrences
    enumerateRegions
    refineRegion
    occurrenceDomain
    guardDomain
    (\_ _ _ _ -> defaultCapability)
    (\_ _ -> Accepted ())

mkSectionCertificationAlgebraWithCapabilitiesAndCachePolicy ::
  (query -> [occurrence]) ->
  (forall runtime. request runtime -> query -> RegionCarrierPlan region) ->
  (forall runtime. request runtime -> query -> region -> [region]) ->
  (forall runtime. request runtime -> occurrence -> region -> candidate) ->
  (forall runtime. request runtime -> guard -> region -> candidate) ->
  (forall runtime. request runtime -> region -> [occurrence] -> [guard] -> capability) ->
  (forall runtime. request runtime -> region -> Verdict () kernelFailure) ->
  (query -> Int) ->
  (forall runtime. request runtime -> CachePolicy) ->
  SectionCertificationAlgebra request query occurrence guard region candidate capability kernelFailure
mkSectionCertificationAlgebraWithCapabilitiesAndCachePolicy collectOccurrences regionCarrierPlan refineRegion occurrenceDomain guardDomain capabilityEnvironment kernelVerdict patternFingerprint queryCachePolicy =
  SectionCertificationAlgebra
    { socCollectOccurrences = collectOccurrences,
      socRegionCarrierPlan = regionCarrierPlan,
      socRefineRegion = refineRegion,
      socOccurrenceDomain = occurrenceDomain,
      socGuardDomain = guardDomain,
      socCapabilityEnvironment = capabilityEnvironment,
      socKernelVerdict = kernelVerdict,
      socPatternFingerprint = patternFingerprint,
      socQueryCachePolicy = queryCachePolicy,
      socEnvironmentFingerprint = environmentFingerprintFromCachePolicy . queryCachePolicy
    }
