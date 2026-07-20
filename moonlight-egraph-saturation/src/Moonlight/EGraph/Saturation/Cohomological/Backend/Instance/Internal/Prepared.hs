{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Prepared
  ( PreparedCohomologicalBackend (..),
    prepareCohomologicalBackend,
    preparedRequestCacheKeyFor,
    preparedRequestCachePolicyFor,
    cohomologicalBackendResolutionBundle,
    queryFingerprint,
  )
where

import Data.ByteString.Char8 qualified as ByteString.Char8
import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Data.Either (fromRight)
import Data.Kind (Type)
import Data.Word (Word64)
import Moonlight.Core
  ( ConstructorTag,
    HasConstructorTag,
    StableHashDigest (..),
    ZipMatch,
    stableHashByteStrings,
  )
import Moonlight.EGraph.Introspection.Analysis.Resolution
  ( ResolutionBundle,
    buildResolutionBundle,
  )
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchingRequest,
    SaturationPurpose,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Site
  ( CohomologicalBackend (..),
  )
import Moonlight.Homology
  ( HomologyFailure,
  )
import Moonlight.Rewrite.Algebra
  ( CompiledPatternQuery,
    cpqCondition,
    cpqQuery,
    patternQueryPatterns,
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
    compiledGuardDigestWith,
  )
import Moonlight.Saturation.Matching qualified as GenericMatching
import Moonlight.Saturation.Obstruction.Cohomological.Prepared qualified as GenericPrepared
import Moonlight.Sheaf.Obstruction.Cohomological.Evidence.Certification
  ( CachePolicy,
    SectionCertificationAlgebra (..),
  )
import Moonlight.EGraph.Saturation.Cohomological.Types
  ( SheafCapabilityAtom,
    mixFingerprint,
  )
import Numeric.Natural (Natural)

cohomologicalBackendResolutionBundle ::
  (HasConstructorTag f, ZipMatch f) =>
  Natural ->
  CohomologicalBackend owner c f ->
  Either HomologyFailure (Maybe (ResolutionBundle f))
cohomologicalBackendResolutionBundle depthValue configuration =
  traverse (`buildResolutionBundle` depthValue) (cbRewriteSystem configuration)

prepareCohomologicalBackend ::
  (HasConstructorTag f, ZipMatch f) =>
  CohomologicalBackend owner c f ->
  PreparedCohomologicalBackend owner c f
prepareCohomologicalBackend configuration =
  PreparedCohomologicalBackend
    { pcbConfiguration = configuration,
      pcbResolution = fromRight Nothing (cohomologicalBackendResolutionBundle 2 configuration)
    }

type PreparedCohomologicalBackend :: Type -> Type -> (Type -> Type) -> Type
data PreparedCohomologicalBackend owner c f = PreparedCohomologicalBackend
  { pcbConfiguration :: !(CohomologicalBackend owner c f),
    pcbResolution :: !(Maybe (ResolutionBundle f))
  }

preparedRequestCacheKeyFor ::
  (HasConstructorTag f, Show (ConstructorTag f)) =>
  CohomologicalBackend owner c f ->
  MatchingRequest owner c SheafCapabilityAtom f a ->
  GenericPrepared.PreparedRequestCacheKey SaturationPurpose
preparedRequestCacheKeyFor configuration request =
  GenericPrepared.mkPreparedRequestCacheKey
    (queryFingerprint configuration (GenericMatching.qrQuery request))
    (GenericMatching.qrPurpose request)
    (socEnvironmentFingerprint (cbContext configuration) request)

preparedRequestCachePolicyFor ::
  CohomologicalBackend owner c f ->
  MatchingRequest owner c SheafCapabilityAtom f a ->
  CachePolicy
preparedRequestCachePolicyFor configuration =
  socQueryCachePolicy (cbContext configuration)

queryFingerprint ::
  (HasConstructorTag f, Show (ConstructorTag f)) =>
  CohomologicalBackend owner c f ->
  CompiledPatternQuery (CompiledGuard SheafCapabilityAtom f) f ->
  Int
queryFingerprint configuration compiledQuery =
  case cpqCondition compiledQuery of
    Nothing ->
      queryPatternFingerprint configuration compiledQuery
    Just guardCondition ->
      mixFingerprint
        (queryPatternFingerprint configuration compiledQuery)
        (word64Fingerprint (compiledGuardDigestWith constructorTagDigest constructorTagDigest guardCondition))

queryPatternFingerprint ::
  CohomologicalBackend owner c f ->
  CompiledPatternQuery (CompiledGuard SheafCapabilityAtom f) f ->
  Int
queryPatternFingerprint configuration =
  foldl'
    mixFingerprint
    146959810
    . fmap (socPatternFingerprint (cbContext configuration))
    . patternQueryPatterns
    . cpqQuery

constructorTagDigest :: Show tag => tag -> Word64
constructorTagDigest =
  unStableHashDigest . stableHashByteStrings . (: []) . ByteString.Char8.pack . show

word64Fingerprint :: Word64 -> Int
word64Fingerprint =
  fromIntegral
