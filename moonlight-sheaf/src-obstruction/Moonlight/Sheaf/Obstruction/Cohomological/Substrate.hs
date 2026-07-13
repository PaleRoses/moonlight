{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Sheaf.Obstruction.Cohomological.Substrate
  ( CohomologicalAnchor,
    CohomologicalCoordinate,
    SubstrateRegion,
    SubstrateExactMatch,
    SubstrateExactEvidence,
    SubstrateRegionCoverage,
    SubstrateRegionSummary,
    SubstrateWitness,
    SubstrateCache,
    SubstrateCacheKey,
    SubstrateRootCoverage,
    CohomologicalRegionSupport (..),
    CohomologicalLift (..),
    CohomologicalSupportAlgebra (..),
    CohomologicalSubstrate (..),
    environmentFingerprintFor,
    normalizeCohomologicalRegion,
    regionDependencyKeys,
    cacheKeyForRegion,
    emptyObstructionWitness,
    modalityCoverageWitness,
    obstructionWitnessFor,
  )
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Moonlight.Sheaf.Obstruction.Cohomological.Core.Cache
  ( CohomologicalCache,
    ObstructionCacheKey (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Evidence.Certification
  ( SectionCertificationAlgebra (..),
    environmentFingerprintFromCachePolicy,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Core.Environment
  ( IndexedEnvironment,
    ObstructionEnvironmentAlgebra,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section.Exact
  ( CohomologicalExactMatch (..),
    CohomologicalExactMatchEvidence,
    CohomologicalRootCoverage,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Modality
  ( LoweringGap,
    ModalityContribution (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Core.Policy
  ( CohomologicalPolicy,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section.Projection
  ( RelationProjectionConflict,
    SectionCoordinate,
    SectionProjection,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Evidence.Pruning
  ( CohomologicalPruningCertificate,
    CohomologicalPruningGates,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section.Region
  ( RegionExactCoverage,
    RegionTraversalSummary,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section
  ( RelationEvidence,
    SectionReification,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Types
  ( Anchor (..),
    CandidateRegion (..),
    ExactConstraint,
    ExpandedMorphism,
    ExpandedObstructionCell,
    ModalityCoverage,
    ObstructionCell,
    ObstructionLift,
    ObstructionReason (..),
    ObstructionWitness (..),
    OccurrenceId,
  )
import Moonlight.Sheaf.Kernel.Basis (SheafBasis)

-- Fixed anchor and coordinate aliases.

type CohomologicalAnchor :: Type
type CohomologicalAnchor =
  Anchor OccurrenceId

type CohomologicalCoordinate :: Type
type CohomologicalCoordinate =
  SectionCoordinate CohomologicalAnchor

-- Region alias: fixed sheaf region type, substrate-parameterised root.

type SubstrateRegion :: Type -> Type
type SubstrateRegion substrate =
  CandidateRegion (SubstrateRoot substrate)

-- Derived exact-match aliases.

type SubstrateExactEvidence :: Type -> Type
type SubstrateExactEvidence substrate =
  CohomologicalExactMatchEvidence CohomologicalCoordinate

type SubstrateExactMatch :: Type -> Type
type SubstrateExactMatch substrate =
  CohomologicalExactMatch
    (SubstrateRoot substrate)
    (SubstrateResult substrate)
    CohomologicalCoordinate

type SubstrateRootCoverage :: Type -> Type
type SubstrateRootCoverage substrate =
  CohomologicalRootCoverage
    (SubstrateRoot substrate)
    (SubstrateResult substrate)
    CohomologicalCoordinate
    (SubstrateGap substrate)

type SubstrateRegionCoverage :: Type -> Type
type SubstrateRegionCoverage substrate =
  RegionExactCoverage
    (SubstrateExactMatch substrate)
    (SubstrateGap substrate)

type SubstrateWitness :: Type -> Type
type SubstrateWitness substrate =
  ObstructionWitness
    (SubstrateRoot substrate)
    (SubstrateRegion substrate)
    (SubstratePurpose substrate)
    (ModalityCoverage
      (SubstrateModalityTag substrate)
      RelationProjectionConflict)

type SubstrateRegionSummary :: Type -> Type
type SubstrateRegionSummary substrate =
  RegionTraversalSummary
    (SubstrateRegion substrate)
    (SubstrateExactMatch substrate)
    (SubstrateGap substrate)
    (SubstrateWitness substrate)
    CohomologicalPruningCertificate

type SubstrateCacheKey :: Type -> Type
type SubstrateCacheKey substrate =
  ObstructionCacheKey (SubstratePurpose substrate)

type SubstrateCache :: Type -> Type
type SubstrateCache substrate =
  CohomologicalCache
    (SubstratePurpose substrate)
    (SubstrateRegionSummary substrate)

-- Support algebra record.

type CohomologicalSupportAlgebra ::
  (Type -> Type) ->
  (Type -> Type -> Type) ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type

data CohomologicalSupportAlgebra request key region evidence result ref gap tag =
  CohomologicalSupportAlgebra
    { csaCoverage ::
        ModalityCoverage tag RelationProjectionConflict,

      csaMissingOccurrenceDomainCoverage ::
        ModalityCoverage tag RelationProjectionConflict,

      csaOccurrenceDomains ::
        forall runtime.
        IndexedEnvironment (key runtime) ->
        Maybe (Map OccurrenceId IntSet),

      csaEvaluateSupport ::
        forall runtime.
        IndexedEnvironment (key runtime) ->
        (ModalityContribution CohomologicalAnchor ref, evidence),

      csaSectionReification ::
        forall runtime.
        IndexedEnvironment (key runtime) ->
        SectionReification CohomologicalCoordinate result,

      csaSectionProjection ::
        Either
          [RelationProjectionConflict]
          (SectionProjection CohomologicalAnchor CohomologicalCoordinate),

      csaMapGap ::
        LoweringGap CohomologicalAnchor ref ->
        gap
    }

-- Region support record.

type CohomologicalRegionSupport :: Type -> Type -> Type -> Type -> Type -> Type
data CohomologicalRegionSupport occurrence guard evidence result gap = CohomologicalRegionSupport
  { crsOccurrences :: ![occurrence],
    crsGuards :: ![guard],
    crsOccurrenceDomains :: !(Map OccurrenceId IntSet),
    crsExactConstraints :: ![ExactConstraint CohomologicalAnchor],
    crsExactLoweringGaps :: ![gap],
    crsSectionReification :: !(SectionReification CohomologicalCoordinate result),
    crsEvidence :: !evidence
  }

-- Lift record storing constructed bases explicitly.

type CohomologicalLift :: Type -> Type
data CohomologicalLift substrate = CohomologicalLift
  { clQuery :: !(SubstrateQuery substrate),
    clOccurrences :: ![SubstrateOccurrence substrate],
    clOccurrenceDomains :: !(Map OccurrenceId IntSet),
    clGuards :: ![SubstrateGuard substrate],
    clExactConstraints :: ![ExactConstraint CohomologicalAnchor],
    clExactLoweringGaps :: ![SubstrateGap substrate],
    clSectionReification ::
      !(SectionReification CohomologicalCoordinate (SubstrateResult substrate)),
    clSupportEvidence :: !(SubstrateSupportEvidence substrate),
    clZeroBasis :: !(SheafBasis ExpandedObstructionCell),
    clOneBasis :: !(SheafBasis ExpandedObstructionCell),
    clTwoBasis :: !(SheafBasis ExpandedObstructionCell),
    clSupportCells :: ![ObstructionCell],
    clObstructionLift ::
      ObstructionLift
        (SubstrateRoot substrate)
        (SubstrateRegion substrate)
        ExpandedMorphism
  }

-- The substrate class.

class CohomologicalSubstrate substrate where
  type SubstrateRequest substrate :: Type -> Type
  type SubstrateQuery substrate :: Type
  type SubstratePattern substrate :: Type
  type SubstrateOccurrence substrate :: Type
  type SubstrateGuard substrate :: Type
  type SubstrateCandidate substrate :: Type
  type SubstrateCapability substrate :: Type
  type SubstrateRoot substrate :: Type
  type SubstrateResult substrate :: Type
  type SubstratePurpose substrate :: Type
  type SubstrateReference substrate :: Type
  type SubstrateKernelFailure substrate :: Type
  type SubstrateSupportEvidence substrate :: Type
  type SubstrateModalityTag substrate :: Type
  type SubstrateModalityKey substrate :: Type -> Type -> Type

  type SubstrateGap substrate :: Type
  type SubstrateGap substrate =
    LoweringGap CohomologicalAnchor (SubstrateReference substrate)

  substratePolicy ::
    substrate ->
    CohomologicalPolicy

  substrateCertification ::
    substrate ->
    SectionCertificationAlgebra
      (SubstrateRequest substrate)
      (SubstratePattern substrate)
      (SubstrateOccurrence substrate)
      (SubstrateGuard substrate)
      (SubstrateRegion substrate)
      (SubstrateCandidate substrate)
      (SubstrateCapability substrate)
      (SubstrateKernelFailure substrate)

  substrateEnvironment ::
    substrate ->
    ObstructionEnvironmentAlgebra
      (SubstrateRequest substrate)
      (SubstrateModalityKey substrate)
      (SubstrateQuery substrate)
      (SubstrateOccurrence substrate)
      (SubstrateGuard substrate)
      (SubstrateRegion substrate)

  substrateSupportAlgebra ::
    substrate ->
    CohomologicalSupportAlgebra
      (SubstrateRequest substrate)
      (SubstrateModalityKey substrate)
      (SubstrateRegion substrate)
      (SubstrateSupportEvidence substrate)
      (SubstrateResult substrate)
      (SubstrateReference substrate)
      (SubstrateGap substrate)
      (SubstrateModalityTag substrate)

  substrateRequestQuery ::
    substrate ->
    SubstrateRequest substrate runtime ->
    SubstrateQuery substrate

  substrateRequestPattern ::
    substrate ->
    SubstrateRequest substrate runtime ->
    SubstratePattern substrate

  substrateRequestPurpose ::
    substrate ->
    SubstrateRequest substrate runtime ->
    SubstratePurpose substrate

  substrateCollectGuards ::
    substrate ->
    SubstrateQuery substrate ->
    [SubstrateGuard substrate]

  substrateQueryFingerprint ::
    substrate ->
    SubstrateQuery substrate ->
    Int

  substrateOccurrenceId ::
    substrate ->
    SubstrateOccurrence substrate ->
    OccurrenceId

  substrateCanonicalRoot ::
    substrate ->
    SubstrateRequest substrate runtime ->
    SubstrateRoot substrate ->
    SubstrateRoot substrate

  substrateRootKey ::
    substrate ->
    SubstrateRequest substrate runtime ->
    SubstrateRoot substrate ->
    Int

  substrateMemberKey ::
    substrate ->
    SubstrateRequest substrate runtime ->
    Int ->
    Int

  substrateShouldRefine ::
    substrate ->
    SubstrateRegion substrate ->
    Bool

  substrateRefinedRegions ::
    substrate ->
    CohomologicalPruningGates (SubstrateRoot substrate) ->
    SubstrateRequest substrate runtime ->
    SubstrateRegion substrate ->
    [SubstrateRegion substrate]

  substrateEmptyResult ::
    substrate ->
    SubstrateResult substrate

  substrateExactEvidence ::
    substrate ->
    SubstrateRequest substrate runtime ->
    CohomologicalLift substrate ->
    SubstrateRoot substrate ->
    [RelationEvidence CohomologicalCoordinate] ->
    CohomologicalExactMatchEvidence CohomologicalCoordinate

-- Generic helpers.

environmentFingerprintFor ::
  CohomologicalSubstrate substrate =>
  substrate ->
  SubstrateRequest substrate runtime ->
  Maybe Int
environmentFingerprintFor substrate request =
  environmentFingerprintFromCachePolicy
    (socQueryCachePolicy (substrateCertification substrate) request)

normalizeCohomologicalRegion ::
  CohomologicalSubstrate substrate =>
  substrate ->
  SubstrateRequest substrate runtime ->
  SubstrateRegion substrate ->
  SubstrateRegion substrate
normalizeCohomologicalRegion substrate request region =
  region
    { crRoot =
        substrateCanonicalRoot substrate request (crRoot region)
    }

regionDependencyKeys ::
  CohomologicalSubstrate substrate =>
  substrate ->
  SubstrateRequest substrate runtime ->
  SubstrateRegion substrate ->
  IntSet
regionDependencyKeys substrate request region =
  IntSet.insert
    (substrateRootKey substrate request (crRoot region))
    (IntSet.map (substrateMemberKey substrate request) (crMembers region))

cacheKeyForRegion ::
  CohomologicalSubstrate substrate =>
  substrate ->
  SubstrateRequest substrate runtime ->
  SubstrateRegion substrate ->
  SubstrateCacheKey substrate
cacheKeyForRegion substrate request region =
  ObstructionCacheKey
    { ockQueryFingerprint =
        substrateQueryFingerprint substrate (substrateRequestQuery substrate request),
      ockRegionFingerprint = crFingerprint region,
      ockScale = crScale region,
      ockPurpose = substrateRequestPurpose substrate request,
      ockEnvironmentFingerprint = environmentFingerprintFor substrate request
    }

emptyObstructionWitness ::
  CohomologicalSubstrate substrate =>
  substrate ->
  SubstrateRequest substrate runtime ->
  SubstrateRegion substrate ->
  [ObstructionCell] ->
  ObstructionReason
    (SubstrateRegion substrate)
    (ModalityCoverage (SubstrateModalityTag substrate) RelationProjectionConflict) ->
  SubstrateWitness substrate
emptyObstructionWitness substrate request region cells reason =
  obstructionWitnessFor
    substrate
    request
    region
    (crRoot region)
    cells
    reason
    0
    0
    0

modalityCoverageWitness ::
  CohomologicalSubstrate substrate =>
  substrate ->
  SubstrateRequest substrate runtime ->
  SubstrateRegion substrate ->
  ModalityCoverage (SubstrateModalityTag substrate) RelationProjectionConflict ->
  SubstrateWitness substrate
modalityCoverageWitness substrate request region coverage =
  emptyObstructionWitness
    substrate
    request
    region
    []
    (ModalityCoverageMismatch coverage)

obstructionWitnessFor ::
  CohomologicalSubstrate substrate =>
  substrate ->
  SubstrateRequest substrate runtime ->
  SubstrateRegion substrate ->
  SubstrateRoot substrate ->
  [ObstructionCell] ->
  ObstructionReason
    (SubstrateRegion substrate)
    (ModalityCoverage (SubstrateModalityTag substrate) RelationProjectionConflict) ->
  Int ->
  Int ->
  Int ->
  SubstrateWitness substrate
obstructionWitnessFor substrate request region root cells reason kernelLower imageUpper representativeCount =
  ObstructionWitness
    { owRootClass = root,
      owRegion = region,
      owPurpose = substrateRequestPurpose substrate request,
      owPatternFingerprint =
        substrateQueryFingerprint substrate (substrateRequestQuery substrate request),
      owEnvironmentFingerprint =
        environmentFingerprintFor substrate request,
      owCells = cells,
      owReason = reason,
      owKernelRankLowerBound = kernelLower,
      owImageRankUpperBound = imageUpper,
      owRepresentativeCocycleCount = representativeCount
    }
