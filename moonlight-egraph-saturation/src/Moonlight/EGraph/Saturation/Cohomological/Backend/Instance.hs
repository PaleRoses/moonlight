{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Moonlight.EGraph.Saturation.Cohomological.Backend.Instance
  ( SeedInterpreter (..),
    CohomologicalBackend (..),
    PreparedCohomologicalBackend (..),
    mkCohomologicalBackend,
    withRewriteSystemWitness,
    cohomologicalBackendForProfile,
    prepareCohomologicalBackend,
    PatternOccurrence (..),
    cachePolicyFromEnvironmentFingerprint,
    mkSheafCapabilityEnvironment,
    mixFingerprint,
  )
where

import Data.Map.Strict (Map)
import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Data.Maybe (isJust, mapMaybe)
import Data.Set qualified as Set
import Moonlight.Core
  ( ConstructorTag,
    HasConstructorTag,
    Pattern,
  )
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchingRequest,
    SaturationPurpose,
  )
import Moonlight.Saturation.Matching qualified as GenericMatching
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    canonicalizeClassId,
    classIdKey,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Seed
  ( materializeSeedWithPruning,
  )
import Moonlight.Saturation.Obstruction.Cohomological.Seed
  ( seedFrontierPlanSeeds,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Evidence
  ( deriveProofRelationsFromContext,
    deriveRelationEvidence,
    mergeRelationEvidence,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Modality
  ( eGraphModalityRegistry,
    eGraphSectionProjection,
    evaluateEGraphModalitySupport,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Site
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Construction
  ( cohomologicalBackendForProfile,
    mkCohomologicalBackend,
    withRewriteSystemWitness,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Prepared
  ( PreparedCohomologicalBackend (..),
    prepareCohomologicalBackend,
    queryFingerprint,
  )
import Moonlight.EGraph.Saturation.Cohomological.Types
  ( SheafCapabilityAtom,
    CapabilityModalityEnvironment,
    EqualityModalityEnvironment (..),
    PatternOccurrence (..),
    SheafModalityKey,
    data EqualityModalityKey,
    cachePolicyFromEnvironmentFingerprint,
    mkSheafCapabilityEnvironment,
    mixFingerprint,
    refineSheafRegion,
    sheafEnvironmentAlgebra,
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
    GuardAtom,
    GuardRef,
    compiledGuardAtoms,
  )
import Moonlight.Rewrite.System
  ( FactDerivation (..),
  )
import Moonlight.Rewrite.System (FactId)
import Moonlight.Core
  ( Substitution,
    emptySubstitution
  )
import Moonlight.Rewrite.Algebra
  ( CompiledPatternQuery,
    cpqCondition,
    cpqPrimaryPattern,
  )
import Moonlight.Sheaf.Obstruction
  ( CandidateRegion (..),
    CandidateStalk,
    ConstraintId,
    OccurrenceId,
    RegionScale (..),
    RelationEvidence,
    RelationFlavor (..),
    SheafModalityTag (..),
    lookupEnvironmentBinding,
    mkSheafModalityCoverage,
    modalityRegistryReification,
    reFlavor,
  )
import Moonlight.Sheaf.Pruning (pruningDecisionAllowed)
import Moonlight.Sheaf.Obstruction
  ( CohomologicalPolicy (..),
    CohomologicalPruningGates (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Substrate
  ( CohomologicalCoordinate,
    CohomologicalLift (..),
    CohomologicalSubstrate (..),
    CohomologicalSupportAlgebra (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section.Exact
  ( CohomologicalExactMatchEvidence (..),
  )

-- | Egraph-specific query fingerprint lifted to the substrate interface.
eGraphQueryFingerprint ::
  (HasConstructorTag f, Show (ConstructorTag f)) =>
  CohomologicalBackend owner c f ->
  CompiledPatternQuery (CompiledGuard SheafCapabilityAtom f) f ->
  Int
eGraphQueryFingerprint =
  queryFingerprint

-- | Refined regions for a candidate region, wiring the seed interpreter when present.
eGraphRefinedRegions ::
  CohomologicalBackend owner c f ->
  CohomologicalPruningGates ClassId ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  CandidateRegion ClassId ->
  [CandidateRegion ClassId]
eGraphRefinedRegions backend gates request region =
  case cbSeedInterpreter backend of
        Nothing ->
          filter
            (pruningDecisionAllowed . cpgRegionDecision gates)
            (refineSheafRegion (cbContext backend) request (GenericMatching.qrQuery request) region)
        Just seedInterpreter ->
          let queryPattern =
                cpqPrimaryPattern (GenericMatching.qrQuery request)
           in mapMaybe
                (materializeSeedWithPruning gates seedInterpreter request queryPattern)
                ( filter
                    (pruningDecisionAllowed . cpgSeedDecision gates)
                    (seedFrontierPlanSeeds (siRefineSeedPlan seedInterpreter request queryPattern region))
                )

-- | Dispatch a single RelationEvidence into the appropriate evidence bucket.
-- Inlined from Section.Evidence.singletonEvidence (not part of that module's export).
singletonEvidenceFor ::
  RelationEvidence CohomologicalCoordinate ->
  CohomologicalExactMatchEvidence CohomologicalCoordinate
singletonEvidenceFor relationEvidence =
  case reFlavor relationEvidence of
    FactFlavor ->
      CohomologicalExactMatchEvidence
        { cemeFactRelations = [relationEvidence],
          cemeProvenanceRelations = [],
          cemeProofRelations = [],
          cemeCapabilityRelations = []
        }
    ProvenanceFlavor ->
      CohomologicalExactMatchEvidence
        { cemeFactRelations = [],
          cemeProvenanceRelations = [relationEvidence],
          cemeProofRelations = [],
          cemeCapabilityRelations = []
        }
    ProofFlavor ->
      CohomologicalExactMatchEvidence
        { cemeFactRelations = [],
          cemeProvenanceRelations = [],
          cemeProofRelations = [relationEvidence],
          cemeCapabilityRelations = []
        }
    CapabilityFlavor ->
      CohomologicalExactMatchEvidence
        { cemeFactRelations = [],
          cemeProvenanceRelations = [],
          cemeProofRelations = [],
          cemeCapabilityRelations = [relationEvidence]
        }

eGraphExactEvidence ::
  CohomologicalRuntime owner c f a ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  CohomologicalLift (CohomologicalRuntime owner c f a) ->
  ClassId ->
  [RelationEvidence CohomologicalCoordinate] ->
  CohomologicalExactMatchEvidence CohomologicalCoordinate
eGraphExactEvidence runtimeBackend request lift rootClass relationEvidenceValues =
  let canonicalize =
        canonicalizeClassId (crtGraph runtimeBackend)
      baseEvidence = foldMap singletonEvidenceFor relationEvidenceValues
      factRelations = cemeFactRelations baseEvidence
      provenanceRelations =
        mergeRelationEvidence
          (cemeProvenanceRelations baseEvidence)
          ( deriveRelationEvidence
              (crtFactDerivations runtimeBackend)
              (clSupportEvidence lift)
              ProvenanceFlavor
              (not . Set.null)
              factRelations
          )
      proofRelations =
        mergeRelationEvidence
          (cemeProofRelations baseEvidence)
          ( mergeRelationEvidence
              ( deriveRelationEvidence
                  (crtFactDerivations runtimeBackend)
                  (clSupportEvidence lift)
                  ProofFlavor
                  (foldr (\factDerivation -> (||) (isJust (fdGuardEvidence factDerivation))) False)
                  factRelations
              )
              (deriveProofRelationsFromContext canonicalize (crtProofReachability runtimeBackend) request rootClass factRelations)
          )
   in CohomologicalExactMatchEvidence
        { cemeFactRelations = factRelations,
          cemeProvenanceRelations = provenanceRelations,
          cemeProofRelations = proofRelations,
          cemeCapabilityRelations = cemeCapabilityRelations baseEvidence
        }

instance (HasConstructorTag f, Show (ConstructorTag f)) => CohomologicalSubstrate (CohomologicalRuntime owner c f a) where
  type SubstrateRequest (CohomologicalRuntime owner c f a) =
    MatchingRequest owner c SheafCapabilityAtom f

  type SubstrateQuery (CohomologicalRuntime owner c f a) =
    CompiledPatternQuery (CompiledGuard SheafCapabilityAtom f) f

  type SubstratePattern (CohomologicalRuntime owner c f a) =
    Pattern f

  type SubstrateOccurrence (CohomologicalRuntime owner c f a) =
    PatternOccurrence f

  type SubstrateGuard (CohomologicalRuntime owner c f a) =
    GuardAtom SheafCapabilityAtom f

  type SubstrateCandidate (CohomologicalRuntime owner c f a) =
    CandidateStalk

  type SubstrateCapability (CohomologicalRuntime owner c f a) =
    CapabilityModalityEnvironment OccurrenceId

  type SubstrateRoot (CohomologicalRuntime owner c f a) =
    ClassId

  type SubstrateResult (CohomologicalRuntime owner c f a) =
    Substitution

  type SubstratePurpose (CohomologicalRuntime owner c f a) =
    SaturationPurpose

  type SubstrateReference (CohomologicalRuntime owner c f a) =
    GuardRef

  type SubstrateKernelFailure (CohomologicalRuntime owner c f a) =
    ()

  type SubstrateSupportEvidence (CohomologicalRuntime owner c f a) =
    Map ConstraintId FactId

  type SubstrateModalityTag (CohomologicalRuntime owner c f a) =
    SheafModalityTag

  type SubstrateModalityKey (CohomologicalRuntime owner c f a) =
    SheafModalityKey owner c f

  substratePolicy =
    cbPolicy . crtBackend

  substrateCertification =
    cbContext . crtBackend

  substrateEnvironment runtimeBackend =
    let backend = crtBackend runtimeBackend
     in sheafEnvironmentAlgebra
          (cbContext backend)
          (canonicalizeClassId (crtGraph runtimeBackend))

  substrateSupportAlgebra runtimeBackend =
    let backend = crtBackend runtimeBackend
     in CohomologicalSupportAlgebra
      { csaCoverage = cbModalityCoverage backend,
        csaMissingOccurrenceDomainCoverage =
          mkSheafModalityCoverage [EqualityModalityTag] [] [],
        csaOccurrenceDomains =
          \environment ->
            emeOccurrenceDomains <$> lookupEnvironmentBinding EqualityModalityKey environment,
        csaEvaluateSupport =
          evaluateEGraphModalitySupport
            (canonicalizeClassId (crtGraph runtimeBackend))
            (crtFacts runtimeBackend)
            (crtProofReachability runtimeBackend),
        csaSectionReification =
          \environment ->
            modalityRegistryReification environment eGraphModalityRegistry,
        csaSectionProjection =
          eGraphSectionProjection,
        csaMapGap = id
      }

  substrateRequestQuery _ =
    GenericMatching.qrQuery

  substrateRequestPattern _ =
    cpqPrimaryPattern . GenericMatching.qrQuery

  substrateRequestPurpose _ =
    GenericMatching.qrPurpose

  substrateCollectGuards _ query =
    maybe [] compiledGuardAtoms (cpqCondition query)

  substrateQueryFingerprint runtimeBackend query =
    let backend = crtBackend runtimeBackend
     in eGraphQueryFingerprint backend query

  substrateOccurrenceId _ =
    poId

  substrateCanonicalRoot runtimeBackend _request root =
    canonicalizeClassId (crtGraph runtimeBackend) root

  substrateRootKey backend _request root =
    classIdKey (canonicalizeClassId (crtGraph backend) root)

  substrateMemberKey backend _request memberKey =
    classIdKey (canonicalizeClassId (crtGraph backend) (ClassId memberKey))

  substrateShouldRefine runtimeBackend region =
    let backend = crtBackend runtimeBackend
     in cpUseHierarchicalPruning (cbPolicy backend)
      && crScale region == CoarseRegion
      && crDepth region < cpMaxCoarseDepth (cbPolicy backend)

  substrateRefinedRegions runtimeBackend gates request region =
    let backend = crtBackend runtimeBackend
     in eGraphRefinedRegions backend gates request region

  substrateEmptyResult _ =
    emptySubstitution

  substrateExactEvidence =
    eGraphExactEvidence
