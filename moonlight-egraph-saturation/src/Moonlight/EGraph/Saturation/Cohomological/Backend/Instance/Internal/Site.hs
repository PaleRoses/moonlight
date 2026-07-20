{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Site
  ( SeedInterpreter (..),
    CohomologicalBackend (..),
    CohomologicalRuntime (..),
    EGraphCandidateRegion,
    EGraphRootCoverage,
    EGraphObstructionWitness,
    RegionTraversalSummary,
    EGraphObstructionCache,
    EGraphAnchor,
    EGraphExactConstraint,
    EGraphObstructionModality,
    SheafModalityCoverage,
    anchorForGuardRef,
    anchorDomain,
    zeroCellForAnchor,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( Language,
    Pattern,
    patternVarKey,
  )
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteSystem)
import Moonlight.Rewrite.System
  ( GuardRef,
    guardRefPatternVar,
  )
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchingFrontier,
    MatchingRequest,
    SaturationPurpose,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    EGraph,
  )
import Moonlight.EGraph.Saturation.Cohomological.Types
  ( EGraphSectionCertification,
    SheafCapabilityAtom,
  )
import Moonlight.Core (Substitution)
import Moonlight.Rewrite.ProofContext
  ( ProofReachability,
  )
import Moonlight.Rewrite.System
  ( FactDerivationIndex,
  )
import Moonlight.Rewrite.System
  ( FactStore,
  )
import Moonlight.Saturation.Obstruction.Cohomological.Seed
  ( SeedInterpreter (..),
  )
import Moonlight.Sheaf.Obstruction
  ( CohomologicalCache,
  )
import Moonlight.Sheaf.Obstruction
  ( ObstructionModality,
  )
import Moonlight.Sheaf.Obstruction
  ( CohomologicalPolicy,
  )
import Moonlight.Sheaf.Obstruction
  ( SectionCoordinate,
  )
import qualified Moonlight.Sheaf.Obstruction as GenericRegion
import Moonlight.Sheaf.Obstruction
  ( Anchor (..),
    CandidateRegion,
    ExactConstraint,
    LoweringGap,
    ObstructionWitness,
    OccurrenceId (..),
    anchorDomain,
    zeroCellForAnchor,
  )
import Moonlight.Sheaf.Obstruction
  ( SheafModalityCoverage,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section.Exact
  ( CohomologicalExactMatch,
    CohomologicalRootCoverage,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Evidence.Pruning
  ( CohomologicalPruningCertificate,
  )

type CohomologicalBackend :: Type -> Type -> (Type -> Type) -> Type
data CohomologicalBackend owner c f where
  CohomologicalBackend ::
    Language f =>
    { cbContext ::
        !(EGraphSectionCertification owner c f),
      cbPolicy :: !CohomologicalPolicy,
      cbModalityCoverage :: !SheafModalityCoverage,
      cbRewriteSystem :: !(Maybe (RewriteSystem f)),
      cbSeedInterpreter :: !(Maybe (SeedInterpreter (MatchingRequest owner c SheafCapabilityAtom f) (Pattern f) MatchingFrontier ClassId))
    } ->
    CohomologicalBackend owner c f

type CohomologicalRuntime :: Type -> Type -> (Type -> Type) -> Type -> Type
data CohomologicalRuntime owner c f a = CohomologicalRuntime
  { crtBackend :: !(CohomologicalBackend owner c f),
    crtGraph :: !(EGraph f a),
    crtFacts :: !FactStore,
    crtFactDerivations :: !FactDerivationIndex,
    crtProofReachability :: !(Maybe ProofReachability)
  }

type EGraphCandidateRegion :: Type
type EGraphCandidateRegion = CandidateRegion ClassId

type EGraphRootCoverage :: Type
type EGraphRootCoverage =
  CohomologicalRootCoverage ClassId Substitution (SectionCoordinate EGraphAnchor) (LoweringGap EGraphAnchor GuardRef)

type EGraphObstructionWitness :: Type
type EGraphObstructionWitness =
  ObstructionWitness ClassId EGraphCandidateRegion SaturationPurpose SheafModalityCoverage

type RegionTraversalSummary :: Type
type RegionTraversalSummary =
  GenericRegion.RegionTraversalSummary
    EGraphCandidateRegion
    (CohomologicalExactMatch ClassId Substitution (SectionCoordinate EGraphAnchor))
    (LoweringGap EGraphAnchor GuardRef)
    EGraphObstructionWitness
    CohomologicalPruningCertificate

type EGraphObstructionCache :: Type
type EGraphObstructionCache =
  CohomologicalCache SaturationPurpose RegionTraversalSummary

type EGraphAnchor :: Type
type EGraphAnchor = Anchor OccurrenceId

type EGraphExactConstraint :: Type
type EGraphExactConstraint = ExactConstraint EGraphAnchor

type EGraphObstructionModality :: Type -> Type
type EGraphObstructionModality value =
  ObstructionModality EGraphAnchor Substitution GuardRef value

anchorForGuardRef :: Map Int EGraphAnchor -> GuardRef -> Maybe EGraphAnchor
anchorForGuardRef representativeAnchors guardRef =
  case guardRefPatternVar guardRef of
    Nothing -> Just RootAnchor
    Just patternVar -> Map.lookup (patternVarKey patternVar) representativeAnchors
