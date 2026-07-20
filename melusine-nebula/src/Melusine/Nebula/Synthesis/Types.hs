{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Synthesis.Types
  ( SynthesizedName (..),
    SynthesizedSite (..),
    SynthesizedDefinition (..),
    CandidateSiteLabel (..),
    candidateSiteLabel,
    RejectedCandidate (..),
    CandidateRejection (..),
    candidateRejectionKey,
    RecordOwnershipFinding (..),
    RecordOwnershipKind (..),
    recordOwnershipKindKey,
    PlanStagingReport (..),
    SynthesisOutcome (..),
    NebulaBatch,
    CandidatePlan (..),
    CandidatePlanSite (..),
    AdmittedArgument (..),
    ArgumentRealization (..),
    CaptureCost (..),
    CaptureExtractionSections,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Map.Lazy qualified as LazyMap
import Melusine.Nebula.Core (NebulaAnalysis)
import Melusine.Nebula.Discovery.Choose (CandidateSite (..), CandidateSiteKind, ChosenBinding)
import Melusine.Nebula.Harvest.Maintain (HarvestAdvanceDecision)
import Melusine.Nebula.Rewrite.Saturate (SaturatedModule)
import Moonlight.Core (Pattern)
import Moonlight.EGraph.Introspection.Core.HsExpr (FreeScopeSummary, HsExprF, ScopeCtx, SourceRegion)
import Moonlight.EGraph.Pure.Context (ContextRebaseBatch)
import Moonlight.EGraph.Pure.Saturation.Extraction (ContextualExtractionObstruction, ContextualExtractionSection)
import Moonlight.EGraph.Pure.Types (ClassId)
import Data.Fix (Fix)
import Moonlight.Pale.Ghc.Expr (ScopeLookupFailure)

type SynthesizedName :: Type
newtype SynthesizedName = SynthesizedName
  { synthesizedNameText :: String
  }
  deriving stock (Eq, Ord, Show)

type SynthesizedSite :: Type
data SynthesizedSite = SynthesizedSite
  { ssBindingName :: !String,
    ssRegion :: !(Maybe SourceRegion),
    ssKind :: !CandidateSiteKind
  }
  deriving stock (Eq, Ord, Show)

type SynthesizedDefinition :: Type
data SynthesizedDefinition = SynthesizedDefinition
  { sdName :: !SynthesizedName,
    sdSites :: ![SynthesizedSite],
    sdClass :: !ClassId,
    sdTerm :: !(Fix HsExprF),
    sdSize :: !Int,
    sdEstimatedWin :: !Int
  }

type CandidateSiteLabel :: Type
data CandidateSiteLabel = CandidateSiteLabel
  { cslBindingName :: !String,
    cslRegion :: !(Maybe SourceRegion),
    cslKind :: !CandidateSiteKind
  }
  deriving stock (Eq, Ord, Show)

candidateSiteLabel :: CandidateSite -> CandidateSiteLabel
candidateSiteLabel site =
  CandidateSiteLabel
    { cslBindingName = csBindingName site,
      cslRegion = csRegion site,
      cslKind = csSiteKind site
    }

type RejectedCandidate :: Type
data RejectedCandidate = RejectedCandidate
  { rejSites :: ![CandidateSiteLabel],
    rejReason :: !CandidateRejection,
    rejEstimatedWin :: !Int,
    rejRealizedWin :: !(Maybe Int)
  }
  deriving stock (Eq, Show)

type CandidateRejection :: Type
data CandidateRejection
  = RejectedNoEstimatedWin
  | RejectedNoDistinctArgs
  | RejectedNotVisible
  | RejectedOverlap
  | RejectedRegionOverlap
  | RejectedScopeEscape
  | RejectedTypeEvidenceInsufficient
  | RejectedEffectOrderUnknown
  | RejectedCaseOrderUnsafe
  | RejectedTreeEditDiagnostic
  | RejectedProjectionVectorDiagnostic
  | RejectedFoldSkeletonDiagnostic
  | RejectedLetRowsProtocolDiagnostic
  | RejectedPatternBindRhsProtocolDiagnostic
  | RejectedKeyedRowAlignmentProtocolDiagnostic
  | RejectedArityChildUnifierProtocolDiagnostic
  | RejectedRecordOwnershipDiagnostic ![RecordOwnershipFinding]
  | RejectedRecordConstructionSkeletonDiagnostic
  | RejectedRedundantPatternClassCanonicalizationDiagnostic
  | RejectedScopedRegionExtractionProtocolDiagnostic
  | RejectedFiniteValidationDiagnostic
  | RejectedThresholdRefinementDiagnostic
  | RejectedEitherValidationDiagnostic
  | RejectedOracleMissing
  | RejectedRealizedRegression
  deriving stock (Eq, Ord, Show)

candidateRejectionKey :: CandidateRejection -> String
candidateRejectionKey = \case
  RejectedNoEstimatedWin ->
    "no-estimated-win"
  RejectedNoDistinctArgs ->
    "no-distinct-args"
  RejectedNotVisible ->
    "not-visible"
  RejectedOverlap ->
    "overlap"
  RejectedRegionOverlap ->
    "region-overlap"
  RejectedScopeEscape ->
    "scope-escape"
  RejectedTypeEvidenceInsufficient ->
    "type-evidence-insufficient"
  RejectedEffectOrderUnknown ->
    "effect-order-unknown"
  RejectedCaseOrderUnsafe ->
    "case-order-unsafe"
  RejectedTreeEditDiagnostic ->
    "tree-edit-diagnostic"
  RejectedProjectionVectorDiagnostic ->
    "projection-vector-diagnostic"
  RejectedFoldSkeletonDiagnostic ->
    "fold-skeleton-diagnostic"
  RejectedLetRowsProtocolDiagnostic ->
    "let-rows-protocol-diagnostic"
  RejectedPatternBindRhsProtocolDiagnostic ->
    "pattern-bind-rhs-protocol-diagnostic"
  RejectedKeyedRowAlignmentProtocolDiagnostic ->
    "keyed-row-alignment-protocol-diagnostic"
  RejectedArityChildUnifierProtocolDiagnostic ->
    "arity-child-unifier-protocol-diagnostic"
  RejectedRecordOwnershipDiagnostic {} ->
    "record-ownership-diagnostic"
  RejectedRecordConstructionSkeletonDiagnostic ->
    "record-construction-skeleton-diagnostic"
  RejectedRedundantPatternClassCanonicalizationDiagnostic ->
    "redundant-pattern-class-canonicalization-diagnostic"
  RejectedScopedRegionExtractionProtocolDiagnostic ->
    "scoped-region-extraction-protocol-diagnostic"
  RejectedFiniteValidationDiagnostic ->
    "finite-validation-diagnostic"
  RejectedThresholdRefinementDiagnostic ->
    "threshold-refinement-diagnostic"
  RejectedEitherValidationDiagnostic ->
    "either-validation-diagnostic"
  RejectedOracleMissing ->
    "oracle-missing"
  RejectedRealizedRegression ->
    "realized-regression"

type RecordOwnershipKind :: Type
data RecordOwnershipKind
  = ProjectionOwnedCachedField
  | StaleDerivedField
  deriving stock (Eq, Ord, Show)

recordOwnershipKindKey :: RecordOwnershipKind -> String
recordOwnershipKindKey = \case
  ProjectionOwnedCachedField ->
    "projection-owned-cached-field"
  StaleDerivedField ->
    "stale-derived-field"

type RecordOwnershipFinding :: Type
data RecordOwnershipFinding = RecordOwnershipFinding
  { rofConstructorName :: !String,
    rofDerivedField :: !String,
    rofProjectionName :: !String,
    rofOwnerField :: !String,
    rofOwnerBinder :: !String,
    rofKind :: !RecordOwnershipKind
  }
  deriving stock (Eq, Ord, Show)

type PlanStagingReport :: Type
data PlanStagingReport = PlanStagingReport
  { psrLocalizedMerges :: !Int,
    psrGlobalFallbackMerges :: !Int,
    psrLocalizedDefinitionMerges :: !Int,
    psrLocalizedApplicationMerges :: !Int,
    psrGlobalDefinitionFallbackMerges :: !Int,
    psrGlobalApplicationFallbackMerges :: !Int,
    psrDirtyContextCount :: !Int
  }
  deriving stock (Eq, Ord, Show)

type SynthesisOutcome :: Type
data SynthesisOutcome = SynthesisOutcome
  { soDefinitions :: ![SynthesizedDefinition],
    soEstimatedWin :: !Int,
    soRealizedWin :: !Int,
    soPreExtractedTotal :: !Int,
    soPostExtractedTotal :: !Int,
    soRejected :: ![RejectedCandidate],
    soStagingReport :: !PlanStagingReport,
    soHarvestDecision :: !(Maybe HarvestAdvanceDecision),
    soBindings :: ![ChosenBinding],
    soSaturatedModule :: !SaturatedModule
  }

type NebulaBatch :: Type
type NebulaBatch = ContextRebaseBatch HsExprF NebulaAnalysis ScopeCtx

type CandidatePlan :: Type -> Type
data CandidatePlan argument = CandidatePlan
  { cpSites :: ![CandidatePlanSite argument],
    cpJoinContext :: !ScopeCtx,
    cpBody :: !(Pattern HsExprF),
    cpSlotByVar :: !(IntMap.IntMap Int),
    cpSlotCount :: !Int,
    cpEstimatedWin :: !Int
  }

type CandidatePlanSite :: Type -> Type
data CandidatePlanSite argument = CandidatePlanSite
  { cpsSite :: !CandidateSite,
    cpsSeed :: !ClassId,
    cpsArguments :: ![argument]
  }

type AdmittedArgument :: Type
data AdmittedArgument = AdmittedArgument
  { aaOriginalClass :: !ClassId,
    aaRealization :: !ArgumentRealization
  }

type ArgumentRealization :: Type
data ArgumentRealization
  = VisibleAtJoin
  | MaterializedAtJoin !(Fix HsExprF)

type CaptureCost :: Type
data CaptureCost = CaptureCost
  { ccEscaping :: !Int,
    ccSize :: !Int,
    ccFreeScopes :: !FreeScopeSummary,
    ccScopeLookupFailure :: !(Maybe ScopeLookupFailure)
  }
  deriving stock (Eq, Ord, Show)

type CaptureExtractionSections :: Type
type CaptureExtractionSections =
  LazyMap.Map
    (ScopeCtx, ScopeCtx)
    (Either (ContextualExtractionObstruction ScopeCtx) (ContextualExtractionSection HsExprF NebulaAnalysis ScopeCtx CaptureCost))
