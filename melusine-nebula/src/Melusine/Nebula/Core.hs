{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Core
  ( NebulaUniverse,
    NebulaRule,
    NebulaRuleBook,
    NebulaFactBook,
    NodeCountAnalysis (..),
    TypeEvidence,
    typeObservations,
    typeEvidenceObservations,
    NebulaAnalysis (..),
    nebulaAnalysisSpec,
    NebulaCostModel (..),
    CorpusSources (..),
    NebulaConfig (..),
    defaultNebulaConfig,
    ModuleWorkload (..),
    workloadOracle,
    NebulaError (..),
    nebulaErrorKey,
    nebulaErrorMessage,
    nebulaErrorPath,
  )
where

import Data.Kind (Type)
import Data.Set qualified as Set
import Moonlight.Algebra (JoinSemilattice (..))
import Moonlight.Core (UnionFindAllocationError)
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec, semilatticeAnalysis)
import Moonlight.EGraph.Pure.Extraction (ExtractionWorkBudget (..))
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.Rewrite.System (RewriteCondition)
import Moonlight.Rewrite.System (FactRule)
import Moonlight.Rewrite.System (SemanticFidelity (..), TrustTier (..))
import Moonlight.Rewrite.System (RawRewriteRule)
import Moonlight.Saturation.Core (SaturationBudget (..))
import Moonlight.Sheaf.Twist.SupportedRuleSpec (SupportedFactBook, SupportedRuleBook)
import Moonlight.Sheaf.Context.Site (PreparedContextSupportError)
import Moonlight.EGraph.Introspection.Core.HsExpr.FreeScope
  ( FreeScopeWitness,
    HasFreeScopeWitness (..),
    hsExprFreeScopeWitness,
  )
import Moonlight.Pale.Ghc.Expr (HsExprF, ScopeCtx, ScopeIndex)
import Moonlight.Pale.Ghc.Hie.Oracle (ModuleNameOracle)
import Moonlight.Pale.Ghc.Hie.SourceKey (OracleLookup, oracleLookupOracle)
import Moonlight.Flow.Model.Schema.Digest (StableDigest128)

type NodeCountAnalysis :: Type
newtype NodeCountAnalysis = NodeCountAnalysis Int
  deriving stock (Eq, Ord, Show)

nodeCountAnalysisTop :: Int
nodeCountAnalysisTop =
  512

nodeCountAnalysis :: Int -> NodeCountAnalysis
nodeCountAnalysis countValue =
  NodeCountAnalysis (min nodeCountAnalysisTop (max 0 countValue))

instance JoinSemilattice NodeCountAnalysis where
  join (NodeCountAnalysis leftCount) (NodeCountAnalysis rightCount) =
    NodeCountAnalysis (max leftCount rightCount)

type TypeEvidence :: Type
data TypeEvidence
  = NoTypeEvidence
  | TypeObservations !(Set.Set StableDigest128)
  deriving stock (Eq, Ord, Show)

typeObservations :: Set.Set StableDigest128 -> TypeEvidence
typeObservations observations
  | Set.null observations =
      NoTypeEvidence
  | otherwise =
      TypeObservations observations

typeEvidenceObservations :: TypeEvidence -> Set.Set StableDigest128
typeEvidenceObservations = \case
  NoTypeEvidence ->
    Set.empty
  TypeObservations observations ->
    observations

instance JoinSemilattice TypeEvidence where
  join leftEvidence rightEvidence =
    typeObservations (typeEvidenceObservations leftEvidence <> typeEvidenceObservations rightEvidence)

type NebulaAnalysis :: Type
data NebulaAnalysis = NebulaAnalysis
  { naCount :: !NodeCountAnalysis,
    naType :: !TypeEvidence,
    naScope :: !FreeScopeWitness
  }
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice NebulaAnalysis where
  join leftAnalysis rightAnalysis =
    NebulaAnalysis
      { naCount = join (naCount leftAnalysis) (naCount rightAnalysis),
        naType = join (naType leftAnalysis) (naType rightAnalysis),
        naScope = join (naScope leftAnalysis) (naScope rightAnalysis)
      }

instance HasFreeScopeWitness NebulaAnalysis where
  freeScopeWitness = naScope

nebulaAnalysisSpec :: ScopeIndex -> AnalysisSpec HsExprF NebulaAnalysis
nebulaAnalysisSpec scopeIndex =
  semilatticeAnalysis
    ( \nodeValue ->
        NebulaAnalysis
          { naCount =
              nodeCountAnalysis
                (1 + foldr (\childAnalysis total -> nodeCountValue (naCount childAnalysis) + total) 0 nodeValue),
            naType = NoTypeEvidence,
            naScope = hsExprFreeScopeWitness scopeIndex (fmap naScope nodeValue)
          }
    )

nodeCountValue :: NodeCountAnalysis -> Int
nodeCountValue (NodeCountAnalysis countValue) =
  countValue

type NebulaUniverse :: Type
type NebulaUniverse = EGraphU ScopeCtx HsExprF NebulaAnalysis ScopeCtx

type NebulaRule :: Type
type NebulaRule = RawRewriteRule (RewriteCondition ScopeCtx HsExprF) HsExprF

type NebulaRuleBook :: Type
type NebulaRuleBook = SupportedRuleBook ScopeCtx NebulaRule

type NebulaFactBook :: Type
type NebulaFactBook = SupportedFactBook ScopeCtx (FactRule ScopeCtx HsExprF)

type NebulaCostModel :: Type
data NebulaCostModel
  = SizeCost
  | DepthCost
  deriving stock (Eq, Ord, Show)

type CorpusSources :: Type
data CorpusSources
  = SiteFamilyOnly
  | SiteAndBindingFront
  deriving stock (Eq, Ord, Show)

type NebulaConfig :: Type
data NebulaConfig = NebulaConfig
  { ncSaturationBudget :: !SaturationBudget,
    ncExtractionBudget :: !ExtractionWorkBudget,
    ncCostModel :: !NebulaCostModel,
    ncCorpusSources :: !CorpusSources,
    ncAdmissibleTiers :: !(Set.Set TrustTier),
    ncAdmissibleFidelities :: !(Set.Set SemanticFidelity),
    ncAntiUnifyMaxPairs :: !Int,
    ncDiagnosticMinShared :: !Int,
    ncSynthesisRounds :: !Int,
    ncIncrementalFallbackRatio :: !Double
  }

defaultNebulaConfig :: NebulaConfig
defaultNebulaConfig =
  NebulaConfig
    { ncSaturationBudget =
        SaturationBudget
          { sbMaxIterations = 8,
            sbMaxNodes = 2000
          },
      ncExtractionBudget = ExtractionWorkBudget 4194304,
      ncCostModel = SizeCost,
      ncCorpusSources = SiteAndBindingFront,
      ncAdmissibleTiers = Set.fromList [ParserVerified, GhcVerified, RegistryTrusted, MachineProved, ModuleDerived],
      ncAdmissibleFidelities = Set.fromList [Observational, UpToBottom],
      ncAntiUnifyMaxPairs = 64,
      ncDiagnosticMinShared = 1,
      ncSynthesisRounds = 1,
      ncIncrementalFallbackRatio = 1.0
    }

type ModuleWorkload :: Type
data ModuleWorkload = ModuleWorkload
  { mwPath :: !FilePath,
    mwSource :: !String,
    mwOracleLookup :: !OracleLookup
  }
  deriving stock (Eq, Show)

workloadOracle :: ModuleWorkload -> Maybe ModuleNameOracle
workloadOracle =
  oracleLookupOracle . mwOracleLookup

type NebulaError :: Type
data NebulaError
  = NebulaWorkspaceError !FilePath !String
  | NebulaParseError !String
  | NebulaLatticeError !String
  | NebulaInsertionError !String
  | NebulaRuleDerivationError !String
  | NebulaBindingFrontError !String
  | NebulaSaturationError !String
  | NebulaProofReplayAllocationError !UnionFindAllocationError
  | NebulaContextSupportError !(PreparedContextSupportError ScopeCtx)
  | NebulaExtractionError !String !String
  | NebulaSynthesisError !String
  | NebulaArityMismatch !Int !Int !Int
  | NebulaWriteBackError !String
  | NebulaSpliceError !String
  | NebulaSealError !String !String
  deriving stock (Eq, Show)

nebulaErrorKey :: NebulaError -> String
nebulaErrorKey = \case
  NebulaWorkspaceError {} ->
    "workspace-error"
  NebulaParseError {} ->
    "parse-error"
  NebulaLatticeError {} ->
    "lattice-error"
  NebulaInsertionError {} ->
    "insertion-error"
  NebulaRuleDerivationError {} ->
    "rule-derivation-error"
  NebulaBindingFrontError {} ->
    "binding-front-error"
  NebulaSaturationError {} ->
    "saturation-error"
  NebulaProofReplayAllocationError {} ->
    "proof-replay-allocation-error"
  NebulaContextSupportError {} ->
    "context-support-error"
  NebulaExtractionError {} ->
    "extraction-error"
  NebulaSynthesisError {} ->
    "synthesis-error"
  NebulaArityMismatch {} ->
    "arity-mismatch"
  NebulaWriteBackError {} ->
    "write-back-error"
  NebulaSpliceError {} ->
    "splice-error"
  NebulaSealError {} ->
    "seal-error"

nebulaErrorMessage :: NebulaError -> Maybe String
nebulaErrorMessage = \case
  NebulaWorkspaceError _ message ->
    Just message
  NebulaParseError message ->
    Just message
  NebulaLatticeError message ->
    Just message
  NebulaInsertionError message ->
    Just message
  NebulaRuleDerivationError message ->
    Just message
  NebulaBindingFrontError message ->
    Just message
  NebulaSaturationError message ->
    Just message
  NebulaProofReplayAllocationError allocationError ->
    Just (show allocationError)
  NebulaContextSupportError supportError ->
    Just (show supportError)
  NebulaExtractionError _ message ->
    Just message
  NebulaSynthesisError message ->
    Just message
  NebulaArityMismatch {} ->
    Nothing
  NebulaWriteBackError message ->
    Just message
  NebulaSpliceError message ->
    Just message
  NebulaSealError _ message ->
    Just message

nebulaErrorPath :: NebulaError -> Maybe FilePath
nebulaErrorPath = \case
  NebulaWorkspaceError path _ ->
    Just path
  _ ->
    Nothing
