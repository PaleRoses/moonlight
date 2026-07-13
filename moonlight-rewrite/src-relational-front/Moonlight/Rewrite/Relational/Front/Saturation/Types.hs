{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Rewrite.Relational.Front.Saturation.Types
  ( RewriteSigKind,
    RewriteGuardAtomKind,
    RawMatch,
    MatchVar (..),
    matchVarOrdinal,
    matchVarName,
    matchVarSort,
    RewriteTarget (..),
    MatchQuery (..),
    Match (..),
    matchSubstitution,
    rawMatchSubstitution,
    tagRawMatch,
    Rules (..),
    PreparedCache,
    Engine (..),
    RelationalCompiledRule (..),
    rulesRelationalPlanSet,
    CanonicalRelationalRewritePlan,
    ApplyConfig (..),
    defaultApplyConfig,
    ApplyRejection (..),
    ApplyStatus (..),
    ApplyResult (..),
    SaturationConfig (..),
    defaultSaturationConfig,
    SaturationRound (..),
    SaturationResult (..),
    RelationalSaturationContext (..),
    RelationalSaturationRewritePlan,
    RelationalSaturationRuleIdentity,
    RelationalSaturationCarrier (..),
    RelationalSaturationRule (..),
    RelationalSaturationMatch (..),
    RelationalSaturationSupportedMatch (..),
    RelationalSaturationPendingRound (..),
    RelationalSaturationMatchState (..),
    emptyRelationalSaturationMatchState,
    RelationalSaturationApplicationResult (..),
    RelationalSaturationRebuild (..),
    RewriteScheduleKey,
    RelationalSaturationPlanError (..),
    RelationalSaturationObstruction (..),
  )
where

import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Numeric.Natural
  ( Natural,
  )
import Moonlight.Control.Schedule
  ( SchedulerConfig,
    defaultSchedulerConfig,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleTrace,
  )
import Moonlight.Core
  ( ClassId,
    Pattern,
    PatternVar,
    RewriteRuleId,
  )
import Moonlight.Core qualified as EGraph
import Moonlight.Rewrite.Runtime
  ( ExecutedRewrite,
  )
import Moonlight.Core
  ( Substitution,
    emptySubstitution,
    insertSubst,
  )
import Moonlight.Rewrite.Runtime
  ( RewriteApplicationError,
    RulePlan,
  )
import Moonlight.Rewrite.Runtime
  ( BinderSubstAlgebra,
  )
import Moonlight.Rewrite.DSL
  ( CanonicalProgram,
  )
import Moonlight.Rewrite.DSL
  ( ContextName,
  )
import Moonlight.Rewrite.DSL
  ( GuardCapabilityKey,
  )
import Moonlight.Rewrite.DSL
  ( Node,
    NodeTag,
  )
import Moonlight.Rewrite.DSL
  ( SortName,
  )
import Moonlight.Rewrite.Relational
  ( RelationalPlanSet (..),
    RewritePlan,
  )
import Moonlight.Rewrite.Relational.Front.ApplicationCondition
  ( RelationalApplicationConditionCache,
    RelationalApplicationConditionPlans,
  )
import Moonlight.Rewrite.Relational.Front.Host
  ( Host,
  )
import Moonlight.Rewrite.Relational.Front.Saturation.Error
  ( RelationalSaturationContext (..),
    RelationalSaturationObstruction (..),
    RelationalSaturationPlanError (..),
  )
import Moonlight.Rewrite.Relational
  ( RewriteRunStats,
    emptyRewriteRunStats,
  )
import Moonlight.Rewrite.Relational
  ( RelationalRewriteMatch (..),
  )
import Moonlight.Rewrite.Relational
  ( RelationalPreparedSystem,
    RewriteRunConfig,
    defaultRewriteRunConfig,
  )
import Moonlight.Rewrite.System
  ( RuleName,
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
  )
import Moonlight.Rewrite.ProofContext
  ( ProofRegistry,
    ProofRetention (..),
  )
import GHC.TypeLits
  ( Symbol,
  )
import Moonlight.FiniteLattice
  ( ContextLattice
  )
import Moonlight.FiniteLattice
  ( SupportBasis
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite
  )


type RewriteSigKind :: Type
type RewriteSigKind = Symbol -> (Symbol -> Type) -> Type

type RewriteGuardAtomKind :: Type
type RewriteGuardAtomKind = RewriteSigKind -> Type

type RawMatch :: Type
type RawMatch =
  RelationalRewriteMatch MatchVar ClassId

data MatchVar = MatchVar !Int !String !(Maybe SortName)
  deriving stock (Show)

instance Eq MatchVar where
  left == right =
    matchVarOrdinal left == matchVarOrdinal right

instance Ord MatchVar where
  compare left right =
    compare (matchVarOrdinal left) (matchVarOrdinal right)

matchVarOrdinal :: MatchVar -> Int
matchVarOrdinal (MatchVar ordinal _ _) =
  ordinal

matchVarName :: MatchVar -> String
matchVarName (MatchVar _ name _) =
  name

matchVarSort :: MatchVar -> Maybe SortName
matchVarSort (MatchVar _ _ sortName) =
  sortName

type RewriteTarget :: Type
data RewriteTarget
  = RewriteBase
  | RewriteContext !ContextName
  deriving stock (Eq, Ord, Show)

type MatchQuery :: Type
data MatchQuery = MatchQuery
  { matchQueryTarget :: !RewriteTarget,
    matchQueryRule :: !RuleName,
    matchQueryRoot :: !(Maybe ClassId)
  }
  deriving stock (Eq, Ord, Show)

type Match :: Type
data Match = Match
  { matchTarget :: !RewriteTarget,
    matchRule :: !RuleName,
    matchRoot :: !ClassId,
    matchBindings :: !(Map MatchVar ClassId),
    matchRevision :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show)

matchSubstitution :: Match -> Substitution
matchSubstitution =
  substitutionFromBindings . matchBindings

rawMatchSubstitution :: RawMatch -> Substitution
rawMatchSubstitution =
  substitutionFromBindings . rrmBindings

tagRawMatch :: RewriteTarget -> RuleName -> Int -> RawMatch -> Match
tagRawMatch target ruleNameValue revision rawMatch =
  Match
    { matchTarget = target,
      matchRule = ruleNameValue,
      matchRoot = rrmRoot rawMatch,
      matchBindings = rrmBindings rawMatch,
      matchRevision = revision
    }

substitutionFromBindings :: Map MatchVar ClassId -> Substitution
substitutionFromBindings =
  Map.foldlWithKey'
    ( \substitution matchVarValue classId ->
        insertSubst
          (EGraph.mkPatternVar (matchVarOrdinal matchVarValue))
          classId
          substitution
    )
    emptySubstitution

type PreparedCache :: RewriteSigKind -> RewriteGuardAtomKind -> Type
type PreparedCache sig atom =
  RelationalPreparedSystem
    ContextName
    ()
    (RulePlan (CompiledGuard (GuardCapabilityKey atom) (Node sig)) (Node sig))
    MatchVar
    ClassId
    (CompiledGuard (GuardCapabilityKey atom) (Node sig))
    (NodeTag sig)
    (Node sig ClassId)

type Rules :: RewriteSigKind -> RewriteGuardAtomKind -> Type
data Rules sig atom = Rules
  { rulesCanonicalProgram :: !(CanonicalProgram sig atom),
    rulesRelationalRules :: !(Map RuleName (RelationalCompiledRule sig atom))
  }

type Engine :: RewriteSigKind -> RewriteGuardAtomKind -> Type
data Engine sig atom = Engine
  { engRules :: !(Rules sig atom),
    engHost :: !(Host sig),
    engPrepared :: !(PreparedCache sig atom),
    engContexts :: !(Map ContextName (Host sig)),
    engApplicationConditionCaches :: !(Map RewriteTarget (RelationalApplicationConditionCache (GuardCapabilityKey atom) sig))
  }

type CanonicalRelationalRewritePlan :: RewriteSigKind -> RewriteGuardAtomKind -> Type
type CanonicalRelationalRewritePlan sig atom =
  RewritePlan
    (RulePlan (CompiledGuard (GuardCapabilityKey atom) (Node sig)) (Node sig))
    MatchVar
    ClassId
    (CompiledGuard (GuardCapabilityKey atom) (Node sig))
    (NodeTag sig)
    (Node sig ClassId)

type RelationalCompiledRule :: RewriteSigKind -> RewriteGuardAtomKind -> Type
data RelationalCompiledRule sig atom = RelationalCompiledRule
  { rcrRulePlan :: !(RulePlan (CompiledGuard (GuardCapabilityKey atom) (Node sig)) (Node sig)),
    rcrMatchPlan :: !(CanonicalRelationalRewritePlan sig atom),
    rcrApplicationConditionPlans :: !(RelationalApplicationConditionPlans (GuardCapabilityKey atom) sig)
  }

rulesRelationalPlanSet ::
  Rules sig atom ->
  RelationalPlanSet
    (RulePlan (CompiledGuard (GuardCapabilityKey atom) (Node sig)) (Node sig))
    MatchVar
    ClassId
    (CompiledGuard (GuardCapabilityKey atom) (Node sig))
    (NodeTag sig)
    (Node sig ClassId)
rulesRelationalPlanSet rulesValue =
  RelationalPlanSet
    (Map.map rcrMatchPlan (rulesRelationalRules rulesValue))

type ApplyConfig :: RewriteSigKind -> Type
data ApplyConfig sig = ApplyConfig
  { acResolveBindingPattern ::
      !(Maybe (PatternVar -> Either RewriteApplicationError (Pattern (Node sig)))),
    acBinderSubstAlgebra :: !(Maybe (BinderSubstAlgebra (Node sig)))
  }

defaultApplyConfig :: ApplyConfig sig
defaultApplyConfig =
  ApplyConfig
    { acResolveBindingPattern = Nothing,
      acBinderSubstAlgebra = Nothing
    }

type ApplyStatus :: Type
data ApplyRejection
  = RejectedApplicationCondition
  | RejectedStaleMatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show)

data ApplyStatus
  = ApplyRejected !ApplyRejection
  | ApplyExecuted !ExecutedRewrite !Bool
  deriving stock (Eq, Ord, Show)

type ApplyResult :: Type
data ApplyResult = ApplyResult
  { applyResultTarget :: !RewriteTarget,
    applyResultRule :: !RuleName,
    applyResultRoot :: !ClassId,
    applyResultStatus :: !ApplyStatus
  }
  deriving stock (Eq, Ord, Show)

type SaturationConfig :: RewriteSigKind -> Type
data SaturationConfig sig = SaturationConfig
  { scRunConfig :: !(RewriteRunConfig ContextName ()),
    scResolveBindingPattern ::
      !(Maybe (PatternVar -> Either RewriteApplicationError (Pattern (Node sig)))),
    scBinderSubstAlgebra :: !(Maybe (BinderSubstAlgebra (Node sig))),
    scSchedulerConfig :: !(SchedulerConfig RewriteRuleId),
    scHostNodeLimit :: !(Maybe Natural),
    scProofRetention :: !ProofRetention
  }

-- | 'scProofRetention' defaults to 'KeepNoProof': saturation is
-- throughput-bound and retaining proofs costs memory per applied rewrite.
-- This deliberately diverges from @defaultProofRetention@ ('KeepFullProof')
-- in the standalone proof-recording surface, where proofs are the point.
defaultSaturationConfig :: SaturationConfig sig
defaultSaturationConfig =
  SaturationConfig
    { scRunConfig = defaultRewriteRunConfig,
      scResolveBindingPattern = Nothing,
      scBinderSubstAlgebra = Nothing,
      scSchedulerConfig = defaultSchedulerConfig,
      scHostNodeLimit = Just 5000000,
      scProofRetention = KeepNoProof
    }

data SaturationRound = SaturationRound
  { saturationRoundIndex :: {-# UNPACK #-} !Int,
    saturationRoundMatches :: {-# UNPACK #-} !Int,
    saturationRoundExecuted :: ![ExecutedRewrite],
    saturationRoundStats :: !RewriteRunStats
  }
  deriving stock (Eq, Show)

type SaturationResult :: RewriteSigKind -> Type
data SaturationResult sig = SaturationResult
  { saturationHost :: !(Host sig),
    saturationProofs :: !(ProofRegistry (Node sig) ContextName ()),
    saturationRounds :: ![SaturationRound],
    saturationSchedulerTrace :: ![ScheduleTrace RewriteRuleId],
    saturationStats :: !RewriteRunStats
  }

type RelationalSaturationRewritePlan :: RewriteSigKind -> RewriteGuardAtomKind -> Type
type RelationalSaturationRewritePlan sig atom = CanonicalRelationalRewritePlan sig atom

type RelationalSaturationRuleIdentity :: RewriteSigKind -> RewriteGuardAtomKind -> Type
type RelationalSaturationRuleIdentity sig atom =
  RulePlan (CompiledGuard (GuardCapabilityKey atom) (Node sig)) (Node sig)

type RewriteScheduleKey = (RewriteRuleId, ClassId, Substitution)

type RelationalSaturationCarrier :: RewriteSigKind -> RewriteGuardAtomKind -> Type
data RelationalSaturationCarrier sig atom = RelationalSaturationCarrier
  { rscBaseHost :: !(Host sig),
    rscLiveHost :: !(Host sig),
    rscActiveContext :: !RelationalSaturationContext,
    rscContextLattice :: !(ContextLattice RelationalSaturationContext),
    rscPreparedSite :: !(PreparedContextSite RelationalSaturationContext),
    rscProofs :: !(ProofRegistry (Node sig) ContextName ()),
    rscApplicationConditionCache :: !(RelationalApplicationConditionCache (GuardCapabilityKey atom) sig),
    rscBannedScheduleKeys :: !(Set RewriteScheduleKey)
  }

type RelationalSaturationRule :: RewriteSigKind -> RewriteGuardAtomKind -> Type
data RelationalSaturationRule sig atom = RelationalSaturationRule
  { rsrRuleName :: !RuleName,
    rsrRuleId :: !RewriteRuleId,
    rsrRulePlan :: !(RulePlan (CompiledGuard (GuardCapabilityKey atom) (Node sig)) (Node sig)),
    rsrPlan :: !(RelationalSaturationRewritePlan sig atom),
    rsrApplicationConditionPlans :: !(RelationalApplicationConditionPlans (GuardCapabilityKey atom) sig)
  }

type RelationalSaturationMatch :: RewriteSigKind -> RewriteGuardAtomKind -> Type
data RelationalSaturationMatch sig atom = RelationalSaturationMatch
  { rsmRule :: !(RelationalSaturationRule sig atom),
    rsmMatch :: !RawMatch
  }

type RelationalSaturationSupportedMatch :: RewriteSigKind -> RewriteGuardAtomKind -> Type
data RelationalSaturationSupportedMatch sig atom = RelationalSaturationSupportedMatch
  { rssmMatch :: !(RelationalSaturationMatch sig atom),
    rssmSupport :: !(SupportBasis RelationalSaturationContext),
    rssmWitnesses :: !(Map RelationalSaturationContext ())
  }

data RelationalSaturationPendingRound = RelationalSaturationPendingRound
  { rspRoundIndex :: {-# UNPACK #-} !Int,
    rspMatchedCount :: {-# UNPACK #-} !Int,
    rspMatchStats :: !RewriteRunStats
  }

type RelationalSaturationMatchState :: RewriteSigKind -> Type -> Type
data RelationalSaturationMatchState sig projection = RelationalSaturationMatchState
  { rsmsPendingRound :: !(Maybe RelationalSaturationPendingRound),
    rsmsRounds :: ![SaturationRound],
    rsmsStats :: !RewriteRunStats
  }

emptyRelationalSaturationMatchState :: RelationalSaturationMatchState sig projection
emptyRelationalSaturationMatchState =
  RelationalSaturationMatchState
    { rsmsPendingRound = Nothing,
      rsmsRounds = [],
      rsmsStats = emptyRewriteRunStats
    }

data RelationalSaturationApplicationResult = RelationalSaturationApplicationResult
  { rsarExecuted :: ![ExecutedRewrite],
    rsarStats :: !RewriteRunStats
  }

data RelationalSaturationRebuild = RelationalSaturationRebuild
  { rsrEpoch :: {-# UNPACK #-} !Int
  }
