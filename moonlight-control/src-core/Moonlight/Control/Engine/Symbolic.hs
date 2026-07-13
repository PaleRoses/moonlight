{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The symbolic frontend of the engine: name phases, rules, and supports at
-- the type level, write programs with @OverloadedLabels@, and compile the
-- symbolic vocabulary into a 'Plan' through a 'ControlCatalog'.
module Moonlight.Control.Engine.Symbolic
  ( Domain (..),
    PhaseRef,
    phaseRef,
    phaseRefKey,
    SomePhaseRef (..),
    somePhaseRefKey,
    KnownPhase (..),
    RuleRef,
    ruleRef,
    ruleRefKey,
    KnownRule (..),
    SupportRef,
    supportRef,
    supportRefKey,
    KnownSupport (..),
    SymbolicProgram,
    compileSymbolicProgram,
    PriorityTarget (..),
    rulePriorityTarget,
    supportPriorityTarget,
    prioritizeTarget,
    prioritizeRule,
    prioritizeSupport,
    compilePriorityTargets,
    ControlCatalog (..),
    ControlCatalogProjectionFailure (..),
    basicControlCatalog,
    compileControlCatalogPriorityTargets,
    controlCatalogProjectionFailures,
    controlCatalogRuleProjectionFailures,
    controlCatalogSupportProjectionFailures,
    compileSymbolicPlan,
  )
where

import Data.Foldable qualified as Foldable
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Void (Void)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (Symbol)

import Moonlight.Control.Engine.Evidence
  ( EvidencePolicy,
  )
import Moonlight.Control.Engine.Plan
  ( PhaseDecl,
    Plan,
  )
import Moonlight.Control.Engine.Report
  ( Observation,
  )
import Moonlight.Control.Engine.Spec
  ( EngineSpec,
    Validated,
    compilePlanWithControl,
    compileSchedulerConfig,
  )
import Moonlight.Control.Modality
  ( Modality,
  )
import Moonlight.Control.Program
  ( Program,
  )
import Moonlight.Control.Program.Internal
  ( Program (Phase),
  )
import Moonlight.Control.Schedule
  ( SchedulerConfig,
    mergePriorityProfile,
  )
import Moonlight.Control.Weight
  ( PriorityEvidence,
    PriorityProfile,
    expandPriorityProfileKeys,
    singletonPriorityProfile,
  )

-- | A domain names its phase, rule, and support key types.
type Domain :: Type -> Constraint
class Domain domain where
  type PhaseKey domain :: Type
  type RuleKey domain :: Type
  type SupportKey domain :: Type
  type SupportKey domain = Void

type KnownPhase :: Type -> Symbol -> Constraint
class Domain domain => KnownPhase domain name where
  knownPhaseKey :: PhaseKey domain

type KnownRule :: Type -> Symbol -> Constraint
class Domain domain => KnownRule domain name where
  knownRuleKey :: RuleKey domain

type KnownSupport :: Type -> Symbol -> Constraint
class Domain domain => KnownSupport domain name where
  knownSupportKey :: SupportKey domain

type PhaseRef :: Type -> Symbol -> Type
newtype PhaseRef domain name = PhaseRef
  { phaseRefKey :: PhaseKey domain
  }

deriving stock instance Eq (PhaseKey domain) => Eq (PhaseRef domain name)

deriving stock instance Ord (PhaseKey domain) => Ord (PhaseRef domain name)

deriving stock instance Show (PhaseKey domain) => Show (PhaseRef domain name)

type RuleRef :: Type -> Symbol -> Type
newtype RuleRef domain name = RuleRef
  { ruleRefKey :: RuleKey domain
  }

deriving stock instance Eq (RuleKey domain) => Eq (RuleRef domain name)

deriving stock instance Ord (RuleKey domain) => Ord (RuleRef domain name)

deriving stock instance Show (RuleKey domain) => Show (RuleRef domain name)

type SupportRef :: Type -> Symbol -> Type
newtype SupportRef domain name = SupportRef
  { supportRefKey :: SupportKey domain
  }

deriving stock instance Eq (SupportKey domain) => Eq (SupportRef domain name)

deriving stock instance Ord (SupportKey domain) => Ord (SupportRef domain name)

deriving stock instance Show (SupportKey domain) => Show (SupportRef domain name)

type SomePhaseRef :: Type -> Type
data SomePhaseRef domain where
  SomePhaseRef :: PhaseRef domain name -> SomePhaseRef domain

instance Eq (PhaseKey domain) => Eq (SomePhaseRef domain) where
  leftRef == rightRef =
    somePhaseRefKey leftRef == somePhaseRefKey rightRef

instance Ord (PhaseKey domain) => Ord (SomePhaseRef domain) where
  compare leftRef rightRef =
    compare (somePhaseRefKey leftRef) (somePhaseRefKey rightRef)

instance Show (PhaseKey domain) => Show (SomePhaseRef domain) where
  show =
    show . somePhaseRefKey

somePhaseRefKey :: SomePhaseRef domain -> PhaseKey domain
somePhaseRefKey (SomePhaseRef ref) =
  phaseRefKey ref

phaseRef :: forall domain name. KnownPhase domain name => PhaseRef domain name
phaseRef =
  PhaseRef (knownPhaseKey @domain @name)

ruleRef :: forall domain name. KnownRule domain name => RuleRef domain name
ruleRef =
  RuleRef (knownRuleKey @domain @name)

supportRef :: forall domain name. KnownSupport domain name => SupportRef domain name
supportRef =
  SupportRef (knownSupportKey @domain @name)

instance KnownPhase domain name => IsLabel name (PhaseRef domain name) where
  fromLabel =
    phaseRef @domain @name

instance KnownRule domain name => IsLabel name (RuleRef domain name) where
  fromLabel =
    ruleRef @domain @name

instance KnownSupport domain name => IsLabel name (SupportRef domain name) where
  fromLabel =
    supportRef @domain @name

instance KnownPhase domain name => IsLabel name (SomePhaseRef domain) where
  fromLabel =
    SomePhaseRef (phaseRef @domain @name)

-- | A control program over symbolic phase references.
type SymbolicProgram :: Type -> Type -> Type
type SymbolicProgram ctx domain = Program ctx (SomePhaseRef domain)

instance KnownPhase domain name => IsLabel name (Program ctx (SomePhaseRef domain)) where
  fromLabel =
    Phase (SomePhaseRef (phaseRef @domain @name))

-- | Resolve every symbolic phase reference through the domain's phase
-- compiler. O(n).
compileSymbolicProgram ::
  (PhaseKey domain -> p) ->
  SymbolicProgram ctx domain ->
  Program ctx p
compileSymbolicProgram compilePhase =
  fmap (compilePhase . somePhaseRefKey)

type PriorityTarget :: Type -> Type
data PriorityTarget domain
  = PriorityRule !(RuleKey domain)
  | PrioritySupport !(RuleKey domain) !(SupportKey domain)

deriving stock instance
  (Eq (RuleKey domain), Eq (SupportKey domain)) =>
  Eq (PriorityTarget domain)

deriving stock instance
  (Ord (RuleKey domain), Ord (SupportKey domain)) =>
  Ord (PriorityTarget domain)

deriving stock instance
  (Show (RuleKey domain), Show (SupportKey domain)) =>
  Show (PriorityTarget domain)

rulePriorityTarget ::
  RuleRef domain name ->
  PriorityTarget domain
rulePriorityTarget =
  PriorityRule . ruleRefKey

supportPriorityTarget ::
  RuleRef domain ruleName ->
  SupportRef domain supportName ->
  PriorityTarget domain
supportPriorityTarget rule support =
  PrioritySupport
    (ruleRefKey rule)
    (supportRefKey support)

prioritizeTarget ::
  PriorityTarget domain ->
  PriorityEvidence ->
  PriorityProfile (PriorityTarget domain)
prioritizeTarget =
  singletonPriorityProfile

prioritizeRule ::
  RuleRef domain name ->
  PriorityEvidence ->
  PriorityProfile (PriorityTarget domain)
prioritizeRule =
  prioritizeTarget . rulePriorityTarget

prioritizeSupport ::
  RuleRef domain ruleName ->
  SupportRef domain supportName ->
  PriorityEvidence ->
  PriorityProfile (PriorityTarget domain)
prioritizeSupport rule support =
  prioritizeTarget (supportPriorityTarget rule support)

-- | Expand symbolic priority targets into concrete scheduler groups. O(k·g)
-- for @k@ targets expanding to @g@ groups each.
compilePriorityTargets ::
  Ord group =>
  (RuleKey domain -> NonEmpty group) ->
  (RuleKey domain -> SupportKey domain -> NonEmpty group) ->
  PriorityProfile (PriorityTarget domain) ->
  PriorityProfile group
compilePriorityTargets ruleGroups supportGroups =
  expandPriorityProfileKeys compileTarget
  where
    compileTarget target =
      case target of
        PriorityRule ruleKey ->
          ruleGroups ruleKey
        PrioritySupport ruleKey supportKey ->
          supportGroups ruleKey supportKey

-- | How a domain's symbolic vocabulary projects into engine vocabulary.
type ControlCatalog :: Type -> Type -> Type -> Type -> Type
data ControlCatalog domain group traceEntry evidence = ControlCatalog
  { ccPhaseDecl ::
      !(PhaseKey domain -> PhaseDecl),
    ccRuleGroups ::
      !(RuleKey domain -> NonEmpty group),
    ccSupportGroups ::
      !(RuleKey domain -> SupportKey domain -> NonEmpty group),
    ccGroupRuleKey ::
      !(group -> RuleKey domain),
    ccSchedulerConfig ::
      !(EngineSpec Validated -> SchedulerConfig group),
    ccEvidencePolicies ::
      !(EngineSpec Validated -> [EvidencePolicy (Observation group traceEntry evidence) group])
  }

type ControlCatalogProjectionFailure :: Type -> Type -> Type
data ControlCatalogProjectionFailure domain group
  = RuleGroupProjectionMismatch
      !(RuleKey domain)
      !group
      !(RuleKey domain)
  | SupportGroupProjectionMismatch
      !(RuleKey domain)
      !(SupportKey domain)
      !group
      !(RuleKey domain)

deriving stock instance
  ( Eq (RuleKey domain),
    Eq (SupportKey domain),
    Eq group
  ) =>
  Eq (ControlCatalogProjectionFailure domain group)

deriving stock instance
  ( Ord (RuleKey domain),
    Ord (SupportKey domain),
    Ord group
  ) =>
  Ord (ControlCatalogProjectionFailure domain group)

deriving stock instance
  ( Show (RuleKey domain),
    Show (SupportKey domain),
    Show group
  ) =>
  Show (ControlCatalogProjectionFailure domain group)

basicControlCatalog ::
  (PhaseKey domain -> PhaseDecl) ->
  (RuleKey domain -> NonEmpty group) ->
  (RuleKey domain -> SupportKey domain -> NonEmpty group) ->
  (group -> RuleKey domain) ->
  ControlCatalog domain group traceEntry evidence
basicControlCatalog phaseDeclOf ruleGroups supportGroups groupRuleKey =
  ControlCatalog
    { ccPhaseDecl = phaseDeclOf,
      ccRuleGroups = ruleGroups,
      ccSupportGroups = supportGroups,
      ccGroupRuleKey = groupRuleKey,
      ccSchedulerConfig = compileSchedulerConfig,
      ccEvidencePolicies = const []
    }

compileControlCatalogPriorityTargets ::
  Ord group =>
  ControlCatalog domain group traceEntry evidence ->
  PriorityProfile (PriorityTarget domain) ->
  PriorityProfile group
compileControlCatalogPriorityTargets catalog =
  compilePriorityTargets
    (ccRuleGroups catalog)
    (ccSupportGroups catalog)

controlCatalogProjectionFailures ::
  Eq (RuleKey domain) =>
  ControlCatalog domain group traceEntry evidence ->
  [RuleKey domain] ->
  [(RuleKey domain, SupportKey domain)] ->
  [ControlCatalogProjectionFailure domain group]
controlCatalogProjectionFailures catalog ruleKeys supportKeys =
  foldMap
    (controlCatalogRuleProjectionFailures catalog)
    ruleKeys
    <> foldMap
      (\(ruleKey, supportKey) -> controlCatalogSupportProjectionFailures catalog ruleKey supportKey)
      supportKeys

controlCatalogRuleProjectionFailures ::
  Eq (RuleKey domain) =>
  ControlCatalog domain group traceEntry evidence ->
  RuleKey domain ->
  [ControlCatalogProjectionFailure domain group]
controlCatalogRuleProjectionFailures catalog ruleKey =
  projectionFailures
    (ccGroupRuleKey catalog)
    (RuleGroupProjectionMismatch ruleKey)
    ruleKey
    (ccRuleGroups catalog ruleKey)

controlCatalogSupportProjectionFailures ::
  Eq (RuleKey domain) =>
  ControlCatalog domain group traceEntry evidence ->
  RuleKey domain ->
  SupportKey domain ->
  [ControlCatalogProjectionFailure domain group]
controlCatalogSupportProjectionFailures catalog ruleKey supportKey =
  projectionFailures
    (ccGroupRuleKey catalog)
    (SupportGroupProjectionMismatch ruleKey supportKey)
    ruleKey
    (ccSupportGroups catalog ruleKey supportKey)

projectionFailures ::
  Eq ruleKey =>
  (group -> ruleKey) ->
  (group -> ruleKey -> failure) ->
  ruleKey ->
  NonEmpty group ->
  [failure]
projectionFailures groupRuleKey buildFailure expectedRuleKey =
  Foldable.foldMap inspectGroup
  where
    inspectGroup group =
      let !actualRuleKey =
            groupRuleKey group
       in [buildFailure group actualRuleKey | actualRuleKey /= expectedRuleKey]

-- | Compile a symbolic program and its priorities into a 'Plan'.
compileSymbolicPlan ::
  Ord group =>
  ControlCatalog domain group traceEntry evidence ->
  EngineSpec Validated ->
  SymbolicProgram (Modality view group match traceEntry group) domain ->
  PriorityProfile (PriorityTarget domain) ->
  Plan view group match traceEntry evidence
compileSymbolicPlan catalog spec program priorities =
  compilePlanWithControl
    spec
    schedulerConfig
    evidencePolicies
    compiledProgram
  where
    !compiledPriority =
      compileControlCatalogPriorityTargets catalog priorities

    !schedulerConfig =
      mergePriorityProfile
        compiledPriority
        (ccSchedulerConfig catalog spec)

    evidencePolicies =
      ccEvidencePolicies catalog spec

    compiledProgram =
      compileSymbolicProgram
        (ccPhaseDecl catalog)
        program
