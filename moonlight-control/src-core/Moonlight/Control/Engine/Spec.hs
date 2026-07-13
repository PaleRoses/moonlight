{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | The type-state specification of an engine run: 'Raw' specs accumulate
-- settings, 'defaultEngineSpec' fills the gaps, 'validateEngineSpec' is the
-- only door to 'Validated' — and only 'Validated' specs compile to plans.
-- Skipping a stage is a type error, not a runtime surprise.
module Moonlight.Control.Engine.Spec
  ( Raw,
    Defaulted,
    Validated,
    ScheduleOrderSpec (..),
    TracePolicySpec (..),
    EngineSpec,
    EngineSpecError (..),
    rawEngineSpec,
    setScheduleOrderSpec,
    setBackoffMatchLimit,
    setBackoffCooldownRounds,
    setTracePolicySpec,
    setMaxRounds,
    setRoundBudget,
    setPriorityUpdateMode,
    defaultEngineSpec,
    validateEngineSpec,
    specScheduleOrder,
    specTracePolicy,
    specMaxRounds,
    specRoundBudget,
    specPriorityUpdateMode,
    compileSchedulerConfig,
    compilePlan,
    compilePlanWithProgram,
    compilePlanWithControl,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Numeric.Natural (Natural)

import Moonlight.Core
  ( Validation (..),
    validationToEither,
  )
import Moonlight.Control.Class
  ( phase,
    upTo,
  )
import Moonlight.Control.Engine.Evidence
  ( EvidencePolicy,
    PriorityUpdateMode (..),
  )
import Moonlight.Control.Engine.Plan
  ( EngineProgram,
    PhaseDecl,
    Plan (..),
    RoundBudget,
    RoundLimit,
    canonicalRoundBudget,
    canonicalRoundLimit,
    fixedPointStopPolicy,
    roundLimitValue,
  )
import Moonlight.Control.Engine.Report
  ( Observation,
  )
import Moonlight.Control.Schedule
  ( BackoffConfig,
    ScheduleOrder (..),
    SchedulerConfig (..),
    TracePolicy (..),
    backoffConfig,
    defaultSchedulerConfig,
    traceLastEntries,
  )

type Raw :: Type
data Raw

type Defaulted :: Type
data Defaulted

type Validated :: Type
data Validated

data ScheduleOrderSpec
  = ScheduleDeterministicSpec
  | ScheduleBackoffSpec
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data TracePolicySpec
  = TraceNoTraceSpec
  | TraceLastSpec !Int
  | TraceAllSpec
  deriving stock (Eq, Ord, Show, Read)

type EngineSpec :: Type -> Type
data EngineSpec stage where
  RawEngineSpec ::
    { rawScheduleOrder :: !(Maybe ScheduleOrderSpec),
      rawBackoffMatchLimit :: !(Maybe Int),
      rawBackoffCooldownRounds :: !(Maybe Int),
      rawTracePolicy :: !(Maybe TracePolicySpec),
      rawMaxRounds :: !(Maybe Int),
      rawRoundBudget :: !(Maybe Int),
      rawPriorityUpdateMode :: !(Maybe PriorityUpdateMode)
    } ->
    EngineSpec Raw
  DefaultedEngineSpec ::
    { defaultedScheduleOrder :: !ScheduleOrderSpec,
      defaultedBackoffMatchLimit :: !Int,
      defaultedBackoffCooldownRounds :: !Int,
      defaultedTracePolicy :: !TracePolicySpec,
      defaultedMaxRounds :: !Int,
      defaultedRoundBudget :: !Int,
      defaultedPriorityUpdateMode :: !PriorityUpdateMode
    } ->
    EngineSpec Defaulted
  ValidatedEngineSpec ::
    { validatedScheduleOrder :: !ScheduleOrder,
      validatedTracePolicy :: !TracePolicy,
      validatedMaxRounds :: !RoundLimit,
      validatedRoundBudget :: !RoundBudget,
      validatedPriorityUpdateMode :: !PriorityUpdateMode
    } ->
    EngineSpec Validated

data EngineSpecError
  = SpecBackoffMatchLimitNonPositive !Int
  | SpecBackoffCooldownNegative !Int
  | SpecTraceLastNonPositive !Int
  | SpecMaxRoundsNonPositive !Int
  | SpecRoundBudgetNonPositive !Int
  deriving stock (Eq, Ord, Show, Read)

-- | The empty specification. O(1).
rawEngineSpec :: EngineSpec Raw
rawEngineSpec =
  RawEngineSpec
    { rawScheduleOrder = Nothing,
      rawBackoffMatchLimit = Nothing,
      rawBackoffCooldownRounds = Nothing,
      rawTracePolicy = Nothing,
      rawMaxRounds = Nothing,
      rawRoundBudget = Nothing,
      rawPriorityUpdateMode = Nothing
    }

setScheduleOrderSpec :: ScheduleOrderSpec -> EngineSpec Raw -> EngineSpec Raw
setScheduleOrderSpec value spec = spec {rawScheduleOrder = Just value}

setBackoffMatchLimit :: Int -> EngineSpec Raw -> EngineSpec Raw
setBackoffMatchLimit value spec = spec {rawBackoffMatchLimit = Just value}

setBackoffCooldownRounds :: Int -> EngineSpec Raw -> EngineSpec Raw
setBackoffCooldownRounds value spec = spec {rawBackoffCooldownRounds = Just value}

setTracePolicySpec :: TracePolicySpec -> EngineSpec Raw -> EngineSpec Raw
setTracePolicySpec value spec = spec {rawTracePolicy = Just value}

setMaxRounds :: Int -> EngineSpec Raw -> EngineSpec Raw
setMaxRounds value spec = spec {rawMaxRounds = Just value}

setRoundBudget :: Int -> EngineSpec Raw -> EngineSpec Raw
setRoundBudget value spec = spec {rawRoundBudget = Just value}

setPriorityUpdateMode :: PriorityUpdateMode -> EngineSpec Raw -> EngineSpec Raw
setPriorityUpdateMode value spec = spec {rawPriorityUpdateMode = Just value}

-- | Fill every unset field with its documented default. O(1).
defaultEngineSpec ::
  EngineSpec Raw ->
  EngineSpec Defaulted
defaultEngineSpec rawSpec =
  DefaultedEngineSpec
    { defaultedScheduleOrder =
        fromMaybe ScheduleDeterministicSpec (rawScheduleOrder rawSpec),
      defaultedBackoffMatchLimit =
        fromMaybe 64 (rawBackoffMatchLimit rawSpec),
      defaultedBackoffCooldownRounds =
        fromMaybe 3 (rawBackoffCooldownRounds rawSpec),
      defaultedTracePolicy =
        fromMaybe TraceNoTraceSpec (rawTracePolicy rawSpec),
      defaultedMaxRounds =
        fromMaybe 100 (rawMaxRounds rawSpec),
      defaultedRoundBudget =
        fromMaybe 64 (rawRoundBudget rawSpec),
      defaultedPriorityUpdateMode =
        fromMaybe AccumulateDynamicPriority (rawPriorityUpdateMode rawSpec)
    }

-- | Validate every field, reporting all errors at once. O(1).
validateEngineSpec ::
  EngineSpec Defaulted ->
  Either (NonEmpty EngineSpecError) (EngineSpec Validated)
validateEngineSpec spec =
  validationToEither
    ( ValidatedEngineSpec
        <$> validateScheduleOrderSpec spec
        <*> validateTracePolicySpec (defaultedTracePolicy spec)
        <*> validateRoundLimitValue (defaultedMaxRounds spec)
        <*> validateRoundBudgetValue (defaultedRoundBudget spec)
        <*> pure (defaultedPriorityUpdateMode spec)
    )

validateScheduleOrderSpec ::
  EngineSpec Defaulted ->
  Validation (NonEmpty EngineSpecError) ScheduleOrder
validateScheduleOrderSpec spec =
  case defaultedScheduleOrder spec of
    ScheduleDeterministicSpec ->
      ByRuleIdThenSubstitution <$ validateBackoffConfigSpec spec
    ScheduleBackoffSpec ->
      BackoffByGroup <$> validateBackoffConfigSpec spec

validateBackoffConfigSpec ::
  EngineSpec Defaulted ->
  Validation (NonEmpty EngineSpecError) BackoffConfig
validateBackoffConfigSpec spec =
  backoffConfig
    <$> validatePositiveInt
      SpecBackoffMatchLimitNonPositive
      (defaultedBackoffMatchLimit spec)
    <*> validateNonNegativeInt
      SpecBackoffCooldownNegative
      (defaultedBackoffCooldownRounds spec)

validateTracePolicySpec ::
  TracePolicySpec ->
  Validation (NonEmpty EngineSpecError) TracePolicy
validateTracePolicySpec tracePolicySpec =
  case tracePolicySpec of
    TraceNoTraceSpec ->
      pure NoTrace
    TraceAllSpec ->
      pure TraceAll
    TraceLastSpec retainedCount ->
      traceLastEntries
        <$> validatePositiveInt SpecTraceLastNonPositive retainedCount

validateRoundLimitValue ::
  Int ->
  Validation (NonEmpty EngineSpecError) RoundLimit
validateRoundLimitValue =
  fmap canonicalRoundLimit . validatePositiveInt SpecMaxRoundsNonPositive

validateRoundBudgetValue ::
  Int ->
  Validation (NonEmpty EngineSpecError) RoundBudget
validateRoundBudgetValue =
  fmap canonicalRoundBudget . validatePositiveInt SpecRoundBudgetNonPositive

validatePositiveInt ::
  (Int -> EngineSpecError) ->
  Int ->
  Validation (NonEmpty EngineSpecError) Int
validatePositiveInt buildError rawValue =
  if rawValue > 0
    then Valid rawValue
    else Invalid (buildError rawValue :| [])

validateNonNegativeInt ::
  (Int -> EngineSpecError) ->
  Int ->
  Validation (NonEmpty EngineSpecError) Int
validateNonNegativeInt buildError rawValue =
  if rawValue >= 0
    then Valid rawValue
    else Invalid (buildError rawValue :| [])

specScheduleOrder :: EngineSpec Validated -> ScheduleOrder
specScheduleOrder = validatedScheduleOrder

specTracePolicy :: EngineSpec Validated -> TracePolicy
specTracePolicy = validatedTracePolicy

specMaxRounds :: EngineSpec Validated -> RoundLimit
specMaxRounds = validatedMaxRounds

specRoundBudget :: EngineSpec Validated -> RoundBudget
specRoundBudget = validatedRoundBudget

specPriorityUpdateMode :: EngineSpec Validated -> PriorityUpdateMode
specPriorityUpdateMode = validatedPriorityUpdateMode

-- | The scheduler configuration a validated spec denotes. O(1).
compileSchedulerConfig ::
  EngineSpec Validated ->
  SchedulerConfig group
compileSchedulerConfig spec =
  defaultSchedulerConfig
    { scOrder = specScheduleOrder spec,
      scTracePolicy = specTracePolicy spec
    }

-- | The fixed-point plan of one phase repeated up to the round limit. O(1).
compilePlan ::
  Ord group =>
  EngineSpec Validated ->
  PhaseDecl ->
  Plan view group match traceEntry evidence
compilePlan spec decl =
  compilePlanWithProgram
    spec
    (upTo (roundLimitNatural (specMaxRounds spec)) (phase decl))

compilePlanWithProgram ::
  EngineSpec Validated ->
  EngineProgram view group match traceEntry ->
  Plan view group match traceEntry evidence
compilePlanWithProgram spec =
  compilePlanWithControl
    spec
    (compileSchedulerConfig spec)
    []

compilePlanWithControl ::
  EngineSpec Validated ->
  SchedulerConfig group ->
  [EvidencePolicy (Observation group traceEntry evidence) group] ->
  EngineProgram view group match traceEntry ->
  Plan view group match traceEntry evidence
compilePlanWithControl spec schedulerConfig evidencePolicies program =
  Plan
    { planInitialSchedulerConfig =
        schedulerConfig,
      planProgram =
        program,
      planRoundBudget =
        specRoundBudget spec,
      planStopPolicy =
        fixedPointStopPolicy (specMaxRounds spec),
      planEvidencePolicies =
        evidencePolicies
    }

roundLimitNatural ::
  RoundLimit ->
  Natural
roundLimitNatural =
  fromIntegral . roundLimitValue
