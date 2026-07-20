{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Control.SupportSpec
  ( tests,
  )
where

import Data.Functor.Identity (Identity (..), runIdentity)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Control.Candidate
  ( finiteCandidateSpace,
    scheduledBatchMatches,
  )
import Moonlight.Control.Engine.Evidence
  ( EvidencePolicy,
    PriorityUpdateMode (..),
  )
import Moonlight.Control.Engine.Plan
  ( PhaseDecl,
    Plan,
    canonicalRoundBudget,
    phaseDecl,
  )
import Moonlight.Control.Engine.Report
  ( EngineReport (..),
    EngineRound (..),
    Observation (..),
  )
import Moonlight.Control.Engine.Run
  ( runEngine,
  )
import Moonlight.Control.Engine.Spec
  ( EngineSpec,
    EngineSpecError,
    Validated,
    defaultEngineSpec,
    rawEngineSpec,
    setPriorityUpdateMode,
    validateEngineSpec,
  )
import Moonlight.Control.Engine.Symbolic
  ( ControlCatalog (..),
    Domain (..),
    KnownPhase (..),
    SymbolicProgram,
    compileSymbolicPlan,
  )
import Moonlight.Control.Engine.Work
  ( WorkSource (..),
    applyResult,
  )
import Moonlight.Control.Schedule
  ( ScheduleOrder (BackoffByGroup),
    ScheduleGroup (..),
    SchedulerConfig (..),
    TracePolicy (NoTrace),
    backoffConfig,
    defaultSchedulerConfig,
    sgRuleKey,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleTrace,
  )
import Moonlight.Control.Scheduling.Support.Feedback
  ( supportEvidencePolicyWithMode,
  )
import Moonlight.Control.Weight
  ( emptyPriorityProfile,
    criticalPriorityRank,
    lookupPriorityEvidence,
    observedScheduledPriorityEvidence,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "support feedback"
    [ testCase "enabled support feedback feeds runner dynamic priority from trace delta" testSupportFeedbackFeedsRunnerDynamicPriority,
      testCase "disabled support feedback leaves runner dynamic priority empty" testDisabledSupportFeedbackLeavesDynamicPriorityEmpty
    ]

data SupportMatch = SupportMatch
  { smRule :: !SupportRule,
    smSupport :: !SupportSupport,
    smId :: !Int
  }
  deriving stock (Eq, Ord, Show, Read)

data SupportDomain

data SupportPhase
  = SupportFeedbackPhase
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data SupportRule
  = SupportRuleA
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data SupportSupport
  = SupportA
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

type SupportGroup = ScheduleGroup SupportRule SupportSupport

type SupportEngine = WorkSource Identity [SupportMatch] () SupportGroup SupportMatch Int String

type SupportPhaseDecl = PhaseDecl

type SupportCatalog traceEntry = ControlCatalog SupportDomain SupportGroup traceEntry Int

type SupportPlan traceEntry = Plan () SupportGroup SupportMatch traceEntry Int

instance Domain SupportDomain where
  type PhaseKey SupportDomain = SupportPhase
  type RuleKey SupportDomain = SupportRule
  type SupportKey SupportDomain = SupportSupport

instance KnownPhase SupportDomain "support" where
  knownPhaseKey =
    SupportFeedbackPhase

supportMatches :: [SupportMatch]
supportMatches =
  [ SupportMatch SupportRuleA SupportA 1,
    SupportMatch SupportRuleA SupportA 2,
    SupportMatch SupportRuleA SupportA 3
  ]

supportGroup :: SupportGroup
supportGroup =
  SupportedGroup SupportRuleA SupportA

supportEngine :: SupportEngine
supportEngine =
  WorkSource
    { wsView = const (),
      wsCandidateSpace = const (Identity (finiteCandidateSpace [(supportGroup, supportMatches)])),
      wsApplyScheduled = \scheduledBatch _state ->
        let scheduledMatches = scheduledBatchMatches scheduledBatch
         in Identity
              ( Right
                  ( applyResult
                      scheduledMatches
                      (length scheduledMatches)
                      (length scheduledMatches)
                  )
              ),
      wsProgressed = (> 0)
    }

supportSchedulerConfig :: SchedulerConfig SupportGroup
supportSchedulerConfig =
  defaultSchedulerConfig
    { scOrder = BackoffByGroup (backoffConfig 1 1),
      scTracePolicy = NoTrace
    }

supportStrategy :: SymbolicProgram ctx SupportDomain
supportStrategy = #support

supportCatalog ::
  [EvidencePolicy (Observation SupportGroup traceEntry Int) SupportGroup] ->
  SupportCatalog traceEntry
supportCatalog evidencePolicies =
  ControlCatalog
    { ccPhaseDecl = supportPhaseDeclOf,
      ccRuleGroups = supportRuleGroups,
      ccSupportGroups = supportGroups,
      ccGroupRuleKey = sgRuleKey,
      ccSchedulerConfig = const supportSchedulerConfig,
      ccEvidencePolicies = const evidencePolicies
    }
  where
    supportPhaseDeclOf :: SupportPhase -> SupportPhaseDecl
    supportPhaseDeclOf phaseKey =
      case phaseKey of
        SupportFeedbackPhase ->
          phaseDecl "support" (Just (canonicalRoundBudget 16))

    supportRuleGroups :: SupportRule -> NonEmpty SupportGroup
    supportRuleGroups ruleKey =
      RuleGroup ruleKey
        NonEmpty.:| [SupportedGroup ruleKey SupportA]

    supportGroups :: SupportRule -> SupportSupport -> NonEmpty SupportGroup
    supportGroups ruleKey supportKey =
      SupportedGroup ruleKey supportKey
        NonEmpty.:| []

testSupportFeedbackFeedsRunnerDynamicPriority :: Assertion
testSupportFeedbackFeedsRunnerDynamicPriority =
  case enabledSupportSpec of
    Left specErrors ->
      assertFailure ("unexpected support spec validation failure: " <> show specErrors)
    Right spec ->
      case runIdentity (runEngine (supportPlan enabledSupportCatalog spec) supportEngine []) of
        Left failure ->
          assertFailure ("unexpected support runner failure: " <> show failure)
        Right report -> do
          let actualProfile =
                erDynamicPriorityProfile report
          reportScheduleTrace report @?= []
          lookupPriorityEvidence supportGroup actualProfile
            @?= observedScheduledPriorityEvidence 1 criticalPriorityRank

testDisabledSupportFeedbackLeavesDynamicPriorityEmpty :: Assertion
testDisabledSupportFeedbackLeavesDynamicPriorityEmpty =
  case disabledSupportSpec of
    Left specErrors ->
      assertFailure ("unexpected support spec validation failure: " <> show specErrors)
    Right spec ->
      case runIdentity (runEngine (supportPlan disabledSupportCatalog spec) supportEngine []) of
        Left failure ->
          assertFailure ("unexpected support runner failure: " <> show failure)
        Right report -> do
          reportScheduleTrace report @?= []
          erDynamicPriorityProfile report @?= emptyPriorityProfile

supportPlan ::
  SupportCatalog traceEntry ->
  EngineSpec Validated ->
  SupportPlan traceEntry
supportPlan catalog spec =
  compileSymbolicPlan
    catalog
    spec
    supportStrategy
    emptyPriorityProfile

enabledSupportCatalog :: SupportCatalog traceEntry
enabledSupportCatalog =
  supportCatalog [supportEvidencePolicyWithMode ReplaceDynamicPriority]

disabledSupportCatalog :: SupportCatalog traceEntry
disabledSupportCatalog =
  supportCatalog []

enabledSupportSpec :: Either (NonEmpty EngineSpecError) (EngineSpec Validated)
enabledSupportSpec =
  validateEngineSpec
    ( defaultEngineSpec
        ( setPriorityUpdateMode ReplaceDynamicPriority
            $ rawEngineSpec
        )
    )

disabledSupportSpec :: Either (NonEmpty EngineSpecError) (EngineSpec Validated)
disabledSupportSpec =
  validateEngineSpec (defaultEngineSpec rawEngineSpec)

reportScheduleTrace :: EngineReport state group traceEntry evidence -> [ScheduleTrace group]
reportScheduleTrace =
  foldMap (obScheduleTrace . roundObservation) . erRounds
