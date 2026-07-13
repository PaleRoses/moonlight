{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Control.SymbolicSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))

import Moonlight.Control.Class
  ( orElse,
    phase,
    sequenceAll,
    upTo,
  )
import Moonlight.Control.Engine.Plan
  ( phaseDecl,
  )
import Moonlight.Control.Engine.Symbolic
  ( ControlCatalog (..),
    ControlCatalogProjectionFailure (..),
    Domain (..),
    KnownPhase (..),
    KnownRule (..),
    KnownSupport (..),
    RuleRef,
    SupportRef,
    SymbolicProgram,
    basicControlCatalog,
    compileControlCatalogPriorityTargets,
    compileSymbolicProgram,
    controlCatalogProjectionFailures,
    prioritizeRule,
    prioritizeSupport,
  )
import Moonlight.Control.Program
  ( Program,
  )
import Moonlight.Control.Schedule
  ( ScheduleGroup (..),
    sgRuleKey,
  )
import Moonlight.Control.Weight
  ( PriorityProfile,
    criticalPriorityRank,
    lookupPriorityEvidence,
    observedScheduledPriorityEvidence,
    priorityEvidence,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

data TestDomain

data TestPhase
  = AlphaPhase
  | BetaPhase
  | GammaPhase
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data TestRule
  = AlphaRule
  | BetaRule
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data TestSupport
  = HotSupport
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

type TestGroup = ScheduleGroup TestRule TestSupport

instance Domain TestDomain where
  type PhaseKey TestDomain = TestPhase
  type RuleKey TestDomain = TestRule
  type SupportKey TestDomain = TestSupport

instance KnownPhase TestDomain "alpha" where
  knownPhaseKey =
    AlphaPhase

instance KnownPhase TestDomain "beta" where
  knownPhaseKey =
    BetaPhase

instance KnownPhase TestDomain "gamma" where
  knownPhaseKey =
    GammaPhase

instance KnownRule TestDomain "alpha" where
  knownRuleKey =
    AlphaRule

instance KnownRule TestDomain "beta" where
  knownRuleKey =
    BetaRule

instance KnownSupport TestDomain "hot" where
  knownSupportKey =
    HotSupport

tests :: TestTree
tests =
  testGroup
    "symbolic frontend"
    [ testCase "symbolic program compiles to the corresponding program tree" testSymbolicProgramCompiles,
      testCase "symbolic catalog projection and priority compilation are lawful" testCatalogLaws
    ]

testSymbolicProgramCompiles :: Assertion
testSymbolicProgramCompiles =
  compileSymbolicProgram id symbolicProgram
    @?= expectedProgram
  where
    symbolicProgram :: SymbolicProgram () TestDomain
    symbolicProgram =
      sequenceAll
        [ #alpha,
          upTo 2 (orElse #beta #gamma)
        ]

    expectedProgram :: Program () TestPhase
    expectedProgram =
      sequenceAll
        [ phase AlphaPhase,
          upTo 2 (orElse (phase BetaPhase) (phase GammaPhase))
        ]

testCatalogLaws :: Assertion
testCatalogLaws = do
  lookupPriorityEvidence (RuleGroup AlphaRule) compiledProfile
    @?= priorityEvidence 0 0 0 criticalPriorityRank
  lookupPriorityEvidence (SupportedGroup BetaRule HotSupport) compiledProfile
    @?= observedScheduledPriorityEvidence 2 criticalPriorityRank

  controlCatalogProjectionFailures
    catalog
    [AlphaRule]
    [(BetaRule, HotSupport)]
    @?= []

  controlCatalogProjectionFailures
    brokenCatalog
    [AlphaRule]
    [(AlphaRule, HotSupport)]
    @?= [ RuleGroupProjectionMismatch AlphaRule (RuleGroup AlphaRule) BetaRule,
          SupportGroupProjectionMismatch AlphaRule HotSupport (SupportedGroup AlphaRule HotSupport) BetaRule
        ]

  compileControlCatalogPriorityTargets catalog mempty
    @?= (mempty :: PriorityProfile TestGroup)

  compileControlCatalogPriorityTargets catalog (leftProfile <> rightProfile)
    @?= compileControlCatalogPriorityTargets catalog leftProfile
      <> compileControlCatalogPriorityTargets catalog rightProfile
  where
    compiledProfile =
      compileControlCatalogPriorityTargets catalog (leftProfile <> rightProfile)

    brokenCatalog :: ControlCatalog TestDomain TestGroup () ()
    brokenCatalog =
      catalog {ccGroupRuleKey = const BetaRule}

    leftProfile =
      prioritizeRule
        (#alpha :: RuleRef TestDomain "alpha")
        (priorityEvidence 0 0 0 criticalPriorityRank)

    rightProfile =
      prioritizeSupport
        (#beta :: RuleRef TestDomain "beta")
        (#hot :: SupportRef TestDomain "hot")
        (observedScheduledPriorityEvidence 2 criticalPriorityRank)

catalog :: ControlCatalog TestDomain TestGroup () ()
catalog =
  basicControlCatalog
    (const (phaseDecl "symbolic-law" Nothing))
    (\rule -> RuleGroup rule :| [])
    (\rule support -> SupportedGroup rule support :| [])
    sgRuleKey
