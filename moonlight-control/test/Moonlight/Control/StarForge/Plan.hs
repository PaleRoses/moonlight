{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Moonlight.Control.StarForge.Plan
  ( validatedForgeSpec,
    starForgePlan,
    forgePriorityObservation,
  )
where

import Data.List qualified as List
import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import Data.Set qualified as Set

import Moonlight.Control.Class
  ( attempt,
    sequenceAll,
    upTo,
  )
import Moonlight.Control.Engine.Evidence
  ( EvidencePolicy (..),
    PriorityUpdateMode (..),
  )
import Moonlight.Control.Engine.Plan
  ( PhaseDecl,
    Plan (..),
    StopPolicy (..),
    canonicalRoundBudget,
    phaseDecl,
  )
import Moonlight.Control.Engine.Report
  ( Observation (..),
    StopReason (..),
  )
import Moonlight.Control.Engine.Spec
  ( EngineSpec,
    EngineSpecError,
    ScheduleOrderSpec (..),
    TracePolicySpec (..),
    Validated,
    defaultEngineSpec,
    rawEngineSpec,
    setBackoffCooldownRounds,
    setBackoffMatchLimit,
    setMaxRounds,
    setPriorityUpdateMode,
    setRoundBudget,
    setScheduleOrderSpec,
    setTracePolicySpec,
    validateEngineSpec,
  )
import Moonlight.Control.Engine.Symbolic
  ( ControlCatalog (..),
    SymbolicProgram,
    basicControlCatalog,
    compileSymbolicPlan,
  )
import Moonlight.Control.Gate
  ( Gate (..),
    filterGroupSelectorWithTrace,
  )
import Moonlight.Control.Modality
  ( Modality,
    gated,
  )
import Moonlight.Control.Schedule
  ( ScheduleGroup (..),
    ScheduleOrder (..),
    SchedulerConfig (..),
    TracePolicy (..),
    backoffConfig,
    defaultSchedulerConfig,
    sgRuleKey,
  )
import Moonlight.Control.StarForge.Model
  ( Curse (..),
    ForgeEvidence (..),
    ForgeGroup,
    ForgeGateTrace (..),
    ForgeLane (..),
    ForgeMatch (..),
    ForgePhase (..),
    ForgePlan,
    ForgeState (..),
    ForgeSupport (..),
    Fragment (..),
    StarForgeDomain,
    forgeTargetHeat,
  )
import Moonlight.Control.Weight
  ( PriorityEvidence,
    PriorityProfile,
    criticalPriorityRank,
    nonCriticalPriorityRank,
    priorityEvidence,
    priorityProfileFromList,
  )

validatedForgeSpec ::
  Either (NonEmpty EngineSpecError) (EngineSpec Validated)
validatedForgeSpec =
  validateEngineSpec
    ( defaultEngineSpec
        ( setScheduleOrderSpec ScheduleBackoffSpec
            . setBackoffMatchLimit 2
            . setBackoffCooldownRounds 3
            . setTracePolicySpec TraceAllSpec
            . setMaxRounds 16
            . setRoundBudget 8
            . setPriorityUpdateMode ReplaceDynamicPriority
            $ rawEngineSpec
        )
    )

starForgePlan ::
  EngineSpec Validated ->
  ForgePlan
starForgePlan spec =
  ( compileSymbolicPlan
      forgeCatalog
      spec
      forgeProgram
      mempty
  )
    { planStopPolicy = forgeStopPolicy
    }

forgeProgram ::
  SymbolicProgram (Modality ForgeState ForgeGroup ForgeMatch ForgeGateTrace ForgeGroup) StarForgeDomain
forgeProgram =
  sequenceAll
    [ gated (forgeGate StabilizeFirstPhase) #stabilizeFirst,
      gated (forgeGate PrimeFusionPhase) #primeFusion,
      attempt (gated (forgeGate InvokeEclipsePhase) #invokeEclipse),
      upTo 16 (gated (forgeGate NormalForgePhase) #normalForge)
    ]

forgeCatalog ::
  ControlCatalog StarForgeDomain ForgeGroup ForgeGateTrace ForgeEvidence
forgeCatalog =
  ( basicControlCatalog
      forgePhaseDecl
      forgeRuleGroups
      forgeSupportGroups
      sgRuleKey ::
      ControlCatalog StarForgeDomain ForgeGroup ForgeGateTrace ForgeEvidence
  )
    { ccSchedulerConfig = const forgeSchedulerConfig,
      ccEvidencePolicies = const [forgeEvidencePolicy]
    }

forgePhaseDecl ::
  ForgePhase ->
  PhaseDecl
forgePhaseDecl phaseKey =
  case phaseKey of
    StabilizeFirstPhase ->
      phaseDecl "stabilize-first" (Just (canonicalRoundBudget 3))
    PrimeFusionPhase ->
      phaseDecl "prime-fusion" (Just (canonicalRoundBudget 2))
    InvokeEclipsePhase ->
      phaseDecl "invoke-eclipse" (Just (canonicalRoundBudget 1))
    NormalForgePhase ->
      phaseDecl "normal-forge" (Just (canonicalRoundBudget 1))

forgeRuleGroups ::
  ForgeLane ->
  NonEmpty ForgeGroup
forgeRuleGroups lane =
  case lane of
    StabilizeFragment ->
      SupportedGroup StabilizeFragment (FragmentSupport WolfFang)
        :| [ SupportedGroup StabilizeFragment (FragmentSupport StarShard),
             SupportedGroup StabilizeFragment (FragmentSupport GlassShard)
           ]
    FusePair ->
      RuleGroup FusePair :| []
    BreakCurse ->
      SupportedGroup BreakCurse (CurseSupport MirrorHex) :| []
    CoolForge ->
      SupportedGroup CoolForge HeatSupport :| []
    InvokeEclipse ->
      SupportedGroup InvokeEclipse EclipseSupport :| []

forgeSupportGroups ::
  ForgeLane ->
  ForgeSupport ->
  NonEmpty ForgeGroup
forgeSupportGroups lane support =
  SupportedGroup lane support :| []

forgeSchedulerConfig ::
  SchedulerConfig ForgeGroup
forgeSchedulerConfig =
  defaultSchedulerConfig
    { scOrder = BackoffByGroup (backoffConfig 2 3),
      scTracePolicy = TraceAll,
      scPriorityProfile = initialForgePriority
    }

initialForgePriority ::
  PriorityProfile ForgeGroup
initialForgePriority =
  priorityProfileFromList
    [ (SupportedGroup StabilizeFragment (FragmentSupport WolfFang), structuralCritical 100),
      (SupportedGroup StabilizeFragment (FragmentSupport StarShard), structuralCritical 100),
      (SupportedGroup StabilizeFragment (FragmentSupport GlassShard), structuralCritical 100),
      (RuleGroup FusePair, priorityEvidence 80 0 0 nonCriticalPriorityRank),
      (SupportedGroup BreakCurse (CurseSupport MirrorHex), priorityEvidence 60 0 0 nonCriticalPriorityRank),
      (SupportedGroup CoolForge HeatSupport, priorityEvidence 40 0 0 nonCriticalPriorityRank),
      (SupportedGroup InvokeEclipse EclipseSupport, priorityEvidence 10 0 0 nonCriticalPriorityRank)
    ]

structuralCritical :: Int -> PriorityEvidence
structuralCritical influence =
  priorityEvidence influence 0 0 criticalPriorityRank

forgeEvidencePolicy ::
  EvidencePolicy
    (Observation ForgeGroup ForgeGateTrace ForgeEvidence)
    ForgeGroup
forgeEvidencePolicy =
  EvidencePolicy
    { epObserve = forgePriorityObservation,
      epUpdateMode = ReplaceDynamicPriority,
      epNeedsScheduleTrace = True
    }

forgePriorityObservation ::
  Observation ForgeGroup ForgeGateTrace ForgeEvidence ->
  PriorityProfile ForgeGroup
forgePriorityObservation observation =
  priorityProfileFromList
    (cursePriorities <> heatPriorities)
  where
    evidence =
      obEvidence observation

    cursePriorities =
      if Set.member MirrorHex (feCursesAfter evidence)
        then [(SupportedGroup BreakCurse (CurseSupport MirrorHex), priorityEvidence 100 1 0 criticalPriorityRank)]
        else []

    heatPriorities
      | feHeatAfter evidence <= forgeTargetHeat =
          []
      | Set.member MirrorHex (feCursesAfter evidence) =
          []
      | otherwise =
          [ (RuleGroup FusePair, priorityEvidence 90 1 0 criticalPriorityRank),
            (SupportedGroup CoolForge HeatSupport, priorityEvidence 50 0 0 nonCriticalPriorityRank)
          ]

forgeStopPolicy ::
  StopPolicy (Observation ForgeGroup ForgeGateTrace ForgeEvidence)
forgeStopPolicy =
  StopPolicy $ \observation ->
    if obRound observation >= 15
      then Just RoundLimitReached
      else
        if feFixedPoint (obEvidence observation)
          then Just Converged
          else Nothing

forgeGate ::
  ForgePhase ->
  Gate ForgeState ForgeGroup ForgeMatch ForgeGateTrace ForgeGroup
forgeGate phaseKey =
  Gate
    { gateSelector =
        filterGroupSelectorWithTrace
          ("star-forge:" <> show phaseKey)
          (forgeMatchDecision phaseKey),
      gateValidation = mempty
    }

forgeMatchDecision ::
  ForgePhase ->
  ForgeState ->
  ForgeGroup ->
  ForgeMatch ->
  Either ForgeGateTrace ()
forgeMatchDecision phaseKey state group match =
  let lane =
        laneOfMatch match
   in if lane /= sgRuleKey group
        then Left (WrongPhase phaseKey lane)
        else
          if not (phaseAllowsLane phaseKey lane)
            then Left (WrongPhase phaseKey lane)
            else forgeStateDecision state match

phaseAllowsLane ::
  ForgePhase ->
  ForgeLane ->
  Bool
phaseAllowsLane phaseKey lane =
  case phaseKey of
    StabilizeFirstPhase ->
      lane == StabilizeFragment
    PrimeFusionPhase ->
      lane == FusePair
    InvokeEclipsePhase ->
      lane == InvokeEclipse
    NormalForgePhase ->
      lane == BreakCurse || lane == FusePair || lane == CoolForge

forgeStateDecision ::
  ForgeState ->
  ForgeMatch ->
  Either ForgeGateTrace ()
forgeStateDecision state match =
  case match of
    Stabilize fragment ->
      if Set.member fragment (fsFragments state)
        then Left (FragmentAlreadyStable fragment)
        else Right ()
    Fuse leftFragment rightFragment constellation ->
      case firstMissingFragment state [leftFragment, rightFragment] of
        Just missingFragment ->
          Left (FragmentMissing missingFragment)
        Nothing ->
          if Set.member constellation (fsConstellations state)
            then Left (ConstellationAlreadyForged constellation)
            else
              if fsHeat state <= forgeTargetHeat
                then Left (ForgeTooCool (fsHeat state))
                else Right ()
    Break curse ->
      if Set.member curse (fsCurses state)
        then Right ()
        else Left (CurseAbsent curse)
    Cool _amount ->
      if fsHeat state > forgeTargetHeat
        then Right ()
        else Left (ForgeAlreadyCool (fsHeat state))
    Eclipse ->
      Right ()

firstMissingFragment ::
  ForgeState ->
  [Fragment] ->
  Maybe Fragment
firstMissingFragment state =
  List.find (`Set.notMember` fsFragments state)

laneOfMatch ::
  ForgeMatch ->
  ForgeLane
laneOfMatch match =
  case match of
    Stabilize _fragment ->
      StabilizeFragment
    Fuse _left _right _constellation ->
      FusePair
    Break _curse ->
      BreakCurse
    Cool _amount ->
      CoolForge
    Eclipse ->
      InvokeEclipse
