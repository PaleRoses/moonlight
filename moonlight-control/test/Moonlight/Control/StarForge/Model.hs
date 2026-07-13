{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Control.StarForge.Model
  ( Fragment (..),
    Constellation (..),
    Curse (..),
    ForgeState (..),
    ForgeLane (..),
    ForgeSupport (..),
    ForgeGroup,
    ForgeMatch (..),
    ForgePhase (..),
    ForgeGateTrace (..),
    ForgeDelta (..),
    ForgeExecutionBatch (..),
    ForgeEvidence (..),
    ForgeError (..),
    StarForgeDomain,
    ForgePlan,
    ForgeReport,
    forgeTargetHeat,
    initialForgeState,
  )
where

import Data.Map.Strict
  ( Map,
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Numeric.Natural
  ( Natural,
  )

import Moonlight.Control.Engine.Plan
  ( Plan,
  )
import Moonlight.Control.Engine.Report
  ( EngineReport,
  )
import Moonlight.Control.Engine.Symbolic
  ( Domain (..),
    KnownPhase (..),
  )
import Moonlight.Control.Schedule
  ( ScheduleGroup (..),
  )

data Fragment
  = WolfFang
  | StarShard
  | GlassShard
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data Constellation
  = WolfStar
  | GlassCrown
  | AshMirror
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data Curse
  = MirrorHex
  | EclipseShadow
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data ForgeState = ForgeState
  { fsFragments :: !(Set Fragment),
    fsConstellations :: !(Set Constellation),
    fsCurses :: !(Set Curse),
    fsHeat :: !Int
  }
  deriving stock (Eq, Show, Read)

data ForgeLane
  = StabilizeFragment
  | FusePair
  | BreakCurse
  | CoolForge
  | InvokeEclipse
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data ForgeSupport
  = FragmentSupport !Fragment
  | CurseSupport !Curse
  | HeatSupport
  | EclipseSupport
  deriving stock (Eq, Ord, Show, Read)

type ForgeGroup = ScheduleGroup ForgeLane ForgeSupport

data ForgeMatch
  = Stabilize !Fragment
  | Fuse !Fragment !Fragment !Constellation
  | Break !Curse
  | Cool !Int
  | Eclipse
  deriving stock (Eq, Ord, Show, Read)

data ForgePhase
  = StabilizeFirstPhase
  | PrimeFusionPhase
  | InvokeEclipsePhase
  | NormalForgePhase
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data ForgeGateTrace
  = WrongPhase !ForgePhase !ForgeLane
  | FragmentAlreadyStable !Fragment
  | FragmentMissing !Fragment
  | ConstellationAlreadyForged !Constellation
  | CurseAbsent !Curse
  | ForgeTooCool !Int
  | ForgeAlreadyCool !Int
  deriving stock (Eq, Ord, Show, Read)

data ForgeDelta
  = DeltaStabilize !Fragment
  | DeltaFuse !Fragment !Fragment !Constellation
  | DeltaBreak !Curse
  | DeltaCool !Int
  | DeltaEclipse
  deriving stock (Eq, Ord, Show, Read)

data ForgeExecutionBatch = ForgeExecutionBatch
  { febScheduledDeltaCount :: !Natural,
    febCommittedDeltaCount :: !Natural
  }
  deriving stock (Eq, Ord, Show, Read)

data ForgeEvidence = ForgeEvidence
  { feAppliedByLane :: !(Map ForgeLane Natural),
    feForged :: !(Set Constellation),
    feCursesBroken :: !(Set Curse),
    feHeatDelta :: !Int,
    feHeatAfter :: !Int,
    feCursesAfter :: !(Set Curse),
    feCommittedProgress :: !Bool,
    feFixedPoint :: !Bool,
    feExecution :: ![ForgeExecutionBatch]
  }
  deriving stock (Eq, Show, Read)

data ForgeError
  = ForgeInvariantViolation !String
  deriving stock (Eq, Ord, Show, Read)

data StarForgeDomain

instance Domain StarForgeDomain where
  type PhaseKey StarForgeDomain = ForgePhase
  type RuleKey StarForgeDomain = ForgeLane
  type SupportKey StarForgeDomain = ForgeSupport

instance KnownPhase StarForgeDomain "stabilizeFirst" where
  knownPhaseKey =
    StabilizeFirstPhase

instance KnownPhase StarForgeDomain "primeFusion" where
  knownPhaseKey =
    PrimeFusionPhase

instance KnownPhase StarForgeDomain "invokeEclipse" where
  knownPhaseKey =
    InvokeEclipsePhase

instance KnownPhase StarForgeDomain "normalForge" where
  knownPhaseKey =
    NormalForgePhase

type ForgePlan =
  Plan ForgeState ForgeGroup ForgeMatch ForgeGateTrace ForgeEvidence

type ForgeReport =
  EngineReport ForgeState ForgeGroup ForgeGateTrace ForgeEvidence

forgeTargetHeat :: Int
forgeTargetHeat =
  4

initialForgeState :: ForgeState
initialForgeState =
  ForgeState
    { fsFragments = Set.empty,
      fsConstellations = Set.empty,
      fsCurses = Set.singleton MirrorHex,
      fsHeat = 6
    }
