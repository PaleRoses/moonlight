{-# LANGUAGE BangPatterns #-}

module Moonlight.Control.StarForge.Engine
  ( runStarForgeCampaign,
    forgeParallelExecution,
  )
where

import Control.Scheduler
  ( Comp (ParN),
  )
import Data.Foldable qualified as Foldable
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Numeric.Natural
  ( Natural,
  )

import Moonlight.Control.Candidate
  ( CandidateSpace,
    finiteCandidateSpace,
  )
import Moonlight.Control.Engine.Parallel
  ( MatchExecution (..),
    ParallelMatchExecution (..),
    applyScheduledBatchDeltas,
  )
import Moonlight.Control.Engine.Run
  ( EngineFailure (..),
    runEngine,
  )
import Moonlight.Control.Engine.Spec
  ( EngineSpec,
    Validated,
  )
import Moonlight.Control.Engine.Work
  ( ApplyResult (..),
    WorkSource (..),
    applyResult,
  )
import Moonlight.Control.Schedule
  ( ScheduleGroup (..),
  )
import Moonlight.Control.StarForge.Model
  ( Constellation (..),
    Curse (..),
    ForgeDelta (..),
    ForgeError (..),
    ForgeEvidence (..),
    ForgeExecutionBatch (..),
    ForgeGroup,
    ForgeLane (..),
    ForgeMatch (..),
    ForgeReport,
    ForgeState (..),
    ForgeSupport (..),
    Fragment (..),
    forgeTargetHeat,
    initialForgeState,
  )
import Moonlight.Control.StarForge.Plan
  ( starForgePlan,
  )

runStarForgeCampaign ::
  MatchExecution ->
  EngineSpec Validated ->
  IO (Either (EngineFailure ForgeError ForgeGroup) ForgeReport)
runStarForgeCampaign execution spec =
  runEngine
    (starForgePlan spec)
    (forgeSource execution)
    initialForgeState

forgeSource ::
  MatchExecution ->
  WorkSource IO ForgeState ForgeState ForgeGroup ForgeMatch ForgeEvidence ForgeError
forgeSource execution =
  WorkSource
    { wsView = id,
      wsCandidateSpace = pure . forgeCandidateSpace,
      wsApplyScheduled =
        \scheduledBatch state ->
          applyScheduledBatchDeltas
            execution
            runForgeMatchDelta
            mergeForgeDeltas
            scheduledBatch
            state,
      wsProgressed = feCommittedProgress
    }

forgeParallelExecution ::
  MatchExecution
forgeParallelExecution =
  ParallelMatches
    ParallelMatchExecution
      { pmeComp = ParN 2,
        pmeMinBatchSize = 1,
        pmeChunkSize = 1
      }

forgeCandidateSpace ::
  ForgeState ->
  CandidateSpace IO ForgeGroup () ForgeMatch
forgeCandidateSpace _state =
  finiteCandidateSpace
    [ (SupportedGroup StabilizeFragment (FragmentSupport WolfFang), [Stabilize WolfFang]),
      (SupportedGroup StabilizeFragment (FragmentSupport StarShard), [Stabilize StarShard]),
      (SupportedGroup StabilizeFragment (FragmentSupport GlassShard), [Stabilize GlassShard]),
      (RuleGroup FusePair, fuseMatches),
      (SupportedGroup BreakCurse (CurseSupport MirrorHex), [Break MirrorHex]),
      (SupportedGroup CoolForge HeatSupport, [Cool 4]),
      (SupportedGroup InvokeEclipse EclipseSupport, [Eclipse])
    ]

fuseMatches :: [ForgeMatch]
fuseMatches =
  [ Fuse WolfFang StarShard WolfStar,
    Fuse GlassShard StarShard GlassCrown,
    Fuse WolfFang GlassShard AshMirror
  ]

runForgeMatchDelta ::
  ForgeMatch ->
  IO (Either ForgeError ForgeDelta)
runForgeMatchDelta match =
  pure
    ( Right
        ( case match of
            Stabilize fragment ->
              DeltaStabilize fragment
            Fuse leftFragment rightFragment constellation ->
              DeltaFuse leftFragment rightFragment constellation
            Break curse ->
              DeltaBreak curse
            Cool amount ->
              DeltaCool amount
            Eclipse ->
              DeltaEclipse
        )
    )

data ForgeMergeAcc = ForgeMergeAcc
  { fmaState :: !ForgeState,
    fmaAppliedByLane :: !(Map ForgeLane Natural),
    fmaForged :: !(Set.Set Constellation),
    fmaCursesBroken :: !(Set.Set Curse),
    fmaCommittedCount :: !Natural,
    fmaCommittedProgress :: !Bool
  }
  deriving stock (Eq, Show)

mergeForgeDeltas ::
  ForgeState ->
  [ForgeDelta] ->
  Either ForgeError (ApplyResult ForgeState ForgeEvidence)
mergeForgeDeltas initialState deltas =
  let !finalAcc =
        Foldable.foldl' applyForgeDelta initialMergeAcc deltas
      !nextState =
        fmaState finalAcc
      !scheduledDeltaCount =
        lengthNatural deltas
      !evidence =
        ForgeEvidence
          { feAppliedByLane = fmaAppliedByLane finalAcc,
            feForged = fmaForged finalAcc,
            feCursesBroken = fmaCursesBroken finalAcc,
            feHeatDelta = fsHeat nextState - fsHeat initialState,
            feHeatAfter = fsHeat nextState,
            feCursesAfter = fsCurses nextState,
            feCommittedProgress = fmaCommittedProgress finalAcc,
            feFixedPoint = forgeStateFixedPoint nextState,
            feExecution =
              [ ForgeExecutionBatch
                  { febScheduledDeltaCount = scheduledDeltaCount,
                    febCommittedDeltaCount = fmaCommittedCount finalAcc
                  }
              ]
          }
   in Right
        (applyResult nextState evidence (fromIntegral (fmaCommittedCount finalAcc)))
  where
    initialMergeAcc =
      ForgeMergeAcc
        { fmaState = initialState,
          fmaAppliedByLane = Map.empty,
          fmaForged = Set.empty,
          fmaCursesBroken = Set.empty,
          fmaCommittedCount = 0,
          fmaCommittedProgress = False
        }

applyForgeDelta ::
  ForgeMergeAcc ->
  ForgeDelta ->
  ForgeMergeAcc
applyForgeDelta acc delta =
  case delta of
    DeltaStabilize fragment ->
      if Set.member fragment (fsFragments state)
        then acc
        else
          commitLane StabilizeFragment
            acc
              { fmaState =
                  state
                    { fsFragments = Set.insert fragment (fsFragments state)
                    }
              }
    DeltaFuse leftFragment rightFragment constellation ->
      if Set.member leftFragment (fsFragments state)
        && Set.member rightFragment (fsFragments state)
        && Set.notMember constellation (fsConstellations state)
        then
          commitLane FusePair
            acc
              { fmaState =
                  state
                    { fsConstellations =
                        Set.insert constellation (fsConstellations state),
                      fsHeat = fsHeat state + 1
                    },
                fmaForged = Set.insert constellation (fmaForged acc)
              }
        else acc
    DeltaBreak curse ->
      if Set.member curse (fsCurses state)
        then
          commitLane BreakCurse
            acc
              { fmaState =
                  state
                    { fsCurses = Set.delete curse (fsCurses state)
                    },
                fmaCursesBroken = Set.insert curse (fmaCursesBroken acc)
              }
        else acc
    DeltaCool amount ->
      let !nextHeat =
            max forgeTargetHeat (fsHeat state - max 0 amount)
       in if nextHeat < fsHeat state
            then
              commitLane CoolForge
                acc
                  { fmaState =
                      state
                        { fsHeat = nextHeat
                        }
                  }
            else acc
    DeltaEclipse ->
      acc
        { fmaState =
            state
              { fsCurses = Set.insert EclipseShadow (fsCurses state),
                fsHeat = fsHeat state + 99
              }
        }
  where
    state =
      fmaState acc

commitLane ::
  ForgeLane ->
  ForgeMergeAcc ->
  ForgeMergeAcc
commitLane lane acc =
  acc
    { fmaAppliedByLane =
        Map.insertWith (+) lane 1 (fmaAppliedByLane acc),
      fmaCommittedCount = fmaCommittedCount acc + 1,
      fmaCommittedProgress = True
    }

forgeStateFixedPoint ::
  ForgeState ->
  Bool
forgeStateFixedPoint state =
  Set.fromList [WolfStar, GlassCrown] `Set.isSubsetOf` fsConstellations state
    && Set.null (fsCurses state)
    && fsHeat state <= forgeTargetHeat

lengthNatural :: [value] -> Natural
lengthNatural = Foldable.foldl' (\count _ -> count + 1) 0
