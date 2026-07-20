{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Plan.Rewrite.Internal.Saturation
  ( rewritePlanSaturationState,
  )
where

import Data.Kind
  ( Type,
  )
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.EGraph.Pure.Rebuild
  ( rebuild,
  )
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    eGraphNodeCount,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.Canonicalization
  ( canonicalizeDirtyClassKeys,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.Rules
  ( applyPlanRewriteRound,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.Types
  ( PlanAnalysis,
    PlanSaturationError (..),
    PlanSaturationState (..),
  )
import Moonlight.Flow.Plan.Rewrite.Node
  ( PlanNode,
  )
import Moonlight.Flow.Plan.Rewrite.Proof
  ( PlanEquivalenceStep,
    PlanRewriteSystem,
  )
import Moonlight.Saturation.Core
  ( ApplyOutcome (..),
    RebuildOutcome (..),
    RoundPlan (..),
    SaturationBudget (..),
    SaturationKernel (..),
    SaturationRun (..),
    SaturationTermination (..),
    runSaturation,
  )

type PlanRewriteRunState :: Type
data PlanRewriteRunState = PlanRewriteRunState
  { prrsGraph :: !(EGraph PlanNode PlanAnalysis),
    prrsDirtyClassKeys :: !IntSet,
    prrsIteration :: !Int,
    prrsSteps :: ![PlanEquivalenceStep]
  }

type PlanRewriteRound :: Type
data PlanRewriteRound = PlanRewriteRound
  { prrInputState :: !PlanRewriteRunState,
    prrGraph :: !(EGraph PlanNode PlanAnalysis),
    prrDirtyClassKeys :: !IntSet,
    prrSteps :: ![PlanEquivalenceStep]
  }

rewritePlanSaturationState ::
  SaturationBudget ->
  PlanRewriteSystem ->
  PlanSaturationState ->
  Either PlanSaturationError (PlanSaturationState, [PlanEquivalenceStep])
rewritePlanSaturationState rewriteBudget rewriteSystem state = do
  report <-
    runSaturation
      rewriteBudget
      (planRewriteKernel rewriteSystem)
      PlanRewriteRunState
        { prrsGraph = pssGraph state,
          prrsDirtyClassKeys = pssDirtyClassKeys state,
          prrsIteration = 0,
          prrsSteps = []
        }
  let finalRunState =
        srFinalState report
      finalState =
        state
          { pssGraph = prrsGraph finalRunState,
            pssDirtyClassKeys = prrsDirtyClassKeys finalRunState
          }
      finalSteps =
        reverse (prrsSteps finalRunState)
  case srTermination report of
    ReachedFixedPoint ->
      Right (finalState, finalSteps)
    ReachedGoal ->
      Right (finalState, finalSteps)
    HitIterationLimit ->
      Left (PlanSaturationIterationLimit (sbMaxIterations rewriteBudget))
    HitNodeLimit ->
      Left (PlanSaturationNodeLimit (sbMaxNodes rewriteBudget))

planRewriteKernel ::
  PlanRewriteSystem ->
  SaturationKernel PlanRewriteRunState PlanRewriteRound PlanEquivalenceStep [PlanEquivalenceStep] PlanSaturationError
planRewriteKernel rewriteSystem =
  SaturationKernel
    { skIterationOf = prrsIteration,
      skNodeCountOf = eGraphNodeCount . prrsGraph,
      skGoal = mempty,
      skPlanRound =
        \runState -> do
          (nextGraph, nextDirtyClassKeys, steps) <-
            applyPlanRewriteRound
              rewriteSystem
              (prrsDirtyClassKeys runState)
              (prrsGraph runState)
          let roundValue =
                PlanRewriteRound
                  { prrInputState = runState,
                    prrGraph = nextGraph,
                    prrDirtyClassKeys = nextDirtyClassKeys,
                    prrSteps = steps
                  }
              applyState =
                (prrInputState roundValue)
                  { prrsGraph = prrGraph roundValue,
                    prrsDirtyClassKeys = prrDirtyClassKeys roundValue
                  }
          case NonEmpty.nonEmpty steps of
            Just scheduledSteps ->
              Right (ApplyRound roundValue applyState scheduledSteps)
            Nothing ->
              Right
                ( StopRound
                    ( (prrInputState roundValue)
                        { prrsDirtyClassKeys = IntSet.empty
                        }
                    )
                ),
      skApply =
        \steps runState ->
          Right
            ApplyOutcome
              { aoState = runState,
                aoEffect = NonEmpty.toList steps
              },
      skRebuild =
        \roundValue _ runState ->
          let rebuiltGraph =
                rebuild (prrsGraph runState)
              rebuiltDirtyClassKeys =
                canonicalizeDirtyClassKeys rebuiltGraph (prrsDirtyClassKeys runState)
           in Right
                RebuildOutcome
                  { roRound = roundValue,
                    roState =
                      runState
                        { prrsGraph = rebuiltGraph,
                          prrsDirtyClassKeys = rebuiltDirtyClassKeys
                        }
                  },
      skCommit =
        \_ steps runState ->
          runState
            { prrsIteration = prrsIteration runState + 1,
              prrsSteps = List.foldl' (flip (:)) (prrsSteps runState) steps
            },
      skConverged = const (const False)
    }
