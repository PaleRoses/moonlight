module Moonlight.Differential.Projection.Maintenance
  ( ProjectionJob (..),
    ProjectionWorkResult (..),
    ProjectionPhaseResult (..),
    projectionPhaseJob,
    projectionNeighborJob,
    projectionJobContext,
    projectionJobDelta,
    runProjectionPhases,
  )
where

import Control.Monad (foldM)
import Data.Foldable (toList)
import Data.IntSet qualified as IntSet
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence ((><))
import Data.Sequence qualified as Seq
import Moonlight.Differential.Projection.Work
  ( ProjectionPhase (..),
    ProjectionWork,
    projectionWorkBootstrap,
    projectionWorkDirtyForPhase,
  )

data ProjectionJob direction ctx delta
  = ProjectionPhaseJob !ProjectionPhase !ctx !delta
  | ProjectionNeighborJob !direction !ctx !delta
  deriving stock (Eq, Show)

data ProjectionWorkResult graph direction ctx delta = ProjectionWorkResult
  { pwrGraph :: !graph,
    pwrJobs :: ![ProjectionJob direction ctx delta]
  }

data ProjectionPhaseResult graph direction ctx delta = ProjectionPhaseResult
  { pprGraph :: !graph,
    pprJobs :: ![ProjectionJob direction ctx delta]
  }

data ProjectionTraversalState graph direction ctx delta = ProjectionTraversalState
  { ptsGraph :: !graph,
    ptsJobs :: !(Seq.Seq (ProjectionJob direction ctx delta))
  }

projectionPhaseJob :: ProjectionPhase -> ctx -> delta -> ProjectionJob direction ctx delta
projectionPhaseJob phase =
  ProjectionPhaseJob phase
{-# INLINE projectionPhaseJob #-}

projectionNeighborJob :: direction -> ctx -> delta -> ProjectionJob direction ctx delta
projectionNeighborJob direction =
  ProjectionNeighborJob direction
{-# INLINE projectionNeighborJob #-}

projectionJobContext :: ProjectionJob direction ctx delta -> ctx
projectionJobContext job =
  case job of
    ProjectionPhaseJob _phase contextValue _deltaValue ->
      contextValue
    ProjectionNeighborJob _direction contextValue _deltaValue ->
      contextValue
{-# INLINE projectionJobContext #-}

projectionJobDelta :: ProjectionJob direction ctx delta -> delta
projectionJobDelta job =
  case job of
    ProjectionPhaseJob _phase _contextValue deltaValue ->
      deltaValue
    ProjectionNeighborJob _direction _contextValue deltaValue ->
      deltaValue
{-# INLINE projectionJobDelta #-}

runProjectionPhases ::
  forall query plan current graph err direction ctx delta.
  (query -> graph -> Maybe plan) ->
  (query -> ProjectionWork -> graph -> current) ->
  (ProjectionPhase -> query -> ProjectionWork -> plan -> current -> graph -> Either err (ProjectionPhaseResult graph direction ctx delta)) ->
  [ProjectionPhase] ->
  Map query ProjectionWork ->
  graph ->
  Either err (ProjectionWorkResult graph direction ctx delta)
runProjectionPhases lookupPlan lookupCurrent runPhase phases projectionWorkMap initialGraph =
  fmap
    ( \finalState ->
        ProjectionWorkResult
          { pwrGraph = ptsGraph finalState,
            pwrJobs = toList (ptsJobs finalState)
          }
    )
    ( foldM
        drainProjectionPhase
        ProjectionTraversalState
          { ptsGraph = initialGraph,
            ptsJobs = Seq.empty
          }
        (nub phases)
    )
  where
    projectionWorkEntries =
      Map.toAscList projectionWorkMap

    drainProjectionPhase stateValue projectionPhase =
      foldM
        (runProjectionQueryPhase projectionPhase)
        stateValue
        projectionWorkEntries

    runProjectionQueryPhase projectionPhase stateValue (queryId, projectionWork)
      | not (projectionWorkQueuedForPhase projectionPhase projectionWork) =
          Right stateValue
      | otherwise =
          case lookupPlan queryId (ptsGraph stateValue) of
            Nothing ->
              Right stateValue
            Just queryPlan -> do
              let currentLookup = lookupCurrent queryId projectionWork (ptsGraph stateValue)
              phaseResultValue <-
                runPhase
                  projectionPhase
                  queryId
                  projectionWork
                  queryPlan
                  currentLookup
                  (ptsGraph stateValue)
              Right
                stateValue
                  { ptsGraph = pprGraph phaseResultValue,
                    ptsJobs = ptsJobs stateValue >< Seq.fromList (pprJobs phaseResultValue)
                  }
{-# INLINE runProjectionPhases #-}

projectionWorkQueuedForPhase :: ProjectionPhase -> ProjectionWork -> Bool
projectionWorkQueuedForPhase phase projectionWork =
  case phase of
    Project ->
      projectionWorkBootstrap projectionWork
        || not (IntSet.null (projectionWorkDirtyForPhase Project projectionWork))
    Prune ->
      not (IntSet.null (projectionWorkDirtyForPhase Prune projectionWork))
    Restrict ->
      not (IntSet.null (projectionWorkDirtyForPhase Restrict projectionWork))
{-# INLINE projectionWorkQueuedForPhase #-}
