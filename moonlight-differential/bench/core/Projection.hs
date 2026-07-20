module Projection where

import Control.DeepSeq (NFData (..))
import Data.Foldable qualified as Foldable
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Delta.Epoch (initialVersion)
import Moonlight.Differential.Projection.Delta qualified as ProjectionDelta
import Moonlight.Differential.Projection.Maintenance qualified as ProjectionMaintenance
import Moonlight.Differential.Projection.Propagation qualified as ProjectionPropagation
import Moonlight.Differential.Projection.Work qualified as ProjectionWork

type BenchProjectionDelta = ProjectionDelta.ProjectionDelta Int IntSet.IntSet
type BenchProjectionWork = ProjectionWork.ProjectionWork

newtype PreparedProjectionWork = PreparedProjectionWork (Map.Map Int BenchProjectionWork)

instance NFData PreparedProjectionWork where
  rnf (PreparedProjectionWork workByQuery) =
    Map.size workByQuery `seq` ()

newtype PreparedProjectionDelta = PreparedProjectionDelta [BenchProjectionDelta]

instance NFData PreparedProjectionDelta where
  rnf (PreparedProjectionDelta deltas) =
    length deltas `seq` ()

data PreparedProjectionMaintenance = PreparedProjectionMaintenance
  { preparedProjectionMaintenanceWork :: !(Map.Map Int BenchProjectionWork),
    preparedProjectionMaintenancePlans :: !(Set Int)
  }

instance NFData PreparedProjectionMaintenance where
  rnf preparedCase =
    Map.size (preparedProjectionMaintenanceWork preparedCase)
      `seq` Set.size (preparedProjectionMaintenancePlans preparedCase)
      `seq` ()

type BenchProjectionState =
  ProjectionPropagation.ProjectionPropagationState
    (Map.Map Int IntSet.IntSet)
    ()
    ()
    Int
    IntSet.IntSet
    ()
    ()
    ()

data PreparedProjectionPropagation = PreparedProjectionPropagation
  { preparedProjectionDirtyKeys :: !IntSet.IntSet,
    preparedProjectionContexts :: ![Int],
    preparedProjectionState :: !BenchProjectionState
  }

instance NFData PreparedProjectionPropagation where
  rnf preparedCase =
    IntSet.size (preparedProjectionDirtyKeys preparedCase)
      `seq` length (preparedProjectionContexts preparedCase)
      `seq` Map.size (ProjectionPropagation.cpsContextGraph (preparedProjectionState preparedCase))
      `seq` ()

projectionSizes :: [Int]
projectionSizes =
  [128, 512]

projectionWorkCase :: Int -> PreparedProjectionWork
projectionWorkCase size =
  PreparedProjectionWork
    ( Map.fromAscList
        ( fmap
            (\query -> (query, projectionWorkAt query))
            [0 .. size - 1]
        )
    )

projectionDeltaCase :: Int -> PreparedProjectionDelta
projectionDeltaCase size =
  PreparedProjectionDelta
    ( fmap
        ( \query ->
            ProjectionDelta.projectQuery query (projectionDirtySet query)
              <> ProjectionDelta.pruneQuery query (projectionDirtySet (query + 1))
              <> ProjectionDelta.restrictQuery query (projectionDirtySet (query + 2))
              <> ProjectionDelta.invalidationOnly (projectionDirtySet query)
        )
        [0 .. size - 1]
    )

projectionMaintenanceCase :: Int -> PreparedProjectionMaintenance
projectionMaintenanceCase size =
  PreparedProjectionMaintenance
    { preparedProjectionMaintenanceWork =
        let PreparedProjectionWork workByQuery = projectionWorkCase size
         in workByQuery,
      preparedProjectionMaintenancePlans =
        Set.fromAscList [query | query <- [0 .. size - 1], query `mod` 5 /= 0]
    }

projectionPropagationCase :: Int -> PreparedProjectionPropagation
projectionPropagationCase size =
  PreparedProjectionPropagation
    { preparedProjectionDirtyKeys = IntSet.fromDistinctAscList [0, 3 .. 127],
      preparedProjectionContexts = contexts,
      preparedProjectionState =
        ProjectionPropagation.ProjectionPropagationState
          { ProjectionPropagation.cpsContextGraph =
              Map.fromAscList (fmap (\context -> (context, projectionSupportFor context)) contexts),
            ProjectionPropagation.cpsCanonEpoch = Nothing,
            ProjectionPropagation.cpsRepairIndex = Nothing,
            ProjectionPropagation.cpsImpacted = IntSet.empty,
            ProjectionPropagation.cpsDirtyResults = IntSet.empty,
            ProjectionPropagation.cpsVersion = initialVersion,
            ProjectionPropagation.cpsPruneSignals = Map.empty,
            ProjectionPropagation.cpsContextViews =
              Map.fromAscList (fmap (\context -> (context, projectionSupportFor context)) contexts),
            ProjectionPropagation.cpsBaseToCtx =
              ProjectionPropagation.buildDependencyIndexFromSupports contexts projectionSupportFor,
            ProjectionPropagation.cpsEpochDelta = Nothing,
            ProjectionPropagation.cpsAnalysis = (),
            ProjectionPropagation.cpsCanonicalize = ()
          }
    }
  where
    contexts =
      [0 .. size - 1]

projectionWorkAt :: Int -> BenchProjectionWork
projectionWorkAt query =
  ProjectionWork.projectKeys (projectionDirtySet query)
    <> ProjectionWork.pruneKeys (projectionDirtySet (query + 1))
    <> ProjectionWork.restrictKeys (projectionDirtySet (query + 2))
    <> if query `mod` 17 == 0 then ProjectionWork.bootstrapProjection else mempty

projectionDirtySet :: Int -> IntSet.IntSet
projectionDirtySet seed =
  IntSet.fromList [seed `mod` 128, (seed * 3 + 1) `mod` 128, (seed * 5 + 2) `mod` 128]

projectionSupportFor :: Int -> IntSet.IntSet
projectionSupportFor context =
  IntSet.fromList [context `mod` 128, (context * 7 + 11) `mod` 128, (context * 13 + 17) `mod` 128]

projectionWorkWeight :: PreparedProjectionWork -> Int
projectionWorkWeight (PreparedProjectionWork workByQuery) =
  Foldable.foldl'
    ( \acc work ->
        acc
          + boolWeight (ProjectionWork.projectionWorkNeedsExecution False work)
          + IntSet.size (ProjectionWork.projectionWorkDirtyForPhase ProjectionWork.Project work)
          + IntSet.size (ProjectionWork.projectionWorkDirtyForPhase ProjectionWork.Prune work)
          + IntSet.size (ProjectionWork.projectionWorkDirtyForPhase ProjectionWork.Restrict work)
    )
    0
    (Map.elems workByQuery)

projectionDeltaComposeWeight :: PreparedProjectionDelta -> Int
projectionDeltaComposeWeight (PreparedProjectionDelta deltas) =
  Map.size (ProjectionDelta.projectionDeltaWork (Foldable.foldl' (<>) mempty deltas))

projectionMaintenanceWeight :: PreparedProjectionMaintenance -> Either String Int
projectionMaintenanceWeight preparedCase =
  case ProjectionMaintenance.runProjectionPhases lookupPlan lookupCurrent runPhase phases (preparedProjectionMaintenanceWork preparedCase) 0 of
    Left obstruction ->
      Left (show obstruction)
    Right resultValue ->
      Right (ProjectionMaintenance.pwrGraph resultValue + length (ProjectionMaintenance.pwrJobs resultValue))
  where
    phases :: [ProjectionWork.ProjectionPhase]
    phases =
      [ProjectionWork.Project, ProjectionWork.Prune, ProjectionWork.Restrict]

    lookupPlan :: Int -> Int -> Maybe Int
    lookupPlan query _graph =
      if Set.member query (preparedProjectionMaintenancePlans preparedCase)
        then Just query
        else Nothing

    lookupCurrent :: Int -> BenchProjectionWork -> Int -> BenchProjectionWork
    lookupCurrent _query projectionWork _graph =
      projectionWork

    runPhase ::
      ProjectionWork.ProjectionPhase ->
      Int ->
      BenchProjectionWork ->
      Int ->
      BenchProjectionWork ->
      Int ->
      Either () (ProjectionMaintenance.ProjectionPhaseResult Int ProjectionWork.ProjectionPhase Int BenchProjectionWork)
    runPhase phase query projectionWork plan current graph
      | plan == query && current == projectionWork =
          Right
            ProjectionMaintenance.ProjectionPhaseResult
              { ProjectionMaintenance.pprGraph = graph + 1,
                ProjectionMaintenance.pprJobs = [ProjectionMaintenance.projectionPhaseJob phase query projectionWork]
              }
      | otherwise =
          Left ()

projectionAffectedContextsWeight :: PreparedProjectionPropagation -> Int
projectionAffectedContextsWeight preparedCase =
  length
    ( ProjectionPropagation.affectedContextsForDirtyKeys
        (preparedProjectionDirtyKeys preparedCase)
        (ProjectionPropagation.cpsBaseToCtx (preparedProjectionState preparedCase))
    )

projectionCommitWeight :: PreparedProjectionPropagation -> Int
projectionCommitWeight preparedCase =
  IntSet.size
    ( ProjectionPropagation.cpsDirtyResults
        ( Foldable.foldl'
            commitContext
            (preparedProjectionState preparedCase)
            (preparedProjectionContexts preparedCase)
        )
    )
  where
    commitContext :: BenchProjectionState -> Int -> BenchProjectionState
    commitContext stateValue context =
      ProjectionPropagation.cpcState
        ( ProjectionPropagation.commitProjection
            Map.insert
            (\_context _site section -> section)
            id
            (\contextValue site -> Map.findWithDefault IntSet.empty contextValue site)
            context
            (projectionSupportFor (context + 1))
            (projectionSupportFor (context + 2))
            (projectionDirtySet context)
            True
            stateValue
        )

boolWeight :: Bool -> Int
boolWeight value =
  if value then 1 else 0
