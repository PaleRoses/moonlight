{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Differential.Effect.Harness.Projection
  ( GeneratedProjectionCommitState,
    GeneratedProjectionMaintenanceResult,
    ProjectionMaintenanceTrace,
    TestProjectionCommit (..),
    TestProjectionMaintenanceRun (..),
    affectedContextsForCommitKeys,
    expectedAffectedContextsForCommitKeys,
    expectedProjectionCommitDirtyResults,
    expectedProjectionCommitSite,
    expectedProjectionCommitViews,
    expectedProjectionMaintenanceJobs,
    expectedProjectionMaintenanceTrace,
    makeProjectionDelta,
    makeProjectionWork,
    projectionCommitMatchesSupportRecomputation,
    projectionDeltaObeysSharedActionAlgebra,
    projectionMaintenanceMatchesRecomputation,
    projectionWorkObeysSharedActionAlgebra,
    runGeneratedProjectionCommit,
    runGeneratedProjectionMaintenance,
  )
where

import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Delta.Epoch
  ( initialVersion,
  )
import Moonlight.Differential.Delta
  ( deltaApplyMany,
    deltaCombineMany,
    deltaIsEmpty,
  )
import Moonlight.Differential.Projection.Delta
  ( ProjectionDelta,
    invalidationOnly,
    projectQuery,
    projectionDeltaOps,
    projectionOnly,
    pruneQuery,
    restrictQuery,
  )
import Moonlight.Differential.Projection.Maintenance qualified as ProjectionMaintenance
import Moonlight.Differential.Projection.Propagation
  ( ProjectionCommit (..),
    ProjectionPropagationState (..),
    ProjectionSupportIndex,
    affectedContextsForDirtyKeys,
    buildDependencyIndexFromSupports,
    commitProjection,
  )
import Moonlight.Differential.Projection.Work
  ( ProjectionPhase (..),
    ProjectionWork,
    bootstrapProjection,
    projectKeys,
    projectionWorkBootstrap,
    projectionWorkDeltaOps,
    projectionWorkDirtyForPhase,
    pruneKeys,
    restrictKeys,
  )

data TestProjectionMaintenanceRun = TestProjectionMaintenanceRun
  { tpmrPhases :: ![ProjectionPhase],
    tpmrPlannedQueries :: !(Set Int),
    tpmrWorkByQuery :: !(Map.Map Int ProjectionWork)
  }
  deriving stock (Eq, Show)

data TestProjectionCommit = TestProjectionCommit
  { tpcContext :: !Int,
    tpcOldView :: !(Maybe IntSet.IntSet),
    tpcDefaultSupport :: !IntSet.IntSet,
    tpcNextView :: !IntSet.IntSet,
    tpcRawNextSection :: !IntSet.IntSet,
    tpcDirtyResults :: !IntSet.IntSet,
    tpcInitialDirtyResults :: !IntSet.IntSet,
    tpcSectionChanged :: !Bool
  }
  deriving stock (Eq, Show)

makeProjectionWork :: Bool -> IntSet.IntSet -> IntSet.IntSet -> IntSet.IntSet -> ProjectionWork
makeProjectionWork bootstrapDirty projectDirty pruneDirty restrictDirty =
  (if bootstrapDirty then bootstrapProjection else mempty)
    <> projectKeys projectDirty
    <> pruneKeys pruneDirty
    <> restrictKeys restrictDirty

makeProjectionDelta ::
  IntSet.IntSet ->
  IntSet.IntSet ->
  IntSet.IntSet ->
  Int ->
  IntSet.IntSet ->
  Int ->
  IntSet.IntSet ->
  Int ->
  IntSet.IntSet ->
  ProjectionDelta Int IntSet.IntSet
makeProjectionDelta dirtyBase dirtyResult invalidation queryA projectDirty queryB pruneDirty queryC restrictDirty =
  projectionOnly dirtyBase dirtyResult
    <> invalidationOnly invalidation
    <> projectQuery queryA projectDirty
    <> pruneQuery queryB pruneDirty
    <> restrictQuery queryC restrictDirty

projectionWorkObeysSharedActionAlgebra :: ProjectionWork -> ProjectionWork -> ProjectionWork -> Bool
projectionWorkObeysSharedActionAlgebra left middle right =
  deltaCombineMany projectionWorkDeltaOps [left, middle, right] == left <> middle <> right
    && deltaApplyMany projectionWorkDeltaOps [middle, right] left == left <> middle <> right
    && deltaIsEmpty projectionWorkDeltaOps left == (left == mempty)

projectionDeltaObeysSharedActionAlgebra ::
  ProjectionDelta Int IntSet.IntSet ->
  ProjectionDelta Int IntSet.IntSet ->
  ProjectionDelta Int IntSet.IntSet ->
  Bool
projectionDeltaObeysSharedActionAlgebra left middle right =
  deltaCombineMany projectionDeltaOps [left, middle, right] == left <> middle <> right
    && deltaApplyMany projectionDeltaOps [middle, right] left == left <> middle <> right
    && deltaIsEmpty projectionDeltaOps left == (left == mempty)

projectionMaintenanceMatchesRecomputation :: TestProjectionMaintenanceRun -> Either String Bool
projectionMaintenanceMatchesRecomputation runValue =
  fmap
    ( \resultValue ->
        ProjectionMaintenance.pwrGraph resultValue == expectedProjectionMaintenanceTrace runValue
          && ProjectionMaintenance.pwrJobs resultValue == expectedProjectionMaintenanceJobs runValue
    )
    (runGeneratedProjectionMaintenance runValue)

projectionCommitMatchesSupportRecomputation :: TestProjectionCommit -> Bool
projectionCommitMatchesSupportRecomputation commitValue =
  cpcSectionChanged projectionCommitValue == tpcSectionChanged commitValue
    && cpsContextGraph committedState == expectedProjectionCommitSite commitValue
    && cpsContextViews committedState == expectedProjectionCommitViews commitValue
    && cpsDirtyResults committedState == expectedProjectionCommitDirtyResults commitValue
    && affectedContextsForCommitKeys committedState commitValue == expectedAffectedContextsForCommitKeys commitValue
  where
    projectionCommitValue =
      runGeneratedProjectionCommit commitValue

    committedState =
      cpcState projectionCommitValue

type ProjectionMaintenanceTrace = [(ProjectionPhase, Int)]

type GeneratedProjectionMaintenanceResult =
  ProjectionMaintenance.ProjectionWorkResult ProjectionMaintenanceTrace ProjectionPhase Int ProjectionWork

runGeneratedProjectionMaintenance ::
  TestProjectionMaintenanceRun ->
  Either String GeneratedProjectionMaintenanceResult
runGeneratedProjectionMaintenance runValue =
  ProjectionMaintenance.runProjectionPhases
    (\query _trace -> if Set.member query (tpmrPlannedQueries runValue) then Just query else Nothing)
    (\_query projectionWork _trace -> projectionWork)
    ( \phase query projectionWork plan current traceValue ->
        if plan == query && current == projectionWork
          then
            Right
              ProjectionMaintenance.ProjectionPhaseResult
                { ProjectionMaintenance.pprGraph = traceValue <> [(phase, query)],
                  ProjectionMaintenance.pprJobs = [ProjectionMaintenance.projectionPhaseJob phase query projectionWork]
                }
          else Left "projection maintenance supplied mismatched plan/current values"
    )
    (tpmrPhases runValue)
    (tpmrWorkByQuery runValue)
    []

expectedProjectionMaintenanceTrace ::
  TestProjectionMaintenanceRun ->
  ProjectionMaintenanceTrace
expectedProjectionMaintenanceTrace =
  fmap (\(phase, query, _work) -> (phase, query)) . expectedProjectionMaintenanceExecutions

expectedProjectionMaintenanceJobs ::
  TestProjectionMaintenanceRun ->
  [ProjectionMaintenance.ProjectionJob ProjectionPhase Int ProjectionWork]
expectedProjectionMaintenanceJobs =
  fmap
    ( \(phase, query, projectionWork) ->
        ProjectionMaintenance.projectionPhaseJob phase query projectionWork
    )
    . expectedProjectionMaintenanceExecutions

expectedProjectionMaintenanceExecutions ::
  TestProjectionMaintenanceRun ->
  [(ProjectionPhase, Int, ProjectionWork)]
expectedProjectionMaintenanceExecutions runValue =
  foldMap
    ( \phase ->
        foldMap
          ( \(query, projectionWork) ->
              if Set.member query (tpmrPlannedQueries runValue)
                && projectionWorkQueuedForPhaseOracle phase projectionWork
                then [(phase, query, projectionWork)]
                else []
          )
          (Map.toAscList (tpmrWorkByQuery runValue))
    )
    (List.nub (tpmrPhases runValue))

projectionWorkQueuedForPhaseOracle :: ProjectionPhase -> ProjectionWork -> Bool
projectionWorkQueuedForPhaseOracle phase projectionWork =
  case phase of
    Project ->
      projectionWorkBootstrap projectionWork
        || not (IntSet.null (projectionWorkDirtyForPhase Project projectionWork))
    Prune ->
      not (IntSet.null (projectionWorkDirtyForPhase Prune projectionWork))
    Restrict ->
      not (IntSet.null (projectionWorkDirtyForPhase Restrict projectionWork))

type GeneratedProjectionCommitState =
  ProjectionPropagationState
    (Map.Map Int IntSet.IntSet)
    ()
    ()
    Int
    IntSet.IntSet
    ()
    ()
    ()

runGeneratedProjectionCommit ::
  TestProjectionCommit ->
  ProjectionCommit GeneratedProjectionCommitState
runGeneratedProjectionCommit commitValue =
  commitProjection
    Map.insert
    (\contextValue site section -> IntSet.union section (defaultProjectionSupport contextValue site))
    id
    defaultProjectionSupport
    (tpcContext commitValue)
    (tpcNextView commitValue)
    (tpcRawNextSection commitValue)
    (tpcDirtyResults commitValue)
    (tpcSectionChanged commitValue)
    (initialProjectionCommitState commitValue)

initialProjectionCommitState ::
  TestProjectionCommit ->
  GeneratedProjectionCommitState
initialProjectionCommitState commitValue =
  ProjectionPropagationState
    { cpsContextGraph = initialProjectionCommitSite commitValue,
      cpsCanonEpoch = Nothing,
      cpsRepairIndex = Nothing,
      cpsImpacted = IntSet.empty,
      cpsDirtyResults = tpcInitialDirtyResults commitValue,
      cpsVersion = initialVersion,
      cpsPruneSignals = Map.empty,
      cpsContextViews = initialProjectionCommitViews commitValue,
      cpsBaseToCtx =
        buildDependencyIndexFromSupports
          [tpcContext commitValue]
          (const (initialProjectionCommitSupport commitValue)),
      cpsEpochDelta = Nothing,
      cpsAnalysis = (),
      cpsCanonicalize = ()
    }

initialProjectionCommitSite ::
  TestProjectionCommit ->
  Map.Map Int IntSet.IntSet
initialProjectionCommitSite commitValue =
  Map.singleton (tpcContext commitValue) (tpcDefaultSupport commitValue)

initialProjectionCommitViews ::
  TestProjectionCommit ->
  Map.Map Int IntSet.IntSet
initialProjectionCommitViews commitValue =
  maybe
    Map.empty
    (Map.singleton (tpcContext commitValue))
    (tpcOldView commitValue)

initialProjectionCommitSupport :: TestProjectionCommit -> IntSet.IntSet
initialProjectionCommitSupport commitValue =
  maybe (tpcDefaultSupport commitValue) id (tpcOldView commitValue)

defaultProjectionSupport :: Int -> Map.Map Int IntSet.IntSet -> IntSet.IntSet
defaultProjectionSupport contextValue site =
  Map.findWithDefault IntSet.empty contextValue site

expectedProjectionCommitSite ::
  TestProjectionCommit ->
  Map.Map Int IntSet.IntSet
expectedProjectionCommitSite commitValue =
  Map.insert
    (tpcContext commitValue)
    (IntSet.union (tpcRawNextSection commitValue) (tpcDefaultSupport commitValue))
    (initialProjectionCommitSite commitValue)

expectedProjectionCommitViews ::
  TestProjectionCommit ->
  Map.Map Int IntSet.IntSet
expectedProjectionCommitViews commitValue =
  Map.insert
    (tpcContext commitValue)
    (tpcNextView commitValue)
    (initialProjectionCommitViews commitValue)

expectedProjectionCommitDirtyResults :: TestProjectionCommit -> IntSet.IntSet
expectedProjectionCommitDirtyResults commitValue =
  IntSet.union (tpcInitialDirtyResults commitValue) (tpcDirtyResults commitValue)

affectedContextsForCommitKeys ::
  GeneratedProjectionCommitState ->
  TestProjectionCommit ->
  Map.Map Int [Int]
affectedContextsForCommitKeys stateValue =
  affectedContextsByKey (cpsBaseToCtx stateValue) . projectionCommitObservedKeys

expectedAffectedContextsForCommitKeys ::
  TestProjectionCommit ->
  Map.Map Int [Int]
expectedAffectedContextsForCommitKeys commitValue =
  Map.fromList
    [ (key, if IntSet.member key (tpcNextView commitValue) then [tpcContext commitValue] else [])
    | key <- IntSet.toAscList (projectionCommitObservedKeys commitValue)
    ]

affectedContextsByKey ::
  ProjectionSupportIndex Int ->
  IntSet.IntSet ->
  Map.Map Int [Int]
affectedContextsByKey supportIndex keys =
  Map.fromList
    [ (key, affectedContextsForDirtyKeys (IntSet.singleton key) supportIndex)
    | key <- IntSet.toAscList keys
    ]

projectionCommitObservedKeys :: TestProjectionCommit -> IntSet.IntSet
projectionCommitObservedKeys commitValue =
  IntSet.unions
    [ initialProjectionCommitSupport commitValue,
      tpcNextView commitValue,
      tpcRawNextSection commitValue,
      tpcDirtyResults commitValue,
      tpcInitialDirtyResults commitValue
    ]
