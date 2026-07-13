module ProjectionSpec
  ( tests,
  )
where

import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Functor.Identity
  ( Identity (..),
  )

import Moonlight.Delta.Epoch
  ( initialVersion,
  )
import Moonlight.Differential.Delta
  ( deltaApply,
    deltaApplyMany,
    deltaCombineMany,
    deltaIsEmpty,
  )
import Moonlight.Differential.Projection.Delta
  ( ProjectionDelta,
    projectQuery,
    projectionDeltaOps,
    projectionDeltaWork,
    projectionOnly,
  )
import Moonlight.Differential.Projection.Maintenance qualified as ProjectionMaintenance
import Moonlight.Differential.Projection.Propagation
  ( ProjectionCommit (..),
    ProjectionPropagationState (..),
    ProjectionSupportIndex,
    affectedContextsForDirtyKeys,
    buildDependencyIndexFromSupports,
    commitProjection,
    reindexContextSupport,
  )
import Moonlight.Differential.Projection.Work
  ( ProjectionPhase (..),
    ProjectionWork,
    bootstrapProjection,
    projectKeys,
    projectionWorkDeltaOps,
    pruneKeys,
    restrictKeys,
  )
import Moonlight.Differential.Runtime.Error
  ( RuntimeSettleBudgetExhausted (..),
  )
import Moonlight.Differential.Runtime.Settle
  ( RuntimeScopedSettleStep (..),
    RuntimeSettleStep (..),
    runRuntimeSettleLoop,
    runRuntimeSettleLoopScoped,
  )
import Moonlight.Differential.Time
  ( emptyRuntimeScope,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "projection laws"
    [ testCase "ProjectionWork uses the shared differential delta algebra" projectionWorkDeltaAlgebra,
      testCase "Projection maintenance drains project/prune/restrict in phase order" projectionMaintenanceDrainsInPhaseOrder,
      testCase "Projection maintenance skips empty phase work" projectionMaintenanceSkipsEmptyPhaseWork,
      testCase "Projection maintenance queues bootstrap project" projectionMaintenanceQueuesBootstrapProject,
      testCase "Projection maintenance coalesces duplicate query/phase work before traversal" projectionMaintenanceCoalescesSameQueryPhaseWork,
      testCase "Projection maintenance jobs expose typed constructors" projectionMaintenanceJobsExposeTypedConstructors,
      testCase "ProjectionDelta composes through the shared differential delta algebra" projectionDeltaAlgebra,
      testCase "Projection propagation reindexes dirty support" projectionPropagationReindexesSupport,
      testCase "Projection commit updates site, view, dirty results, and support index" projectionCommitUpdatesSupportIndex,
      testCase "Runtime settle loop drains cyclic residuals to quiescence" runtimeSettleLoopDrainsToQuiescence,
      testCase "Runtime settle loop reports typed budget exhaustion" runtimeSettleLoopReportsBudgetExhaustion
    ]

projectionWorkDeltaAlgebra :: IO ()
projectionWorkDeltaAlgebra = do
  let projectWork =
        projectKeys (IntSet.singleton 1)
      pruneWork =
        pruneKeys (IntSet.singleton 2)
      applied =
        deltaApply projectionWorkDeltaOps pruneWork projectWork
  assertEqual "work delta apply is work composition" (projectWork <> pruneWork) applied
  assertBool "empty work is empty under DeltaOps" (deltaIsEmpty projectionWorkDeltaOps mempty)
  assertBool "dirty work is not empty under DeltaOps" (not (deltaIsEmpty projectionWorkDeltaOps applied))

projectionMaintenanceDrainsInPhaseOrder :: IO ()
projectionMaintenanceDrainsInPhaseOrder =
  case runProjectionMaintenanceTrace [Project, Prune, Restrict] workByQuery of
    Left err ->
      assertFailure err
    Right resultValue ->
      assertEqual
        "phase scheduler drains all project work before prune, then restrict, with deterministic query order"
        [(Project, 1), (Project, 2), (Prune, 1), (Prune, 2), (Restrict, 1), (Restrict, 2)]
        (ProjectionMaintenance.pwrGraph resultValue)
  where
    allPhaseWork =
      projectKeys dirtyOne <> pruneKeys dirtyTwo <> restrictKeys dirtyThree

    workByQuery =
      Map.fromList [(2, allPhaseWork), (1, allPhaseWork)]

projectionMaintenanceSkipsEmptyPhaseWork :: IO ()
projectionMaintenanceSkipsEmptyPhaseWork =
  case runProjectionMaintenanceTrace [Project, Prune, Restrict] workByQuery of
    Left err ->
      assertFailure err
    Right resultValue ->
      assertEqual
        "only the dirty project phase is executed"
        [(Project, 1)]
        (ProjectionMaintenance.pwrGraph resultValue)
  where
    workByQuery =
      Map.singleton 1 (projectKeys dirtyOne)

projectionMaintenanceQueuesBootstrapProject :: IO ()
projectionMaintenanceQueuesBootstrapProject =
  case runProjectionMaintenanceTrace [Project, Prune, Restrict] workByQuery of
    Left err ->
      assertFailure err
    Right resultValue ->
      assertEqual
        "bootstrap queues project even without dirty project keys"
        [(Project, 1)]
        (ProjectionMaintenance.pwrGraph resultValue)
  where
    workByQuery =
      Map.singleton 1 bootstrapProjection

projectionMaintenanceCoalescesSameQueryPhaseWork :: IO ()
projectionMaintenanceCoalescesSameQueryPhaseWork =
  case runProjectionMaintenanceTrace [Project, Project] workByQuery of
    Left err ->
      assertFailure err
    Right resultValue ->
      assertEqual
        "duplicated phase/context work coalesces before traversal"
        [(Project, 1)]
        (ProjectionMaintenance.pwrGraph resultValue)
  where
    workByQuery =
      Map.singleton 1 (projectKeys dirtyOne)

projectionMaintenanceJobsExposeTypedConstructors :: IO ()
projectionMaintenanceJobsExposeTypedConstructors = do
  assertEqual
    "project phase is a typed phase job"
    (ProjectionMaintenance.ProjectionPhaseJob Project "ctx" ())
    (testProjectionJob Project)
  assertEqual
    "prune phase is a typed phase job"
    (ProjectionMaintenance.ProjectionPhaseJob Prune "ctx" ())
    (testProjectionJob Prune)
  assertEqual
    "restrict phase is a typed phase job"
    (ProjectionMaintenance.ProjectionPhaseJob Restrict "ctx" ())
    (testProjectionJob Restrict)
  assertEqual
    "neighbor propagation is a typed neighbor job"
    (ProjectionMaintenance.ProjectionNeighborJob "child" "ctx" ())
    testProjectionNeighborJob
  assertEqual
    "job context accessor is lane-agnostic"
    "ctx"
    (ProjectionMaintenance.projectionJobContext testProjectionNeighborJob)
  assertEqual
    "job delta accessor is lane-agnostic"
    ()
    (ProjectionMaintenance.projectionJobDelta testProjectionNeighborJob)

projectionDeltaAlgebra :: IO ()
projectionDeltaAlgebra = do
  let dirtyBase =
        IntSet.singleton 1
      dirtyResult =
        IntSet.singleton 2
      projectionDeltaValue =
        projectionOnly dirtyBase dirtyResult :: ProjectionDelta Int ()
      queryDelta =
        projectQuery 7 dirtyResult :: ProjectionDelta Int ()
      expected =
        projectionDeltaValue <> queryDelta
      combined =
        deltaCombineMany projectionDeltaOps [projectionDeltaValue, queryDelta]
      applied =
        deltaApplyMany projectionDeltaOps [projectionDeltaValue, queryDelta] mempty
      emptyProjectionDeltaValue =
        mempty :: ProjectionDelta Int ()
  assertEqual "delta combination follows the ProjectionDelta semigroup" expected combined
  assertEqual "delta application replays the same ordered composition" expected applied
  assertEqual "query work remains keyed by query id" (Map.singleton 7 (projectKeys dirtyResult)) (projectionDeltaWork combined)
  assertBool "empty projection delta is empty under DeltaOps" (deltaIsEmpty projectionDeltaOps emptyProjectionDeltaValue)
  assertBool "composed projection delta is not empty under DeltaOps" (not (deltaIsEmpty projectionDeltaOps combined))

projectionPropagationReindexesSupport :: IO ()
projectionPropagationReindexesSupport = do
  let state0 =
        testProjectionPropagationState
          (buildDependencyIndexFromSupports ["left", "right"] initialSupportForContext)
      state1 =
        reindexContextSupport "left" (IntSet.fromList [1, 2]) (IntSet.fromList [2, 4]) state0
      supportIndex =
        cpsBaseToCtx state1
  assertEqual "old exclusive support was removed" [] (affectedContextsForDirtyKeys (IntSet.singleton 1) supportIndex)
  assertEqual "shared retained support still reaches the context" ["left"] (affectedContextsForDirtyKeys (IntSet.singleton 2) supportIndex)
  assertEqual "new support reaches the context" ["left"] (affectedContextsForDirtyKeys (IntSet.singleton 4) supportIndex)
  assertEqual "untouched support remains indexed" ["right"] (affectedContextsForDirtyKeys (IntSet.singleton 3) supportIndex)

projectionCommitUpdatesSupportIndex :: IO ()
projectionCommitUpdatesSupportIndex = do
  let oldView =
        IntSet.singleton 1
      nextView =
        IntSet.singleton 7
      nextSection =
        IntSet.fromList [7, 8]
      dirtyResults =
        IntSet.singleton 8
      state0 =
        (testProjectionPropagationState (buildDependencyIndexFromSupports ["left"] (const oldView)))
          { cpsContextGraph = Map.singleton "left" oldView,
            cpsContextViews = Map.singleton "left" oldView
          }
      ProjectionCommit state1 sectionChanged =
        commitProjection
          Map.insert
          (\_context _site section -> section)
          id
          (\contextValue site -> Map.findWithDefault IntSet.empty contextValue site)
          "left"
          nextView
          nextSection
          dirtyResults
          True
          state0
      supportIndex =
        cpsBaseToCtx state1
  assertBool "commit preserves the section-changed witness" sectionChanged
  assertEqual "site section was installed" (Map.singleton "left" nextSection) (cpsContextGraph state1)
  assertEqual "view cache was updated" (Map.singleton "left" nextView) (cpsContextViews state1)
  assertEqual "dirty results were accumulated" dirtyResults (cpsDirtyResults state1)
  assertEqual "old view support was removed" [] (affectedContextsForDirtyKeys (IntSet.singleton 1) supportIndex)
  assertEqual "next view support was inserted" ["left"] (affectedContextsForDirtyKeys (IntSet.singleton 7) supportIndex)

runtimeSettleLoopDrainsToQuiescence :: IO ()
runtimeSettleLoopDrainsToQuiescence = do
  assertEqual
    "unscoped settle loop drains to quiescence"
    (Right 0)
    (runIdentity (runRuntimeSettleLoop 8 runtimeSettleStep 4))
  assertEqual
    "scoped settle loop consumes the same typed residual protocol"
    (Right 0)
    (runIdentity (runRuntimeSettleLoopScoped (const True) 8 runtimeScopedSettleStep 4))

runtimeSettleLoopReportsBudgetExhaustion :: IO ()
runtimeSettleLoopReportsBudgetExhaustion =
  assertEqual
    "budget exhaustion keeps the residual as typed data"
    (Left (RuntimeSettleBudgetExhausted {rsbeIterationLimit = 2, rsbeResidual = 2}))
    (runIdentity (runRuntimeSettleLoop 2 runtimeSettleStep 4))

runtimeSettleStep :: RuntimeSettleStep Identity Int Int
runtimeSettleStep =
  RuntimeSettleStep
    { rssDrain = pure . subtractOneUntilZero,
      rssFlush = pure,
      rssQuiescent = (== 0),
      rssResidual = id
    }

runtimeScopedSettleStep :: RuntimeScopedSettleStep Identity Int Int
runtimeScopedSettleStep =
  RuntimeScopedSettleStep
    { rssScopedDrain = \keepScope state -> pure (if keepScope emptyRuntimeScope then subtractOneUntilZero state else state),
      rssScopedFlush = \_keepScope -> pure,
      rssScopedQuiescent = \keepScope state -> keepScope emptyRuntimeScope && state == 0,
      rssScopedResidual = \_keepScope -> id
    }

subtractOneUntilZero :: Int -> Int
subtractOneUntilZero value =
  max 0 (value - 1)

type ProjectionMaintenanceTrace = [(ProjectionPhase, Int)]

type ProjectionMaintenanceTraceResult =
  ProjectionMaintenance.ProjectionWorkResult ProjectionMaintenanceTrace String String ()

type ProjectionMaintenanceTestJob =
  ProjectionMaintenance.ProjectionJob String String ()

testProjectionJob :: ProjectionPhase -> ProjectionMaintenanceTestJob
testProjectionJob phase =
  ProjectionMaintenance.projectionPhaseJob phase "ctx" ()

testProjectionNeighborJob :: ProjectionMaintenanceTestJob
testProjectionNeighborJob =
  ProjectionMaintenance.projectionNeighborJob "child" "ctx" ()

runProjectionMaintenanceTrace ::
  [ProjectionPhase] ->
  Map.Map Int ProjectionWork ->
  Either String ProjectionMaintenanceTraceResult
runProjectionMaintenanceTrace phases workByQuery =
  ProjectionMaintenance.runProjectionPhases
    (\_query _trace -> Just ())
    (\_query projectionWork _trace -> projectionWork)
    ( \phase query _projectionWork _plan _current traceValue ->
        Right
          ProjectionMaintenance.ProjectionPhaseResult
            { ProjectionMaintenance.pprGraph = traceValue <> [(phase, query)],
              ProjectionMaintenance.pprJobs = []
            }
    )
    phases
    workByQuery
    []

dirtyOne :: IntSet.IntSet
dirtyOne =
  IntSet.singleton 1

dirtyTwo :: IntSet.IntSet
dirtyTwo =
  IntSet.singleton 2

dirtyThree :: IntSet.IntSet
dirtyThree =
  IntSet.singleton 3

type TestProjectionPropagationState =
  ProjectionPropagationState
    (Map.Map String IntSet.IntSet)
    ()
    ()
    String
    IntSet.IntSet
    ()
    ()
    ()

testProjectionPropagationState ::
  ProjectionSupportIndex String ->
  TestProjectionPropagationState
testProjectionPropagationState supportIndex =
  ProjectionPropagationState
    { cpsContextGraph = Map.empty,
      cpsCanonEpoch = Nothing,
      cpsRepairIndex = Nothing,
      cpsImpacted = IntSet.empty,
      cpsDirtyResults = IntSet.empty,
      cpsVersion = initialVersion,
      cpsPruneSignals = Map.empty,
      cpsContextViews = Map.empty,
      cpsBaseToCtx = supportIndex,
      cpsEpochDelta = Nothing,
      cpsAnalysis = (),
      cpsCanonicalize = ()
    }

initialSupportForContext :: String -> IntSet.IntSet
initialSupportForContext contextValue =
  Map.findWithDefault
    IntSet.empty
    contextValue
    ( Map.fromList
        [ ("left", IntSet.fromList [1, 2]),
          ("right", IntSet.singleton 3)
        ]
    )
