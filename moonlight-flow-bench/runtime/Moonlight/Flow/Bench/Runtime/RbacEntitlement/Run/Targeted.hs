{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Bench.Runtime.RbacEntitlement.Run.Targeted
  ( runRbacTargetedTimingMatrix,
  )
where

import Moonlight.Flow.Runtime.Types qualified as R
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Config
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Stats
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Types
import Moonlight.Flow.Runtime.RbacFixture.Patch
  ( generatePatchBatch,
  )
import Moonlight.Flow.Runtime.RbacFixture.Plans
  ( buildRbacModel,
  )
import Moonlight.Flow.Runtime.RbacFixture.Truth
  ( buildRuntimeFromModel,
    seedTruth,
  )

runRbacTargetedTimingMatrix :: IO (Either RbacBenchError RbacTargetedTimingReport)
runRbacTargetedTimingMatrix =
  case fromRbacFixture buildRbacModel of
    Left err ->
      pure (Left err)
    Right model -> do
      let !config = workstationRbacWorkloadConfig
          (!truth0, !rng0) = seedTruth (rwcSize config) (rwcSeedCounts config) (rwcPatchSeed config)
      case fromRbacFixture (buildRuntimeFromModel model truth0) of
        Left err ->
          pure (Left err)
        Right runtime0 ->
          case fromRbacFixture (generatePatchBatch (rbmAtoms model) (rwcSize config) (rwcPatchShape config) truth0 rng0) of
            Left err ->
              pure (Left err)
            Right (!truthWarm, !warmupPatch, !rngWarm, !warmupSummary) -> do
              (!warmupApplyNs, warmupApplyResult) <-
                timedApplyPatch warmupPatch runtime0
              case warmupApplyResult of
                Left err ->
                  pure (Left err)
                Right runtimeWarm -> do
                  let !warmupRepairStats =
                        repairStatsDelta (runtimeDiagnosticsRepairStats runtime0) (runtimeDiagnosticsRepairStats runtimeWarm)
                      !warmupDiagnostics =
                        runtimeDiagnosticsReuseDiagnostics runtimeWarm
                  scenarioResults <-
                    traverse
                      (runRbacTargetedScenario model config truthWarm rngWarm runtimeWarm warmupDiagnostics)
                      allRbacTargetedScenarios
                  pure $ do
                    scenarioReports <- sequenceA scenarioResults
                    pure
                      RbacTargetedTimingReport
                        { rttrConfig = config,
                          rttrWarmupPatch = warmupSummary,
                          rttrWarmupApplyNs = warmupApplyNs,
                          rttrWarmupRepairStats = warmupRepairStats,
                          rttrWarmupDiagnostics = warmupDiagnostics,
                          rttrScenarios = scenarioReports
                        }

runRbacTargetedScenario ::
  RbacModel ->
  RbacWorkloadConfig ->
  RbacTruth ->
  Rng ->
  R.Runtime RbacContext RbacProp ->
  R.RuntimeReuseDiagnostics ->
  RbacTargetedScenario ->
  IO (Either RbacBenchError RbacTargetedScenarioReport)
runRbacTargetedScenario model config truthWarm rngWarm runtimeWarm diagnosticsBefore scenario = do
  let !patchShape = targetedScenarioPatchShape config scenario
      !repairStatsBefore = runtimeDiagnosticsRepairStats runtimeWarm
  case fromRbacFixture (generatePatchBatch (rbmAtoms model) (rwcSize config) patchShape truthWarm rngWarm) of
    Left err ->
      pure (Left err)
    Right (!_truth1, !patchValue, !_rng1, !patchSummary) -> do
      runtimeStatsBefore <- readRuntimeStatsSample
      (!applyNs, applyResult) <-
        timedApplyPatch patchValue runtimeWarm
      runtimeStatsAfter <- readRuntimeStatsSample
      pure $
        case applyResult of
          Left err ->
            Left err
          Right runtime1 ->
            let !diagnosticsAfter =
                  runtimeDiagnosticsReuseDiagnostics runtime1
                !repairStats =
                  repairStatsDelta repairStatsBefore (runtimeDiagnosticsRepairStats runtime1)
             in Right
                  RbacTargetedScenarioReport
                    { rtsrScenario = scenario,
                      rtsrPatchShape = patchShape,
                      rtsrPatch = patchSummary,
                      rtsrApplyNs = applyNs,
                      rtsrRepairStats = repairStats,
                      rtsrRuntimeStats = runtimeStatsDelta runtimeStatsBefore runtimeStatsAfter,
                      rtsrReuseStats = runtimeDiagnosticsReuseStats runtime1,
                      rtsrReuseDiagnosticsBefore = diagnosticsBefore,
                      rtsrReuseDiagnosticsAfter = diagnosticsAfter,
                      rtsrStaleRejectedDelta = staleRejectedDelta diagnosticsBefore diagnosticsAfter,
                      rtsrRegisteredNewDelta = registeredNewDelta diagnosticsBefore diagnosticsAfter
                    }
