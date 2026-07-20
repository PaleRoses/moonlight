{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Bench.Runtime.RbacEntitlement.Run.Locality
  ( runRbacLocalityMatrix,
  )
where

import Moonlight.Flow.Runtime.Types qualified as R
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Config
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Run.Checks
  ( freshCheckRuntime,
  )
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Stats
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Types
import Moonlight.Flow.Runtime.RbacFixture.Config
  ( allRbacLocalityScenarios,
    localityScenarioPatchShape,
    localityWarmupPatchShape,
  )
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

runRbacLocalityMatrix :: IO (Either RbacBenchError RbacLocalityMatrixReport)
runRbacLocalityMatrix =
  case fromRbacFixture buildRbacModel of
    Left err ->
      pure (Left err)
    Right model -> do
      let !config = localityMatrixRbacWorkloadConfig
          (!truth0, !rng0) = seedTruth (rwcSize config) (rwcSeedCounts config) (rwcPatchSeed config)
      case fromRbacFixture (buildRuntimeFromModel model truth0) of
        Left err ->
          pure (Left err)
        Right runtime0 ->
          case fromRbacFixture (generatePatchBatch (rbmAtoms model) (rwcSize config) localityWarmupPatchShape truth0 rng0) of
            Left err ->
              pure (Left err)
            Right (!truthWarm, !warmupPatch, !rngWarm, !warmupSummary) -> do
              (!warmupApplyNs, warmupApplyResult) <-
                timedApplyPatch warmupPatch runtime0
              case warmupApplyResult of
                Left err ->
                  pure (Left err)
                Right runtimeWarm -> do
                  let !warmupDiagnostics = runtimeDiagnosticsReuseDiagnostics runtimeWarm
                  case validateLocalityWarmup warmupDiagnostics of
                    Left err ->
                      pure (Left err)
                    Right () -> do
                      scenarioResults <-
                        traverse
                          (runRbacLocalityScenario model config truthWarm rngWarm runtimeWarm warmupDiagnostics)
                          allRbacLocalityScenarios
                      pure $ do
                        scenarioReports <- sequenceA scenarioResults
                        pure
                          RbacLocalityMatrixReport
                            { rlmrConfig = config,
                              rlmrWarmupPatch = warmupSummary,
                              rlmrWarmupApplyNs = warmupApplyNs,
                              rlmrWarmupDiagnostics = warmupDiagnostics,
                              rlmrScenarios = scenarioReports
                            }

runRbacLocalityScenario ::
  RbacModel ->
  RbacWorkloadConfig ->
  RbacTruth ->
  Rng ->
  R.Runtime RbacContext RbacProp ->
  R.RuntimeReuseDiagnostics ->
  RbacLocalityScenario ->
  IO (Either RbacBenchError RbacLocalityScenarioReport)
runRbacLocalityScenario model config truthWarm rngWarm runtimeWarm diagnosticsBefore scenario = do
  let !patchShape = localityScenarioPatchShape scenario
  case fromRbacFixture (generatePatchBatch (rbmAtoms model) (rwcSize config) patchShape truthWarm rngWarm) of
    Left err ->
      pure (Left err)
    Right (!truth1, !patchValue, !_rng1, !patchSummary) -> do
      (!applyNs, applyResult) <-
        timedApplyPatch patchValue runtimeWarm
      case applyResult of
        Left err ->
          pure (Left err)
        Right runtime1 -> do
          (!freshNs, freshResult) <-
            timed (freshCheckRuntime model 0 truth1 runtime1)
          pure $
            case freshResult of
              Left err ->
                Left err
              Right () ->
                let !diagnosticsAfter = runtimeDiagnosticsReuseDiagnostics runtime1
                    !report =
                      RbacLocalityScenarioReport
                        { rlsrScenario = scenario,
                          rlsrPatchShape = patchShape,
                          rlsrPatch = patchSummary,
                          rlsrApplyNs = applyNs,
                          rlsrFreshCheckNs = freshNs,
                          rlsrRegisteredFactorShapesBefore = R.rrdRegisteredFactorShapes diagnosticsBefore,
                          rlsrRegisteredFactorShapesAfter = R.rrdRegisteredFactorShapes diagnosticsAfter,
                          rlsrStaleRejectedDelta = staleRejectedDelta diagnosticsBefore diagnosticsAfter,
                          rlsrRegisteredNewDelta = registeredNewDelta diagnosticsBefore diagnosticsAfter,
                          rlsrDiagnosticsBefore = diagnosticsBefore,
                          rlsrDiagnosticsAfter = diagnosticsAfter
                        }
                 in Right report

validateLocalityWarmup :: R.RuntimeReuseDiagnostics -> Either RbacBenchError ()
validateLocalityWarmup diagnostics
  | R.rrdRegisteredFactorShapes diagnostics <= 0 =
      Left (RbacLocalityWarmupFailed "warmup produced no registered factor shapes" diagnostics)
  | otherwise =
      Right ()
