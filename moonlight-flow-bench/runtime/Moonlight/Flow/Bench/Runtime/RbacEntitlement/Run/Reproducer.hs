{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Moonlight.Flow.Bench.Runtime.RbacEntitlement.Run.Reproducer
  ( runResourceScopeFrontierReproducer,
  )
where

import Data.Set qualified as Set
import Moonlight.Flow.Patch qualified as R
import Moonlight.Flow.Runtime.Apply qualified as R
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Config
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Stats
  ( runtimeDiagnosticsReuseStats,
  )
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Types
import Moonlight.Flow.Runtime.RbacFixture.Patch
  ( mutateRelation,
  )
import Moonlight.Flow.Runtime.RbacFixture.Plans
  ( rbacAtoms,
    resourceScopeReproducerCases,
  )
import Moonlight.Flow.Runtime.RbacFixture.Truth
  ( buildRuntimeFromTruthForPlans,
    seedTruth,
    truthRelation,
  )

runResourceScopeFrontierReproducer :: IO (Either RbacBenchError RbacResourceScopeReproducerReport)
runResourceScopeFrontierReproducer =
  pure buildResourceScopeFrontierReproducer

buildResourceScopeFrontierReproducer :: Either RbacBenchError RbacResourceScopeReproducerReport
buildResourceScopeFrontierReproducer = do
  let !config = resourceScopeFrontierReproducerConfig
      !atomsValue = rbacAtoms
      (!truth0, !rng0) = seedTruth (rwcSize config) (rwcSeedCounts config) (rwcPatchSeed config)
  (!truth1, !patchValue, !_rng1, !patchSummary) <-
    fromRbacFixture (mutateRelation atomsValue (rwcSize config) ResourceScope 1 truth0 rng0)
  let !deletedRows =
        Set.toAscList
          (Set.difference (truthRelation ResourceScope truth0) (truthRelation ResourceScope truth1))
      !insertedRows =
        Set.toAscList
          (Set.difference (truthRelation ResourceScope truth1) (truthRelation ResourceScope truth0))
  caseReports <-
    traverse
      (runResourceScopeReproducerCase atomsValue truth0 patchValue)
      resourceScopeReproducerCases
  pure
    RbacResourceScopeReproducerReport
      { rrsrConfig = config,
        rrsrDeletedResourceScopeRows = deletedRows,
        rrsrInsertedResourceScopeRows = insertedRows,
        rrsrPatch = patchSummary,
        rrsrCases = caseReports
      }

runResourceScopeReproducerCase ::
  RbacAtoms ->
  RbacTruth ->
  R.Patch ->
  RbacResourceScopeReproducerCase ->
  Either RbacBenchError RbacResourceScopeReproducerCaseReport
runResourceScopeReproducerCase atomsValue truth patchValue reproCase = do
  plansValue <- fromRbacFixture (rrscPlans reproCase atomsValue)
  runtime0 <-
    fromRbacFixture
      ( buildRuntimeFromTruthForPlans
          atomsValue
          plansValue
          (rrscSeedAtoms reproCase)
          truth
      )
  let !outcome =
        case R.applyPatch patchValue runtime0 of
          Left err ->
            RbacResourceScopeReproducerRejected err
          Right runtime1 ->
            RbacResourceScopeReproducerApplied (runtimeDiagnosticsReuseStats runtime1)
  pure
    RbacResourceScopeReproducerCaseReport
      { rrscrPlanSet = rrscPlanSet reproCase,
        rrscrSeedAtoms = rrscSeedAtoms reproCase,
        rrscrOutcome = outcome
      }
