{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Moonlight.Flow.Bench.Runtime.RbacEntitlement.Config
  ( smokeRbacWorkloadConfig,
    smokePerfRbacWorkloadConfig,
    workstationRbacWorkloadConfig,
    hugeRbacWorkloadConfig,
    localityMatrixRbacWorkloadConfig,
    allRbacTargetedScenarios,
    targetedScenarioPatchShape,
    resourceScopeFrontierReproducerConfig,
  )
where

import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Types
import Moonlight.Flow.Runtime.RbacFixture.Config
  ( emptyRbacPatchShape,
    localityWarmupPatchShape,
  )

smokeRbacWorkloadConfig :: RbacWorkloadConfig
smokeRbacWorkloadConfig =
  RbacWorkloadConfig
    { rwcPatchSeed = 0x726261635f736d6f,
      rwcSize =
        RbacSize
          { rbsTenants = 3,
            rbsUsersPerTenant = 120,
            rbsGroupsPerTenant = 48,
            rbsScopesPerTenant = 32,
            rbsResourcesPerTenant = 480,
            rbsRolesPerTenant = 32,
            rbsActions = 16,
            rbsAttrs = 32,
            rbsDenyGroupsPerTenant = 16
          },
      rwcSeedCounts =
        RbacSeedCounts
          { rscMember = 720,
            rscGroupScope = 180,
            rscGroupRole = 240,
            rscRoleAction = 96,
            rscResourceScope = 960,
            rscRoleAttr = 160,
            rscUserAttr = 960,
            rscDenyMember = 96,
            rscDenyGroupScope = 48,
            rscDenyGroupAction = 48
          },
      rwcPatchShape =
        RbacPatchShape
          { rpsMemberMoves = 18,
            rpsUserAttrMoves = 12,
            rpsResourceScopeMoves = 8,
            rpsRoleActionMoves = 0,
            rpsGroupRoleMoves = 5,
            rpsDenyMoves = 6,
            rpsGroupScopeMoves = 3
          },
      rwcBatches = 5,
      rwcFreshCheckEvery = 1,
      rwcAdversarialEvery = 1,
      rwcSemanticCheckEvery = 1,
      rwcReadInitialOutputs = True,
      rwcReadFinalOutputs = True
    }

workstationRbacWorkloadConfig :: RbacWorkloadConfig
workstationRbacWorkloadConfig =
  RbacWorkloadConfig
    { rwcPatchSeed = 0x726261635f776f72,
      rwcSize =
        RbacSize
          { rbsTenants = 20,
            rbsUsersPerTenant = 5000,
            rbsGroupsPerTenant = 1000,
            rbsScopesPerTenant = 500,
            rbsResourcesPerTenant = 5000,
            rbsRolesPerTenant = 128,
            rbsActions = 64,
            rbsAttrs = 256,
            rbsDenyGroupsPerTenant = 128
          },
      rwcSeedCounts =
        RbacSeedCounts
          { rscMember = 20000,
            rscGroupScope = 2000,
            rscGroupRole = 3500,
            rscRoleAction = 350,
            rscResourceScope = 10000,
            rscRoleAttr = 200,
            rscUserAttr = 12000,
            rscDenyMember = 1600,
            rscDenyGroupScope = 200,
            rscDenyGroupAction = 250
          },
      rwcPatchShape =
        RbacPatchShape
          { rpsMemberMoves = 200,
            rpsUserAttrMoves = 70,
            rpsResourceScopeMoves = 40,
            rpsRoleActionMoves = 8,
            rpsGroupRoleMoves = 18,
            rpsDenyMoves = 24,
            rpsGroupScopeMoves = 8
          },
      rwcBatches = 200,
      rwcFreshCheckEvery = 25,
      rwcAdversarialEvery = 10,
      rwcSemanticCheckEvery = 0,
      rwcReadInitialOutputs = False,
      rwcReadFinalOutputs = False
    }

smokePerfRbacWorkloadConfig :: RbacWorkloadConfig
smokePerfRbacWorkloadConfig =
  smokeRbacWorkloadConfig
    { rwcFreshCheckEvery = 0,
      rwcAdversarialEvery = 0,
      rwcSemanticCheckEvery = 0,
      rwcReadInitialOutputs = False,
      rwcReadFinalOutputs = False
    }

hugeRbacWorkloadConfig :: RbacWorkloadConfig
hugeRbacWorkloadConfig =
  workstationRbacWorkloadConfig
    { rwcPatchSeed = 0x726261635f687567,
      rwcPatchShape =
        (rwcPatchShape workstationRbacWorkloadConfig)
          { rpsMemberMoves = 10000,
            rpsUserAttrMoves = 3000,
            rpsResourceScopeMoves = 2000,
            rpsRoleActionMoves = 400,
            rpsGroupRoleMoves = 800,
            rpsDenyMoves = 1000,
            rpsGroupScopeMoves = 400
          },
      rwcBatches = 1000,
      rwcFreshCheckEvery = 100,
      rwcAdversarialEvery = 50,
      rwcSemanticCheckEvery = 0,
      rwcReadInitialOutputs = False,
      rwcReadFinalOutputs = False
    }

localityMatrixRbacWorkloadConfig :: RbacWorkloadConfig
localityMatrixRbacWorkloadConfig =
  smokePerfRbacWorkloadConfig
    { rwcPatchSeed = 0x726261635f6c6f63,
      rwcPatchShape = localityWarmupPatchShape,
      rwcBatches = 1
    }

allRbacTargetedScenarios :: [RbacTargetedScenario]
allRbacTargetedScenarios =
  [minBound .. maxBound]
{-# INLINE allRbacTargetedScenarios #-}

targetedScenarioPatchShape :: RbacWorkloadConfig -> RbacTargetedScenario -> RbacPatchShape
targetedScenarioPatchShape config scenario =
  case scenario of
    RbacTargetMemberOnly ->
      emptyRbacPatchShape {rpsMemberMoves = rpsMemberMoves baseShape}
    RbacTargetUserAttrOnly ->
      emptyRbacPatchShape {rpsUserAttrMoves = rpsUserAttrMoves baseShape}
    RbacTargetRoleActionOnly ->
      emptyRbacPatchShape {rpsRoleActionMoves = rpsRoleActionMoves baseShape}
    RbacTargetResourceScopeOnly ->
      emptyRbacPatchShape {rpsResourceScopeMoves = rpsResourceScopeMoves baseShape}
    RbacTargetDenyOnly ->
      emptyRbacPatchShape {rpsDenyMoves = rpsDenyMoves baseShape}
  where
    baseShape =
      rwcPatchShape config

resourceScopeFrontierReproducerConfig :: RbacWorkloadConfig
resourceScopeFrontierReproducerConfig =
  RbacWorkloadConfig
    { rwcPatchSeed = 0x726261635f726570,
      rwcSize =
        RbacSize
          { rbsTenants = 1,
            rbsUsersPerTenant = 4,
            rbsGroupsPerTenant = 4,
            rbsScopesPerTenant = 4,
            rbsResourcesPerTenant = 4,
            rbsRolesPerTenant = 4,
            rbsActions = 4,
            rbsAttrs = 4,
            rbsDenyGroupsPerTenant = 2
          },
      rwcSeedCounts =
        RbacSeedCounts
          { rscMember = 6,
            rscGroupScope = 6,
            rscGroupRole = 6,
            rscRoleAction = 6,
            rscResourceScope = 8,
            rscRoleAttr = 6,
            rscUserAttr = 6,
            rscDenyMember = 2,
            rscDenyGroupScope = 2,
            rscDenyGroupAction = 2
          },
      rwcPatchShape =
        RbacPatchShape
          { rpsMemberMoves = 0,
            rpsUserAttrMoves = 0,
            rpsResourceScopeMoves = 1,
            rpsRoleActionMoves = 0,
            rpsGroupRoleMoves = 0,
            rpsDenyMoves = 0,
            rpsGroupScopeMoves = 0
          },
      rwcBatches = 1,
      rwcFreshCheckEvery = 0,
      rwcAdversarialEvery = 0,
      rwcSemanticCheckEvery = 0,
      rwcReadInitialOutputs = False,
      rwcReadFinalOutputs = False
    }
