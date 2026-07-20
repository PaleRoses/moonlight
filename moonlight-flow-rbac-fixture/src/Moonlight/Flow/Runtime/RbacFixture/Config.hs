{-# LANGUAGE LambdaCase #-}

module Moonlight.Flow.Runtime.RbacFixture.Config
  ( testRbacSize,
    testRbacSeedCounts,
    emptyRbacPatchShape,
    testRbacPatchShape,
    localityWarmupPatchShape,
    localityScenarioPatchShape,
    localityScenarioShouldStayNarrow,
    allRbacLocalityScenarios,
  )
where

import Moonlight.Flow.Runtime.RbacFixture.Types

testRbacSize :: RbacSize
testRbacSize =
  RbacSize
    { rbsTenants = 2,
      rbsUsersPerTenant = 24,
      rbsGroupsPerTenant = 12,
      rbsScopesPerTenant = 8,
      rbsResourcesPerTenant = 48,
      rbsRolesPerTenant = 8,
      rbsActions = 8,
      rbsAttrs = 8,
      rbsDenyGroupsPerTenant = 6
    }

testRbacSeedCounts :: RbacSeedCounts
testRbacSeedCounts =
  RbacSeedCounts
    { rscMember = 80,
      rscGroupScope = 30,
      rscGroupRole = 40,
      rscRoleAction = 24,
      rscResourceScope = 120,
      rscRoleAttr = 24,
      rscUserAttr = 96,
      rscDenyMember = 16,
      rscDenyGroupScope = 8,
      rscDenyGroupAction = 8
    }

emptyRbacPatchShape :: RbacPatchShape
emptyRbacPatchShape =
  RbacPatchShape
    { rpsMemberMoves = 0,
      rpsUserAttrMoves = 0,
      rpsResourceScopeMoves = 0,
      rpsRoleActionMoves = 0,
      rpsGroupRoleMoves = 0,
      rpsDenyMoves = 0,
      rpsGroupScopeMoves = 0
    }
{-# INLINE emptyRbacPatchShape #-}

testRbacPatchShape :: RbacPatchShape
testRbacPatchShape =
  RbacPatchShape
    { rpsMemberMoves = 4,
      rpsUserAttrMoves = 4,
      rpsResourceScopeMoves = 4,
      rpsRoleActionMoves = 2,
      rpsGroupRoleMoves = 2,
      rpsDenyMoves = 3,
      rpsGroupScopeMoves = 2
    }

localityWarmupPatchShape :: RbacPatchShape
localityWarmupPatchShape =
  RbacPatchShape
    { rpsMemberMoves = 18,
      rpsUserAttrMoves = 12,
      rpsResourceScopeMoves = 8,
      rpsRoleActionMoves = 4,
      rpsGroupRoleMoves = 5,
      rpsDenyMoves = 6,
      rpsGroupScopeMoves = 3
    }

allRbacLocalityScenarios :: [RbacLocalityScenario]
allRbacLocalityScenarios =
  [minBound .. maxBound]
{-# INLINE allRbacLocalityScenarios #-}

localityScenarioPatchShape :: RbacLocalityScenario -> RbacPatchShape
localityScenarioPatchShape =
  \case
    RbacLocalityMemberChurn ->
      emptyRbacPatchShape {rpsMemberMoves = 18}
    RbacLocalityUserAttrChurn ->
      emptyRbacPatchShape {rpsUserAttrMoves = 12}
    RbacLocalityResourceScopeMove ->
      emptyRbacPatchShape {rpsResourceScopeMoves = 8}
    RbacLocalityRoleActionToggle ->
      emptyRbacPatchShape {rpsRoleActionMoves = 4}
    RbacLocalityGroupRoleChange ->
      emptyRbacPatchShape {rpsGroupRoleMoves = 5}
    RbacLocalityGroupScopeMove ->
      emptyRbacPatchShape {rpsGroupScopeMoves = 3}
    RbacLocalityDenyChange ->
      emptyRbacPatchShape {rpsDenyMoves = 6}

localityScenarioShouldStayNarrow :: RbacLocalityScenario -> Bool
localityScenarioShouldStayNarrow =
  \case
    RbacLocalityResourceScopeMove -> False
    RbacLocalityMemberChurn -> True
    RbacLocalityUserAttrChurn -> True
    RbacLocalityRoleActionToggle -> True
    RbacLocalityGroupRoleChange -> True
    RbacLocalityGroupScopeMove -> True
    RbacLocalityDenyChange -> True
