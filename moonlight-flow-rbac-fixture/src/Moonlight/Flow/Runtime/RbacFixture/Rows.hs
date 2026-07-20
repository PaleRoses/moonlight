{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.Flow.Runtime.RbacFixture.Rows
  ( seedTarget,
    relationCapacity,
    boundedTarget,
    tenantCountAllocation,
    rowForGlobalOrdinal,
    rowForTenantLocalOrdinal,
    positiveBound,
  )
where

import Data.Foldable qualified as Foldable
import Moonlight.Differential.Row.Tuple qualified as R
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.Flow.Runtime.RbacFixture.Types

seedTarget :: RbacSeedCounts -> RbacAtomName -> Int
seedTarget counts =
  \case
    Member -> rscMember counts
    GroupScope -> rscGroupScope counts
    GroupRole -> rscGroupRole counts
    RoleAction -> rscRoleAction counts
    ResourceScope -> rscResourceScope counts
    RoleAttr -> rscRoleAttr counts
    UserAttr -> rscUserAttr counts
    DenyMember -> rscDenyMember counts
    DenyGroupScope -> rscDenyGroupScope counts
    DenyGroupAction -> rscDenyGroupAction counts

relationCapacity :: RbacSize -> RbacAtomName -> Integer
relationCapacity sizeValue =
  \case
    Member -> cap3 (rbsTenants sizeValue) (rbsUsersPerTenant sizeValue) (rbsGroupsPerTenant sizeValue)
    GroupScope -> cap3 (rbsTenants sizeValue) (rbsGroupsPerTenant sizeValue) (rbsScopesPerTenant sizeValue)
    GroupRole -> cap3 (rbsTenants sizeValue) (rbsGroupsPerTenant sizeValue) (rbsRolesPerTenant sizeValue)
    RoleAction -> cap3 (rbsTenants sizeValue) (rbsRolesPerTenant sizeValue) (rbsActions sizeValue)
    ResourceScope -> cap3 (rbsTenants sizeValue) (rbsResourcesPerTenant sizeValue) (rbsScopesPerTenant sizeValue)
    RoleAttr -> cap3 (rbsTenants sizeValue) (rbsRolesPerTenant sizeValue) (rbsAttrs sizeValue)
    UserAttr -> cap3 (rbsTenants sizeValue) (rbsUsersPerTenant sizeValue) (rbsAttrs sizeValue)
    DenyMember -> cap3 (rbsTenants sizeValue) (rbsUsersPerTenant sizeValue) (rbsDenyGroupsPerTenant sizeValue)
    DenyGroupScope -> cap3 (rbsTenants sizeValue) (rbsDenyGroupsPerTenant sizeValue) (rbsScopesPerTenant sizeValue)
    DenyGroupAction -> cap3 (rbsTenants sizeValue) (rbsDenyGroupsPerTenant sizeValue) (rbsActions sizeValue)
  where
    cap3 a b c = positiveInteger a * positiveInteger b * positiveInteger c
{-# INLINE relationCapacity #-}

positiveInteger :: Int -> Integer
positiveInteger value =
  fromIntegral (max 0 value)
{-# INLINE positiveInteger #-}

boundedTarget :: Integer -> Int -> Int
boundedTarget capacity requested =
  max 0 (min requested capacityAsInt)
  where
    capacityAsInt
      | capacity <= 0 = 0
      | capacity > fromIntegral (maxBound :: Int) = maxBound
      | otherwise = fromInteger capacity
{-# INLINE boundedTarget #-}

tenantCountAllocation :: Int -> Int -> [(Int, Int)]
tenantCountAllocation tenants requested =
  fmap addRemainder baseAllocations
  where
    !tenantCount = positiveBound tenants
    !target = max 0 requested
    weightedTenants = fmap (\tenant -> (tenant, tenantCount - tenant)) [0 .. tenantCount - 1]
    !totalWeight = Foldable.foldl' (\acc (_tenant, weight) -> acc + weight) 0 weightedTenants
    baseAllocations =
      fmap
        ( \(!tenant, !weight) ->
            let !count = if totalWeight <= 0 then 0 else (target * weight) `quot` totalWeight
             in (tenant, count)
        )
        weightedTenants
    !baseTotal = Foldable.foldl' (\acc (_tenant, count) -> acc + count) 0 baseAllocations
    !remainder = target - baseTotal
    addRemainder (!tenant, !count) =
      (tenant, count + if tenant < remainder then 1 else 0)

rowForGlobalOrdinal :: RbacSize -> RbacAtomName -> Int -> RowTupleKey
rowForGlobalOrdinal sizeValue atomName ordinal =
  rowForTenantLocalOrdinal sizeValue atomName tenant localOrdinal
  where
    !tenantCount = positiveBound (rbsTenants sizeValue)
    !tenant = ordinal `rem` tenantCount
    !localOrdinal = ordinal `quot` tenantCount

rowForTenantLocalOrdinal :: RbacSize -> RbacAtomName -> Int -> Int -> RowTupleKey
rowForTenantLocalOrdinal sizeValue atomName tenant ordinal =
  case atomName of
    Member ->
      let !userRaw = ordinal `rem` positiveBound (rbsUsersPerTenant sizeValue)
          !roundRaw = ordinal `quot` positiveBound (rbsUsersPerTenant sizeValue)
          !hotGroups = max 1 (positiveBound (rbsGroupsPerTenant sizeValue) `quot` 32)
          !groupRaw =
            if ordinal `rem` 16 == 0
              then roundRaw `rem` hotGroups
              else (userRaw + roundRaw * 17) `rem` positiveBound (rbsGroupsPerTenant sizeValue)
       in R.tupleKeyFromInts [tenant, scopedKey tenant (rbsUsersPerTenant sizeValue) userRaw, scopedKey tenant (rbsGroupsPerTenant sizeValue) groupRaw]
    GroupScope ->
      let !groupRaw = ordinal `rem` positiveBound (rbsGroupsPerTenant sizeValue)
          !groupBlock = max 1 (positiveBound (rbsGroupsPerTenant sizeValue) `quot` 20)
          !scopeRaw = ((groupRaw `quot` groupBlock) + ordinal `quot` positiveBound (rbsGroupsPerTenant sizeValue)) `rem` positiveBound (rbsScopesPerTenant sizeValue)
       in R.tupleKeyFromInts [tenant, scopedKey tenant (rbsGroupsPerTenant sizeValue) groupRaw, scopedKey tenant (rbsScopesPerTenant sizeValue) scopeRaw]
    GroupRole ->
      let !groupRaw = ordinal `rem` positiveBound (rbsGroupsPerTenant sizeValue)
          !roundRaw = ordinal `quot` positiveBound (rbsGroupsPerTenant sizeValue)
          !roleRaw = (groupRaw + roundRaw * 31) `rem` positiveBound (rbsRolesPerTenant sizeValue)
       in R.tupleKeyFromInts [tenant, scopedKey tenant (rbsGroupsPerTenant sizeValue) groupRaw, scopedKey tenant (rbsRolesPerTenant sizeValue) roleRaw]
    RoleAction ->
      let !roleRaw = ordinal `rem` positiveBound (rbsRolesPerTenant sizeValue)
          !actionRaw = (ordinal `quot` positiveBound (rbsRolesPerTenant sizeValue)) `rem` positiveBound (rbsActions sizeValue)
       in R.tupleKeyFromInts [tenant, scopedKey tenant (rbsRolesPerTenant sizeValue) roleRaw, actionRaw]
    ResourceScope ->
      let !resourceRaw = ordinal `rem` positiveBound (rbsResourcesPerTenant sizeValue)
          !resourceBlock = max 1 (positiveBound (rbsResourcesPerTenant sizeValue) `quot` 50)
          !scopeRaw = ((resourceRaw `quot` resourceBlock) + ordinal `quot` positiveBound (rbsResourcesPerTenant sizeValue)) `rem` positiveBound (rbsScopesPerTenant sizeValue)
       in R.tupleKeyFromInts [tenant, scopedKey tenant (rbsResourcesPerTenant sizeValue) resourceRaw, scopedKey tenant (rbsScopesPerTenant sizeValue) scopeRaw]
    RoleAttr ->
      let !roleRaw = ordinal `rem` positiveBound (rbsRolesPerTenant sizeValue)
          !attrRaw = (ordinal `quot` positiveBound (rbsRolesPerTenant sizeValue)) `rem` positiveBound (rbsAttrs sizeValue)
       in R.tupleKeyFromInts [tenant, scopedKey tenant (rbsRolesPerTenant sizeValue) roleRaw, attrRaw]
    UserAttr ->
      let !userRaw = ordinal `rem` positiveBound (rbsUsersPerTenant sizeValue)
          !attrRaw = (ordinal `quot` positiveBound (rbsUsersPerTenant sizeValue)) `rem` positiveBound (rbsAttrs sizeValue)
       in R.tupleKeyFromInts [tenant, scopedKey tenant (rbsUsersPerTenant sizeValue) userRaw, attrRaw]
    DenyMember ->
      let !userRaw = ordinal `rem` positiveBound (rbsUsersPerTenant sizeValue)
          !denyRaw = (userRaw + ordinal `quot` positiveBound (rbsUsersPerTenant sizeValue)) `rem` positiveBound (rbsDenyGroupsPerTenant sizeValue)
       in R.tupleKeyFromInts [tenant, scopedKey tenant (rbsUsersPerTenant sizeValue) userRaw, scopedKey tenant (rbsDenyGroupsPerTenant sizeValue) denyRaw]
    DenyGroupScope ->
      let !denyRaw = ordinal `rem` positiveBound (rbsDenyGroupsPerTenant sizeValue)
          !scopeRaw = (denyRaw + ordinal `quot` positiveBound (rbsDenyGroupsPerTenant sizeValue)) `rem` positiveBound (rbsScopesPerTenant sizeValue)
       in R.tupleKeyFromInts [tenant, scopedKey tenant (rbsDenyGroupsPerTenant sizeValue) denyRaw, scopedKey tenant (rbsScopesPerTenant sizeValue) scopeRaw]
    DenyGroupAction ->
      let !denyRaw = ordinal `rem` positiveBound (rbsDenyGroupsPerTenant sizeValue)
          !actionRaw = (denyRaw + ordinal `quot` positiveBound (rbsDenyGroupsPerTenant sizeValue)) `rem` positiveBound (rbsActions sizeValue)
       in R.tupleKeyFromInts [tenant, scopedKey tenant (rbsDenyGroupsPerTenant sizeValue) denyRaw, actionRaw]

scopedKey :: Int -> Int -> Int -> Int
scopedKey tenant width localKey =
  tenant * positiveBound width + localKey
{-# INLINE scopedKey #-}

positiveBound :: Int -> Int
positiveBound value =
  max 1 value
{-# INLINE positiveBound #-}
