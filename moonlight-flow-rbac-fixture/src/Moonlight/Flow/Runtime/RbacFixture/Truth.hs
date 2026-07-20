{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Runtime.RbacFixture.Truth
  ( seedTruth,
    seedRelationRows,
    truthRelation,
    writeTruthRelation,
    truthPatch,
    truthPatchForAtoms,
    buildRuntimeFromTruth,
    runtimeSpecFromModel,
    buildRuntimeFromModel,
    buildRuntimeFromTruthForPlans,
    readAll,
    rowsDigestWithCount,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Patch qualified as Patch
import Moonlight.Flow.Read qualified as Read
import Moonlight.Flow.Runtime.Create qualified as RuntimeCreate
import Moonlight.Flow.Runtime.Spec.Schema qualified as RuntimeSpec
import Moonlight.Flow.Runtime.Types qualified as Runtime
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.Flow.Runtime.RbacFixture.Plans
  ( atomByName,
    planList,
    rbacSchema,
  )
import Moonlight.Flow.Runtime.RbacFixture.Rows
import Moonlight.Flow.Runtime.RbacFixture.Types

seedTruth :: RbacSize -> RbacSeedCounts -> Word64 -> (RbacTruth, Rng)
seedTruth sizeValue counts patchSeed =
  ( RbacTruth
      { rbtRelations = relations,
        rbtNextOrdinals = nextOrdinals
      },
    Rng patchSeed
  )
  where
    relations =
      IntMap.fromList
        ( fmap
            (\atomName -> (rbacAtomKey atomName, seedRelationRows sizeValue atomName (seedTarget counts atomName)))
            allAtomNames
        )
    nextOrdinals =
      IntMap.fromList
        ( fmap
            ( \atomName ->
                let !target = boundedTarget (relationCapacity sizeValue atomName) (seedTarget counts atomName)
                    !tenantCount = positiveBound (rbsTenants sizeValue)
                 in (rbacAtomKey atomName, target * tenantCount + 4096)
            )
            allAtomNames
        )

seedRelationRows :: RbacSize -> RbacAtomName -> Int -> Set RowTupleKey
seedRelationRows sizeValue atomName requested =
  Set.fromList
    ( foldMap
        rowsForTenant
        (tenantCountAllocation (positiveBound (rbsTenants sizeValue)) target)
    )
  where
    !target = boundedTarget (relationCapacity sizeValue atomName) requested
    rowsForTenant (!tenant, !count) =
      fmap
        (rowForTenantLocalOrdinal sizeValue atomName tenant)
        [0 .. count - 1]

truthRelation :: RbacAtomName -> RbacTruth -> Set RowTupleKey
truthRelation atomName truth =
  IntMap.findWithDefault Set.empty (rbacAtomKey atomName) (rbtRelations truth)
{-# INLINE truthRelation #-}

writeTruthRelation :: RbacAtomName -> Set RowTupleKey -> RbacTruth -> RbacTruth
writeTruthRelation atomName rowsValue truth =
  truth
    { rbtRelations =
        IntMap.insert
          (rbacAtomKey atomName)
          rowsValue
          (rbtRelations truth)
    }
{-# INLINE writeTruthRelation #-}

truthPatch :: RbacAtoms -> RbacTruth -> Either Patch.PatchError Patch.Patch
truthPatch atomsValue truth =
  truthPatchForAtoms atomsValue allAtomNames truth

truthPatchForAtoms :: RbacAtoms -> [RbacAtomName] -> RbacTruth -> Either Patch.PatchError Patch.Patch
truthPatchForAtoms atomsValue atomNames truth =
  Patch.patch <$> traverse insertOne atomNames
  where
    insertOne atomName =
      Patch.insert
        (atomByName atomsValue atomName)
        (Set.toAscList (truthRelation atomName truth))

buildRuntimeFromTruth ::
  RbacAtoms ->
  [RuntimeSpec.RuntimePlan RbacContext RbacProp] ->
  RbacTruth ->
  Either RbacFixtureError (Runtime.Runtime RbacContext RbacProp)
buildRuntimeFromTruth atomsValue plansValue truth =
  buildRuntimeFromTruthForPlans atomsValue plansValue allAtomNames truth

runtimeSpecFromModel ::
  RbacModel ->
  RbacTruth ->
  Either RbacFixtureError (RuntimeSpec.RuntimeSpec RbacContext RbacProp)
runtimeSpecFromModel model truth = do
  seedPatch <- first RbacFixturePatchError (truthPatch (rbmAtoms model) truth)
  pure
    ( RuntimeSpec.withInitialData
        (RuntimeSpec.runtimeInitialData seedPatch)
        (RuntimeSpec.runtimeSpec (rbmSchema model) (planList (rbmPlans model)))
    )

buildRuntimeFromModel ::
  RbacModel ->
  RbacTruth ->
  Either RbacFixtureError (Runtime.Runtime RbacContext RbacProp)
buildRuntimeFromModel model truth = do
  specValue <- runtimeSpecFromModel model truth
  first RbacFixtureCreateError (RuntimeCreate.createRuntime specValue)

buildRuntimeFromTruthForPlans ::
  RbacAtoms ->
  [RuntimeSpec.RuntimePlan RbacContext RbacProp] ->
  [RbacAtomName] ->
  RbacTruth ->
  Either RbacFixtureError (Runtime.Runtime RbacContext RbacProp)
buildRuntimeFromTruthForPlans atomsValue plansValue seedAtoms truth = do
  seedPatch <- first RbacFixturePatchError (truthPatchForAtoms atomsValue seedAtoms truth)
  first RbacFixtureCreateError $
    RuntimeCreate.createRuntime
      ( RuntimeSpec.withInitialData
          (RuntimeSpec.runtimeInitialData seedPatch)
          (RuntimeSpec.runtimeSpec (rbacSchema atomsValue) plansValue)
      )

readAll :: [RuntimeSpec.RuntimePlan RbacContext RbacProp] -> Runtime.Runtime RbacContext RbacProp -> Either String [Read.Rows]
readAll plansValue runtime =
  traverse (\planValue -> first show (Read.readRows planValue runtime)) plansValue

rowsDigestWithCount :: Read.Rows -> RbacRowsDigest
rowsDigestWithCount rowsValue =
  RbacRowsDigest
    { rrdPositiveCount = length (Read.positiveRows rowsValue),
      rrdDigest = Read.rowsDigest rowsValue
    }
{-# INLINE rowsDigestWithCount #-}
