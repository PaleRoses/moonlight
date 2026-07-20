{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Moonlight.Flow.Carrier.Reuse.Internal.Index.Materialization
  ( InstalledReuseMaterialization (..),
    ReuseMaterializationIndex (..),
    ReuseMaterializationReverseIndex (..),
    rmiInstalledByReuse,
    rmiIndex,
    MaterializationInvariantError (..),
    emptyReuseMaterializationIndex,
    lookupInstalledReuseMaterialization,
    upsertInstalledReuseMaterialization,
    removeInstalledReuseMaterialization,
    dropInstalledReuseMaterialization,
    dropInstalledReuseMaterializationsForCarrier,
    dropInstalledReuseMaterializationsForReuses,
    selectStaleInstalledReuseMaterializations,
    validateReuseMaterializationIndex,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Reuse
  ( CarrierReuseId,
  )
import Moonlight.Differential.Index.Registry
  ( IndexedRegistry,
    RegistryOps (..),
    deleteRegistryRow,
    deleteRegistryRowReturning,
    emptyIndexedRegistry,
    lookupRegistryRow,
    registryIndexes,
    registryRows,
    upsertRegistryRow,
    validateIndexedRegistry,
  )
import Moonlight.Differential.Index.Reverse
  ( intSetIntersects,
    validateIntReverseIndex,
    validateMapReverseIndex,
  )
import Moonlight.Differential.Index.Reverse.Batch
  ( addMembership,
    dropMapAxis,
    dropMembership,
    insertMapAxis,
    lookupMany,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Differential.Row.Patch
  ( subtractPlainRowPatch,
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
    scopeDeps,
    scopeTopo,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )

data InstalledReuseMaterialization ctx prop = InstalledReuseMaterialization
  { irmReuseId :: !(CarrierReuseId ctx prop),
    irmTarget :: !(CarrierAddr ctx Carrier prop),
    irmRows :: !RowDelta,
    irmBoundaryDigest :: !StableDigest128,
    irmSourceCurrentDigest :: !StableDigest128,
    irmDeps :: !IntSet,
    irmTopo :: !IntSet
  }
  deriving stock (Eq, Show)

newtype ReuseMaterializationIndex ctx prop = ReuseMaterializationIndex
  { rmiRegistry ::
      IndexedRegistry
        (CarrierReuseId ctx prop)
        (InstalledReuseMaterialization ctx prop)
        (ReuseMaterializationReverseIndex ctx prop)
  }
  deriving stock (Eq, Show)

rmiInstalledByReuse ::
  ReuseMaterializationIndex ctx prop ->
  Map (CarrierReuseId ctx prop) (InstalledReuseMaterialization ctx prop)
rmiInstalledByReuse =
  registryRows . rmiRegistry
{-# INLINE rmiInstalledByReuse #-}

rmiIndex ::
  ReuseMaterializationIndex ctx prop ->
  ReuseMaterializationReverseIndex ctx prop
rmiIndex =
  registryIndexes . rmiRegistry
{-# INLINE rmiIndex #-}

data ReuseMaterializationReverseIndex ctx prop = ReuseMaterializationReverseIndex
  { rmriByTarget :: !(Map (CarrierAddr ctx Carrier prop) (Set (CarrierReuseId ctx prop))),
    rmriByDep :: !(IntMap (Set (CarrierReuseId ctx prop))),
    rmriByTopo :: !(IntMap (Set (CarrierReuseId ctx prop)))
  }
  deriving stock (Eq, Show)

data MaterializationInvariantError ctx prop
  = MaterializationStoredUnderWrongReuseId
      !(CarrierReuseId ctx prop)
      !(CarrierReuseId ctx prop)
  | MaterializationTargetReverseMissing
      !(CarrierReuseId ctx prop)
      !(CarrierAddr ctx Carrier prop)
  | MaterializationTargetReverseStale
      !(CarrierReuseId ctx prop)
      !(CarrierAddr ctx Carrier prop)
  | MaterializationDepReverseMissing
      !(CarrierReuseId ctx prop)
      !Int
  | MaterializationDepReverseStale
      !(CarrierReuseId ctx prop)
      !Int
  | MaterializationTopoReverseMissing
      !(CarrierReuseId ctx prop)
      !Int
  | MaterializationTopoReverseStale
      !(CarrierReuseId ctx prop)
      !Int
  deriving stock (Eq, Show)

emptyReuseMaterializationIndex ::
  (Ord ctx, Ord prop) =>
  ReuseMaterializationIndex ctx prop
emptyReuseMaterializationIndex =
  ReuseMaterializationIndex
    { rmiRegistry = emptyIndexedRegistry reuseMaterializationOps
    }
{-# INLINE emptyReuseMaterializationIndex #-}

emptyReuseMaterializationReverseIndex :: ReuseMaterializationReverseIndex ctx prop
emptyReuseMaterializationReverseIndex =
  ReuseMaterializationReverseIndex
    { rmriByTarget = Map.empty,
      rmriByDep = IntMap.empty,
      rmriByTopo = IntMap.empty
    }
{-# INLINE emptyReuseMaterializationReverseIndex #-}

lookupInstalledReuseMaterialization ::
  Ord ctx =>
  Ord prop =>
  CarrierReuseId ctx prop ->
  ReuseMaterializationIndex ctx prop ->
  Maybe (InstalledReuseMaterialization ctx prop)
lookupInstalledReuseMaterialization reuseId =
  lookupRegistryRow reuseId . rmiRegistry
{-# INLINE lookupInstalledReuseMaterialization #-}

upsertInstalledReuseMaterialization ::
  Ord ctx =>
  Ord prop =>
  InstalledReuseMaterialization ctx prop ->
  ReuseMaterializationIndex ctx prop ->
  (RowDelta, ReuseMaterializationIndex ctx prop)
upsertInstalledReuseMaterialization installed index0 =
  let (previous, registry1) =
        upsertRegistryRow reuseMaterializationOps installed (rmiRegistry index0)
      deltaRows =
        case previous of
          Nothing ->
            irmRows installed
          Just oldInstalled ->
            subtractPlainRowPatch (irmRows installed) (irmRows oldInstalled)
   in (deltaRows, ReuseMaterializationIndex {rmiRegistry = registry1})
{-# INLINE upsertInstalledReuseMaterialization #-}

insertMaterializationReverse ::
  Ord ctx =>
  Ord prop =>
  CarrierReuseId ctx prop ->
  InstalledReuseMaterialization ctx prop ->
  ReuseMaterializationReverseIndex ctx prop ->
  ReuseMaterializationReverseIndex ctx prop
insertMaterializationReverse reuseId installed reverse0 =
  reverse0
    { rmriByTarget =
        insertMapAxis
          reuseId
          (Set.singleton (irmTarget installed))
          (rmriByTarget reverse0),
      rmriByDep =
        addMembership
          reuseId
          (irmDeps installed)
          (rmriByDep reverse0),
      rmriByTopo =
        addMembership
          reuseId
          (irmTopo installed)
          (rmriByTopo reverse0)
    }
{-# INLINE insertMaterializationReverse #-}

removeInstalledReuseMaterialization ::
  Ord ctx =>
  Ord prop =>
  CarrierReuseId ctx prop ->
  ReuseMaterializationIndex ctx prop ->
  (Maybe (InstalledReuseMaterialization ctx prop), ReuseMaterializationIndex ctx prop)
removeInstalledReuseMaterialization reuseId index =
  let (deleted, registry) =
        deleteRegistryRowReturning reuseMaterializationOps reuseId (rmiRegistry index)
   in (deleted, ReuseMaterializationIndex {rmiRegistry = registry})
{-# INLINE removeInstalledReuseMaterialization #-}

dropInstalledReuseMaterializationsForCarrier ::
  Ord ctx =>
  Ord prop =>
  CarrierAddr ctx Carrier prop ->
  ReuseMaterializationIndex ctx prop ->
  ReuseMaterializationIndex ctx prop
dropInstalledReuseMaterializationsForCarrier target index =
  Set.foldl'
    (flip dropInstalledReuseMaterialization)
    index
    (Map.findWithDefault Set.empty target (rmriByTarget (rmiIndex index)))
{-# INLINE dropInstalledReuseMaterializationsForCarrier #-}

dropInstalledReuseMaterializationsForReuses ::
  Ord ctx =>
  Ord prop =>
  Set (CarrierReuseId ctx prop) ->
  ReuseMaterializationIndex ctx prop ->
  ReuseMaterializationIndex ctx prop
dropInstalledReuseMaterializationsForReuses reuseIds index =
  Set.foldl'
    (flip dropInstalledReuseMaterialization)
    index
    reuseIds
{-# INLINE dropInstalledReuseMaterializationsForReuses #-}

selectStaleInstalledReuseMaterializations ::
  Ord ctx =>
  Ord prop =>
  RelationalScope ->
  ReuseMaterializationIndex ctx prop ->
  [(CarrierReuseId ctx prop, InstalledReuseMaterialization ctx prop)]
selectStaleInstalledReuseMaterializations dirty index =
  [ (reuseId, installed)
  | reuseId <- Set.toAscList candidateReuseIds,
    Just installed <- [Map.lookup reuseId (rmiInstalledByReuse index)],
    installedReuseMaterializationStale dirtyDeps dirtyTopo installed
  ]
  where
    dirtyDeps =
      scopeDeps dirty

    dirtyTopo =
      scopeTopo dirty

    reverseIndex =
      rmiIndex index

    candidateReuseIds =
      Set.union
        (lookupMany (rmriByDep reverseIndex) dirtyDeps)
        (lookupMany (rmriByTopo reverseIndex) dirtyTopo)
{-# INLINE selectStaleInstalledReuseMaterializations #-}

dropInstalledReuseMaterialization ::
  Ord ctx =>
  Ord prop =>
  CarrierReuseId ctx prop ->
  ReuseMaterializationIndex ctx prop ->
  ReuseMaterializationIndex ctx prop
dropInstalledReuseMaterialization reuseId index =
  ReuseMaterializationIndex
    { rmiRegistry =
        deleteRegistryRow reuseMaterializationOps reuseId (rmiRegistry index)
    }
{-# INLINE dropInstalledReuseMaterialization #-}

dropMaterializationReverse ::
  Ord ctx =>
  Ord prop =>
  CarrierReuseId ctx prop ->
  InstalledReuseMaterialization ctx prop ->
  ReuseMaterializationReverseIndex ctx prop ->
  ReuseMaterializationReverseIndex ctx prop
dropMaterializationReverse reuseId installed reverse0 =
  reverse0
    { rmriByTarget =
        dropMapAxis
          reuseId
          (Set.singleton (irmTarget installed))
          (rmriByTarget reverse0),
      rmriByDep =
        dropMembership
          reuseId
          (irmDeps installed)
          (rmriByDep reverse0),
      rmriByTopo =
        dropMembership
          reuseId
          (irmTopo installed)
          (rmriByTopo reverse0)
    }
{-# INLINE dropMaterializationReverse #-}

validateReuseMaterializationIndex ::
  (Ord ctx, Ord prop) =>
  ReuseMaterializationIndex ctx prop ->
  Either [MaterializationInvariantError ctx prop] ()
validateReuseMaterializationIndex index =
  validateIndexedRegistry
    reuseMaterializationOps
    MaterializationStoredUnderWrongReuseId
    (rmiRegistry index)
{-# INLINE validateReuseMaterializationIndex #-}

reuseMaterializationOps ::
  (Ord ctx, Ord prop) =>
  RegistryOps
    (CarrierReuseId ctx prop)
    (InstalledReuseMaterialization ctx prop)
    (ReuseMaterializationReverseIndex ctx prop)
    (MaterializationInvariantError ctx prop)
reuseMaterializationOps =
  RegistryOps
    { registryRowId = irmReuseId,
      registryEmptyIndexes = emptyReuseMaterializationReverseIndex,
      registryInsertIndexes = insertMaterializationReverse,
      registryDeleteIndexes = dropMaterializationReverse,
      registryValidateIndexes = validateReuseMaterializationReverseIndexes
    }

validateReuseMaterializationReverseIndexes ::
  (Ord ctx, Ord prop) =>
  Map (CarrierReuseId ctx prop) (InstalledReuseMaterialization ctx prop) ->
  ReuseMaterializationReverseIndex ctx prop ->
  [MaterializationInvariantError ctx prop]
validateReuseMaterializationReverseIndexes installedByReuse reverseIndex =
  validateMapReverseIndex
    materializationTargetKeys
    MaterializationTargetReverseMissing
    MaterializationTargetReverseStale
    installedByReuse
    (rmriByTarget reverseIndex)
    <> validateIntReverseIndex
      materializationDepKeys
      MaterializationDepReverseMissing
      MaterializationDepReverseStale
      installedByReuse
      (rmriByDep reverseIndex)
    <> validateIntReverseIndex
      materializationTopoKeys
      MaterializationTopoReverseMissing
      MaterializationTopoReverseStale
      installedByReuse
      (rmriByTopo reverseIndex)
{-# INLINE validateReuseMaterializationReverseIndexes #-}

materializationTargetKeys ::
  CarrierReuseId ctx prop ->
  InstalledReuseMaterialization ctx prop ->
  Set (CarrierAddr ctx Carrier prop)
materializationTargetKeys _reuseId installed =
  Set.singleton (irmTarget installed)
{-# INLINE materializationTargetKeys #-}

materializationDepKeys ::
  CarrierReuseId ctx prop ->
  InstalledReuseMaterialization ctx prop ->
  IntSet
materializationDepKeys _reuseId =
  irmDeps
{-# INLINE materializationDepKeys #-}

materializationTopoKeys ::
  CarrierReuseId ctx prop ->
  InstalledReuseMaterialization ctx prop ->
  IntSet
materializationTopoKeys _reuseId =
  irmTopo
{-# INLINE materializationTopoKeys #-}

installedReuseMaterializationStale ::
  IntSet ->
  IntSet ->
  InstalledReuseMaterialization ctx prop ->
  Bool
installedReuseMaterializationStale dirtyDeps dirtyTopo installed =
  intSetIntersects dirtyDeps (irmDeps installed)
    || intSetIntersects dirtyTopo (irmTopo installed)
{-# INLINE installedReuseMaterializationStale #-}
