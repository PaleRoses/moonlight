{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Reuse.Internal.Index.Reuse
  ( CarrierReuseRegistry (..),
    CarrierReuseIndex (..),
    CarrierReuseIndexKeys (..),
    CarrierReuseRegistryInvariantError (..),
    crrReuses,
    crrIndex,
    emptyCarrierReuseRegistry,
    carrierReuseIndexKeys,
    carrierReuseRegistryEntries,
    carrierReuseRegistrySize,
    lookupCarrierReuseRegistry,
    carrierReuseRegistryIdsForCarrier,
    carrierReuseRegistryIdsForSource,
    carrierReuseRegistryIdsForTarget,
    carrierReuseRegistryStaleEntries,
    insertCarrierReuseRegistry,
    insertCarrierReuseRegistries,
    deleteCarrierReuseRegistry,
    carrierReuseStale,
    carrierReuseRegistryValid,
    validateCarrierReuseRegistry,
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
import Moonlight.Flow.Carrier.Morphism.Subsumption
  ( CarrierReuse (..),
    CarrierReuseId,
    ReuseWitness (..),
    carrierReuseExpectedTarget,
    carrierReuseId,
  )
import Moonlight.Differential.Index.Registry
  ( IndexedRegistry,
    RegistryOps (..),
    deleteRegistryRowReturning,
    emptyIndexedRegistry,
    insertRegistryRows,
    lookupRegistryRow,
    registryIndexes,
    registryRows,
    registryRowsAscList,
    registrySize,
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

newtype CarrierReuseRegistry ctx prop = CarrierReuseRegistry
  { crrRegistry ::
      IndexedRegistry
        (CarrierReuseId ctx prop)
        (CarrierReuse ctx prop)
        (CarrierReuseIndex ctx prop)
  }
  deriving stock (Eq, Show)

crrReuses ::
  CarrierReuseRegistry ctx prop ->
  Map (CarrierReuseId ctx prop) (CarrierReuse ctx prop)
crrReuses =
  registryRows . crrRegistry
{-# INLINE crrReuses #-}

crrIndex ::
  CarrierReuseRegistry ctx prop ->
  CarrierReuseIndex ctx prop
crrIndex =
  registryIndexes . crrRegistry
{-# INLINE crrIndex #-}

data CarrierReuseIndex ctx prop = CarrierReuseIndex
  { criBySource :: !(Map (CarrierAddr ctx Carrier prop) (Set (CarrierReuseId ctx prop))),
    criByTarget :: !(Map (CarrierAddr ctx Carrier prop) (Set (CarrierReuseId ctx prop))),
    criByDep :: !(IntMap (Set (CarrierReuseId ctx prop))),
    criByTopo :: !(IntMap (Set (CarrierReuseId ctx prop)))
  }
  deriving stock (Eq, Show)

data CarrierReuseIndexKeys ctx prop = CarrierReuseIndexKeys
  { crikSources :: !(Set (CarrierAddr ctx Carrier prop)),
    crikTargets :: !(Set (CarrierAddr ctx Carrier prop)),
    crikDeps :: !IntSet,
    crikTopo :: !IntSet
  }
  deriving stock (Eq, Show)

data CarrierReuseRegistryInvariantError ctx prop
  = CarrierReuseStoredUnderWrongId
      !(CarrierReuseId ctx prop)
      !(CarrierReuseId ctx prop)
  | CarrierReuseSourceReverseMissing
      !(CarrierReuseId ctx prop)
      !(CarrierAddr ctx Carrier prop)
  | CarrierReuseSourceReverseStale
      !(CarrierReuseId ctx prop)
      !(CarrierAddr ctx Carrier prop)
  | CarrierReuseTargetReverseMissing
      !(CarrierReuseId ctx prop)
      !(CarrierAddr ctx Carrier prop)
  | CarrierReuseTargetReverseStale
      !(CarrierReuseId ctx prop)
      !(CarrierAddr ctx Carrier prop)
  | CarrierReuseDepReverseMissing
      !(CarrierReuseId ctx prop)
      !Int
  | CarrierReuseDepReverseStale
      !(CarrierReuseId ctx prop)
      !Int
  | CarrierReuseTopoReverseMissing
      !(CarrierReuseId ctx prop)
      !Int
  | CarrierReuseTopoReverseStale
      !(CarrierReuseId ctx prop)
      !Int
  deriving stock (Eq, Show)

emptyCarrierReuseRegistry ::
  (Ord ctx, Ord prop) =>
  CarrierReuseRegistry ctx prop
emptyCarrierReuseRegistry =
  CarrierReuseRegistry
    { crrRegistry = emptyIndexedRegistry carrierReuseRegistryOps
    }
{-# INLINE emptyCarrierReuseRegistry #-}

emptyCarrierReuseIndex :: CarrierReuseIndex ctx prop
emptyCarrierReuseIndex =
  CarrierReuseIndex
    { criBySource = Map.empty,
      criByTarget = Map.empty,
      criByDep = IntMap.empty,
      criByTopo = IntMap.empty
    }
{-# INLINE emptyCarrierReuseIndex #-}

carrierReuseIndexKeys ::
  CarrierReuse ctx prop ->
  CarrierReuseIndexKeys ctx prop
carrierReuseIndexKeys reuse =
  CarrierReuseIndexKeys
    { crikSources =
        Set.singleton (rwSourceCarrier witness),
      crikTargets =
        maybe Set.empty Set.singleton (carrierReuseExpectedTarget reuse),
      crikDeps =
        cruWitnessDeps reuse,
      crikTopo =
        cruWitnessTopo reuse
    }
  where
    witness =
      cruWitness reuse
{-# INLINE carrierReuseIndexKeys #-}

carrierReuseRegistryEntries ::
  CarrierReuseRegistry ctx prop ->
  [(CarrierReuseId ctx prop, CarrierReuse ctx prop)]
carrierReuseRegistryEntries =
  registryRowsAscList . crrRegistry
{-# INLINE carrierReuseRegistryEntries #-}

carrierReuseRegistrySize ::
  CarrierReuseRegistry ctx prop ->
  Int
carrierReuseRegistrySize =
  registrySize . crrRegistry
{-# INLINE carrierReuseRegistrySize #-}

lookupCarrierReuseRegistry ::
  (Ord ctx, Ord prop) =>
  CarrierReuseId ctx prop ->
  CarrierReuseRegistry ctx prop ->
  Maybe (CarrierReuse ctx prop)
lookupCarrierReuseRegistry reuseId =
  lookupRegistryRow reuseId . crrRegistry
{-# INLINE lookupCarrierReuseRegistry #-}

carrierReuseRegistryIdsForCarrier ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  CarrierReuseRegistry ctx prop ->
  Set (CarrierReuseId ctx prop)
carrierReuseRegistryIdsForCarrier addr registry =
  carrierReuseRegistryIdsForSource addr registry
    <> carrierReuseRegistryIdsForTarget addr registry
{-# INLINE carrierReuseRegistryIdsForCarrier #-}

carrierReuseRegistryIdsForSource ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  CarrierReuseRegistry ctx prop ->
  Set (CarrierReuseId ctx prop)
carrierReuseRegistryIdsForSource addr registry =
  Map.findWithDefault Set.empty addr (criBySource (crrIndex registry))
{-# INLINE carrierReuseRegistryIdsForSource #-}

carrierReuseRegistryIdsForTarget ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  CarrierReuseRegistry ctx prop ->
  Set (CarrierReuseId ctx prop)
carrierReuseRegistryIdsForTarget addr registry =
  Map.findWithDefault Set.empty addr (criByTarget (crrIndex registry))
{-# INLINE carrierReuseRegistryIdsForTarget #-}

carrierReuseRegistryStaleEntries ::
  (Ord ctx, Ord prop) =>
  IntSet ->
  IntSet ->
  CarrierReuseRegistry ctx prop ->
  [(CarrierReuseId ctx prop, CarrierReuse ctx prop)]
carrierReuseRegistryStaleEntries dirtyDeps dirtyTopo registry =
  [ (reuseId, reuse)
  | reuseId <- Set.toAscList staleIds,
    Just reuse <- [Map.lookup reuseId (crrReuses registry)],
    carrierReuseStale dirtyDeps dirtyTopo reuse
  ]
  where
    index =
      crrIndex registry

    staleIds =
      lookupMany (criByDep index) dirtyDeps
        <> lookupMany (criByTopo index) dirtyTopo
{-# INLINE carrierReuseRegistryStaleEntries #-}

insertCarrierReuseRegistry ::
  (Ord ctx, Ord prop) =>
  CarrierReuse ctx prop ->
  CarrierReuseRegistry ctx prop ->
  CarrierReuseRegistry ctx prop
insertCarrierReuseRegistry reuse registry =
  CarrierReuseRegistry
    { crrRegistry =
        snd (upsertRegistryRow carrierReuseRegistryOps reuse (crrRegistry registry))
    }
{-# INLINE insertCarrierReuseRegistry #-}

insertCarrierReuseRegistries ::
  (Ord ctx, Ord prop, Foldable f) =>
  f (CarrierReuse ctx prop) ->
  CarrierReuseRegistry ctx prop ->
  CarrierReuseRegistry ctx prop
insertCarrierReuseRegistries reuses registry =
  CarrierReuseRegistry
    { crrRegistry =
        insertRegistryRows carrierReuseRegistryOps reuses (crrRegistry registry)
    }
{-# INLINE insertCarrierReuseRegistries #-}

deleteCarrierReuseRegistry ::
  (Ord ctx, Ord prop) =>
  CarrierReuseId ctx prop ->
  CarrierReuseRegistry ctx prop ->
  (Maybe (CarrierReuse ctx prop), CarrierReuseRegistry ctx prop)
deleteCarrierReuseRegistry reuseId registry =
  let (deleted, nextRegistry) =
        deleteRegistryRowReturning carrierReuseRegistryOps reuseId (crrRegistry registry)
   in (deleted, CarrierReuseRegistry {crrRegistry = nextRegistry})
{-# INLINE deleteCarrierReuseRegistry #-}

insertCarrierReuseIndex ::
  (Ord ctx, Ord prop) =>
  CarrierReuseId ctx prop ->
  CarrierReuseIndexKeys ctx prop ->
  CarrierReuseIndex ctx prop ->
  CarrierReuseIndex ctx prop
insertCarrierReuseIndex reuseId keys index =
  index
    { criBySource =
        insertMapAxis reuseId (crikSources keys) (criBySource index),
      criByTarget =
        insertMapAxis reuseId (crikTargets keys) (criByTarget index),
      criByDep =
        addMembership reuseId (crikDeps keys) (criByDep index),
      criByTopo =
        addMembership reuseId (crikTopo keys) (criByTopo index)
    }
{-# INLINE insertCarrierReuseIndex #-}

deleteCarrierReuseIndex ::
  (Ord ctx, Ord prop) =>
  CarrierReuseId ctx prop ->
  CarrierReuseIndexKeys ctx prop ->
  CarrierReuseIndex ctx prop ->
  CarrierReuseIndex ctx prop
deleteCarrierReuseIndex reuseId keys index =
  index
    { criBySource =
        dropMapAxis reuseId (crikSources keys) (criBySource index),
      criByTarget =
        dropMapAxis reuseId (crikTargets keys) (criByTarget index),
      criByDep =
        dropMembership reuseId (crikDeps keys) (criByDep index),
      criByTopo =
        dropMembership reuseId (crikTopo keys) (criByTopo index)
    }
{-# INLINE deleteCarrierReuseIndex #-}

carrierReuseStale ::
  IntSet ->
  IntSet ->
  CarrierReuse ctx prop ->
  Bool
carrierReuseStale dirtyDeps dirtyTopo reuse =
  intSetIntersects dirtyDeps (cruWitnessDeps reuse)
    || intSetIntersects dirtyTopo (cruWitnessTopo reuse)
{-# INLINE carrierReuseStale #-}

carrierReuseRegistryValid ::
  (Ord ctx, Ord prop) =>
  CarrierReuseRegistry ctx prop ->
  Bool
carrierReuseRegistryValid registry =
  case validateCarrierReuseRegistry registry of
    Right () ->
      True
    Left _errors ->
      False
{-# INLINE carrierReuseRegistryValid #-}

validateCarrierReuseRegistry ::
  (Ord ctx, Ord prop) =>
  CarrierReuseRegistry ctx prop ->
  Either [CarrierReuseRegistryInvariantError ctx prop] ()
validateCarrierReuseRegistry registry =
  validateIndexedRegistry
    carrierReuseRegistryOps
    CarrierReuseStoredUnderWrongId
    (crrRegistry registry)
{-# INLINE validateCarrierReuseRegistry #-}

carrierReuseRegistryOps ::
  (Ord ctx, Ord prop) =>
  RegistryOps
    (CarrierReuseId ctx prop)
    (CarrierReuse ctx prop)
    (CarrierReuseIndex ctx prop)
    (CarrierReuseRegistryInvariantError ctx prop)
carrierReuseRegistryOps =
  RegistryOps
    { registryRowId = carrierReuseId,
      registryEmptyIndexes = emptyCarrierReuseIndex,
      registryInsertIndexes = \reuseId reuse ->
        insertCarrierReuseIndex reuseId (carrierReuseIndexKeys reuse),
      registryDeleteIndexes = \reuseId reuse ->
        deleteCarrierReuseIndex reuseId (carrierReuseIndexKeys reuse),
      registryValidateIndexes = validateCarrierReuseIndexes
    }

validateCarrierReuseIndexes ::
  (Ord ctx, Ord prop) =>
  Map (CarrierReuseId ctx prop) (CarrierReuse ctx prop) ->
  CarrierReuseIndex ctx prop ->
  [CarrierReuseRegistryInvariantError ctx prop]
validateCarrierReuseIndexes rows index =
  validateMapReverseIndex
    carrierReuseSourceKeys
    CarrierReuseSourceReverseMissing
    CarrierReuseSourceReverseStale
    rows
    (criBySource index)
    <> validateMapReverseIndex
      carrierReuseTargetKeys
      CarrierReuseTargetReverseMissing
      CarrierReuseTargetReverseStale
      rows
      (criByTarget index)
    <> validateIntReverseIndex
      carrierReuseDepKeys
      CarrierReuseDepReverseMissing
      CarrierReuseDepReverseStale
      rows
      (criByDep index)
    <> validateIntReverseIndex
      carrierReuseTopoKeys
      CarrierReuseTopoReverseMissing
      CarrierReuseTopoReverseStale
      rows
      (criByTopo index)
{-# INLINE validateCarrierReuseIndexes #-}

carrierReuseSourceKeys ::
  CarrierReuseId ctx prop ->
  CarrierReuse ctx prop ->
  Set (CarrierAddr ctx Carrier prop)
carrierReuseSourceKeys _reuseId =
  crikSources . carrierReuseIndexKeys
{-# INLINE carrierReuseSourceKeys #-}

carrierReuseTargetKeys ::
  CarrierReuseId ctx prop ->
  CarrierReuse ctx prop ->
  Set (CarrierAddr ctx Carrier prop)
carrierReuseTargetKeys _reuseId =
  crikTargets . carrierReuseIndexKeys
{-# INLINE carrierReuseTargetKeys #-}

carrierReuseDepKeys ::
  CarrierReuseId ctx prop ->
  CarrierReuse ctx prop ->
  IntSet
carrierReuseDepKeys _reuseId =
  crikDeps . carrierReuseIndexKeys
{-# INLINE carrierReuseDepKeys #-}

carrierReuseTopoKeys ::
  CarrierReuseId ctx prop ->
  CarrierReuse ctx prop ->
  IntSet
carrierReuseTopoKeys _reuseId =
  crikTopo . carrierReuseIndexKeys
{-# INLINE carrierReuseTopoKeys #-}
