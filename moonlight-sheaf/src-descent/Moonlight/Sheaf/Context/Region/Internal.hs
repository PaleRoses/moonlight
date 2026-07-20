{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Context.Region.Internal
  ( ContextObjectKey (..),
    RegionCube (..),
    ContextRegion (..),
    DenseRegionTable (..),
    RegionTable (..),
    regionTableFromUpsets,
    powersetRegionTable,
    packKeySet,
  )
where

import Data.Bits (bit, setBit)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Numeric.Natural (Natural)

type ContextObjectKey :: Type -> Type
newtype ContextObjectKey owner = ContextObjectKey
  { contextObjectKeyValue :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type role ContextObjectKey nominal

type RegionCube :: Type
data RegionCube = RegionCube
  { regionCubeMust :: !Int,
    regionCubeMay :: !Int
  }
  deriving stock (Eq, Ord, Show)

type ContextRegion :: Type -> Type
data ContextRegion owner
  = RegionVoid
  | RegionDense !Natural
  | RegionCubes ![RegionCube]
  deriving stock (Eq, Ord, Show)

type role ContextRegion nominal

type DenseRegionTable :: Type -> Type
data DenseRegionTable owner = DenseRegionTable
  { drtObjectCount :: !Int,
    drtUpsetByKey :: !(IntMap (ContextRegion owner)),
    drtStrictLowerByKey :: !(IntMap (ContextRegion owner)),
    drtTop :: !(ContextRegion owner)
  }
  deriving stock (Eq, Show)

type role DenseRegionTable nominal

type RegionTable :: Type -> Type
data RegionTable owner
  = RegionTableDense !(DenseRegionTable owner)
  | RegionTablePowerset !Int
  deriving stock (Eq, Show)

type role RegionTable nominal

regionTableFromUpsets :: Int -> IntMap IntSet -> IntMap IntSet -> RegionTable owner
regionTableFromUpsets objectCount upsetRows strictLowerRows =
  RegionTableDense
    DenseRegionTable
      { drtObjectCount = objectCount,
        drtUpsetByKey = fmap packKeySet upsetRows,
        drtStrictLowerByKey = fmap packKeySet strictLowerRows,
        drtTop = packedTop objectCount
      }
{-# INLINE regionTableFromUpsets #-}

powersetRegionTable :: Int -> RegionTable owner
powersetRegionTable =
  RegionTablePowerset
{-# INLINE powersetRegionTable #-}

packKeySet :: IntSet -> ContextRegion owner
packKeySet keySet =
  let packedBits = IntSet.foldl' setBit 0 keySet
   in if packedBits == 0 then RegionVoid else RegionDense packedBits
{-# INLINE packKeySet #-}

packedTop :: Int -> ContextRegion owner
packedTop objectCount =
  let packedBits = bit objectCount - 1
   in if packedBits == 0 then RegionVoid else RegionDense packedBits
{-# INLINE packedTop #-}
