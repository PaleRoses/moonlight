module Moonlight.EGraph.Pure.Rebuild.Index
  ( BaseRepairIndex (..),
    emptyRepairIndex,
    baseRepairIndexFromStore,
    CanonicalEpoch (..),
    ensureRepairIndex,
    canonicalEpochForGraph,
    canonicalizeClassKeys,
    parentKeysOf,
    repairClosure,
  )
where

import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Maybe (fromMaybe)
import Moonlight.Core (Language)
import Moonlight.Delta.Epoch (Version)
import Moonlight.EGraph.Pure.Structural.Store
  ( StructuralStore,
    structuralRepairIndex,
  )
import Moonlight.Repair.Index
  ( RepairIndex (..),
  )
import Moonlight.EGraph.Pure.Types (ClassId (..), ENode, classIdKey)
import Moonlight.EGraph.Pure.Types.Internal (EGraph (..))

type BaseRepairIndex :: (Type -> Type) -> Type
data BaseRepairIndex f = BaseRepairIndex
  { briIndex :: RepairIndex (ENode f)
  }

emptyRepairIndex :: BaseRepairIndex f
emptyRepairIndex =
  BaseRepairIndex
    { briIndex =
        RepairIndex
          { riParents = IntMap.empty,
            riChildren = IntMap.empty,
            riTuplesByResult = IntMap.empty
          }
    }

type CanonicalEpoch :: Type
data CanonicalEpoch = CanonicalEpoch
  { ceVersion :: !Version
  }
  deriving stock (Eq, Show)

ensureRepairIndex :: Language f => Maybe (BaseRepairIndex f) -> EGraph f a -> BaseRepairIndex f
ensureRepairIndex maybeIndex graph =
  fromMaybe (baseRepairIndexFromStore (egStore graph)) maybeIndex

canonicalEpochForGraph :: Version -> EGraph f a -> CanonicalEpoch
canonicalEpochForGraph epochVersion _graph =
  CanonicalEpoch epochVersion

canonicalizeClassKeys ::
  (ClassId -> ClassId) ->
  IntSet ->
  IntSet
canonicalizeClassKeys canonicalize =
  IntSet.map (classIdKey . canonicalize . ClassId)

parentKeysOf ::
  BaseRepairIndex f ->
  IntSet ->
  IntSet
parentKeysOf repairIndex =
  IntSet.foldl'
    (\acc key -> IntSet.union acc (IntMap.findWithDefault IntSet.empty key (riParents (briIndex repairIndex))))
    IntSet.empty

repairClosure :: BaseRepairIndex f -> IntSet -> IntSet
repairClosure repairIndex dirtyKeys =
  go dirtyKeys dirtyKeys
  where
    go frontier visited
      | IntSet.null frontier =
          visited
      | otherwise =
          let next =
                parentKeysOf repairIndex frontier
              fresh = IntSet.difference next visited
           in go fresh (IntSet.union visited fresh)

baseRepairIndexFromStore :: Language f => StructuralStore f -> BaseRepairIndex f
baseRepairIndexFromStore store =
  BaseRepairIndex
    { briIndex = structuralRepairIndex store
    }
