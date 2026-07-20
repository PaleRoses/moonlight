{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Trace.ReadIndex
  ( TimeFrontier (..),
    TimeIndex,
    timeIndexMap,
    emptyTimeIndex,
    insertTimeIndex,
    deleteTimeIndex,
    sliceTimeIndexAfter,
    sliceTimeIndexAfterDescription,
  )
where

import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( PartialOrder (..),
  )
import Moonlight.Differential.Trace.Description
  ( TraceDescription,
    TraceDescriptionReadError,
    traceDescriptionReadAfter,
  )

type TimeFrontier :: Type -> Type -> Type
data TimeFrontier epoch stamp = TimeFrontier
  { tfEpoch :: !epoch,
    tfStamp :: !stamp
  }
  deriving stock (Eq, Ord, Show, Read)

instance (PartialOrder epoch, PartialOrder stamp) => PartialOrder (TimeFrontier epoch stamp) where
  left `leq` right =
    tfEpoch left `leq` tfEpoch right
      && tfStamp left `leq` tfStamp right

type TimeIndex :: Type -> Type -> Type -> Type
newtype TimeIndex key epoch stamp = TimeIndex
  { timeIndexMapRaw :: Map key (Map epoch (Map stamp IntSet))
  }
  deriving stock (Eq, Ord, Show)

timeIndexMap :: TimeIndex key epoch stamp -> Map key (Map epoch (Map stamp IntSet))
timeIndexMap =
  timeIndexMapRaw
{-# INLINE timeIndexMap #-}

emptyTimeIndex :: TimeIndex key epoch stamp
emptyTimeIndex =
  TimeIndex Map.empty

insertTimeIndex ::
  (Ord key, Ord epoch, Ord stamp) =>
  key ->
  epoch ->
  stamp ->
  IntSet ->
  TimeIndex key epoch stamp ->
  TimeIndex key epoch stamp
insertTimeIndex key epoch stamp members (TimeIndex index)
  | IntSet.null members =
      TimeIndex index
  | otherwise =
      TimeIndex
        ( Map.insertWith
            (Map.unionWith (Map.unionWith IntSet.union))
            key
            (Map.singleton epoch (Map.singleton stamp members))
            index
        )

deleteTimeIndex ::
  (Ord key, Ord epoch, Ord stamp) =>
  key ->
  epoch ->
  stamp ->
  Int ->
  TimeIndex key epoch stamp ->
  TimeIndex key epoch stamp
deleteTimeIndex key epoch stamp member (TimeIndex index) =
  TimeIndex (Map.update deleteKey key index)
  where
    deleteKey byEpoch =
      pruneMap (Map.update deleteEpoch epoch byEpoch)

    deleteEpoch byStamp =
      pruneMap (Map.update deleteStamp stamp byStamp)

    deleteStamp members =
      pruneIntSet (IntSet.delete member members)

sliceTimeIndexAfter ::
  (Ord key, Ord epoch, Ord stamp) =>
  key ->
  TimeFrontier epoch stamp ->
  TimeIndex key epoch stamp ->
  IntSet
sliceTimeIndexAfter key frontier (TimeIndex index) =
  case Map.lookup key index >>= Map.lookup (tfEpoch frontier) of
    Nothing ->
      IntSet.empty
    Just byStamp ->
      let (_atOrBefore, afterFrontier) =
            Map.split (tfStamp frontier) byStamp
       in foldMap id (Map.elems afterFrontier)

sliceTimeIndexAfterDescription ::
  (Ord key, Ord epoch, Ord stamp, PartialOrder epoch, PartialOrder stamp) =>
  key ->
  TimeFrontier epoch stamp ->
  TraceDescription (TimeFrontier epoch stamp) ->
  TimeIndex key epoch stamp ->
  Either (TraceDescriptionReadError (TimeFrontier epoch stamp)) IntSet
sliceTimeIndexAfterDescription key readFrontier description index = do
  traceDescriptionReadAfter readFrontier description
  pure (sliceTimeIndexAfter key readFrontier index)

pruneMap :: Map key value -> Maybe (Map key value)
pruneMap values
  | Map.null values =
      Nothing
  | otherwise =
      Just values

pruneIntSet :: IntSet -> Maybe IntSet
pruneIntSet values
  | IntSet.null values =
      Nothing
  | otherwise =
      Just values
