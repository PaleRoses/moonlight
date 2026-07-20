{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Execution.Prepared.Topology
  ( PreparedTopologyStamp (..),
    preparedTopologyStamp,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Foldable qualified as Foldable
import Data.Word (Word64)
import Moonlight.Flow.Internal.Digest
  ( digestWordsHigh,
    digestWordsLow,
    wordOfInt,
  )
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Storage.Relation
  ( Relation,
    relationLayout,
  )
import Moonlight.Flow.Storage.Separator
  ( SeparatorIndex (..),
    SeparatorSpec (..),
  )
import Moonlight.Flow.Storage.Store
  ( Store,
    storeRelations,
    storeSeparatorCache,
  )

data PreparedTopologyStamp = PreparedTopologyStamp
  { ptsDigestHigh :: {-# UNPACK #-} !Word64,
    ptsDigestLow :: {-# UNPACK #-} !Word64
  }
  deriving stock (Eq, Ord, Show)

preparedTopologyStamp ::
  PlanCacheKey ->
  Store ->
  PreparedTopologyStamp
preparedTopologyStamp planKey db =
  let words0 =
        [ 0x70726570546f706f,
          pckDigestHigh planKey,
          pckDigestLow planKey
        ]
          <> storeRelationsTopologyWords db
          <> separatorTopologyWords db
   in PreparedTopologyStamp
        { ptsDigestHigh = digestWordsHigh words0,
          ptsDigestLow = digestWordsLow words0
        }

storeRelationsTopologyWords :: Store -> [Word64]
storeRelationsTopologyWords store =
  listWords
    0x10
    relationTopologyWords
    (IntMap.toAscList (storeRelations store))

relationTopologyWords :: (Int, Relation) -> [Word64]
relationTopologyWords (atomKey, relation) =
  [0x20, wordOfInt atomKey]
    <> slotListWords 0x21 (relationLayout relation)

separatorTopologyWords :: Store -> [Word64]
separatorTopologyWords store =
  listWords
    0x30
    separatorEntryWords
    (Map.toAscList (storeSeparatorCache store))

separatorEntryWords :: (SeparatorSpec, SeparatorIndex) -> [Word64]
separatorEntryWords (SeparatorSpec atomId slots, sepIndex) =
  [0x40, wordOfInt (atomIdKey atomId)]
    <> slotListWords 0x41 slots
    <> slotListWords 0x42 (siSlots sepIndex)

slotListWords :: Foldable f => Word64 -> f SlotId -> [Word64]
slotListWords tag slots =
  listWords tag (\slotIdValue -> [wordOfInt (slotIdKey slotIdValue)]) (Foldable.toList slots)

listWords :: Word64 -> (a -> [Word64]) -> [a] -> [Word64]
listWords tag encode values =
  tag : wordOfInt (length values) : foldMap encode values
{-# INLINE listWords #-}
