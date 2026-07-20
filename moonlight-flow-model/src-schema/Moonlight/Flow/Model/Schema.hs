{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Model.Schema
  ( Schema,
    SchemaError (..),
    mkSchema,
    schemaSlots,
    schemaIndex,
    schemaSlotSet,
    schemaWidth,
    schemaContains,
    schemaUnique,
  )
where

import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Core
  ( firstDuplicate,
  )

type Schema :: Type -> Type
data Schema slot = Schema
  { sSlots :: ![slot],
    sIndex :: !(Map slot Int),
    sSlotSet :: !(Set slot)
  }
  deriving stock (Eq, Ord, Show, Read)

type SchemaError :: Type -> Type
data SchemaError slot
  = SchemaDuplicateSlot !slot
  deriving stock (Eq, Ord, Show, Read)

mkSchema ::
  Ord slot =>
  [slot] ->
  Either (SchemaError slot) (Schema slot)
mkSchema slots = do
  schemaUnique slots
  let !index =
        Map.fromList
          [ (slot, ix)
          | (ix, slot) <- zip [0 :: Int ..] slots
          ]
      !slotSet =
        Set.fromList slots
  pure
    Schema
      { sSlots = slots,
        sIndex = index,
        sSlotSet = slotSet
      }
{-# INLINE mkSchema #-}

schemaSlots :: Schema slot -> [slot]
schemaSlots =
  sSlots
{-# INLINE schemaSlots #-}

schemaIndex :: Schema slot -> Map slot Int
schemaIndex =
  sIndex
{-# INLINE schemaIndex #-}

schemaSlotSet :: Schema slot -> Set slot
schemaSlotSet =
  sSlotSet
{-# INLINE schemaSlotSet #-}

schemaWidth :: Schema slot -> Int
schemaWidth =
  length . sSlots
{-# INLINE schemaWidth #-}

schemaContains ::
  Ord slot =>
  slot ->
  Schema slot ->
  Bool
schemaContains slot =
  Set.member slot . sSlotSet
{-# INLINE schemaContains #-}

schemaUnique ::
  Ord slot =>
  [slot] ->
  Either (SchemaError slot) ()
schemaUnique slots =
  maybe
    (Right ())
    (Left . SchemaDuplicateSlot)
    (firstDuplicate slots)
{-# INLINE schemaUnique #-}
