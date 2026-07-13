{-# LANGUAGE StandaloneKindSignatures #-}

-- | Quarantined slot coercion: sound only because handles are region-typed and
-- minted exactly once, so a slot is always read at the type its producer wrote.
module Moonlight.Differential.Circuit.Slot
  ( SlotValue,
    mkSlotValue,
    unsafeReadSlot,
  )
where

import Data.Kind
  ( Type,
  )
import GHC.Exts
  ( Any,
  )
import Unsafe.Coerce
  ( unsafeCoerce,
  )

type SlotValue :: Type
newtype SlotValue = SlotValue Any

mkSlotValue :: a -> SlotValue
mkSlotValue =
  SlotValue . unsafeCoerce
{-# INLINE mkSlotValue #-}

unsafeReadSlot :: SlotValue -> a
unsafeReadSlot (SlotValue heldValue) =
  unsafeCoerce heldValue
{-# INLINE unsafeReadSlot #-}
