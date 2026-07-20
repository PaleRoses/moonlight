{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Carrier.Core.Delta
  ( rowDeltaDigest,
    RelationalCarrierDeltaP (..),
    RelationalCarrierDelta,
  )
where

import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Word
  ( Word64,
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange,
    multiplicityChangeValue
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( RelationalOrigin,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Flow.Internal.Digest
  ( wordOfInt,
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchChangeMap
  )

import Moonlight.Flow.Model.Scope
  ( RelationalScope,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
    stableDigest128,
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.FiniteLattice
  ( SupportBasis
  )

rowDeltaDigest :: RowDelta -> StableDigest128
rowDeltaDigest rows =
  stableDigest128
    ( [0x7264656c7461, wordOfInt (Map.size plainRows)]
        <> foldMap rowMultiplicityWords (Map.toAscList plainRows)
    )
  where
    plainRows =
      plainRowPatchChangeMap rows

    rowMultiplicityWords :: (RowTupleKey, MultiplicityChange) -> [Word64]
    rowMultiplicityWords (rowValue, multiplicity) =
      [0x01]
        <> atomRowDigestWords rowValue
        <> [fromIntegral (multiplicityChangeValue multiplicity)]
{-# INLINE rowDeltaDigest #-}

atomRowDigestWords :: RowTupleKey -> [Word64]
atomRowDigestWords rowValue =
  [0x20, wordOfInt (tupleKeyWidth rowValue)]
    <> fmap wordOfInt (tupleKeyToInts rowValue)
{-# INLINE atomRowDigestWords #-}

type RelationalCarrierDeltaP :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data RelationalCarrierDeltaP ctx carrier prop boundary evidence payload = RelationalCarrierDelta
  { deAddr :: !(CarrierAddr ctx carrier prop),
    deTime :: !(RelationalCarrierTime ctx),
    deSupport :: !(SupportBasis ctx),
    deBoundary :: !boundary,
    deEvidence :: !evidence,
    deOrigin :: !(RelationalOrigin ctx carrier prop),
    deScope :: !RelationalScope,
    deRows :: !RowDelta,
    dePayload :: !payload
  }
  deriving stock (Eq, Show)

type RelationalCarrierDelta :: Type -> Type -> Type -> Type -> Type -> Type
type RelationalCarrierDelta ctx carrier prop boundary evidence =
  RelationalCarrierDeltaP ctx carrier prop boundary evidence ()
