{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Morphism.Internal.Digest
  ( boundaryProjectionWords,
    carrierAddrPayloadWords,
    containmentProofWords,
    maybePayloadWords,
    boundaryProjectionProofWords,
  )
where

import Data.Word
  ( Word64,
  )
import Moonlight.Core
  ( atomIdKey,
    queryIdKey,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    DerivedCarrierId (..),
    QueryCarrierNode (..),
    SubsumptionWitnessDigest (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caCarrier,
  )
import Moonlight.Flow.Execution.Subsumption.Proof
  ( BoundaryProjectionProof (..),
    ContainmentProof (..),
  )
import Moonlight.Flow.Model.Schema.Morphism
  ( BoundaryProjection (..),
    ProjectionProfile (..),
    projectionProfile,
  )
import Moonlight.Flow.Internal.Digest
  ( wordOfInt,
  )
import Moonlight.Flow.Plan.Query.Core
  ( BagId (..),
    FactorNode (..),
  )
import Moonlight.Flow.Plan.Shape.Encode
  ( canonicalSlotWords,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot,
    canonSlotKey,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( stableDigestWords,
  )
import Moonlight.Flow.Model.Schema.Digest.Words
  ( digestMaybeWords,
  )

boundaryProjectionWords :: BoundaryProjection CanonSlot -> [Word64]
boundaryProjectionWords (BoundaryProjection slotProjection) =
  [0x6270] <> stableDigestWords (ppDigest (projectionProfile canonSlotKey canonicalSlotWords slotProjection))
{-# INLINE boundaryProjectionWords #-}

containmentProofWords :: ContainmentProof -> [Word64]
containmentProofWords proof =
  [0x6370] <> stableDigestWords (cpProjectionDigest proof)
{-# INLINE containmentProofWords #-}

boundaryProjectionProofWords :: BoundaryProjectionProof -> [Word64]
boundaryProjectionProofWords proof =
  [0x627070] <> stableDigestWords (bppDigest proof)
{-# INLINE boundaryProjectionProofWords #-}

maybePayloadWords :: (value -> [Word64]) -> Maybe value -> [Word64]
maybePayloadWords =
  digestMaybeWords 0x00 0x01
{-# INLINE maybePayloadWords #-}

carrierAddrPayloadWords :: CarrierAddr ctx Carrier prop -> [Word64]
carrierAddrPayloadWords addr =
  [0x10] <> carrierPayloadWords (caCarrier addr)
{-# INLINE carrierAddrPayloadWords #-}

carrierPayloadWords :: Carrier -> [Word64]
carrierPayloadWords carrier =
  case carrier of
    QueryCarrier queryId (QueryAtom atomId) ->
      [0x11, wordOfInt (queryIdKey queryId), wordOfInt (atomIdKey atomId)]
    QueryCarrier queryId (QueryFactor factorNode) ->
      [0x12, wordOfInt (queryIdKey queryId)] <> factorNodePayloadWords factorNode
    DerivedCarrier derived ->
      [0x13]
        <> stableDigestWords (unSubsumptionWitnessDigest (dciWitness derived))
        <> stableDigestWords (dciShape derived)
{-# INLINE carrierPayloadWords #-}

factorNodePayloadWords :: FactorNode -> [Word64]
factorNodePayloadWords node =
  case node of
    FactorNodeRoot ->
      [0x20]
    FactorNodeBag (BagId bagKey) ->
      [0x21, wordOfInt bagKey]
    FactorNodeBagBelief (BagId bagKey) ->
      [0x22, wordOfInt bagKey]
    FactorNodeSeparator (BagId childKey) (BagId parentKey) ->
      [0x23, wordOfInt childKey, wordOfInt parentKey]
{-# INLINE factorNodePayloadWords #-}
