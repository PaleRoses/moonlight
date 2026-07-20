{-# LANGUAGE BangPatterns #-}

module Moonlight.Delta.Patch.Internal.MultisetDeltaHash
  ( MultisetDeltaHash,
    buildMultisetDeltaHash,
    multisetDeltaHashState,
    multisetDeltaHashDigest,
    applyMultisetDeltaHash,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed qualified as UVector
import Data.Word (Word64)
import Moonlight.Algebra.Pure.LaneVector
  ( LaneVector,
    laneCount,
    laneVectorFromLanes,
    laneVectorLanes,
    laneVectorZero,
  )
import Moonlight.Core
  ( AdditiveGroup (sub),
    AdditiveMonoid (add),
    SipKey (..),
    StableHashDigest (..),
    StableHashEncoding,
    stableHashEncodingDigest,
    stableHashEncodingLength,
    stableHashEncodingWord8,
    stableHashEncodingWord64LE,
  )
import Moonlight.Delta.Patch.Internal.Apply qualified as Patch
import Moonlight.Delta.Patch.Internal.Construction qualified as Patch
import Moonlight.Delta.Patch.Internal.IncrementalDigest
  ( DeltaHashApplyError (..),
    Digest128,
    IncrementalDigest (..),
    deltaHashEncodingVersion,
    digest128Encoding,
  )
import Moonlight.Delta.Patch.Internal.Types (Patch)
import Prelude

data MultisetDeltaHash key value = MultisetDeltaHash
  { multisetDeltaHashKeyEncoding :: !(key -> StableHashEncoding),
    multisetDeltaHashValueEncoding :: !(value -> StableHashEncoding),
    multisetDeltaHashAuthoritativeState :: !(Map key value),
    multisetDeltaHashAccumulator :: !LaneVector
  }

buildMultisetDeltaHash ::
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  Map key value ->
  MultisetDeltaHash key value
buildMultisetDeltaHash encodeKey encodeValue authoritativeState =
  MultisetDeltaHash
    { multisetDeltaHashKeyEncoding = encodeKey,
      multisetDeltaHashValueEncoding = encodeValue,
      multisetDeltaHashAuthoritativeState = authoritativeState,
      multisetDeltaHashAccumulator =
        Map.foldlWithKey'
          (addEncodedEntry encodeKey encodeValue)
          laneVectorZero
          authoritativeState
    }

multisetDeltaHashState :: MultisetDeltaHash key value -> Map key value
multisetDeltaHashState =
  multisetDeltaHashAuthoritativeState

multisetDeltaHashDigest :: MultisetDeltaHash key value -> Digest128
multisetDeltaHashDigest =
  digestLaneVector . multisetDeltaHashAccumulator

applyMultisetDeltaHash ::
  (Ord key, Eq value) =>
  Patch key value ->
  MultisetDeltaHash key value ->
  Either (DeltaHashApplyError key value) (MultisetDeltaHash key value)
applyMultisetDeltaHash patchValue currentMultisetDeltaHash = do
  updatedState <-
    either
      (Left . DeltaHashPatchRejected)
      Right
      (Patch.apply patchValue (multisetDeltaHashAuthoritativeState currentMultisetDeltaHash))
  let !encodeKey = multisetDeltaHashKeyEncoding currentMultisetDeltaHash
      !encodeValue = multisetDeltaHashValueEncoding currentMultisetDeltaHash
      !authoritativeState = multisetDeltaHashAuthoritativeState currentMultisetDeltaHash
      !updatedAccumulator =
        Patch.foldWithKey'
          const
          (addEncodedEntry encodeKey encodeValue)
          (\accumulator key before ->
             let (storedKey, storedValue) =
                   storedEntryRepresentative authoritativeState key before
              in subtractEncodedEntry encodeKey encodeValue accumulator storedKey storedValue
          )
          (\accumulator key before after ->
             let (storedKey, storedValue) =
                   storedEntryRepresentative authoritativeState key before
              in addEncodedEntry
                   encodeKey
                   encodeValue
                   (subtractEncodedEntry encodeKey encodeValue accumulator storedKey storedValue)
                   key
                   after
          )
          (multisetDeltaHashAccumulator currentMultisetDeltaHash)
          patchValue
  pure
    MultisetDeltaHash
      { multisetDeltaHashKeyEncoding = encodeKey,
        multisetDeltaHashValueEncoding = encodeValue,
        multisetDeltaHashAuthoritativeState = updatedState,
        multisetDeltaHashAccumulator = updatedAccumulator
      }

addEncodedEntry ::
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  LaneVector ->
  key ->
  value ->
  LaneVector
addEncodedEntry encodeKey encodeValue accumulator key value =
  add accumulator (expand (encodeKey key) (encodeValue value))

subtractEncodedEntry ::
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  LaneVector ->
  key ->
  value ->
  LaneVector
subtractEncodedEntry encodeKey encodeValue accumulator key value =
  sub accumulator (expand (encodeKey key) (encodeValue value))

expand :: StableHashEncoding -> StableHashEncoding -> LaneVector
expand keyEncoding valueEncoding =
  laneVectorFromLanes
    ( UVector.generate
        laneCount
        (\laneIndex ->
           stableHashDigestWord
             multisetExpansionSipKey
             (multisetElementEncoding laneIndex keyEncoding valueEncoding)
        )
    )

multisetElementEncoding ::
  Int ->
  StableHashEncoding ->
  StableHashEncoding ->
  StableHashEncoding
multisetElementEncoding laneIndex keyEncoding valueEncoding =
  stableHashEncodingWord8 3
    <> stableHashEncodingWord64LE deltaHashEncodingVersion
    <> stableHashEncodingWord64LE (fromIntegral (stableHashEncodingLength keyEncoding))
    <> keyEncoding
    <> stableHashEncodingWord64LE (fromIntegral (stableHashEncodingLength valueEncoding))
    <> valueEncoding
    <> stableHashEncodingWord64LE (fromIntegral laneIndex)

digestLaneVector :: LaneVector -> Digest128
digestLaneVector laneVector =
  digest128Encoding
    multisetDigestSipKeyLane0
    multisetDigestSipKeyLane1
    ( stableHashEncodingWord8 4
        <> stableHashEncodingWord64LE deltaHashEncodingVersion
        <> stableHashEncodingWord64LE (fromIntegral laneCount)
        <> UVector.foldl'
          (\encoding lane -> encoding <> stableHashEncodingWord64LE lane)
          mempty
          (laneVectorLanes laneVector)
    )

storedEntryRepresentative :: Ord key => Map key value -> key -> value -> (key, value)
storedEntryRepresentative authoritativeState patchKey patchValue =
  case Map.lookupGE patchKey authoritativeState of
    Just storedEntry@(storedKey, _storedValue)
      | compare storedKey patchKey == EQ -> storedEntry
    _ -> (patchKey, patchValue)

stableHashDigestWord :: SipKey -> StableHashEncoding -> Word64
stableHashDigestWord sipKey encoding =
  case stableHashEncodingDigest sipKey encoding of
    StableHashDigest digestWord -> digestWord

multisetExpansionSipKey :: SipKey
multisetExpansionSipKey =
  SipKey 0x6d6c2d64656c7461 0x6d756c74692d7632

multisetDigestSipKeyLane0 :: SipKey
multisetDigestSipKeyLane0 =
  SipKey 0x6d6c2d64656c7461 0x6d7365742d763031

multisetDigestSipKeyLane1 :: SipKey
multisetDigestSipKeyLane1 =
  SipKey 0x6d6c2d64656c7461 0x6d7365742d763032

instance IncrementalDigest MultisetDeltaHash where
  type IncrementalDigestError MultisetDeltaHash key value = DeltaHashApplyError key value
  empty encodeKey encodeValue = buildMultisetDeltaHash encodeKey encodeValue Map.empty
  applyPatch = applyMultisetDeltaHash
  digest = multisetDeltaHashDigest
