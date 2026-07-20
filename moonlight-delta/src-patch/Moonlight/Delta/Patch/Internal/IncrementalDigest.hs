{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Delta.Patch.Internal.IncrementalDigest
  ( Digest128,
    DeltaHashDigest (..),
    DeltaHashBuildError (..),
    DeltaHashApplyError (..),
    IncrementalDigest (..),
    deltaHashEncodingVersion,
    digest128Encoding,
    digest128EncodingChunks,
  )
where

import Data.Bits (Bits (shiftR, (.&.), (.|.)))
import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Moonlight.Core
  ( SipHashState,
    SipKey,
    StableHashDigest (..),
    StableHashEncoding,
    stableHashEncodingDigest,
    stableHashEncodingLength,
    stableHashEncodingVersion,
    stableHashUpdateEncoding,
    sipHashFinalize,
    sipHashInit,
    sipHashUpdateWord8,
    sipHashUpdateWord64LE,
  )
import Moonlight.Delta.Patch.Internal.Types (ApplyError, Patch)
import Prelude

data DeltaHashDigest = DeltaHashDigest
  { deltaHashDigestLane0 :: {-# UNPACK #-} !Word64,
    deltaHashDigestLane1 :: {-# UNPACK #-} !Word64
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

type Digest128 :: Type
type Digest128 = DeltaHashDigest

data DeltaHashBuildError key = DeltaHashKeyCollision
  { deltaHashCollisionDigest :: !StableHashDigest,
    deltaHashExistingKey :: !key,
    deltaHashIncomingKey :: !key
  }
  deriving stock (Eq, Ord, Show)

data DeltaHashApplyError key value
  = DeltaHashPatchRejected !(ApplyError key value)
  | DeltaHashUpdateRejected !(DeltaHashBuildError key)
  deriving stock (Eq, Ord, Show)

class IncrementalDigest (derived :: Type -> Type -> Type) where
  type IncrementalDigestError derived key value :: Type
  empty :: (key -> StableHashEncoding) -> (value -> StableHashEncoding) -> derived key value
  applyPatch ::
    (Ord key, Eq value) =>
    Patch key value ->
    derived key value ->
    Either (IncrementalDigestError derived key value) (derived key value)
  digest :: derived key value -> Digest128

deltaHashEncodingVersion :: Word64
deltaHashEncodingVersion = 2

digest128Encoding :: SipKey -> SipKey -> StableHashEncoding -> Digest128
digest128Encoding lane0Key lane1Key encoding =
  DeltaHashDigest (stableHashDigestWord lane0Key encoding) (stableHashDigestWord lane1Key encoding)

digest128EncodingChunks ::
  Foldable values =>
  SipKey ->
  SipKey ->
  (value -> StableHashEncoding) ->
  values value ->
  Digest128
digest128EncodingChunks lane0Key lane1Key project values =
  finalizeDigest128State (Foldable.foldl' absorbEncoding initialState values)
  where
    !chunkCount = fromIntegral (Foldable.length values)
    !initialState =
      Digest128HashState
        (framedChunksSeed lane0Key chunkCount)
        (framedChunksSeed lane1Key chunkCount)
    absorbEncoding (Digest128HashState lane0 lane1) value =
      let !encoding = project value
          !encodedLength = fromIntegral (stableHashEncodingLength encoding)
       in Digest128HashState
            (stableHashUpdateEncoding (sipHashUpdateCompactWord64 lane0 encodedLength) encoding)
            (stableHashUpdateEncoding (sipHashUpdateCompactWord64 lane1 encodedLength) encoding)

data Digest128HashState = Digest128HashState !SipHashState !SipHashState

finalizeDigest128State :: Digest128HashState -> Digest128
finalizeDigest128State (Digest128HashState lane0 lane1) =
  DeltaHashDigest (sipHashFinalize lane0) (sipHashFinalize lane1)

stableHashDigestWord :: SipKey -> StableHashEncoding -> Word64
stableHashDigestWord sipKey encoding =
  case stableHashEncodingDigest sipKey encoding of
    StableHashDigest digestWord -> digestWord

framedChunksSeed :: SipKey -> Word64 -> SipHashState
framedChunksSeed sipKey =
  sipHashUpdateCompactWord64
    (sipHashUpdateWord64LE (sipHashInit sipKey) stableHashEncodingVersion)

sipHashUpdateCompactWord64 :: SipHashState -> Word64 -> SipHashState
sipHashUpdateCompactWord64 state wordValue =
  let !byteValue = fromIntegral (wordValue .&. 0x7f)
      !remainingValue = wordValue `shiftR` 7
      !updatedState =
        sipHashUpdateWord8 state (if remainingValue == 0 then byteValue else byteValue .|. 0x80)
   in if remainingValue == 0
        then updatedState
        else sipHashUpdateCompactWord64 updatedState remainingValue
