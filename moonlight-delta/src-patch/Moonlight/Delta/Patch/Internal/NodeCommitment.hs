module Moonlight.Delta.Patch.Internal.NodeCommitment
  ( DigestSipKeys (..),
    NodeCommitment (..),
    SipHashNodeCommitment,
    sipHashNodeCommitment,
  )
where

import Data.Word (Word64)
import Moonlight.Core
  ( SipKey (..),
    StableHashDigest (..),
    StableHashEncoding,
    stableHashEncodingDigest,
    stableHashEncodingWord8,
    stableHashEncodingWord64LE,
  )
import Moonlight.Delta.Patch.Internal.IncrementalDigest
  ( DeltaHashDigest (..),
    Digest128,
    deltaHashEncodingVersion,
    digest128Encoding,
  )
import Prelude

data DigestSipKeys = DigestSipKeys !SipKey !SipKey

class NodeCommitment commitment where
  pathSipKey :: commitment -> SipKey
  valueSipKey :: commitment -> SipKey
  emptySipKey :: commitment -> DigestSipKeys
  leafSipKey :: commitment -> DigestSipKeys
  branchSipKey :: commitment -> DigestSipKeys

  localizePath :: commitment -> StableHashEncoding -> Word64
  localizePath commitment =
    stableHashDigestWord (pathSipKey commitment)

  commitValue :: commitment -> StableHashEncoding -> Word64
  commitValue commitment =
    stableHashDigestWord (valueSipKey commitment)

  emptyDigest :: commitment -> Digest128
  emptyDigest commitment =
    digestWithKeys
      (emptySipKey commitment)
      (stableHashEncodingWord8 0)

  leafDigest :: commitment -> Word64 -> Word64 -> Digest128
  leafDigest commitment path valueDigest =
    digestWithKeys
      (leafSipKey commitment)
      ( stableHashEncodingWord8 1
          <> stableHashEncodingWord64LE deltaHashEncodingVersion
          <> stableHashEncodingWord64LE path
          <> stableHashEncodingWord64LE valueDigest
      )

  branchDigest :: commitment -> Word64 -> Word64 -> Digest128 -> Digest128 -> Digest128
  branchDigest commitment prefix branchingBit leftDigest rightDigest =
    digestWithKeys
      (branchSipKey commitment)
      ( stableHashEncodingWord8 2
          <> stableHashEncodingWord64LE deltaHashEncodingVersion
          <> stableHashEncodingWord64LE prefix
          <> stableHashEncodingWord64LE branchingBit
          <> stableHashEncodingWord64LE (deltaHashDigestLane0 leftDigest)
          <> stableHashEncodingWord64LE (deltaHashDigestLane1 leftDigest)
          <> stableHashEncodingWord64LE (deltaHashDigestLane0 rightDigest)
          <> stableHashEncodingWord64LE (deltaHashDigestLane1 rightDigest)
      )

data SipHashNodeCommitment = SipHashNodeCommitment

sipHashNodeCommitment :: SipHashNodeCommitment
sipHashNodeCommitment = SipHashNodeCommitment

instance NodeCommitment SipHashNodeCommitment where
  pathSipKey _commitment =
    SipKey 0x6d6c2d64656c7461 0x706174682d763031

  valueSipKey _commitment =
    SipKey 0x6d6c2d64656c7461 0x76616c752d763031

  emptySipKey _commitment =
    DigestSipKeys
      (SipKey 0x6d6c2d64656c7461 0x656d70742d763031)
      (SipKey 0x6d6c2d64656c7461 0x656d70742d763032)

  leafSipKey _commitment =
    DigestSipKeys
      (SipKey 0x6d6c2d64656c7461 0x6c6561662d763031)
      (SipKey 0x6d6c2d64656c7461 0x6c6561662d763032)

  branchSipKey _commitment =
    DigestSipKeys
      (SipKey 0x6d6c2d64656c7461 0x6272616e2d763031)
      (SipKey 0x6d6c2d64656c7461 0x6272616e2d763032)

digestWithKeys :: DigestSipKeys -> StableHashEncoding -> Digest128
digestWithKeys (DigestSipKeys lane0Key lane1Key) =
  digest128Encoding lane0Key lane1Key

stableHashDigestWord :: SipKey -> StableHashEncoding -> Word64
stableHashDigestWord sipKey encoding =
  case stableHashEncodingDigest sipKey encoding of
    StableHashDigest digestWord -> digestWord
