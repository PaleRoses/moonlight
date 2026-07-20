{-# LANGUAGE BangPatterns #-}

module Moonlight.Delta.Patch.Internal.MerkleDeltaHash
  ( MerkleDeltaHash,
    buildMerkleDeltaHash,
    merkleDeltaHashState,
    merkleDeltaHashDigest,
    applyMerkleDeltaHash,
    deltaHashFlatMaximumSize,
  )
where

import Data.Bits
  ( Bits (complement, shiftL, xor, (.&.), (.|.)),
    FiniteBits (countLeadingZeros, finiteBitSize),
  )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Moonlight.Core
  ( SipKey (..),
    StableHashDigest (..),
    StableHashEncoding,
    defaultStableHashKey,
  )
import Moonlight.Delta.Patch.Internal.Apply qualified as Patch
import Moonlight.Delta.Patch.Internal.Construction qualified as Patch
import Moonlight.Delta.Patch.Internal.IncrementalDigest
  ( DeltaHashApplyError (..),
    DeltaHashBuildError (..),
    DeltaHashDigest,
    IncrementalDigest (..),
    digest128EncodingChunks,
  )
import Moonlight.Delta.Patch.Internal.NodeCommitment qualified as Node
import Moonlight.Delta.Patch.Internal.Types (Patch)
import Prelude

data MerkleDeltaHash key value = MerkleDeltaHash
  { merkleDeltaHashKeyEncoding :: !(key -> StableHashEncoding),
    merkleDeltaHashValueEncoding :: !(value -> StableHashEncoding),
    merkleDeltaHashAuthoritativeState :: !(Map key value),
    merkleDeltaHashDerivedView :: !(MerkleDeltaHashView key)
  }

data MerkleDeltaHashView key
  = FlatStableHash !DeltaHashDigest
  | IncrementalMerkleHash !(MerkleTrie key)

data MerkleTrie key
  = MerkleEmpty
  | MerkleLeaf {-# UNPACK #-} !Word64 !key !DeltaHashDigest
  | MerkleBranch
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      !(MerkleTrie key)
      !(MerkleTrie key)
      !DeltaHashDigest

buildMerkleDeltaHash ::
  Eq key =>
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  Map key value ->
  Either (DeltaHashBuildError key) (MerkleDeltaHash key value)
buildMerkleDeltaHash encodeKey encodeValue authoritativeState =
  fmap
    (MerkleDeltaHash encodeKey encodeValue authoritativeState)
    (buildMerkleDeltaHashView encodeKey encodeValue authoritativeState)

merkleDeltaHashState :: MerkleDeltaHash key value -> Map key value
merkleDeltaHashState =
  merkleDeltaHashAuthoritativeState

merkleDeltaHashDigest :: MerkleDeltaHash key value -> DeltaHashDigest
merkleDeltaHashDigest =
  merkleDeltaHashViewDigest . merkleDeltaHashDerivedView

applyMerkleDeltaHash ::
  (Ord key, Eq value) =>
  Patch key value ->
  MerkleDeltaHash key value ->
  Either (DeltaHashApplyError key value) (MerkleDeltaHash key value)
applyMerkleDeltaHash patchValue currentMerkleDeltaHash = do
  updatedState <-
    either
      (Left . DeltaHashPatchRejected)
      Right
      (Patch.apply patchValue (merkleDeltaHashAuthoritativeState currentMerkleDeltaHash))
  updatedView <-
    either
      (Left . DeltaHashUpdateRejected)
      Right
      ( updateMerkleDeltaHashView
          (merkleDeltaHashKeyEncoding currentMerkleDeltaHash)
          (merkleDeltaHashValueEncoding currentMerkleDeltaHash)
          (merkleDeltaHashAuthoritativeState currentMerkleDeltaHash)
          patchValue
          updatedState
          (merkleDeltaHashDerivedView currentMerkleDeltaHash)
      )
  pure
    MerkleDeltaHash
      { merkleDeltaHashKeyEncoding = merkleDeltaHashKeyEncoding currentMerkleDeltaHash,
        merkleDeltaHashValueEncoding = merkleDeltaHashValueEncoding currentMerkleDeltaHash,
        merkleDeltaHashAuthoritativeState = updatedState,
        merkleDeltaHashDerivedView = updatedView
      }

buildMerkleDeltaHashView ::
  Eq key =>
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  Map key value ->
  Either (DeltaHashBuildError key) (MerkleDeltaHashView key)
buildMerkleDeltaHashView encodeKey encodeValue authoritativeState
  | usesFlatStableHash authoritativeState =
      Right (FlatStableHash (flatStateDigest encodeKey encodeValue authoritativeState))
  | otherwise =
      fmap IncrementalMerkleHash (buildMerkleTrie encodeKey encodeValue authoritativeState)

updateMerkleDeltaHashView ::
  Ord key =>
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  Map key value ->
  Patch key value ->
  Map key value ->
  MerkleDeltaHashView key ->
  Either (DeltaHashBuildError key) (MerkleDeltaHashView key)
updateMerkleDeltaHashView encodeKey encodeValue authoritativeState patchValue updatedState currentView
  | usesFlatStableHash updatedState =
      Right (FlatStableHash (flatStateDigest encodeKey encodeValue updatedState))
  | otherwise =
      case currentView of
        FlatStableHash _digest ->
          fmap IncrementalMerkleHash (buildMerkleTrie encodeKey encodeValue updatedState)
        IncrementalMerkleHash trie ->
          fmap IncrementalMerkleHash (applyPatchToTrie encodeKey encodeValue authoritativeState patchValue trie)

merkleDeltaHashViewDigest :: MerkleDeltaHashView key -> DeltaHashDigest
merkleDeltaHashViewDigest view =
  case view of
    FlatStableHash digestValue -> digestValue
    IncrementalMerkleHash trie -> merkleTrieDigest trie

usesFlatStableHash :: Map key value -> Bool
usesFlatStableHash authoritativeState =
  Map.size authoritativeState <= deltaHashFlatMaximumSize

flatStateDigest ::
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  Map key value ->
  DeltaHashDigest
flatStateDigest encodeKey encodeValue authoritativeState =
  digest128EncodingChunks
    defaultStableHashKey
    flatSipKeyLane1
    (either encodeKey encodeValue)
    (flatStateEncodings authoritativeState)

flatStateEncodings :: Map key value -> [Either key value]
flatStateEncodings =
  Map.foldrWithKey (\key value encodings -> Left key : Right value : encodings) []

buildMerkleTrie ::
  Eq key =>
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  Map key value ->
  Either (DeltaHashBuildError key) (MerkleTrie key)
buildMerkleTrie encodeKey encodeValue =
  Map.foldlWithKey' insertEntry (Right MerkleEmpty)
  where
    insertEntry accumulated key value =
      accumulated >>= insertEncodedValue encodeKey encodeValue key value

applyPatchToTrie ::
  Ord key =>
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  Map key value ->
  Patch key value ->
  MerkleTrie key ->
  Either (DeltaHashBuildError key) (MerkleTrie key)
applyPatchToTrie encodeKey encodeValue authoritativeState patchValue initialTrie =
  Patch.foldWithKey'
    const
    (\accumulated key after -> accumulated >>= insertEncodedValue encodeKey encodeValue key after)
    (\accumulated key _before -> fmap (deleteAuthoritativeKey encodeKey authoritativeState key) accumulated)
    ( \accumulated key _before after ->
        accumulated
          >>= replaceAuthoritativeKey encodeKey encodeValue authoritativeState key after
    )
    (Right initialTrie)
    patchValue

insertEncodedValue ::
  Eq key =>
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  key ->
  value ->
  MerkleTrie key ->
  Either (DeltaHashBuildError key) (MerkleTrie key)
insertEncodedValue encodeKey encodeValue key value =
  insertMerkleLeaf
    (stableHashPathWord (encodeKey key))
    key
    (stableHashValueWord (encodeValue value))

deleteEncodedKey ::
  Eq key =>
  (key -> StableHashEncoding) ->
  key ->
  MerkleTrie key ->
  MerkleTrie key
deleteEncodedKey encodeKey key =
  deleteMerkleLeaf (stableHashPathWord (encodeKey key)) key

deleteAuthoritativeKey ::
  Ord key =>
  (key -> StableHashEncoding) ->
  Map key value ->
  key ->
  MerkleTrie key ->
  MerkleTrie key
deleteAuthoritativeKey encodeKey authoritativeState patchKey trie =
  case Map.lookupGE patchKey authoritativeState of
    Just (storedKey, _value)
      | compare storedKey patchKey == EQ -> deleteEncodedKey encodeKey storedKey trie
    _ -> trie

replaceAuthoritativeKey ::
  Ord key =>
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  Map key value ->
  key ->
  value ->
  MerkleTrie key ->
  Either (DeltaHashBuildError key) (MerkleTrie key)
replaceAuthoritativeKey encodeKey encodeValue authoritativeState patchKey after trie =
  case Map.lookupGE patchKey authoritativeState of
    Just (storedKey, _value)
      | compare storedKey patchKey == EQ ->
          let !storedPath = stableHashPathWord (encodeKey storedKey)
              !patchPath = stableHashPathWord (encodeKey patchKey)
              !withoutStoredRepresentative =
                if storedPath == patchPath
                  then trie
                  else deleteMerkleLeaf storedPath storedKey trie
           in insertMerkleLeaf patchPath patchKey (stableHashValueWord (encodeValue after)) withoutStoredRepresentative
    _ -> insertEncodedValue encodeKey encodeValue patchKey after trie

insertMerkleLeaf ::
  Eq key =>
  Word64 ->
  key ->
  Word64 ->
  MerkleTrie key ->
  Either (DeltaHashBuildError key) (MerkleTrie key)
insertMerkleLeaf path key valueDigest trie =
  case trie of
    MerkleEmpty ->
      Right (merkleLeaf path key valueDigest)
    MerkleLeaf existingPath existingKey _existingDigest
      | path == existingPath ->
          if key == existingKey
            then Right (merkleLeaf path key valueDigest)
            else
              Left
                DeltaHashKeyCollision
                  { deltaHashCollisionDigest = StableHashDigest path,
                    deltaHashExistingKey = existingKey,
                    deltaHashIncomingKey = key
                  }
      | otherwise ->
          Right (linkMerkleTries path (merkleLeaf path key valueDigest) existingPath trie)
    branch@(MerkleBranch prefix branchingBit left right _branchDigest)
      | not (matchesPrefix path prefix branchingBit) ->
          Right (linkMerkleTries path (merkleLeaf path key valueDigest) prefix branch)
      | zeroAtBit path branchingBit ->
          fmap
            (\updatedLeft -> merkleBranch prefix branchingBit updatedLeft right)
            (insertMerkleLeaf path key valueDigest left)
      | otherwise ->
          fmap
            (\updatedRight -> merkleBranch prefix branchingBit left updatedRight)
            (insertMerkleLeaf path key valueDigest right)

deleteMerkleLeaf ::
  Eq key =>
  Word64 ->
  key ->
  MerkleTrie key ->
  MerkleTrie key
deleteMerkleLeaf path key trie =
  case trie of
    MerkleEmpty -> MerkleEmpty
    MerkleLeaf existingPath existingKey _digest ->
      if path == existingPath && key == existingKey then MerkleEmpty else trie
    MerkleBranch prefix branchingBit left right _branchDigest
      | not (matchesPrefix path prefix branchingBit) -> trie
      | zeroAtBit path branchingBit ->
          compactMerkleBranch prefix branchingBit (deleteMerkleLeaf path key left) right
      | otherwise ->
          compactMerkleBranch prefix branchingBit left (deleteMerkleLeaf path key right)

linkMerkleTries ::
  Word64 ->
  MerkleTrie key ->
  Word64 ->
  MerkleTrie key ->
  MerkleTrie key
linkMerkleTries leftPath leftTrie rightPath rightTrie =
  let !branchingBit = highestDifferingBit leftPath rightPath
      !prefix = maskPrefix leftPath branchingBit
   in if zeroAtBit leftPath branchingBit
        then merkleBranch prefix branchingBit leftTrie rightTrie
        else merkleBranch prefix branchingBit rightTrie leftTrie

compactMerkleBranch ::
  Word64 ->
  Word64 ->
  MerkleTrie key ->
  MerkleTrie key ->
  MerkleTrie key
compactMerkleBranch prefix branchingBit left right =
  case (left, right) of
    (MerkleEmpty, remaining) -> remaining
    (remaining, MerkleEmpty) -> remaining
    _ -> merkleBranch prefix branchingBit left right

merkleLeaf :: Word64 -> key -> Word64 -> MerkleTrie key
merkleLeaf path key valueDigest =
  MerkleLeaf path key (Node.leafDigest Node.sipHashNodeCommitment path valueDigest)

merkleBranch ::
  Word64 ->
  Word64 ->
  MerkleTrie key ->
  MerkleTrie key ->
  MerkleTrie key
merkleBranch prefix branchingBit left right =
  MerkleBranch
    prefix
    branchingBit
    left
    right
    (Node.branchDigest Node.sipHashNodeCommitment prefix branchingBit (merkleTrieDigest left) (merkleTrieDigest right))

merkleTrieDigest :: MerkleTrie key -> DeltaHashDigest
merkleTrieDigest trie =
  case trie of
    MerkleEmpty -> Node.emptyDigest Node.sipHashNodeCommitment
    MerkleLeaf _path _key digestValue -> digestValue
    MerkleBranch _prefix _branchingBit _left _right digestValue -> digestValue

stableHashPathWord :: StableHashEncoding -> Word64
stableHashPathWord =
  Node.localizePath Node.sipHashNodeCommitment

stableHashValueWord :: StableHashEncoding -> Word64
stableHashValueWord =
  Node.commitValue Node.sipHashNodeCommitment

deltaHashFlatMaximumSize :: Int
deltaHashFlatMaximumSize =
  256

flatSipKeyLane1 :: SipKey
flatSipKeyLane1 =
  SipKey 0x6d6f6f6e6c696768 0x742d636f72652d32

matchesPrefix :: Word64 -> Word64 -> Word64 -> Bool
matchesPrefix path prefix branchingBit =
  maskPrefix path branchingBit == prefix

maskPrefix :: Word64 -> Word64 -> Word64
maskPrefix path branchingBit =
  path .&. complement (branchingBit .|. (branchingBit - 1))

zeroAtBit :: Word64 -> Word64 -> Bool
zeroAtBit path branchingBit =
  path .&. branchingBit == 0

highestDifferingBit :: Word64 -> Word64 -> Word64
highestDifferingBit leftPath rightPath =
  let !differentBits = leftPath `xor` rightPath
      !highestIndex = finiteBitSize differentBits - countLeadingZeros differentBits - 1
   in 1 `shiftL` highestIndex

emptyMerkleDeltaHash ::
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  MerkleDeltaHash key value
emptyMerkleDeltaHash encodeKey encodeValue =
  MerkleDeltaHash encodeKey encodeValue Map.empty (FlatStableHash (flatStateDigest encodeKey encodeValue Map.empty))

instance IncrementalDigest MerkleDeltaHash where
  type IncrementalDigestError MerkleDeltaHash key value = DeltaHashApplyError key value
  empty = emptyMerkleDeltaHash
  applyPatch = applyMerkleDeltaHash
  digest = merkleDeltaHashDigest
