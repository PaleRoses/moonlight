{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Model.Schema.Boundary
  ( BoundaryShape (..),
    Boundary,
    BoundaryError (..),
    BoundaryDigestEncoder (..),

    boundaryShape,
    boundaryKeys,
    boundaryDigest,
    boundaryCardinality,

    mkBoundary,
    mkCheckedBoundaryWith,
    validateBoundaryWith,
    validateBoundaryShapeWith,
    validateSchemaUnique,

    normalizeBoundaryShape,
    boundaryShapeKeySet,
    boundaryOverlap,
    boundaryCoherence,
    boundarySubsumes,
    restrictBoundaryToOverlap,

    boundaryShapeDigestWith,
    boundaryShapeWordsWith,

    RuntimeBoundary,
    RuntimeBoundaryError,
    runtimeBoundaryDigest,
    emptyRuntimeBoundary,
    mkRuntimeBoundary,
    mkRuntimeBoundaryFromShape,
    validateRuntimeBoundary,
    runtimeBoundarySensitiveSlots,
    runtimeBoundarySlotKeys,
    runtimeBoundaryKeys,
  )
where

import Data.Foldable
  ( traverse_,
  )
import Control.Monad
  ( unless,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
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
import Data.Maybe
  ( mapMaybe,
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Word
  ( Word64,
  )
import Moonlight.Core
  ( BoundaryOps (..),
  )
import Moonlight.Core
  ( SlotId,
    mkSlotId,
    slotIdKey,
  )
import Moonlight.Flow.Internal.Digest
  ( wordOfInt,
  )
import Moonlight.Flow.Model.Schema
  ( SchemaError,
    mkSchema,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
    stableDigest128,
  )
import Moonlight.Flow.Model.Schema.Digest.Words
  ( digestListWords,
    digestMapSetWords,
    digestSetWords,
  )

type BoundaryShape :: Type -> Type -> Type
data BoundaryShape slot key = BoundaryShape
  { bsSchema :: ![slot],
    bsSensitive :: !(Set slot),
    bsSlotKeys :: !(Map slot (Set key))
  }
  deriving stock (Eq, Ord, Show, Read)

type Boundary :: Type -> Type -> Type
data Boundary slot key = Boundary
  { bShape :: !(BoundaryShape slot key),
    bKeys :: !(Set key),
    bDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

type BoundaryError :: Type -> Type -> Type
data BoundaryError slot key
  = BoundarySchemaError !(SchemaError slot)
  | BoundarySensitiveSlotOutOfSchema !slot
  | BoundarySlotKeyOutOfSchema !slot
  | BoundaryInvalidKey !slot !key
  | BoundaryKeySetMismatch !(Set key) !(Set key)
  | BoundaryDigestMismatch !StableDigest128 !StableDigest128
  | BoundaryCardinalityMismatch !Int !Int
  deriving stock (Eq, Ord, Show, Read)

type BoundaryDigestEncoder :: Type -> Type -> Type
data BoundaryDigestEncoder slot key = BoundaryDigestEncoder
  { bdeSalt :: {-# UNPACK #-} !Word64,
    bdeShapeTag :: {-# UNPACK #-} !Word64,
    bdeListTag :: {-# UNPACK #-} !Word64,
    bdeSetTag :: {-# UNPACK #-} !Word64,
    bdeMapSetTag :: {-# UNPACK #-} !Word64,
    bdeSlotWords :: slot -> [Word64],
    bdeKeyWords :: key -> [Word64]
  }

boundaryShape :: Boundary slot key -> BoundaryShape slot key
boundaryShape =
  bShape
{-# INLINE boundaryShape #-}

boundaryKeys :: Boundary slot key -> Set key
boundaryKeys =
  bKeys
{-# INLINE boundaryKeys #-}

boundaryDigest :: Boundary slot key -> StableDigest128
boundaryDigest =
  bDigest
{-# INLINE boundaryDigest #-}

boundaryCardinality :: Boundary slot key -> Int
boundaryCardinality =
  Set.size . bKeys
{-# INLINE boundaryCardinality #-}

mkBoundary ::
  Ord key =>
  (BoundaryShape slot key -> StableDigest128) ->
  BoundaryShape slot key ->
  Boundary slot key
mkBoundary digestOf shape0 =
  let !shape =
        normalizeBoundaryShape shape0
      !keys =
        boundaryShapeKeySet shape
      !digest =
        digestOf shape
   in Boundary
        { bShape = shape,
          bKeys = keys,
          bDigest = digest
        }
{-# INLINE mkBoundary #-}

mkCheckedBoundaryWith ::
  (Ord slot, Ord key) =>
  (slot -> key -> Bool) ->
  (BoundaryShape slot key -> StableDigest128) ->
  BoundaryShape slot key ->
  Either (BoundaryError slot key) (Boundary slot key)
mkCheckedBoundaryWith validKey digestOf shape0 = do
  let !shape =
        normalizeBoundaryShape shape0
  validateBoundaryShapeWith validKey shape
  pure (mkBoundary digestOf shape)
{-# INLINE mkCheckedBoundaryWith #-}

validateBoundaryWith ::
  (Ord slot, Ord key) =>
  (slot -> key -> Bool) ->
  (BoundaryShape slot key -> StableDigest128) ->
  Boundary slot key ->
  Either (BoundaryError slot key) ()
validateBoundaryWith validKey digestOf boundary = do
  canonical <-
    mkCheckedBoundaryWith validKey digestOf (bShape boundary)
  unless (bKeys boundary == bKeys canonical) $
    Left (BoundaryKeySetMismatch (bKeys canonical) (bKeys boundary))
  unless (bDigest boundary == bDigest canonical) $
    Left (BoundaryDigestMismatch (bDigest canonical) (bDigest boundary))
  unless (boundaryCardinality boundary == boundaryCardinality canonical) $
    Left (BoundaryCardinalityMismatch (boundaryCardinality canonical) (boundaryCardinality boundary))
{-# INLINE validateBoundaryWith #-}

validateBoundaryShapeWith ::
  Ord slot =>
  (slot -> key -> Bool) ->
  BoundaryShape slot key ->
  Either (BoundaryError slot key) ()
validateBoundaryShapeWith validKey shape = do
  validateSchemaUnique (bsSchema shape)
  let schemaSlots =
        Set.fromList (bsSchema shape)
  validateSensitiveSlots schemaSlots (bsSensitive shape)
  validateSlotKeys validKey schemaSlots (bsSlotKeys shape)
{-# INLINE validateBoundaryShapeWith #-}

validateSchemaUnique ::
  Ord slot =>
  [slot] ->
  Either (BoundaryError slot key) ()
validateSchemaUnique =
  firstSchemaError . mkSchema
  where
    firstSchemaError :: Either (SchemaError slot) value -> Either (BoundaryError slot key) ()
    firstSchemaError =
      either (Left . BoundarySchemaError) (const (Right ()))
{-# INLINE validateSchemaUnique #-}

validateSensitiveSlots ::
  Ord slot =>
  Set slot ->
  Set slot ->
  Either (BoundaryError slot key) ()
validateSensitiveSlots schemaSlots =
  traverse_
    ( \slot ->
        unless (Set.member slot schemaSlots) $
          Left (BoundarySensitiveSlotOutOfSchema slot)
    )
    . Set.toAscList
{-# INLINE validateSensitiveSlots #-}

validateSlotKeys ::
  Ord slot =>
  (slot -> key -> Bool) ->
  Set slot ->
  Map slot (Set key) ->
  Either (BoundaryError slot key) ()
validateSlotKeys validKey schemaSlots =
  traverse_
    ( \(slot, keys) -> do
        unless (Set.member slot schemaSlots) $
          Left (BoundarySlotKeyOutOfSchema slot)
        traverse_
          ( \key ->
              unless (validKey slot key) $
                Left (BoundaryInvalidKey slot key)
          )
          (Set.toAscList keys)
    )
    . Map.toAscList
{-# INLINE validateSlotKeys #-}

normalizeBoundaryShape ::
  BoundaryShape slot key ->
  BoundaryShape slot key
normalizeBoundaryShape shape =
  shape
    { bsSlotKeys =
        Map.filter
          (not . Set.null)
          (bsSlotKeys shape)
    }
{-# INLINE normalizeBoundaryShape #-}

boundaryShapeKeySet ::
  Ord key =>
  BoundaryShape slot key ->
  Set key
boundaryShapeKeySet =
  Set.unions
    . Map.elems
    . bsSlotKeys
    . normalizeBoundaryShape
{-# INLINE boundaryShapeKeySet #-}

boundaryOverlap ::
  (Ord slot, Ord key) =>
  Boundary slot key ->
  Boundary slot key ->
  BoundaryShape slot key
boundaryOverlap leftBoundary rightBoundary =
  BoundaryShape
    { bsSchema = commonSchema,
      bsSensitive = commonSensitive,
      bsSlotKeys = commonSlotKeys
    }
  where
    leftShape =
      bShape leftBoundary

    rightShape =
      bShape rightBoundary

    commonSchemaSet =
      Set.intersection
        (Set.fromList (bsSchema leftShape))
        (Set.fromList (bsSchema rightShape))

    commonSchema =
      filter
        (`Set.member` commonSchemaSet)
        (bsSchema leftShape)

    commonSensitive =
      Set.intersection
        commonSchemaSet
        (Set.intersection (bsSensitive leftShape) (bsSensitive rightShape))

    commonSlotKeys =
      Map.restrictKeys
        ( Map.filter (not . Set.null) $
            Map.intersectionWith
              Set.intersection
              (bsSlotKeys leftShape)
              (bsSlotKeys rightShape)
        )
        commonSchemaSet
{-# INLINE boundaryOverlap #-}

boundaryCoherence ::
  (Ord slot, Ord key) =>
  (BoundaryShape slot key -> StableDigest128) ->
  Boundary slot key ->
  Boundary slot key ->
  Either (Boundary slot key) (Boundary slot key)
boundaryCoherence digestOf leftBoundary rightBoundary =
  let conflicts =
        sensitiveKeyConflicts leftBoundary rightBoundary
      overlapShape =
        boundaryOverlap leftBoundary rightBoundary
   in if Map.null conflicts
        then Right (mkBoundary digestOf overlapShape)
        else Left (boundaryFromConflictKeys digestOf leftBoundary conflicts)
{-# INLINE boundaryCoherence #-}

restrictBoundaryToOverlap ::
  (Ord slot, Ord key) =>
  (BoundaryShape slot key -> StableDigest128) ->
  BoundaryShape slot key ->
  Boundary slot key ->
  Boundary slot key
restrictBoundaryToOverlap digestOf overlapShape boundary =
  mkBoundary
    digestOf
    BoundaryShape
      { bsSchema = bsSchema overlapShape,
        bsSensitive = bsSensitive overlapShape,
        bsSlotKeys =
          Map.restrictKeys
            (bsSlotKeys (bShape boundary))
            (Set.fromList (bsSchema overlapShape))
      }
{-# INLINE restrictBoundaryToOverlap #-}

boundarySubsumes ::
  (Ord slot, Ord key) =>
  Boundary slot key ->
  Boundary slot key ->
  Bool
boundarySubsumes leftBoundary rightBoundary =
  Set.isSubsetOf
    (Set.fromList (bsSchema (bShape rightBoundary)))
    (Set.fromList (bsSchema (bShape leftBoundary)))
    && Set.isSubsetOf
      (bsSensitive (bShape rightBoundary))
      (bsSensitive (bShape leftBoundary))
    && slotKeysSubsetOf
      (bsSlotKeys (bShape rightBoundary))
      (bsSlotKeys (bShape leftBoundary))
{-# INLINE boundarySubsumes #-}

sensitiveKeyConflicts ::
  (Ord slot, Ord key) =>
  Boundary slot key ->
  Boundary slot key ->
  Map slot (Set key)
sensitiveKeyConflicts leftBoundary rightBoundary =
  Map.fromList
    [ (slot, Set.union leftKeys rightKeys)
    | slot <- Set.toAscList sharedSensitiveSlots,
      Just leftKeys <- [Map.lookup slot (bsSlotKeys leftShape)],
      Just rightKeys <- [Map.lookup slot (bsSlotKeys rightShape)],
      Set.null (Set.intersection leftKeys rightKeys)
    ]
  where
    leftShape =
      bShape leftBoundary

    rightShape =
      bShape rightBoundary

    sharedSensitiveSlots =
      Set.intersection
        (Set.intersection (bsSensitive leftShape) (bsSensitive rightShape))
        (Set.intersection (Set.fromList (bsSchema leftShape)) (Set.fromList (bsSchema rightShape)))
{-# INLINE sensitiveKeyConflicts #-}

boundaryFromConflictKeys ::
  (Ord slot, Ord key) =>
  (BoundaryShape slot key -> StableDigest128) ->
  Boundary slot key ->
  Map slot (Set key) ->
  Boundary slot key
boundaryFromConflictKeys digestOf boundary conflicts =
  let conflictSchema =
        filter (`Map.member` conflicts) (bsSchema (bShape boundary))
      conflictSlots =
        Set.fromList conflictSchema
   in mkBoundary
        digestOf
        BoundaryShape
          { bsSchema = conflictSchema,
            bsSensitive = conflictSlots,
            bsSlotKeys = Map.restrictKeys conflicts conflictSlots
          }
{-# INLINE boundaryFromConflictKeys #-}

slotKeysSubsetOf ::
  (Ord slot, Ord key) =>
  Map slot (Set key) ->
  Map slot (Set key) ->
  Bool
slotKeysSubsetOf smaller larger =
  all
    ( \(slot, smallerKeys) ->
        Set.isSubsetOf
          smallerKeys
          (Map.findWithDefault Set.empty slot larger)
    )
    (Map.toAscList smaller)
{-# INLINE slotKeysSubsetOf #-}

boundaryShapeDigestWith ::
  BoundaryDigestEncoder slot key ->
  BoundaryShape slot key ->
  StableDigest128
boundaryShapeDigestWith encoder shape =
  stableDigest128
    (bdeSalt encoder : boundaryShapeWordsWith encoder shape)
{-# INLINE boundaryShapeDigestWith #-}

boundaryShapeWordsWith ::
  BoundaryDigestEncoder slot key ->
  BoundaryShape slot key ->
  [Word64]
boundaryShapeWordsWith encoder shape =
  [bdeShapeTag encoder]
    <> digestListWords (bdeListTag encoder) (bdeSlotWords encoder) (bsSchema shape)
    <> digestSetWords (bdeSetTag encoder) (bdeSlotWords encoder) (bsSensitive shape)
    <> digestMapSetWords
      (bdeMapSetTag encoder)
      (bdeSetTag encoder)
      (bdeSlotWords encoder)
      (bdeKeyWords encoder)
      (bsSlotKeys shape)
{-# INLINE boundaryShapeWordsWith #-}

type RuntimeBoundary =
  Boundary SlotId Int

type RuntimeBoundaryError =
  BoundaryError SlotId Int

runtimeBoundaryDigest :: BoundaryShape SlotId Int -> StableDigest128
runtimeBoundaryDigest =
  boundaryShapeDigestWith
    BoundaryDigestEncoder
      { bdeSalt = 0x626f756e64617279,
        bdeShapeTag = 0x6f756e5368617065,
        bdeListTag = 0x02,
        bdeSetTag = 0x03,
        bdeMapSetTag = 0x04,
        bdeSlotWords = \slot -> [0x01, wordOfInt (slotIdKey slot)],
        bdeKeyWords = \key -> [wordOfInt key]
      }
{-# INLINE runtimeBoundaryDigest #-}

emptyRuntimeBoundary :: RuntimeBoundary
emptyRuntimeBoundary =
  mkBoundary runtimeBoundaryDigest (BoundaryShape [] Set.empty Map.empty)
{-# INLINE emptyRuntimeBoundary #-}

mkRuntimeBoundary ::
  [SlotId] ->
  IntSet ->
  IntMap IntSet ->
  Either RuntimeBoundaryError RuntimeBoundary
mkRuntimeBoundary schema sensitiveSlotKeys rawSlotKeys = do
  validateRuntimeRaw schema sensitiveSlotKeys slotKeys
  mkCheckedBoundaryWith
    (\_slot key -> key >= 0)
    runtimeBoundaryDigest
    (runtimeBoundaryShapeFromRaw schema sensitiveSlotKeys slotKeys)
  where
    slotKeys =
      IntMap.filter (not . IntSet.null) rawSlotKeys
{-# INLINE mkRuntimeBoundary #-}

mkRuntimeBoundaryFromShape ::
  BoundaryShape SlotId Int ->
  Either RuntimeBoundaryError RuntimeBoundary
mkRuntimeBoundaryFromShape =
  mkCheckedBoundaryWith
    (\_slot key -> key >= 0)
    runtimeBoundaryDigest
{-# INLINE mkRuntimeBoundaryFromShape #-}

validateRuntimeBoundary ::
  RuntimeBoundary ->
  Either RuntimeBoundaryError ()
validateRuntimeBoundary =
  validateBoundaryWith
    (\_slot key -> key >= 0)
    runtimeBoundaryDigest
{-# INLINE validateRuntimeBoundary #-}

validateRuntimeRaw ::
  [SlotId] ->
  IntSet ->
  IntMap IntSet ->
  Either RuntimeBoundaryError ()
validateRuntimeRaw schema sensitiveSlotKeys slotKeys = do
  validateSchemaUnique schema
  traverse_
    ( \slotKey ->
        unless (IntSet.member slotKey schemaKeys) $
          Left (BoundarySensitiveSlotOutOfSchema (mkSlotId slotKey))
    )
    (IntSet.toAscList sensitiveSlotKeys)
  traverse_
    validateOneSlotKeys
    (IntMap.toAscList slotKeys)
  where
    schemaKeys =
      schemaSlotKeySet schema

    validateOneSlotKeys (slotKey, keys) = do
      unless (IntSet.member slotKey schemaKeys) $
        Left (BoundarySlotKeyOutOfSchema (mkSlotId slotKey))
      traverse_
        ( \key ->
            unless (key >= 0) $
              Left (BoundaryInvalidKey (mkSlotId slotKey) key)
        )
        (IntSet.toAscList keys)
{-# INLINE validateRuntimeRaw #-}

runtimeBoundaryShapeFromRaw ::
  [SlotId] ->
  IntSet ->
  IntMap IntSet ->
  BoundaryShape SlotId Int
runtimeBoundaryShapeFromRaw schema sensitiveSlotKeys slotKeys =
  BoundaryShape
    { bsSchema = schema,
      bsSensitive =
        Set.fromList
          ( filter
              (\slot -> IntSet.member (slotIdKey slot) sensitiveSlotKeys)
              schema
          ),
      bsSlotKeys =
        Map.fromList
          ( mapMaybe
              ( \slot ->
                  fmap
                    (\keys -> (slot, Set.fromAscList (IntSet.toAscList keys)))
                    (IntMap.lookup (slotIdKey slot) slotKeys)
              )
              schema
          )
    }
{-# INLINE runtimeBoundaryShapeFromRaw #-}

runtimeBoundarySensitiveSlots :: RuntimeBoundary -> IntSet
runtimeBoundarySensitiveSlots =
  IntSet.fromList
    . fmap slotIdKey
    . Set.toAscList
    . bsSensitive
    . bShape
{-# INLINE runtimeBoundarySensitiveSlots #-}

runtimeBoundarySlotKeys :: RuntimeBoundary -> IntMap IntSet
runtimeBoundarySlotKeys =
  IntMap.fromList
    . fmap
      ( \(slot, keys) ->
          (slotIdKey slot, IntSet.fromAscList (Set.toAscList keys))
      )
    . Map.toAscList
    . bsSlotKeys
    . bShape
{-# INLINE runtimeBoundarySlotKeys #-}

runtimeBoundaryKeys :: RuntimeBoundary -> IntSet
runtimeBoundaryKeys =
  IntSet.fromAscList
    . Set.toAscList
    . bKeys
{-# INLINE runtimeBoundaryKeys #-}

schemaSlotKeySet :: [SlotId] -> IntSet
schemaSlotKeySet =
  IntSet.fromList . fmap slotIdKey
{-# INLINE schemaSlotKeySet #-}

instance BoundaryOps (Boundary SlotId Int) where
  type BoundaryOverlap (Boundary SlotId Int) = BoundaryShape SlotId Int

  overlapBetweenBoundary =
    boundaryOverlap

  restrictBoundaryRaw =
    restrictBoundaryToOverlap runtimeBoundaryDigest

  compatibleBoundaryRaw =
    boundaryCoherence runtimeBoundaryDigest

  subsumesBoundaryRaw =
    boundarySubsumes
