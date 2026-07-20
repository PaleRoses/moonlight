{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

-- | Role-tagged compact tuple keys over raw representative Ints; RepKey
-- non-negativity is enforced with typed 'RepKeyError' at dense-representation
-- boundaries, never at construction (negative keys remain lawful consumer
-- sentinel space), and 'coerceTupleKey' is the explicit role cast.
module Moonlight.Differential.Row.Tuple
  ( RepKey (..),
    RepKeyError (..),
    TupleRole (..),
    TupleKey,
    RowTupleKey,
    AssignmentTupleKey,
    SeparatorTupleKey,
    OutputTupleKey,
    mkRepKey,
    emptyTupleKey,
    tupleKeyFromInts,
    tupleKeyFromRepKeys,
    tupleKeyFromSlotEnv,
    tupleKeyWidth,
    tupleKeyIndexInt,
    tupleKeyIndex,
    tupleKeyToInts,
    tupleKeyToRepKeys,
    tupleKeyToWord64Vector,
    withTupleKeyWord64Slots,
    tupleKeyFoldlInts',
    tupleKeyFoldlRepKeys',
    tupleKeyFoldlSlotInts',
    tupleKeyFoldlSlotRepKeys',
    tupleKeyClassKeys,
    tupleKeyTouches,
    restrictTupleKey,
    coerceTupleKey,
    repKeyWord64,
  )
where

import Data.Coerce (coerce)
import Data.Foldable qualified as Foldable
import Data.Hashable (Hashable (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Constraint, Type)
import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.PrimArray qualified as PrimArray
import Data.Proxy (Proxy (..))
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import Moonlight.Core (DenseKey (..))
import Moonlight.Core
  ( SlotId,
    mkSlotId,
    slotIdKey,
  )

type RepKey :: Type
newtype RepKey = RepKey {unRepKey :: Int}
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (DenseKey, Hashable)

type RepKeyError :: Type
data RepKeyError
  = NegativeRepKey !Int
  deriving stock (Eq, Ord, Show, Read)

mkRepKey :: Int -> Either RepKeyError RepKey
mkRepKey rawKey
  | rawKey < 0 =
      Left (NegativeRepKey rawKey)
  | otherwise =
      Right (RepKey rawKey)
{-# INLINE mkRepKey #-}

type TupleRole :: Type
data TupleRole
  = RowTupleRole
  | AssignmentTupleRole
  | SeparatorTupleRole
  | OutputTupleRole

type TupleKey :: TupleRole -> Type
data TupleKey tupleRole
  = Key0
  | Key1 {-# UNPACK #-} !Int
  | Key2 {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | KeyN !(PrimArray Int)

type RowTupleKey = TupleKey 'RowTupleRole

type AssignmentTupleKey = TupleKey 'AssignmentTupleRole

type SeparatorTupleKey = TupleKey 'SeparatorTupleRole

type OutputTupleKey = TupleKey 'OutputTupleRole

type TupleRoleName :: TupleRole -> Constraint
class TupleRoleName tupleRole where
  tupleRoleName :: Proxy tupleRole -> String

instance TupleRoleName 'RowTupleRole where
  tupleRoleName _ =
    "RowTupleKey"
  {-# INLINE tupleRoleName #-}

instance TupleRoleName 'AssignmentTupleRole where
  tupleRoleName _ =
    "AssignmentTupleKey"
  {-# INLINE tupleRoleName #-}

instance TupleRoleName 'SeparatorTupleRole where
  tupleRoleName _ =
    "SeparatorTupleKey"
  {-# INLINE tupleRoleName #-}

instance TupleRoleName 'OutputTupleRole where
  tupleRoleName _ =
    "OutputTupleKey"
  {-# INLINE tupleRoleName #-}

instance Eq (TupleKey tupleRole) where
  left == right =
    compareTupleKey left right == EQ
  {-# INLINE (==) #-}

instance Ord (TupleKey tupleRole) where
  compare =
    compareTupleKey
  {-# INLINE compare #-}

instance TupleRoleName tupleRole => Show (TupleKey tupleRole) where
  showsPrec precedence key =
    showParen (precedence > 10) $
      showString (tupleRoleName (Proxy @tupleRole))
        . showString " "
        . shows (tupleKeyToInts key)

instance Hashable (TupleKey tupleRole) where
  hashWithSalt salt key =
    tupleKeyFoldlInts'
      hashWithSalt
      (hashWithSalt salt (tupleKeyWidth key))
      key
  {-# INLINE hashWithSalt #-}

emptyTupleKey :: TupleKey tupleRole
emptyTupleKey =
  Key0
{-# INLINE emptyTupleKey #-}

tupleKeyFromInts :: Foldable f => f Int -> TupleKey tupleRole
tupleKeyFromInts values =
  case Foldable.toList values of
    [] ->
      Key0
    [a] ->
      Key1 a
    [a, b] ->
      Key2 a b
    xs ->
      KeyN (PrimArray.primArrayFromList xs)
{-# INLINE tupleKeyFromInts #-}

tupleKeyFromRepKeys :: Foldable f => f RepKey -> TupleKey tupleRole
tupleKeyFromRepKeys =
  tupleKeyFromInts . fmap unRepKey . Foldable.toList
{-# INLINE tupleKeyFromRepKeys #-}

tupleKeyFromSlotEnv ::
  [SlotId] ->
  IntMap RepKey ->
  Maybe (TupleKey tupleRole)
tupleKeyFromSlotEnv schema env =
  tupleKeyFromRepKeys
    <$> traverse
      (\slot -> IntMap.lookup (slotIdKey slot) env)
      schema
{-# INLINE tupleKeyFromSlotEnv #-}

tupleKeyWidth :: TupleKey tupleRole -> Int
tupleKeyWidth = \case
  Key0 ->
    0
  Key1 _ ->
    1
  Key2 _ _ ->
    2
  KeyN values ->
    PrimArray.sizeofPrimArray values
{-# INLINE tupleKeyWidth #-}

tupleKeyIndexInt :: TupleKey tupleRole -> Int -> Maybe Int
tupleKeyIndexInt key ix
  | ix < 0 =
      Nothing
  | otherwise =
      case key of
        Key0 ->
          Nothing
        Key1 a ->
          if ix == 0 then Just a else Nothing
        Key2 a b ->
          case ix of
            0 ->
              Just a
            1 ->
              Just b
            _ ->
              Nothing
        KeyN values ->
          if ix < PrimArray.sizeofPrimArray values
            then Just (PrimArray.indexPrimArray values ix)
            else Nothing
{-# INLINE tupleKeyIndexInt #-}

tupleKeyIndex :: TupleKey tupleRole -> Int -> Maybe RepKey
tupleKeyIndex key ix =
  RepKey <$> tupleKeyIndexInt key ix
{-# INLINE tupleKeyIndex #-}

tupleKeyToInts :: TupleKey tupleRole -> [Int]
tupleKeyToInts = \case
  Key0 ->
    []
  Key1 a ->
    [a]
  Key2 a b ->
    [a, b]
  KeyN values ->
    PrimArray.primArrayToList values
{-# INLINE tupleKeyToInts #-}

tupleKeyToRepKeys :: TupleKey tupleRole -> [RepKey]
tupleKeyToRepKeys =
  fmap RepKey . tupleKeyToInts
{-# INLINE tupleKeyToRepKeys #-}

tupleKeyToWord64Vector :: TupleKey tupleRole -> Either RepKeyError (VU.Vector Word64)
tupleKeyToWord64Vector = \case
  Key0 ->
    Right VU.empty
  Key1 a ->
    VU.singleton <$> repKeyIntWord64 a
  Key2 a b ->
    VU.fromListN 2 <$> traverse repKeyIntWord64 [a, b]
  KeyN values ->
    let !count =
          PrimArray.sizeofPrimArray values
     in VU.fromListN count <$> traverse repKeyIntWord64 (PrimArray.primArrayToList values)
{-# INLINE tupleKeyToWord64Vector #-}

withTupleKeyWord64Slots ::
  TupleKey tupleRole ->
  (Int -> (Int -> Word64) -> result) ->
  Either RepKeyError result
withTupleKeyWord64Slots key consume = do
  validateTupleKeyRepKeys key
  pure (consume (tupleKeyWidth key) (tupleKeySlotWord64 key))
{-# INLINE withTupleKeyWord64Slots #-}

repKeyIntWord64 :: Int -> Either RepKeyError Word64
repKeyIntWord64 =
  repKeyWord64 . RepKey
{-# INLINE repKeyIntWord64 #-}

validateTupleKeyRepKeys :: TupleKey tupleRole -> Either RepKeyError ()
validateTupleKeyRepKeys = \case
  Key0 ->
    Right ()
  Key1 a ->
    () <$ repKeyIntWord64 a
  Key2 a b ->
    () <$ repKeyIntWord64 a <* repKeyIntWord64 b
  KeyN values ->
    let validateRawKey rawKey =
          () <$ repKeyIntWord64 rawKey
     in Foldable.traverse_ validateRawKey (PrimArray.primArrayToList values)
{-# INLINE validateTupleKeyRepKeys #-}

tupleKeySlotWord64 :: TupleKey tupleRole -> Int -> Word64
tupleKeySlotWord64 key ix =
  maybe 0 fromIntegral (tupleKeyIndexInt key ix)
{-# INLINE tupleKeySlotWord64 #-}

tupleKeyFoldlInts' ::
  (acc -> Int -> acc) ->
  acc ->
  TupleKey tupleRole ->
  acc
tupleKeyFoldlInts' step initial = \case
  Key0 ->
    initial
  Key1 a ->
    step initial a
  Key2 a b ->
    let !acc1 =
          step initial a
     in step acc1 b
  KeyN values ->
    let !count =
          PrimArray.sizeofPrimArray values

        go !ix !acc
          | ix == count =
              acc
          | otherwise =
              let !value =
                    PrimArray.indexPrimArray values ix
                  !acc' =
                    step acc value
               in go (ix + 1) acc'
     in go 0 initial
{-# INLINE tupleKeyFoldlInts' #-}

tupleKeyFoldlRepKeys' ::
  (acc -> RepKey -> acc) ->
  acc ->
  TupleKey tupleRole ->
  acc
tupleKeyFoldlRepKeys' step =
  tupleKeyFoldlInts'
    (\acc key -> step acc (RepKey key))
{-# INLINE tupleKeyFoldlRepKeys' #-}

tupleKeyFoldlSlotInts' ::
  (acc -> Int -> Int -> acc) ->
  acc ->
  [SlotId] ->
  TupleKey tupleRole ->
  Maybe acc
tupleKeyFoldlSlotInts' step initial slots key =
  go 0 initial slots
  where
    !width =
      tupleKeyWidth key

    go !ix !acc [] =
      if ix == width
        then Just acc
        else Nothing
    go !ix !acc (slot : rest) =
      case tupleKeyIndexInt key ix of
        Nothing ->
          Nothing
        Just value ->
          let !acc' =
                step acc (slotIdKey slot) value
           in go (ix + 1) acc' rest
{-# INLINE tupleKeyFoldlSlotInts' #-}

tupleKeyFoldlSlotRepKeys' ::
  (acc -> SlotId -> RepKey -> acc) ->
  acc ->
  [SlotId] ->
  TupleKey tupleRole ->
  Maybe acc
tupleKeyFoldlSlotRepKeys' step =
  tupleKeyFoldlSlotInts'
    ( \acc slotKey repKey ->
        step acc (mkSlotId slotKey) (RepKey repKey)
    )
{-# INLINE tupleKeyFoldlSlotRepKeys' #-}

tupleKeyClassKeys :: TupleKey tupleRole -> IntSet
tupleKeyClassKeys =
  tupleKeyFoldlInts'
    (\acc key -> IntSet.insert key acc)
    IntSet.empty
{-# INLINE tupleKeyClassKeys #-}

tupleKeyTouches :: IntSet -> TupleKey tupleRole -> Bool
tupleKeyTouches dirtyKeys =
  not
    . IntSet.null
    . IntSet.intersection dirtyKeys
    . tupleKeyClassKeys
{-# INLINE tupleKeyTouches #-}

restrictTupleKey ::
  DenseKey key =>
  IntMap key ->
  TupleKey tupleRole ->
  TupleKey tupleRole
restrictTupleKey targetClasses = \case
  Key0 ->
    Key0
  Key1 a ->
    Key1 (restrictValue a)
  Key2 a b ->
    Key2 (restrictValue a) (restrictValue b)
  KeyN values ->
    KeyN (PrimArray.mapPrimArray restrictValue values)
  where
    restrictValue !source =
      encodeDenseKey
        (IntMap.findWithDefault (decodeDenseKey source) source targetClasses)
{-# INLINE restrictTupleKey #-}

coerceTupleKey :: TupleKey source -> TupleKey target
coerceTupleKey =
  coerce
{-# INLINE coerceTupleKey #-}

repKeyWord64 :: RepKey -> Either RepKeyError Word64
repKeyWord64 (RepKey rawKey) =
  case mkRepKey rawKey of
    Left obstruction ->
      Left obstruction
    Right _keyValue ->
      Right (fromIntegral rawKey)
{-# INLINE repKeyWord64 #-}

compareTupleKey :: TupleKey leftRole -> TupleKey rightRole -> Ordering
compareTupleKey left right =
  let !leftWidth =
        tupleKeyWidth left
      !rightWidth =
        tupleKeyWidth right
      !shared =
        min leftWidth rightWidth

      go !ix
        | ix == shared =
            compare leftWidth rightWidth
        | otherwise =
            case (tupleKeyIndexInt left ix, tupleKeyIndexInt right ix) of
              (Just l, Just r) ->
                case compare l r of
                  EQ ->
                    go (ix + 1)
                  ordering ->
                    ordering
              _ ->
                compare leftWidth rightWidth
   in go 0
{-# INLINE compareTupleKey #-}
