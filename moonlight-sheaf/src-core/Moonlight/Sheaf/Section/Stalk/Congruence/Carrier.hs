module Moonlight.Sheaf.Section.Stalk.Congruence.Carrier
  ( CarrierId (..),
    CarrierKey (..),
    GlobalCarrier (..),
    CongruenceVisibleSide (..),
    CongruenceConstructionError (..),
    mkGlobalCarrier,
    mkGlobalCarrierFromIndexedValues,
    mkGlobalCarrierFromDomain,
    globalCarrierId,
    carrierDomain,
    carrierKeyOf,
    carrierValueAt,
    carrierIndexedValues,
    sameCarrier,
    visibleKeySet,
    congruenceEndomapError,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Moonlight.Core (dedupStableOn, duplicateValuesOn)
import Moonlight.Core (DenseKey (..))
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation

type CarrierId :: Type
newtype CarrierId = CarrierId
  { unCarrierId :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

type CarrierKey :: Type -> Type
newtype CarrierKey atom = CarrierKey
  { carrierKeyId :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

instance DenseKey (CarrierKey atom) where
  encodeDenseKey =
    carrierKeyId

  decodeDenseKey =
    CarrierKey
  {-# INLINE encodeDenseKey #-}
  {-# INLINE decodeDenseKey #-}

type GlobalCarrier :: Type -> Type -> Type
data GlobalCarrier rep atom = GlobalCarrier
  { gcId :: !CarrierId,
    gcValuesByKey :: !(IntMap atom),
    gcKeyByValue :: !(Map.Map atom rep)
  }
  deriving stock (Eq, Show)

type CongruenceVisibleSide :: Type
data CongruenceVisibleSide
  = CongruenceStalkVisible
  | CongruenceRestrictionSourceVisible
  | CongruenceRestrictionTargetVisible
  deriving stock (Eq, Ord, Show, Read)

type CongruenceConstructionError :: Type -> Type
data CongruenceConstructionError atom
  = CongruenceDuplicateCarrierKeys !(NonEmpty Int)
  | CongruenceDuplicateCarrierAtoms !(NonEmpty atom)
  | CongruenceCarrierNegativeKey !Int
  | CongruenceVisibleKeyOutsideCarrier !CongruenceVisibleSide !Int
  | CongruenceRelationFailure !EquivalenceRelationError
  | CongruenceRestrictionMapMissingCarrierKey !Int
  | CongruenceRestrictionMapKeyOutsideCarrier !Int
  | CongruenceRestrictionMapImageOutsideCarrier !Int !Int
  | CongruenceRestrictionImageOutsideTargetVisible !Int !Int
  deriving stock (Eq, Ord, Show)

mkGlobalCarrier ::
  Ord atom =>
  CarrierId ->
  [atom] ->
  Either (CongruenceConstructionError atom) (GlobalCarrier (CarrierKey atom) atom)
mkGlobalCarrier carrierId atomValues =
  mkGlobalCarrierFromIndexedValues
    carrierId
    (zip (fmap decodeDenseKey [0 :: Int ..]) atomValues)
{-# INLINEABLE mkGlobalCarrier #-}

mkGlobalCarrierFromIndexedValues ::
  (DenseKey rep, Ord atom) =>
  CarrierId ->
  [(rep, atom)] ->
  Either (CongruenceConstructionError atom) (GlobalCarrier rep atom)
mkGlobalCarrierFromIndexedValues carrierId indexedValues = do
  validateNonNegativeCarrierKeys encodedEntries
  rejectDuplicateCarrierKeys encodedEntries
  rejectDuplicateCarrierAtoms encodedEntries
  pure
    GlobalCarrier
      { gcId = carrierId,
        gcValuesByKey = IntMap.fromList encodedEntries,
        gcKeyByValue = Map.fromList [(atomValue, keyValue) | (keyValue, atomValue) <- indexedValues]
      }
  where
    encodedEntries =
      [(encodeDenseKey keyValue, atomValue) | (keyValue, atomValue) <- indexedValues]
{-# INLINEABLE mkGlobalCarrierFromIndexedValues #-}

mkGlobalCarrierFromDomain ::
  DenseKey rep =>
  CarrierId ->
  IntSet ->
  Either (CongruenceConstructionError Int) (GlobalCarrier rep Int)
mkGlobalCarrierFromDomain carrierId domainKeys =
  mkGlobalCarrierFromIndexedValues
    carrierId
    [(decodeDenseKey key, key) | key <- IntSet.toAscList domainKeys]
{-# INLINEABLE mkGlobalCarrierFromDomain #-}

globalCarrierId :: GlobalCarrier rep atom -> CarrierId
globalCarrierId =
  gcId
{-# INLINE globalCarrierId #-}

carrierDomain :: GlobalCarrier rep atom -> IntSet
carrierDomain =
  IntMap.keysSet . gcValuesByKey
{-# INLINE carrierDomain #-}

carrierKeyOf ::
  Ord atom =>
  atom ->
  GlobalCarrier rep atom ->
  Maybe rep
carrierKeyOf atomValue carrier =
  Map.lookup atomValue (gcKeyByValue carrier)
{-# INLINE carrierKeyOf #-}

carrierValueAt ::
  DenseKey rep =>
  rep ->
  GlobalCarrier rep atom ->
  Maybe atom
carrierValueAt key carrier =
  IntMap.lookup (encodeDenseKey key) (gcValuesByKey carrier)
{-# INLINE carrierValueAt #-}

carrierIndexedValues ::
  DenseKey rep =>
  GlobalCarrier rep atom ->
  [(rep, atom)]
carrierIndexedValues carrier =
  [(decodeDenseKey key, atomValue) | (key, atomValue) <- IntMap.toAscList (gcValuesByKey carrier)]
{-# INLINEABLE carrierIndexedValues #-}

sameCarrier ::
  Eq atom =>
  GlobalCarrier rep atom ->
  GlobalCarrier rep atom ->
  Bool
sameCarrier leftCarrier rightCarrier =
  globalCarrierId leftCarrier == globalCarrierId rightCarrier
    && gcValuesByKey leftCarrier == gcValuesByKey rightCarrier
{-# INLINEABLE sameCarrier #-}

visibleKeySet ::
  DenseKey rep =>
  GlobalCarrier rep atom ->
  CongruenceVisibleSide ->
  [rep] ->
  Either (CongruenceConstructionError atom) IntSet
visibleKeySet carrier side keys =
  let keySet =
        IntSet.fromList (fmap encodeDenseKey keys)
      outside =
        IntSet.difference keySet (carrierDomain carrier)
   in case IntSet.lookupMin outside of
        Nothing ->
          Right keySet
        Just badKey ->
          Left (CongruenceVisibleKeyOutsideCarrier side badKey)
{-# INLINEABLE visibleKeySet #-}

congruenceEndomapError ::
  EquivalenceRelationError ->
  CongruenceConstructionError atom
congruenceEndomapError errorValue =
  case errorValue of
    EquivalenceEndomapMissingDomainKey key ->
      CongruenceRestrictionMapMissingCarrierKey key
    EquivalenceEndomapMapKeyOutsideDomain key ->
      CongruenceRestrictionMapKeyOutsideCarrier key
    EquivalenceEndomapImageOutsideDomain sourceKey targetKey ->
      CongruenceRestrictionMapImageOutsideCarrier sourceKey targetKey
    otherError ->
      CongruenceRelationFailure otherError
{-# INLINE congruenceEndomapError #-}

validateNonNegativeCarrierKeys ::
  [(Int, atom)] ->
  Either (CongruenceConstructionError atom) ()
validateNonNegativeCarrierKeys encodedEntries =
  case IntSet.lookupMin (IntSet.fromList (fmap fst encodedEntries)) of
    Just key | key < 0 ->
      Left (CongruenceCarrierNegativeKey key)
    _ ->
      Right ()
{-# INLINE validateNonNegativeCarrierKeys #-}

rejectDuplicateCarrierKeys ::
  [(Int, atom)] ->
  Either (CongruenceConstructionError atom) ()
rejectDuplicateCarrierKeys encodedEntries =
  case NonEmpty.nonEmpty duplicateKeys of
    Nothing ->
      Right ()
    Just duplicateKeyValues ->
      Left (CongruenceDuplicateCarrierKeys duplicateKeyValues)
  where
    duplicateKeys =
      dedupStableOn id $
        fmap (fst . snd) (duplicateValuesOn fst encodedEntries)
{-# INLINEABLE rejectDuplicateCarrierKeys #-}

rejectDuplicateCarrierAtoms ::
  Ord atom =>
  [(Int, atom)] ->
  Either (CongruenceConstructionError atom) ()
rejectDuplicateCarrierAtoms encodedEntries =
  case NonEmpty.nonEmpty duplicateAtoms of
    Nothing ->
      Right ()
    Just duplicateAtomValues ->
      Left (CongruenceDuplicateCarrierAtoms duplicateAtomValues)
  where
    duplicateAtoms =
      dedupStableOn id $
        fmap (snd . snd) (duplicateValuesOn snd encodedEntries)
{-# INLINEABLE rejectDuplicateCarrierAtoms #-}
