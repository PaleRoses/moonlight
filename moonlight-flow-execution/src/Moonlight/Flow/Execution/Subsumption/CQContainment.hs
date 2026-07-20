{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Execution.Subsumption.CQContainment
  ( CanonAtomPredicateKey (..),
    CQContainmentWitness (..),
    CQContainmentError (..),
    canonAtomPredicateKey,
    compileCQContainment,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.Foldable
  ( asum,
  )
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( listToMaybe,
  )
import Moonlight.Flow.Execution.Subsumption.Proof
  ( CQContainmentWitness (..),
  )
import Moonlight.Flow.Internal.Digest
  ( wordOfInt,
  )
import Moonlight.Flow.Plan.Shape.Encode
  ( canonAtomWords,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
    stableDigest128,
  )
import Moonlight.Flow.Plan.Shape
  ( CanonAtom (..),
    CanonAtomMultiset,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot,
    CanonStalkRecipe,
    canonSlotKey,
  )
import Data.Word
  ( Word64,
  )

type CanonAtomPredicateKey :: Type
data CanonAtomPredicateKey = CanonAtomPredicateKey
  { capTagDigest :: {-# UNPACK #-} !Word64,
    capArity :: {-# UNPACK #-} !Int,
    capRecipe :: !CanonStalkRecipe
  }
  deriving stock (Eq, Ord, Show, Read)

type CQContainmentError :: Type
data CQContainmentError
  = CQContainmentNonPositiveSourceMultiplicity !CanonAtom !Int
  | CQContainmentNonPositiveTargetMultiplicity !CanonAtom !Int
  | CQContainmentMissingPredicate !CanonAtomPredicateKey
  | CQContainmentNoHomomorphism
  deriving stock (Eq, Ord, Show, Read)

type TargetAtomWork :: Type
data TargetAtomWork = TargetAtomWork
  { tawTarget :: !CanonAtom,
    tawCandidates :: ![CanonAtom]
  }
  deriving stock (Eq, Ord, Show, Read)

type SlotMap :: Type
type SlotMap =
  IntMap CanonSlot

canonAtomPredicateKey ::
  CanonAtom ->
  CanonAtomPredicateKey
canonAtomPredicateKey atomValue =
  CanonAtomPredicateKey
    { capTagDigest = caTagDigest atomValue,
      capArity = length (caColumns atomValue),
      capRecipe = caRecipe atomValue
    }
{-# INLINE canonAtomPredicateKey #-}

compileCQContainment ::
  CanonAtomMultiset ->
  CanonAtomMultiset ->
  [CanonSlot] ->
  [CanonSlot] ->
  Either CQContainmentError CQContainmentWitness
compileCQContainment sourceAtoms targetAtoms sourceOutput targetOutput = do
  sourceAtomList <-
    positiveUniqueAtoms
      CQContainmentNonPositiveSourceMultiplicity
      sourceAtoms
  targetAtomList <-
    positiveUniqueAtoms
      CQContainmentNonPositiveTargetMultiplicity
      targetAtoms

  let sourceByPredicate =
        sourcePredicateIndex sourceAtomList

  targetWork <-
    traverse
      (targetAtomWork sourceByPredicate)
      targetAtomList

  case searchHomomorphism sourceOutput targetOutput (orderedTargetWork targetWork) IntMap.empty [] of
    Nothing ->
      Left CQContainmentNoHomomorphism
    Just witness ->
      Right witness
{-# INLINE compileCQContainment #-}

positiveUniqueAtoms ::
  (CanonAtom -> Int -> CQContainmentError) ->
  CanonAtomMultiset ->
  Either CQContainmentError [CanonAtom]
positiveUniqueAtoms mkError =
  traverse positiveAtom . Map.toAscList
  where
    positiveAtom (atomValue, multiplicity)
      | multiplicity <= 0 =
          Left (mkError atomValue multiplicity)
      | otherwise =
          Right atomValue
{-# INLINE positiveUniqueAtoms #-}

sourcePredicateIndex ::
  [CanonAtom] ->
  Map CanonAtomPredicateKey [CanonAtom]
sourcePredicateIndex atoms =
  Map.map List.sort $
    Map.fromListWith
      (<>)
      [ (canonAtomPredicateKey atomValue, [atomValue])
      | atomValue <- atoms
      ]
{-# INLINE sourcePredicateIndex #-}

targetAtomWork ::
  Map CanonAtomPredicateKey [CanonAtom] ->
  CanonAtom ->
  Either CQContainmentError TargetAtomWork
targetAtomWork sourceByPredicate targetAtom =
  case Map.findWithDefault [] key sourceByPredicate of
    [] ->
      Left (CQContainmentMissingPredicate key)
    candidates ->
      Right
        TargetAtomWork
          { tawTarget = targetAtom,
            tawCandidates = candidates
          }
  where
    key =
      canonAtomPredicateKey targetAtom
{-# INLINE targetAtomWork #-}

orderedTargetWork ::
  [TargetAtomWork] ->
  [TargetAtomWork]
orderedTargetWork =
  List.sortOn
    ( \work ->
        ( length (tawCandidates work),
          tawTarget work
        )
    )
{-# INLINE orderedTargetWork #-}

searchHomomorphism ::
  [CanonSlot] ->
  [CanonSlot] ->
  [TargetAtomWork] ->
  SlotMap ->
  [(CanonAtom, CanonAtom)] ->
  Maybe CQContainmentWitness
searchHomomorphism sourceOutput targetOutput work slotMap images =
  case work of
    [] -> do
      completedMap <-
        completeOutputMap sourceOutput targetOutput slotMap
      pure (mkCQContainmentWitness completedMap images)
    current : rest ->
      asum
        [ do
            slotMap' <-
              unifyAtom
                (tawTarget current)
                sourceAtom
                slotMap
            searchHomomorphism
              sourceOutput
              targetOutput
              rest
              slotMap'
              ((tawTarget current, sourceAtom) : images)
        | sourceAtom <- tawCandidates current
        ]
{-# INLINE searchHomomorphism #-}

unifyAtom ::
  CanonAtom ->
  CanonAtom ->
  SlotMap ->
  Maybe SlotMap
unifyAtom targetAtom sourceAtom slotMap
  | canonAtomPredicateKey targetAtom /= canonAtomPredicateKey sourceAtom =
      Nothing
  | otherwise =
      unifySlots
        slotMap
        (zip (caColumns targetAtom) (caColumns sourceAtom))
{-# INLINE unifyAtom #-}

unifySlots ::
  SlotMap ->
  [(CanonSlot, CanonSlot)] ->
  Maybe SlotMap
unifySlots =
  foldM unifyOne
  where
    unifyOne currentSlotMap (targetSlot, sourceSlot) =
      unifySlot targetSlot sourceSlot currentSlotMap
{-# INLINE unifySlots #-}

unifySlot ::
  CanonSlot ->
  CanonSlot ->
  SlotMap ->
  Maybe SlotMap
unifySlot targetSlot sourceSlot slotMap =
  case IntMap.lookup targetKey slotMap of
    Nothing ->
      Just (IntMap.insert targetKey sourceSlot slotMap)
    Just existing
      | existing == sourceSlot ->
          Just slotMap
      | otherwise ->
          Nothing
  where
    targetKey =
      canonSlotKey targetSlot
{-# INLINE unifySlot #-}

completeOutputMap ::
  [CanonSlot] ->
  [CanonSlot] ->
  SlotMap ->
  Maybe SlotMap
completeOutputMap sourceOutput targetOutput slotMap0 =
  foldM (completeTargetSlot sourceOutputKeys sourceOutput) slotMap0 targetOutput
  where
    sourceOutputKeys =
      IntSet.fromList (fmap canonSlotKey sourceOutput)
{-# INLINE completeOutputMap #-}

completeTargetSlot ::
  IntSet.IntSet ->
  [CanonSlot] ->
  SlotMap ->
  CanonSlot ->
  Maybe SlotMap
completeTargetSlot sourceOutputKeys sourceOutput slotMap targetSlot =
  case IntMap.lookup targetKey slotMap of
    Just sourceSlot
      | IntSet.member (canonSlotKey sourceSlot) sourceOutputKeys ->
          Just slotMap
      | otherwise ->
          Nothing
    Nothing ->
      (\sourceSlot -> IntMap.insert targetKey sourceSlot slotMap)
        <$> listToMaybe sourceOutput
  where
    targetKey =
      canonSlotKey targetSlot
{-# INLINE completeTargetSlot #-}

mkCQContainmentWitness ::
  SlotMap ->
  [(CanonAtom, CanonAtom)] ->
  CQContainmentWitness
mkCQContainmentWitness slotMap images =
  let atomImages =
        List.sort images
      digestValue =
        cqContainmentDigest slotMap atomImages
   in CQContainmentWitness
        { cqwSlotMap = slotMap,
          cqwAtomImages = atomImages,
          cqwDigest = digestValue
        }
{-# INLINE mkCQContainmentWitness #-}

cqContainmentDigest ::
  SlotMap ->
  [(CanonAtom, CanonAtom)] ->
  StableDigest128
cqContainmentDigest slotMap atomImages =
  stableDigest128
    ( [0x6371486f6d]
        <> concatMap slotMapEntryWords (IntMap.toAscList slotMap)
        <> concatMap atomImageWords atomImages
    )
{-# INLINE cqContainmentDigest #-}

slotMapEntryWords ::
  (Int, CanonSlot) ->
  [Word64]
slotMapEntryWords (targetKey, sourceSlot) =
  [ 0x10,
    wordOfInt targetKey,
    wordOfInt (canonSlotKey sourceSlot)
  ]
{-# INLINE slotMapEntryWords #-}

atomImageWords ::
  (CanonAtom, CanonAtom) ->
  [Word64]
atomImageWords (targetAtom, sourceAtom) =
  [0x20]
    <> canonAtomWords targetAtom
    <> canonAtomWords sourceAtom
{-# INLINE atomImageWords #-}
