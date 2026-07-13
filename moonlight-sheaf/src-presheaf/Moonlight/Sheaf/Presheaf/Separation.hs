{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Presheaf.Separation
  ( LocalEqualityFailure (..),
    SeparationFailure (..),
    SeparationConditionFailure (..),
    SeparatedFiber (..),
    SeparatedPresheaf (..),
    locallyEqualOnCover,
    separateFinitePresheaf,
    checkSeparated,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core (DenseKey (..))
import Moonlight.Sheaf.Presheaf.Finite
  ( FiberKey,
    FinitePresheaf (..),
    finiteFiberAt,
    finiteFiberKeyIntSet,
    finiteFiberKeyOf,
    finiteFiberKeys,
    finiteFiberValueAt,
  )
import Moonlight.Sheaf.Section.Congruence.Equivalence.Canonicalization
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
  )
import Moonlight.Sheaf.Site.CoverBasis.Finite
  ( FiniteCoverBasis,
    finiteCoversAt,
  )
import Moonlight.Sheaf.Site.Plan
  ( CoverSlot (..),
    EffectiveCoverPlan,
    effectiveCoverSlots,
  )

data LocalEqualityFailure obj mor mismatch restrictionFailure
  = LocalEqualityRestrictionFailed
      !(CheckedMorphism obj mor)
      !restrictionFailure
  | LocalEqualityRestrictionMismatch
      !(CheckedMorphism obj mor)
      ![mismatch]
  deriving stock (Eq, Show)

data SeparationFailure obj mor value mismatch restrictionFailure
  = SeparationFiberMissing !obj
  | SeparationValueMissing !obj !FiberKey
  | SeparationRestrictionFailed !(CheckedMorphism obj mor) !value !restrictionFailure
  | SeparationRestrictedValueOutsideFiber !(CheckedMorphism obj mor) !value !value
  | SeparationRelationInvalid !obj !EquivalenceRelationError
  | SeparationRestrictionImageInvalid !(CheckedMorphism obj mor) !EquivalenceRelationError
  | SeparationRestrictionNotWellDefined !(CheckedMorphism obj mor) !FiberKey !FiberKey
  deriving stock (Eq, Show)

data SeparationConditionFailure obj value mismatch
  = LocallyEqualButGloballyDifferent
      !obj
      !FiberKey
      !FiberKey
      ![mismatch]
  deriving stock (Eq, Show)

data SeparatedFiber obj = SeparatedFiber
  { sfObject :: !obj,
    sfLocalEquality :: !(EquivalenceRelation FiberKey)
  }
  deriving stock (Eq, Show)

data SeparatedPresheaf site value mismatch restrictionFailure = SeparatedPresheaf
  { sepBase :: !(FinitePresheaf site value mismatch restrictionFailure),
    sepFibers :: !(Map (SiteObject site) (SeparatedFiber (SiteObject site)))
  }

locallyEqualOnCover ::
  FinitePresheaf site value mismatch restrictionFailure ->
  EffectiveCoverPlan (SiteObject site) (SiteMorphism site) ->
  value ->
  value ->
  Either
    [LocalEqualityFailure (SiteObject site) (SiteMorphism site) mismatch restrictionFailure]
    Bool
locallyEqualOnCover presheaf coverPlan leftValue rightValue =
  case concatMap equalityFailuresAtSlot (IntMap.elems (effectiveCoverSlots coverPlan)) of
    [] ->
      Right True
    failures ->
      Left failures
  where
    equalityFailuresAtSlot slot =
      let arrow = coverSlotArrow slot
       in case (fpRestrict presheaf arrow leftValue, fpRestrict presheaf arrow rightValue) of
            (Left failure, _) ->
              [LocalEqualityRestrictionFailed arrow failure]
            (_, Left failure) ->
              [LocalEqualityRestrictionFailed arrow failure]
            (Right leftRestricted, Right rightRestricted) ->
              let mismatches =
                    fpMismatches presheaf (cmSource arrow) leftRestricted rightRestricted
               in [LocalEqualityRestrictionMismatch arrow mismatches | not (null mismatches)]

separateFinitePresheaf ::
  (Site site, Ord value) =>
  FiniteCoverBasis site ->
  FinitePresheaf site value mismatch restrictionFailure ->
  Either
    (SeparationFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    (SeparatedPresheaf site value mismatch restrictionFailure)
separateFinitePresheaf basis presheaf = do
  separatedFibers <-
    Map.fromList
      <$> traverse
        separateObject
        (siteObjects (fpSite presheaf))
  let separated =
        SeparatedPresheaf
          { sepBase = presheaf,
            sepFibers = separatedFibers
          }
  validateSeparatedRestrictions separated
  pure separated
  where
    separateObject objectValue = do
      fiberValue <-
        note
          (SeparationFiberMissing objectValue)
          (finiteFiberAt objectValue presheaf)
      pairs <-
        localEqualityPairs objectValue fiberValue
      relationValue <-
        first
          (SeparationRelationInvalid objectValue)
          (equivalenceFromPairs (finiteFiberKeyIntSet fiberValue) pairs)
      pure
        ( objectValue,
          SeparatedFiber
            { sfObject = objectValue,
              sfLocalEquality = relationValue
            }
        )

    localEqualityPairs objectValue fiberValue =
      fmap concat $
        traverse
          (pairsOnCover objectValue fiberValue)
          (finiteCoversAt basis objectValue)

    pairsOnCover objectValue fiberValue coverPlan =
      fmap concat $
        traverse
          (locallyEqualPair objectValue fiberValue coverPlan)
          [ (leftKey, rightKey)
          | leftKey <- finiteFiberKeys fiberValue,
            rightKey <- finiteFiberKeys fiberValue,
            encodeDenseKey leftKey < encodeDenseKey rightKey
          ]

    locallyEqualPair objectValue fiberValue coverPlan (leftKey, rightKey) = do
      leftValue <-
        note
          (SeparationValueMissing objectValue leftKey)
          (finiteFiberValueAt leftKey fiberValue)
      rightValue <-
        note
          (SeparationValueMissing objectValue rightKey)
          (finiteFiberValueAt rightKey fiberValue)
      locallyEqual <-
        locallyEqualForSeparation coverPlan leftValue rightValue
      pure [(leftKey, rightKey) | locallyEqual]

    locallyEqualForSeparation coverPlan leftValue rightValue =
      fmap and $
        traverse
          (localEqualityAtSlot leftValue rightValue)
          (IntMap.elems (effectiveCoverSlots coverPlan))

    localEqualityAtSlot leftValue rightValue slot = do
      let arrow = coverSlotArrow slot
      leftRestricted <-
        first
          (SeparationRestrictionFailed arrow leftValue)
          (fpRestrict presheaf arrow leftValue)
      rightRestricted <-
        first
          (SeparationRestrictionFailed arrow rightValue)
          (fpRestrict presheaf arrow rightValue)
      pure (null (fpMismatches presheaf (cmSource arrow) leftRestricted rightRestricted))

validateSeparatedRestrictions ::
  (Site site, Ord value) =>
  SeparatedPresheaf site value mismatch restrictionFailure ->
  Either
    (SeparationFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    ()
validateSeparatedRestrictions separated =
  traverse_ validateMorphism (siteMorphisms (fpSite presheaf))
  where
    presheaf =
      sepBase separated

    validateMorphism morphismValue = do
      sourceFiber <-
        note
          (SeparationFiberMissing (cmSource morphismValue))
          (finiteFiberAt (cmSource morphismValue) presheaf)
      targetFiber <-
        note
          (SeparationFiberMissing (cmTarget morphismValue))
          (finiteFiberAt (cmTarget morphismValue) presheaf)
      sourceSeparated <-
        separatedFiberAt (cmSource morphismValue)
      targetSeparated <-
        separatedFiberAt (cmTarget morphismValue)
      sourceToTargetKeys <-
        restrictionKeyMap morphismValue sourceFiber targetFiber
      imageRelation <-
        first
          (SeparationRestrictionImageInvalid morphismValue)
          ( equivalenceImage
              sourceToTargetKeys
              (finiteFiberKeyIntSet sourceFiber)
              (sfLocalEquality targetSeparated)
          )
      traverse_
        (validateImagePair morphismValue (sfLocalEquality sourceSeparated))
        (equivalencePairs imageRelation)

    separatedFiberAt objectValue =
      note
        (SeparationFiberMissing objectValue)
        (Map.lookup objectValue (sepFibers separated))

    restrictionKeyMap morphismValue sourceFiber targetFiber =
      IntMap.fromList
        <$> traverse
          (restrictionKeyEntry morphismValue sourceFiber targetFiber)
          (finiteFiberKeys targetFiber)

    restrictionKeyEntry morphismValue sourceFiber targetFiber targetKey = do
      targetValue <-
        note
          (SeparationValueMissing (cmTarget morphismValue) targetKey)
          (finiteFiberValueAt targetKey targetFiber)
      restrictedValue <-
        first
          (SeparationRestrictionFailed morphismValue targetValue)
          (fpRestrict presheaf morphismValue targetValue)
      sourceKey <-
        note
          (SeparationRestrictedValueOutsideFiber morphismValue targetValue restrictedValue)
          (finiteFiberKeyOf restrictedValue sourceFiber)
      pure (encodeDenseKey targetKey, sourceKey)

    validateImagePair ::
      CheckedMorphism obj mor ->
      EquivalenceRelation FiberKey ->
      (FiberKey, FiberKey) ->
      Either (SeparationFailure obj mor value mismatch restrictionFailure) ()
    validateImagePair morphismValue sourceRelation (leftKey, rightKey) =
      if equivalenceEquivalent sourceRelation leftKey rightKey
        then Right ()
        else Left (SeparationRestrictionNotWellDefined morphismValue leftKey rightKey)

checkSeparated ::
  Site site =>
  SeparatedPresheaf site value mismatch restrictionFailure ->
  [SeparationConditionFailure (SiteObject site) value mismatch]
checkSeparated separated =
  concatMap checkObject (siteObjects (fpSite presheaf))
  where
    presheaf =
      sepBase separated

    checkObject objectValue =
      case (finiteFiberAt objectValue presheaf, Map.lookup objectValue (sepFibers separated)) of
        (Just fiberValue, Just separatedFiber) ->
          [ LocallyEqualButGloballyDifferent objectValue leftKey rightKey mismatches
          | (leftKey, rightKey) <- equivalencePairs (sfLocalEquality separatedFiber),
            Just leftValue <- [finiteFiberValueAt leftKey fiberValue],
            Just rightValue <- [finiteFiberValueAt rightKey fiberValue],
            let mismatches = fpMismatches presheaf objectValue leftValue rightValue,
            not (null mismatches)
          ]
        _ ->
          []


note :: failure -> Maybe value -> Either failure value
note failure =
  maybe (Left failure) Right
{-# INLINE note #-}
