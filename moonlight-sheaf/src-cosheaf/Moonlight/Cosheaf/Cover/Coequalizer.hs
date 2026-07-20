{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Cosheaf.Cover.Coequalizer
  ( CoverCosectionRepresentative (..),
    CoverCosheafCoequalizer (..),
    CoverCosheafFailure (..),
    coverCosheafCoequalizer,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core (note)
import Moonlight.Cosheaf.Chain.Cover
  ( CoverChainFailure,
    CoverNervePlan (..),
    coverNervePlanFromEffectiveCoverPlan,
  )
import Moonlight.Cosheaf.Cosection
  ( CosectionClassKey (..),
    CosectionRepKey,
    cosectionClassOfRepresentativeKey,
    cosectionClassKeyInt,
  )
import Moonlight.Cosheaf.Finite
  ( CostalkKey (..),
    FiniteCostalk,
    FiniteCosheaf,
    corestrictCostalkKey,
    fcSite,
    finiteCostalkAt,
    finiteCostalkKeyIntSet,
    finiteCostalkKeyOf,
    finiteCostalkKeys,
    finiteCostalkValueAt,
    finiteCostalkValues,
  )
import Moonlight.Sheaf.Index.Dense
  ( DenseIndex,
    denseIndexKeyIntSet,
    denseIndexKeyOf,
    denseIndexKeys,
    mkDenseIndex,
  )
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoveringFamily,
    PullbackSquare (..),
    Site (..),
    coverTarget,
  )
import Moonlight.Sheaf.Site.Plan
  ( CoverSlot,
    CoverSlotKey,
    EffectiveCoverPlanFailure,
    OverlapPlan,
    coverSlotArrow,
    coverSlotKey,
    effectiveCoverFamily,
    effectiveCoverOverlapPlans,
    effectiveCoverSlots,
    opLeftArrow,
    opLeftSlot,
    opPullbackSquare,
    opRightArrow,
    opRightSlot,
    prepareEffectiveCoverPlan,
  )

type CoverCosectionRepresentative :: Type -> Type -> Type
data CoverCosectionRepresentative obj value = CoverCosectionRepresentative
  { coverCosectionRepSlot :: !CoverSlotKey,
    coverCosectionRepObject :: !obj,
    coverCosectionRepValue :: !value
  }
  deriving stock (Eq, Ord, Show)

type CoverCosheafCoequalizer :: Type -> Type -> Type
data CoverCosheafCoequalizer site value = CoverCosheafCoequalizer
  { cccCoverPlan :: !(CoverNervePlan (SiteObject site) (SiteMorphism site)),
    cccRepresentativeIndex :: !(DenseIndex CosectionRepKey (CoverCosectionRepresentative (SiteObject site) value)),
    cccEquivalence :: !(EquivalenceRelation CosectionRepKey),
    cccClassTargets :: !(IntMap CostalkKey)
  }

deriving stock instance
  (Eq value, Eq (SiteObject site), Eq (SiteMorphism site)) =>
  Eq (CoverCosheafCoequalizer site value)

deriving stock instance
  (Show value, Show (SiteObject site), Show (SiteMorphism site)) =>
  Show (CoverCosheafCoequalizer site value)

type CoverCosheafFailure :: Type -> Type -> Type -> Type
data CoverCosheafFailure obj mor value
  = CoverCosheafEffectiveCoverInvalid !(EffectiveCoverPlanFailure obj mor)
  | CoverCosheafCoverNerveInvalid !(CoverChainFailure obj mor Int ())
  | CoverCosheafCostalkMissing !obj
  | CoverCosheafValueMissing !obj !CostalkKey
  | CoverCosheafCostalkKeyMissing !obj !value
  | CoverCosheafCorestrictionMissing !(CheckedMorphism obj mor)
  | CoverCosheafRepresentativeMissing !(CoverCosectionRepresentative obj value)
  | CoverCosheafRepresentativeKeyMissing !CosectionRepKey
  | CoverCosheafEquivalenceInvalid !EquivalenceRelationError
  | CoverCosheafClassTargetConflict !CosectionClassKey !CostalkKey !CostalkKey
  | CoverCosheafTargetNotSurjective ![CostalkKey]
  | CoverCosheafTargetOutsideCostalk ![CostalkKey]
  | CoverCosheafTargetNotInjective !CostalkKey ![CosectionClassKey]
  deriving stock (Eq, Show)

coverCosheafCoequalizer ::
  (Site site, Ord (SiteMorphism site), Ord value) =>
  CoveringFamily (SiteObject site) (SiteMorphism site) ->
  FiniteCosheaf site value ->
  Either
    (CoverCosheafFailure (SiteObject site) (SiteMorphism site) value)
    (CoverCosheafCoequalizer site value)
coverCosheafCoequalizer coverValue cosheaf = do
  coverPlan <-
    first CoverCosheafEffectiveCoverInvalid $
      prepareEffectiveCoverPlan (fcSite cosheaf) coverValue
  coverNervePlan <-
    first CoverCosheafCoverNerveInvalid $
      coverNervePlanFromEffectiveCoverPlan 1 coverPlan
  buildCoverCoequalizerFromPlan coverNervePlan cosheaf

buildCoverCoequalizerFromPlan ::
  (Site site, Ord (SiteMorphism site), Ord value) =>
  CoverNervePlan (SiteObject site) (SiteMorphism site) ->
  FiniteCosheaf site value ->
  Either
    (CoverCosheafFailure (SiteObject site) (SiteMorphism site) value)
    (CoverCosheafCoequalizer site value)
buildCoverCoequalizerFromPlan coverNervePlan cosheaf = do
  representatives <-
    fmap concat $
      traverse (slotRepresentatives cosheaf) coverSlots
  let representativeIndex =
        mkDenseIndex representatives
  overlapPairs <-
    fmap concat $
      traverse (overlapEquivalencePairs representativeIndex cosheaf) (effectiveCoverOverlapPlans coverPlan)
  relationValue <-
    first CoverCosheafEquivalenceInvalid $
      equivalenceFromPairs
        (denseIndexKeyIntSet (denseIndexKeys representativeIndex))
        overlapPairs
  repTargets <-
    fmap concat $
      traverse (slotTargetPairs representativeIndex cosheaf) coverSlots
  classTargets <-
    foldM (insertClassTarget relationValue) IntMap.empty repTargets
  targetCostalk <-
    costalkAt (coverTarget (effectiveCoverFamily coverPlan)) cosheaf
  validateTargetBijection targetCostalk classTargets
  pure
    CoverCosheafCoequalizer
      { cccCoverPlan = coverNervePlan,
        cccRepresentativeIndex = representativeIndex,
        cccEquivalence = relationValue,
        cccClassTargets = classTargets
      }
  where
    coverPlan =
      cnpEffectiveCoverPlan coverNervePlan

    coverSlots =
      IntMap.elems (effectiveCoverSlots coverPlan)

slotRepresentatives ::
  Site site =>
  FiniteCosheaf site value ->
  CoverSlot (SiteObject site) (SiteMorphism site) ->
  Either
    (CoverCosheafFailure (SiteObject site) (SiteMorphism site) value)
    [CoverCosectionRepresentative (SiteObject site) value]
slotRepresentatives cosheaf slot = do
  costalkValue <-
    costalkAt (cmSource (coverSlotArrow slot)) cosheaf
  pure
    ( fmap
        ( CoverCosectionRepresentative
            (coverSlotKey slot)
            (cmSource (coverSlotArrow slot))
        )
        (finiteCostalkValues costalkValue)
    )

overlapEquivalencePairs ::
  (Site site, Ord (SiteMorphism site), Ord value) =>
  DenseIndex CosectionRepKey (CoverCosectionRepresentative (SiteObject site) value) ->
  FiniteCosheaf site value ->
  OverlapPlan (SiteObject site) (SiteMorphism site) ->
  Either
    (CoverCosheafFailure (SiteObject site) (SiteMorphism site) value)
    [(CosectionRepKey, CosectionRepKey)]
overlapEquivalencePairs representativeIndex cosheaf overlapPlan = do
  apexCostalk <-
    costalkAt (psApex square) cosheaf
  traverse pairForApexKey (finiteCostalkKeys apexCostalk)
  where
    square =
      opPullbackSquare overlapPlan

    pairForApexKey apexKey = do
      leftKey <-
        corestrictKey (psToLeft square) apexKey cosheaf
      rightKey <-
        corestrictKey (psToRight square) apexKey cosheaf
      leftValue <-
        valueAt (cmSource (opLeftArrow overlapPlan)) leftKey cosheaf
      rightValue <-
        valueAt (cmSource (opRightArrow overlapPlan)) rightKey cosheaf
      leftRep <-
        representativeKey
          representativeIndex
          CoverCosectionRepresentative
            { coverCosectionRepSlot = opLeftSlot overlapPlan,
              coverCosectionRepObject = cmSource (opLeftArrow overlapPlan),
              coverCosectionRepValue = leftValue
            }
      rightRep <-
        representativeKey
          representativeIndex
          CoverCosectionRepresentative
            { coverCosectionRepSlot = opRightSlot overlapPlan,
              coverCosectionRepObject = cmSource (opRightArrow overlapPlan),
              coverCosectionRepValue = rightValue
            }
      pure (leftRep, rightRep)

slotTargetPairs ::
  (Site site, Ord (SiteMorphism site), Ord value) =>
  DenseIndex CosectionRepKey (CoverCosectionRepresentative (SiteObject site) value) ->
  FiniteCosheaf site value ->
  CoverSlot (SiteObject site) (SiteMorphism site) ->
  Either
    (CoverCosheafFailure (SiteObject site) (SiteMorphism site) value)
    [(CosectionRepKey, CostalkKey)]
slotTargetPairs representativeIndex cosheaf slot = do
  sourceCostalk <-
    costalkAt sourceObject cosheaf
  traverse (targetPairForValue sourceCostalk) (finiteCostalkValues sourceCostalk)
  where
    sourceObject =
      cmSource (coverSlotArrow slot)

    targetPairForValue sourceCostalk sourceValue = do
      sourceKey <-
        maybe
          (Left (CoverCosheafCostalkKeyMissing sourceObject sourceValue))
          Right
          (finiteCostalkKeyOf sourceValue sourceCostalk)
      targetKey <-
        corestrictKey (coverSlotArrow slot) sourceKey cosheaf
      repKey <-
        representativeKey
          representativeIndex
          CoverCosectionRepresentative
            { coverCosectionRepSlot = coverSlotKey slot,
              coverCosectionRepObject = sourceObject,
              coverCosectionRepValue = sourceValue
            }
      pure (repKey, targetKey)

insertClassTarget ::
  EquivalenceRelation CosectionRepKey ->
  IntMap CostalkKey ->
  (CosectionRepKey, CostalkKey) ->
  Either
    (CoverCosheafFailure obj mor value)
    (IntMap CostalkKey)
insertClassTarget relationValue classTargets (repKey, targetKey) = do
  classRep <-
    note
      (CoverCosheafRepresentativeKeyMissing repKey)
      (equivalenceRepresentative relationValue repKey)
  let classKey =
        cosectionClassOfRepresentativeKey classRep
      classKeyInt =
        cosectionClassKeyInt classKey
  case IntMap.lookup classKeyInt classTargets of
    Nothing ->
      Right (IntMap.insert classKeyInt targetKey classTargets)
    Just existingTarget
      | existingTarget == targetKey ->
          Right classTargets
      | otherwise ->
          Left (CoverCosheafClassTargetConflict classKey existingTarget targetKey)

validateTargetBijection ::
  FiniteCostalk obj value ->
  IntMap CostalkKey ->
  Either
    (CoverCosheafFailure obj mor value)
    ()
validateTargetBijection targetCostalk classTargets = do
  let targetDomain =
        finiteCostalkKeyIntSet targetCostalk
      assignedTargets =
        IntSet.fromList (fmap unCostalkKey (IntMap.elems classTargets))
      missingTargets =
        missingCostalkKeys targetDomain assignedTargets
      outsideTargets =
        missingCostalkKeys assignedTargets targetDomain
  rejectNonEmptyCostalkKeys missingTargets CoverCosheafTargetNotSurjective
  rejectNonEmptyCostalkKeys outsideTargets CoverCosheafTargetOutsideCostalk
  case findDuplicateTarget classTargets of
    Nothing ->
      Right ()
    Just (targetKey, classKeys) ->
      Left (CoverCosheafTargetNotInjective targetKey classKeys)

findDuplicateTarget :: IntMap CostalkKey -> Maybe (CostalkKey, [CosectionClassKey])
findDuplicateTarget classTargets =
  find hasMultipleClasses (Map.toAscList classesByTarget)
  where
    classesByTarget :: Map CostalkKey [CosectionClassKey]
    classesByTarget =
      Map.fromListWith
        (<>)
        [ (targetKey, [CosectionClassKey classKeyInt])
        | (classKeyInt, targetKey) <- IntMap.toAscList classTargets
        ]

    hasMultipleClasses :: (target, [classKey]) -> Bool
    hasMultipleClasses (_targetKey, classKeys) =
      case classKeys of
        _ : _ : _ -> True
        _ -> False

costalkAt ::
  Site site =>
  SiteObject site ->
  FiniteCosheaf site value ->
  Either
    (CoverCosheafFailure (SiteObject site) (SiteMorphism site) value)
    (FiniteCostalk (SiteObject site) value)
costalkAt objectValue cosheaf =
  note
    (CoverCosheafCostalkMissing objectValue)
    (finiteCostalkAt objectValue cosheaf)

valueAt ::
  Site site =>
  SiteObject site ->
  CostalkKey ->
  FiniteCosheaf site value ->
  Either
    (CoverCosheafFailure (SiteObject site) (SiteMorphism site) value)
    value
valueAt objectValue valueKey cosheaf = do
  costalkValue <-
    costalkAt objectValue cosheaf
  maybe
    (Left (CoverCosheafValueMissing objectValue valueKey))
    Right
    (finiteCostalkValueAt valueKey costalkValue)

corestrictKey ::
  (Site site, Ord (SiteMorphism site)) =>
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  CostalkKey ->
  FiniteCosheaf site value ->
  Either
    (CoverCosheafFailure (SiteObject site) (SiteMorphism site) value)
    CostalkKey
corestrictKey morphismValue sourceKey cosheaf =
  note
    (CoverCosheafCorestrictionMissing morphismValue)
    (corestrictCostalkKey morphismValue sourceKey cosheaf)

representativeKey ::
  (Ord obj, Ord value) =>
  DenseIndex CosectionRepKey (CoverCosectionRepresentative obj value) ->
  CoverCosectionRepresentative obj value ->
  Either (CoverCosheafFailure obj mor value) CosectionRepKey
representativeKey representativeIndex representativeValue =
  note
    (CoverCosheafRepresentativeMissing representativeValue)
    (denseIndexKeyOf representativeValue representativeIndex)

missingCostalkKeys :: IntSet.IntSet -> IntSet.IntSet -> [CostalkKey]
missingCostalkKeys expected actual =
  fmap CostalkKey (IntSet.toAscList (IntSet.difference expected actual))

rejectNonEmptyCostalkKeys :: [CostalkKey] -> ([CostalkKey] -> failure) -> Either failure ()
rejectNonEmptyCostalkKeys keys mkFailure =
  if null keys then Right () else Left (mkFailure keys)
