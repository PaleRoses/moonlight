{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Cosheaf.Colimit
  ( CosheafColimit (..),
    CosheafColimitFailure (..),
    CosheafColimitFactor (..),
    CosheafColimitFactorFailure (..),
    finiteCosheafColimitFromPreparedSupport,
    finiteCosheafColimitFromSupportPlan,
    cosheafColimitRepresentatives,
    cosheafColimitClassOf,
    cosheafColimitEquivalent,
    cosheafColimitMembers,
    cosheafColimitClassKeys,
    factorCosheafColimit,
    cosectionRepresentativeKeyOf,
    cosectionRepresentativeAt,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Moonlight.Core (note)
import Moonlight.Cosheaf.Cosection
  ( CosectionClassKey (..),
    CosectionRepKey (..),
    CosectionRepresentative (..),
    cosectionClassOfRepresentativeKey,
    cosectionClassKeyInt,
  )
import Moonlight.Cosheaf.Finite
  ( CompiledCorestriction (..),
    CostalkKey (..),
    FiniteCostalk (..),
    FiniteCosheaf,
    finiteCostalkAtObjectKey,
    finiteCostalkValueAt,
  )
import Moonlight.Cosheaf.Support
  ( CosheafSupportFailure,
    CosheafSupportPlan,
    PreparedCosheafSupport (..),
    cspCostalkKeys,
    prepareCosheafSupport,
    supportCarrierItems,
  )
import Moonlight.Sheaf.Index.Dense
  ( DenseIndex,
    denseIndexKeyIntSet,
    denseIndexKeyOf,
    denseIndexKeys,
    denseIndexValueAt,
    denseIndexValues,
    mkDenseIndexFromDistinct,
  )
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism,
    Site (..),
  )

type CosheafColimit :: Type -> Type -> Type
data CosheafColimit site value = CosheafColimit
  { ccCosheaf :: !(FiniteCosheaf site value),
    ccRepresentativeIndex :: !(DenseIndex CosectionRepKey (CosectionRepresentative (SiteObject site) value)),
    ccEquivalence :: !(EquivalenceRelation CosectionRepKey)
  }

type CosectionRepKeyByCostalk :: Type
type CosectionRepKeyByCostalk = IntMap (IntMap CosectionRepKey)

deriving stock instance
  (Eq site, Eq value, Eq (SiteObject site), Eq (SiteMorphism site)) =>
  Eq (CosheafColimit site value)

deriving stock instance
  (Show site, Show value, Show (SiteObject site), Show (SiteMorphism site)) =>
  Show (CosheafColimit site value)

type CosheafColimitFailure :: Type -> Type -> Type -> Type
data CosheafColimitFailure obj mor value
  = CosheafColimitCostalkMissing !ObjectKey
  | CosheafColimitCostalkValueMissing !ObjectKey !CostalkKey
  | CosheafColimitRepresentativeMissing !(CosectionRepresentative obj value)
  | CosheafColimitKeyMissing !CosectionRepKey
  | CosheafColimitClassMissing !CosectionClassKey
  | CosheafColimitEquivalenceInvalid !EquivalenceRelationError
  | CosheafColimitCorestrictionMalformed !(CheckedMorphism obj mor) !CostalkKey
  | CosheafColimitSupportInvalid !(CosheafSupportFailure obj mor value)
  deriving stock (Eq, Show)

type CosheafColimitFactor :: Type -> Type -> Type -> Type
data CosheafColimitFactor obj value target = CosheafColimitFactor
  { ccfClassKey :: !CosectionClassKey,
    ccfTarget :: !target,
    ccfWitnesses :: ![CosectionRepresentative obj value]
  }
  deriving stock (Eq, Show)

type CosheafColimitFactorFailure :: Type -> Type -> Type -> Type
data CosheafColimitFactorFailure obj value target
  = CosheafColimitFactorClassEmpty !CosectionClassKey
  | CosheafColimitFactorMemberMissing !CosectionRepKey
  | CosheafColimitFactorIncompatible
      !CosectionClassKey
      !(CosectionRepresentative obj value)
      !(CosectionRepresentative obj value)
      !target
      !target
  deriving stock (Eq, Show)

finiteCosheafColimitFromSupportPlan ::
  (Site site, Ord value) =>
  CosheafSupportPlan ->
  FiniteCosheaf site value ->
  Either
    (CosheafColimitFailure (SiteObject site) (SiteMorphism site) value)
    (CosheafColimit site value)
finiteCosheafColimitFromSupportPlan supportPlan cosheaf = do
  preparedSupport <-
    first CosheafColimitSupportInvalid $
      prepareCosheafSupport cosheaf supportPlan
  finiteCosheafColimitFromPreparedSupport preparedSupport

finiteCosheafColimitFromPreparedSupport ::
  (Site site, Ord value) =>
  PreparedCosheafSupport site value ->
  Either
    (CosheafColimitFailure (SiteObject site) (SiteMorphism site) value)
    (CosheafColimit site value)
finiteCosheafColimitFromPreparedSupport preparedSupport = do
  let supportedCostalkKeys =
        supportCarrierItems (cspCostalkKeys supportPlan)
  representatives <-
    traverse (representativeAtSupportedCostalk cosheaf) supportedCostalkKeys
  let representativeIndex =
        mkDenseIndexFromDistinct representatives
      representativeKeysByCostalk =
        cosectionRepKeysByCostalk supportedCostalkKeys (denseIndexKeys representativeIndex)
  pairs <-
    fmap concat $
      traverse
        (corestrictionPairsFromPreparedSupport preparedSupport representativeKeysByCostalk)
        (pcsCorestrictions preparedSupport)
  relationValue <-
    first CosheafColimitEquivalenceInvalid $
      equivalenceFromPairs
        (denseIndexKeyIntSet (denseIndexKeys representativeIndex))
        pairs
  pure
    CosheafColimit
      { ccCosheaf = cosheaf,
        ccRepresentativeIndex = representativeIndex,
        ccEquivalence = relationValue
      }
  where
    cosheaf =
      pcsCosheaf preparedSupport

    supportPlan =
      pcsPlan preparedSupport

cosheafColimitRepresentatives ::
  CosheafColimit site value ->
  [CosectionRepresentative (SiteObject site) value]
cosheafColimitRepresentatives =
  denseIndexValues . ccRepresentativeIndex
{-# INLINE cosheafColimitRepresentatives #-}

cosheafColimitClassOf ::
  (Site site, Ord value) =>
  CosectionRepresentative (SiteObject site) value ->
  CosheafColimit site value ->
  Either
    (CosheafColimitFailure (SiteObject site) (SiteMorphism site) value)
    CosectionClassKey
cosheafColimitClassOf representativeValue colimit = do
  representativeKey <-
    cosectionRepresentativeKeyOf representativeValue colimit
  relationRepresentative <-
    note
      (CosheafColimitKeyMissing representativeKey)
      (equivalenceRepresentative (ccEquivalence colimit) representativeKey)
  pure (cosectionClassOfRepresentativeKey relationRepresentative)

cosheafColimitEquivalent ::
  CosectionRepKey ->
  CosectionRepKey ->
  CosheafColimit site value ->
  Bool
cosheafColimitEquivalent leftKey rightKey colimit =
  equivalenceEquivalent (ccEquivalence colimit) leftKey rightKey
{-# INLINE cosheafColimitEquivalent #-}

cosheafColimitMembers ::
  CosectionClassKey ->
  CosheafColimit site value ->
  Either
    (CosheafColimitFailure (SiteObject site) (SiteMorphism site) value)
    [CosectionRepresentative (SiteObject site) value]
cosheafColimitMembers classKey colimit = do
  memberKeys <-
    note
      (CosheafColimitClassMissing classKey)
      (IntMap.lookup (cosectionClassKeyInt classKey) (equivalenceMembersByRep (ccEquivalence colimit)))
  traverse (memberAt . CosectionRepKey) (IntSet.toAscList memberKeys)
  where
    memberAt memberKey =
      note
        (CosheafColimitKeyMissing memberKey)
        (cosectionRepresentativeAt memberKey colimit)

cosheafColimitClassKeys ::
  CosheafColimit site value ->
  [CosectionClassKey]
cosheafColimitClassKeys =
  fmap (CosectionClassKey . fst) . IntMap.toAscList . equivalenceMembersByRep . ccEquivalence
{-# INLINE cosheafColimitClassKeys #-}

factorCosheafColimit ::
  Eq target =>
  (CosectionRepresentative (SiteObject site) value -> target) ->
  CosheafColimit site value ->
  Either
    (CosheafColimitFactorFailure (SiteObject site) value target)
    [CosheafColimitFactor (SiteObject site) value target]
factorCosheafColimit targetOf colimit =
  traverse factorClass (IntMap.toAscList (equivalenceMembersByRep (ccEquivalence colimit)))
  where
    factorClass (classKeyInt, memberInts) = do
      members <-
        traverse
          (memberAt . CosectionRepKey)
          (IntSet.toAscList memberInts)
      case members of
        [] ->
          Left (CosheafColimitFactorClassEmpty classKey)
        representativeValue : restMembers -> do
          let targetValue = targetOf representativeValue
          traverse_ (validateCompatible classKey representativeValue targetValue) restMembers
          Right
            CosheafColimitFactor
              { ccfClassKey = classKey,
                ccfTarget = targetValue,
                ccfWitnesses = members
              }
      where
        classKey =
          CosectionClassKey classKeyInt

    memberAt memberKey =
      maybe
        (Left (CosheafColimitFactorMemberMissing memberKey))
        Right
        (cosectionRepresentativeAt memberKey colimit)

    validateCompatible classKey representativeValue targetValue otherRepresentative =
      let otherTarget = targetOf otherRepresentative
       in if otherTarget == targetValue
            then Right ()
            else
              Left
                ( CosheafColimitFactorIncompatible
                    classKey
                    representativeValue
                    otherRepresentative
                    targetValue
                    otherTarget
                )

cosectionRepresentativeKeyOf ::
  (Site site, Ord value) =>
  CosectionRepresentative (SiteObject site) value ->
  CosheafColimit site value ->
  Either
    (CosheafColimitFailure (SiteObject site) (SiteMorphism site) value)
    CosectionRepKey
cosectionRepresentativeKeyOf representativeValue colimit =
  note
    (CosheafColimitRepresentativeMissing representativeValue)
    (denseIndexKeyOf representativeValue (ccRepresentativeIndex colimit))
{-# INLINE cosectionRepresentativeKeyOf #-}

cosectionRepresentativeAt ::
  CosectionRepKey ->
  CosheafColimit site value ->
  Maybe (CosectionRepresentative (SiteObject site) value)
cosectionRepresentativeAt representativeKey =
  denseIndexValueAt representativeKey . ccRepresentativeIndex
{-# INLINE cosectionRepresentativeAt #-}

representativeAtSupportedCostalk ::
  FiniteCosheaf site value ->
  (ObjectKey, CostalkKey) ->
  Either
    (CosheafColimitFailure (SiteObject site) mor value)
    (CosectionRepresentative (SiteObject site) value)
representativeAtSupportedCostalk cosheaf (objectKey, costalkKey) =
  representativeAtCostalkKey cosheaf objectKey costalkKey

representativeAtCostalkKey ::
  FiniteCosheaf site value ->
  ObjectKey ->
  CostalkKey ->
  Either
    (CosheafColimitFailure (SiteObject site) mor value)
    (CosectionRepresentative (SiteObject site) value)
representativeAtCostalkKey cosheaf objectKey costalkKey = do
  costalkValue <- colimitCostalkAtObjectKey cosheaf objectKey
  value <- colimitCostalkValue objectKey costalkKey costalkValue
  pure
    CosectionRepresentative
      { cosectionRepObject = fcostalkObject costalkValue,
        cosectionRepValue = value
      }

colimitCostalkAtObjectKey ::
  FiniteCosheaf site value ->
  ObjectKey ->
  Either
    (CosheafColimitFailure obj mor value)
    (FiniteCostalk (SiteObject site) value)
colimitCostalkAtObjectKey cosheaf objectKey =
  note
    (CosheafColimitCostalkMissing objectKey)
    (finiteCostalkAtObjectKey objectKey cosheaf)

colimitCostalkValue ::
  ObjectKey ->
  CostalkKey ->
  FiniteCostalk obj value ->
  Either
    (CosheafColimitFailure obj mor value)
    value
colimitCostalkValue objectKey costalkKey costalkValue =
  note
    (CosheafColimitCostalkValueMissing objectKey costalkKey)
    (finiteCostalkValueAt costalkKey costalkValue)

corestrictionPairsFromPreparedSupport ::
  Site site =>
  PreparedCosheafSupport site value ->
  CosectionRepKeyByCostalk ->
  CompiledCorestriction (SiteObject site) (SiteMorphism site) ->
  Either
    (CosheafColimitFailure (SiteObject site) (SiteMorphism site) value)
    [(CosectionRepKey, CosectionRepKey)]
corestrictionPairsFromPreparedSupport preparedSupport representativeKeysByCostalk corestrictionValue =
  traverse pairFor retainedSourceKeyInts
  where
    cosheaf =
      pcsCosheaf preparedSupport

    retainedSourceKeyInts =
      maybe
        []
        IntSet.toAscList
        (IntMap.lookup (unObjectKey (ccSourceObjectKey corestrictionValue)) (pcsCostalkKeysByObject preparedSupport))

    pairFor sourceKeyInt = do
      targetKey <-
        maybe
          (Left (CosheafColimitCorestrictionMalformed (ccMorphism corestrictionValue) (CostalkKey sourceKeyInt)))
          Right
          (IntMap.lookup sourceKeyInt (ccSourceToTarget corestrictionValue))
      sourceRepKey <-
        representativeKeyAt
          (ccSourceObjectKey corestrictionValue)
          (CostalkKey sourceKeyInt)
      targetRepKey <-
        representativeKeyAt
          (ccTargetObjectKey corestrictionValue)
          targetKey
      pure (sourceRepKey, targetRepKey)

    representativeKeyAt objectKey costalkKey =
      maybe
        (missingRepresentativeKey objectKey costalkKey)
        Right
        (IntMap.lookup (unObjectKey objectKey) representativeKeysByCostalk >>= IntMap.lookup (unCostalkKey costalkKey))

    missingRepresentativeKey objectKey costalkKey = do
      representativeValue <-
        representativeAtCostalkKey cosheaf objectKey costalkKey
      Left (CosheafColimitRepresentativeMissing representativeValue)

cosectionRepKeysByCostalk ::
  [(ObjectKey, CostalkKey)] ->
  [CosectionRepKey] ->
  CosectionRepKeyByCostalk
cosectionRepKeysByCostalk costalkKeys representativeKeys =
  IntMap.fromListWith
    IntMap.union
    [ (unObjectKey objectKey, IntMap.singleton (unCostalkKey costalkKey) representativeKey)
      | ((objectKey, costalkKey), representativeKey) <- zip costalkKeys representativeKeys
    ]
