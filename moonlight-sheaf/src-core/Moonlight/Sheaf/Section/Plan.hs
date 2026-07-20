{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Section.Plan
  ( RestrictionPlan,
    rpRestrictionId,
    rpSourceKey,
    rpTargetKey,
    rpKind,
    ExtentFrontierPlan,
    efpRestrictionIds,
    efpTargetKeys,
    SheafPlans,
    spRestrictionPlansById,
    spOutgoingRestrictionIdsByObject,
    spIncomingRestrictionIdsByObject,
    spRestrictionIdsByArrow,
    sheafPlansFromRestrictionIndex,
    restrictionExtentForObjectExtent,
    extentFrontierPlanAt,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Moonlight.Delta.Scope
  ( Scope,
    cleanScope,
    dirtyScope,
    foldScope,
    fullScope,
  )
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionId (..),
    RestrictionKind,
    rId,
    rKind,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
  )
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    lookupRestriction,
    restrictionEndpointKeyMap,
    restrictionIdsByArrowKey,
    restrictionIncomingByObject,
    restrictionOutgoingByObject,
  )

type RestrictionPlan :: Type
data RestrictionPlan = RestrictionPlan
  { restrictionPlanIdInternal :: !RestrictionId,
    restrictionPlanSourceKeyInternal :: !ObjectKey,
    restrictionPlanTargetKeyInternal :: !ObjectKey,
    restrictionPlanKindInternal :: !RestrictionKind
  }
  deriving stock (Eq, Show)

type ExtentFrontierPlan :: Type
data ExtentFrontierPlan = ExtentFrontierPlan
  { extentFrontierRestrictionIdsInternal :: !IntSet,
    extentFrontierTargetKeysInternal :: !IntSet
  }
  deriving stock (Eq, Show)

type SheafPlans :: Type
data SheafPlans = SheafPlans
  { sheafRestrictionPlansByIdInternal :: !(IntMap RestrictionPlan),
    sheafOutgoingRestrictionIdsByObjectInternal :: !(IntMap IntSet),
    sheafIncomingRestrictionIdsByObjectInternal :: !(IntMap IntSet),
    sheafRestrictionIdsByArrowInternal :: !(Map (ObjectKey, ObjectKey) IntSet),
    sheafExtentFrontierByObjectInternal :: !(IntMap ExtentFrontierPlan)
  }
  deriving stock (Eq, Show)

rpRestrictionId :: RestrictionPlan -> RestrictionId
rpRestrictionId = restrictionPlanIdInternal

rpSourceKey :: RestrictionPlan -> ObjectKey
rpSourceKey = restrictionPlanSourceKeyInternal

rpTargetKey :: RestrictionPlan -> ObjectKey
rpTargetKey = restrictionPlanTargetKeyInternal

rpKind :: RestrictionPlan -> RestrictionKind
rpKind = restrictionPlanKindInternal

efpRestrictionIds :: ExtentFrontierPlan -> IntSet
efpRestrictionIds = extentFrontierRestrictionIdsInternal

efpTargetKeys :: ExtentFrontierPlan -> IntSet
efpTargetKeys = extentFrontierTargetKeysInternal

spRestrictionPlansById :: SheafPlans -> IntMap RestrictionPlan
spRestrictionPlansById = sheafRestrictionPlansByIdInternal

spOutgoingRestrictionIdsByObject :: SheafPlans -> IntMap IntSet
spOutgoingRestrictionIdsByObject = sheafOutgoingRestrictionIdsByObjectInternal

spIncomingRestrictionIdsByObject :: SheafPlans -> IntMap IntSet
spIncomingRestrictionIdsByObject = sheafIncomingRestrictionIdsByObjectInternal

spRestrictionIdsByArrow :: SheafPlans -> Map (ObjectKey, ObjectKey) IntSet
spRestrictionIdsByArrow = sheafRestrictionIdsByArrowInternal

sheafPlansFromRestrictionIndex :: RestrictionIndex cell witness -> SheafPlans
sheafPlansFromRestrictionIndex restrictions =
  SheafPlans
    { sheafRestrictionPlansByIdInternal = restrictionPlans,
      sheafOutgoingRestrictionIdsByObjectInternal = outgoing,
      sheafIncomingRestrictionIdsByObjectInternal = incoming,
      sheafRestrictionIdsByArrowInternal = restrictionIdsByArrowKey restrictions,
      sheafExtentFrontierByObjectInternal = IntMap.fromSet extentFrontierPlan frontierObjectKeys
    }
  where
    outgoing =
      restrictionOutgoingByObject restrictions
    incoming =
      restrictionIncomingByObject restrictions
    endpointKeys =
      restrictionEndpointKeyMap restrictions
    restrictionPlans =
      IntMap.fromList (mapMaybe restrictionPlanForId (IntMap.toList endpointKeys))
    frontierObjectKeys =
      IntSet.union (IntMap.keysSet outgoing) (IntMap.keysSet incoming)

    restrictionPlanForId (restrictionKey, (sourceKey, targetKey)) =
      fmap
        ( \restriction ->
            ( restrictionKey,
              RestrictionPlan
                { restrictionPlanIdInternal = rId restriction,
                  restrictionPlanSourceKeyInternal = sourceKey,
                  restrictionPlanTargetKeyInternal = targetKey,
                  restrictionPlanKindInternal = rKind restriction
                }
            )
        )
        (lookupRestriction (RestrictionId restrictionKey) restrictions)

    extentFrontierPlan objectKey =
      let restrictionIds = restrictionIdsAtObjectFromMaps outgoing incoming objectKey
       in ExtentFrontierPlan
            { extentFrontierRestrictionIdsInternal = restrictionIds,
              extentFrontierTargetKeysInternal =
                IntSet.fromList
                  ( fmap
                      (unObjectKey . rpTargetKey)
                      (mapMaybe (`IntMap.lookup` restrictionPlans) (IntSet.toAscList restrictionIds))
                  )
            }

restrictionIdsAtObjectFromMaps :: IntMap IntSet -> IntMap IntSet -> Int -> IntSet
restrictionIdsAtObjectFromMaps outgoing incoming objectKey =
  IntSet.union
    (IntMap.findWithDefault IntSet.empty objectKey outgoing)
    (IntMap.findWithDefault IntSet.empty objectKey incoming)

extentFrontierPlanAt :: ObjectKey -> SheafPlans -> ExtentFrontierPlan
extentFrontierPlanAt (ObjectKey objectKey) plans =
  IntMap.findWithDefault
    (ExtentFrontierPlan mempty mempty)
    objectKey
    (sheafExtentFrontierByObjectInternal plans)

restrictionExtentForObjectExtent :: SheafPlans -> Scope IntSet -> Scope IntSet
restrictionExtentForObjectExtent plans objectExtent =
  foldScope
    cleanScope
    (\objectKeys -> dirtyScope (foldMap (restrictionIdsAtObject plans) (IntSet.toAscList objectKeys)))
    fullScope
    objectExtent

restrictionIdsAtObject :: SheafPlans -> Int -> IntSet
restrictionIdsAtObject plans objectKey =
  IntSet.union
    (IntMap.findWithDefault IntSet.empty objectKey (spOutgoingRestrictionIdsByObject plans))
    (IntMap.findWithDefault IntSet.empty objectKey (spIncomingRestrictionIdsByObject plans))
