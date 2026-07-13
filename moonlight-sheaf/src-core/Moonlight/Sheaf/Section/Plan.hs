{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Section.Plan
  ( RestrictionPlan (..),
    ExtentFrontierPlan (..),
    SheafPlans (..),
    emptySheafPlans,
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
  { rpRestrictionId :: !RestrictionId,
    rpSourceKey :: !ObjectKey,
    rpTargetKey :: !ObjectKey,
    rpKind :: !RestrictionKind
  }
  deriving stock (Eq, Show)

type ExtentFrontierPlan :: Type
data ExtentFrontierPlan = ExtentFrontierPlan
  { efpRestrictionIds :: !IntSet,
    efpTargetKeys :: !IntSet
  }
  deriving stock (Eq, Show)

type SheafPlans :: Type
data SheafPlans = SheafPlans
  { spRestrictionPlansById :: !(IntMap RestrictionPlan),
    spOutgoingRestrictionIdsByObject :: !(IntMap IntSet),
    spIncomingRestrictionIdsByObject :: !(IntMap IntSet),
    spRestrictionIdsByArrow :: !(Map (ObjectKey, ObjectKey) IntSet),
    spExtentFrontierByObject :: !(IntMap ExtentFrontierPlan)
  }
  deriving stock (Eq, Show)

emptySheafPlans :: SheafPlans
emptySheafPlans =
  SheafPlans
    { spRestrictionPlansById = IntMap.empty,
      spOutgoingRestrictionIdsByObject = IntMap.empty,
      spIncomingRestrictionIdsByObject = IntMap.empty,
      spRestrictionIdsByArrow = mempty,
      spExtentFrontierByObject = IntMap.empty
    }

sheafPlansFromRestrictionIndex :: RestrictionIndex cell witness -> SheafPlans
sheafPlansFromRestrictionIndex restrictions =
  SheafPlans
    { spRestrictionPlansById = restrictionPlans,
      spOutgoingRestrictionIdsByObject = outgoing,
      spIncomingRestrictionIdsByObject = incoming,
      spRestrictionIdsByArrow = restrictionIdsByArrowKey restrictions,
      spExtentFrontierByObject = IntMap.fromSet extentFrontierPlan frontierObjectKeys
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
                { rpRestrictionId = rId restriction,
                  rpSourceKey = sourceKey,
                  rpTargetKey = targetKey,
                  rpKind = rKind restriction
                }
            )
        )
        (lookupRestriction (RestrictionId restrictionKey) restrictions)

    extentFrontierPlan objectKey =
      let restrictionIds = restrictionIdsAtObjectFromMaps outgoing incoming objectKey
       in ExtentFrontierPlan
            { efpRestrictionIds = restrictionIds,
              efpTargetKeys =
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
    (spExtentFrontierByObject plans)

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
