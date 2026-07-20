{-# LANGUAGE DataKinds #-}

module Moonlight.Flow.Plan.Rewrite.Transform.ProjectionRestriction
  ( projectionIdentity,
    restrictionIdentity,
    identitySlotMap,

    composeProjectionShapes,
    composeRestrictionShapes,

    projectionRestrictionCommutes,
    restrictionProjectionCommutes,

    projectPinnedSlots,
    pullbackPinnedSlots,
    uniqueSourceToTarget,
    slotKeySet,

    projectionShapeFromPayload,
    restrictionShapeFromPayload,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Moonlight.Flow.Plan.Shape.Build qualified as ShapeBuild
import Moonlight.Flow.Plan.Shape.Encode qualified as ShapeEncode
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot,
    PlanShape (..),
    PlanStage (..),
    ProjectionPayload (..),
    RestrictionPayload (..),
    canonSlotKey,
  )

projectionIdentity ::
  ProjectionPayload ->
  Bool
projectionIdentity payload =
  ppSourceShape payload == ppTargetShape payload
    && ppSourceSchema payload == ppTargetSchema payload
    && ppSlotMap payload == identitySlotMap (ppTargetSchema payload)
{-# INLINE projectionIdentity #-}

restrictionIdentity ::
  RestrictionPayload ->
  Bool
restrictionIdentity payload =
  rpSourceShape payload == rpTargetShape payload
    && IntMap.null (rpPinnedSlots payload)
{-# INLINE restrictionIdentity #-}

identitySlotMap ::
  [CanonSlot] ->
  IntMap CanonSlot
identitySlotMap =
  IntMap.fromList . fmap (\slot -> (canonSlotKey slot, slot))
{-# INLINE identitySlotMap #-}

composeProjectionShapes ::
  PlanShape 'Projection ->
  PlanShape 'Projection ->
  Maybe (PlanShape 'Projection)
composeProjectionShapes innerShape outerShape =
  let inner =
        psPayload innerShape
      outer =
        psPayload outerShape
   in if ppTargetShape inner == ppSourceShape outer
        && ppTargetSchema inner == ppSourceSchema outer
        then
          case traverse lookupInnerSource (ppSlotMap outer) of
            Nothing ->
              Nothing
            Just slotMap ->
              either
                (const Nothing)
                Just
                ( ShapeBuild.compileProjectionShape
                    (ppSourceShape inner)
                    (ppTargetShape outer)
                    (ppSourceSchema inner)
                    (ppTargetSchema outer)
                    slotMap
                )
        else Nothing
  where
    lookupInnerSource middleSlot =
      IntMap.lookup (canonSlotKey middleSlot) (ppSlotMap (psPayload innerShape))
{-# INLINE composeProjectionShapes #-}

composeRestrictionShapes ::
  PlanShape 'Restriction ->
  PlanShape 'Restriction ->
  Maybe (PlanShape 'Restriction)
composeRestrictionShapes innerShape outerShape =
  let inner =
        psPayload innerShape
      outer =
        psPayload outerShape
   in if rpTargetShape inner == rpSourceShape outer
        then
          Just
            ( ShapeBuild.mkRestrictionShape
                (rpSourceShape inner)
                (rpTargetShape outer)
                (IntMap.unionWith IntSet.union (rpPinnedSlots inner) (rpPinnedSlots outer))
            )
        else Nothing
{-# INLINE composeRestrictionShapes #-}

projectionRestrictionCommutes ::
  ProjectionPayload ->
  RestrictionPayload ->
  Maybe (ProjectionPayload, RestrictionPayload)
projectionRestrictionCommutes projection restriction
  | ppSourceShape projection /= rpTargetShape restriction =
      Nothing
  | otherwise = do
      projectedPins <-
        projectPinnedSlots projection (rpPinnedSlots restriction)
      let projectionPayload =
            projection
              { ppSourceShape = rpSourceShape restriction,
                ppTargetShape =
                  ShapeBuild.projectedShapeDigest
                    (rpSourceShape restriction)
                    projection
              }
          restrictionPayload =
            RestrictionPayload
              { rpSourceShape = ppTargetShape projectionPayload,
                rpTargetShape = ppTargetShape projection,
                rpPinnedSlots = projectedPins
              }
      Just (projectionPayload, restrictionPayload)
{-# INLINE projectionRestrictionCommutes #-}

restrictionProjectionCommutes ::
  RestrictionPayload ->
  ProjectionPayload ->
  Maybe (RestrictionPayload, ProjectionPayload)
restrictionProjectionCommutes restriction projection
  | rpSourceShape restriction /= ppTargetShape projection =
      Nothing
  | otherwise = do
      pulledPins <-
        pullbackPinnedSlots projection (rpPinnedSlots restriction)
      let restriction0 =
            RestrictionPayload
              { rpSourceShape = ppSourceShape projection,
                rpTargetShape = ppSourceShape projection,
                rpPinnedSlots = pulledPins
              }
          restrictedTarget =
            ShapeBuild.restrictedShapeDigest
              (ppSourceShape projection)
              restriction0
          restrictionPayload =
            restriction0 {rpTargetShape = restrictedTarget}
          projectionPayload =
            projection
              { ppSourceShape = rpTargetShape restrictionPayload,
                ppTargetShape = rpTargetShape restriction
              }
      Just (restrictionPayload, projectionPayload)
{-# INLINE restrictionProjectionCommutes #-}

projectPinnedSlots ::
  ProjectionPayload ->
  IntMap IntSet ->
  Maybe (IntMap IntSet)
projectPinnedSlots projection pinnedSlots = do
  sourceToTarget <-
    uniqueSourceToTarget projection
  IntMap.foldlWithKey'
    ( \maybePins sourceSlotKey pinnedValues -> do
        pins <- maybePins
        targetSlot <- IntMap.lookup sourceSlotKey sourceToTarget
        Just
          ( IntMap.insertWith
              IntSet.union
              (canonSlotKey targetSlot)
              pinnedValues
              pins
          )
    )
    (Just IntMap.empty)
    pinnedSlots
{-# INLINE projectPinnedSlots #-}

pullbackPinnedSlots ::
  ProjectionPayload ->
  IntMap IntSet ->
  Maybe (IntMap IntSet)
pullbackPinnedSlots projection pinnedSlots =
  IntMap.foldlWithKey'
    ( \maybePins targetSlotKey pinnedValues -> do
        pins <- maybePins
        if IntSet.member targetSlotKey targetSlotKeys
          then do
            sourceSlot <- IntMap.lookup targetSlotKey (ppSlotMap projection)
            Just
              ( IntMap.insertWith
                  IntSet.union
                  (canonSlotKey sourceSlot)
                  pinnedValues
                  pins
              )
          else Nothing
    )
    (Just IntMap.empty)
    pinnedSlots
  where
    targetSlotKeys =
      slotKeySet (ppTargetSchema projection)
{-# INLINE pullbackPinnedSlots #-}

uniqueSourceToTarget ::
  ProjectionPayload ->
  Maybe (IntMap CanonSlot)
uniqueSourceToTarget projection =
  IntMap.foldlWithKey'
    insertMapping
    (Just IntMap.empty)
    (ppSlotMap projection)
  where
    sourceSlotKeys =
      slotKeySet (ppSourceSchema projection)

    targetSlots =
      IntMap.fromList
        [ (canonSlotKey slot, slot)
        | slot <- ppTargetSchema projection
        ]

    insertMapping maybeMap targetSlotKey sourceSlot = do
      acc <- maybeMap
      targetSlot <- IntMap.lookup targetSlotKey targetSlots
      let sourceSlotKey =
            canonSlotKey sourceSlot
      if IntSet.notMember sourceSlotKey sourceSlotKeys
        then Nothing
        else
          case IntMap.lookup sourceSlotKey acc of
            Nothing ->
              Just (IntMap.insert sourceSlotKey targetSlot acc)
            Just existingTarget
              | existingTarget == targetSlot ->
                  Just acc
              | otherwise ->
                  Nothing
{-# INLINE uniqueSourceToTarget #-}

slotKeySet ::
  [CanonSlot] ->
  IntSet
slotKeySet =
  IntSet.fromList . fmap canonSlotKey
{-# INLINE slotKeySet #-}

projectionShapeFromPayload ::
  ProjectionPayload ->
  Maybe (PlanShape 'Projection)
projectionShapeFromPayload =
  either (const Nothing) Just . ShapeBuild.checkedProjectionShapeFromPayload
{-# INLINE projectionShapeFromPayload #-}

restrictionShapeFromPayload ::
  RestrictionPayload ->
  PlanShape 'Restriction
restrictionShapeFromPayload =
  ShapeBuild.mkPlanShape ShapeEncode.restrictionPayloadWords
{-# INLINE restrictionShapeFromPayload #-}
