{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Match.Types.Plan
  ( QueryFingerprint (..),
    QueryId,
    mkQueryId,
    queryIdKey,
    AtomId,
    mkAtomId,
    atomIdKey,
    SlotId,
    mkSlotId,
    slotIdKey,
    Schema,
    RestrictionMap,
    RowState (Canonical),
    RowBlockIdentity (..),
    RowDesc,
    RowBlock,
    RowRestrictionProgram,
    RowOperationError (..),
    RowProgramError (..),
    MatchFootprint (..),
    emptyFootprint,
    QuerySnapshot (..),
    PresheafCarrier (..),
    MatchPlan (..),
    atomRowsClassKeys,
    atomRowTouchesDirty,
    emptyPlanSection,
    sectionFromPlanRows,
    restrictPlanSection,
    planSectionRows,
    planAtomSpecId,
    planAtomColumns,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Constraint, Type)
import Data.Vector (Vector)
import Moonlight.Core
  ( MatchFootprint (..),
    QuerySnapshot (..),
    emptyFootprint,
  )
import Moonlight.Core
  ( AtomId,
    QueryId,
    SlotId,
    atomIdKey,
    mkAtomId,
    mkQueryId,
    mkSlotId,
    queryIdKey,
    slotIdKey,
  )
import Moonlight.Differential.Row.Block
  ( RowDesc,
    RowBlock,
    RestrictionMap,
    RowBlockIdentity (..),
    RowOperationError (..),
    RowProgramError (..),
    RowRestrictionProgram,
    RowState (Canonical),
    RowLayout,
    rowBlockSupport,
    rowDescSupport,
  )
import Moonlight.Saturation.Matching
  ( QueryFingerprint (..),
  )

type Schema :: Type
type Schema = RowLayout

type PresheafCarrier :: Type -> Constraint
class PresheafCarrier section where
  emptySection ::
    RowBlockIdentity ->
    Schema ->
    section

  fromRows ::
    RowBlock 'Canonical ->
    section

  sectionRows ::
    section ->
    RowBlock 'Canonical

  sectionSchema ::
    section ->
    Schema

  restrictSection ::
    RowRestrictionProgram ->
    RowBlockIdentity ->
    section ->
    Either RowOperationError section

  sectionSupport ::
    section ->
    IntSet

atomRowsClassKeys :: RowBlock state -> IntSet
atomRowsClassKeys =
  rowBlockSupport
{-# INLINE atomRowsClassKeys #-}

atomRowTouchesDirty ::
  IntSet ->
  RowBlock state ->
  RowDesc ->
  Bool
atomRowTouchesDirty dirtyKeys rows rowDesc =
  not
    ( IntSet.null
        (IntSet.intersection dirtyKeys (rowDescSupport rows rowDesc))
    )
{-# INLINE atomRowTouchesDirty #-}

type MatchPlan :: Type -> Constraint
class PresheafCarrier (PlanSection plan) => MatchPlan plan where
  type PlanSection plan :: Type
  type PlanOutput plan :: Type
  type PlanAtom plan :: Type
  type PlanObstruction plan :: Type
  type PlanProjectionState plan :: Type
  type PlanProjectionState plan = ()

  planId ::
    plan ->
    QueryId

  setPlanId ::
    QueryId ->
    plan ->
    plan

  planFingerprint ::
    plan ->
    QueryFingerprint

  planAtoms ::
    plan ->
    Vector (PlanAtom plan)

  runJoinRows ::
    plan ->
    IntMap (PlanSection plan) ->
    Either (PlanObstruction plan) (RowBlock 'Canonical)

  existsPinnedRow ::
    plan ->
    AtomId ->
    RowBlock 'Canonical ->
    RowDesc ->
    IntMap (PlanSection plan) ->
    Either (PlanObstruction plan) Bool

  planSupportRows ::
    plan ->
    IntMap (PlanSection plan) ->
    Either (PlanObstruction plan) (IntMap (RowBlock 'Canonical))

  outputProjectRow ::
    plan ->
    RowBlock 'Canonical ->
    RowDesc ->
    Either (PlanObstruction plan) (Maybe (PlanOutput plan))

  atomSpecId ::
    PlanAtom plan ->
    AtomId

  atomColumns ::
    PlanAtom plan ->
    Schema

emptyPlanSection ::
  forall plan.
  MatchPlan plan =>
  RowBlockIdentity ->
  Schema ->
  PlanSection plan
emptyPlanSection =
  emptySection @(PlanSection plan)
{-# INLINE emptyPlanSection #-}

sectionFromPlanRows ::
  forall plan.
  MatchPlan plan =>
  RowBlock 'Canonical ->
  PlanSection plan
sectionFromPlanRows =
  fromRows @(PlanSection plan)
{-# INLINE sectionFromPlanRows #-}

restrictPlanSection ::
  forall plan.
  MatchPlan plan =>
  RowRestrictionProgram ->
  RowBlockIdentity ->
  PlanSection plan ->
  Either RowOperationError (PlanSection plan)
restrictPlanSection =
  restrictSection @(PlanSection plan)
{-# INLINE restrictPlanSection #-}

planSectionRows ::
  forall plan.
  MatchPlan plan =>
  PlanSection plan ->
  RowBlock 'Canonical
planSectionRows =
  sectionRows @(PlanSection plan)
{-# INLINE planSectionRows #-}

planAtomSpecId ::
  forall plan.
  MatchPlan plan =>
  PlanAtom plan ->
  AtomId
planAtomSpecId =
  atomSpecId @plan
{-# INLINE planAtomSpecId #-}

planAtomColumns ::
  forall plan.
  MatchPlan plan =>
  PlanAtom plan ->
  Schema
planAtomColumns =
  atomColumns @plan
{-# INLINE planAtomColumns #-}
