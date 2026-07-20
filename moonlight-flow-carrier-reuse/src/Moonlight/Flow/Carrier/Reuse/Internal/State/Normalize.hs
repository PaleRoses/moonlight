{-# LANGUAGE DataKinds #-}

module Moonlight.Flow.Carrier.Reuse.Internal.State.Normalize
  ( planReuseSaturationBudget,
    normalizeRequestedFactorShape,
    normalizeFactorShapeForReuse,
    normalizeFactorShapeForReuseLoop,
    rekeyPlanReuseState,
    normalizeExistingEntriesFixedPoint,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.List qualified as List
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Shape
  ( emptySubsumptionIndex,
    insertEntryIndex,
    subsumptionIndexEntries,
    subsumptionIndexSize,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Shape
  ( RequestedFactorShape (..),
    SubsumptionEntry (..),
    SubsumptionRegistrationError (..),
  )
import Moonlight.Flow.Carrier.Reuse.Internal.State.Types
  ( PlanReuseState (..),
  )
import Moonlight.Flow.Carrier.Reuse.Types
  ( ReuseValidityRequest,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Plan.Rewrite
  ( FactorShapeNormalization,
    PlanSaturationState,
    SaturationBudget (..),
    fsnKey,
    normalizeFactorShapeWithState,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( PlanShape,
    PlanStage (..),
  )

planReuseSaturationBudget :: SaturationBudget
planReuseSaturationBudget =
  SaturationBudget
    { sbMaxIterations = 8,
      sbMaxNodes = maxBound
    }

normalizeRequestedFactorShape ::
  Ord ctx =>
  Ord prop =>
  CarrierAddr ctx Carrier prop ->
  PlanShape 'FactorShape ->
  RuntimeBoundary ->
  ReuseValidityRequest ->
  PlanReuseState ctx prop ->
  Either SubsumptionRegistrationError (PlanReuseState ctx prop, RequestedFactorShape ctx prop)
normalizeRequestedFactorShape targetCarrier rawShape boundary validity state0 = do
  (state1, normalization) <-
    normalizeFactorShapeForReuse rawShape state0
  pure
    ( state1,
      RequestedFactorShape
        { rfsTargetCarrier = targetCarrier,
          rfsShape = rawShape,
          rfsShapeKey = fsnKey normalization,
          rfsShapeNormalization = normalization,
          rfsBoundary = boundary,
          rfsValidity = validity
        }
    )

normalizeFactorShapeForReuse ::
  Ord ctx =>
  Ord prop =>
  PlanShape 'FactorShape ->
  PlanReuseState ctx prop ->
  Either SubsumptionRegistrationError (PlanReuseState ctx prop, FactorShapeNormalization)
normalizeFactorShapeForReuse rawShape state0 =
  normalizeFactorShapeForReuseLoop
    (subsumptionIndexSize (prsSubsumptionIndex state0) + 2)
    rawShape
    state0

normalizeFactorShapeForReuseLoop ::
  Ord ctx =>
  Ord prop =>
  Int ->
  PlanShape 'FactorShape ->
  PlanReuseState ctx prop ->
  Either SubsumptionRegistrationError (PlanReuseState ctx prop, FactorShapeNormalization)
normalizeFactorShapeForReuseLoop remainingPasses rawShape state0
  | remainingPasses <= 0 =
      Left (SubsumptionRegistrationNormalizationUnstable 0)
  | otherwise = do
      (saturation1, normalization) <-
        first SubsumptionRegistrationPlanSaturationError $
          normalizeFactorShapeWithState
            planReuseSaturationBudget
            (prsPlanSaturationState state0)
            rawShape
      if saturation1 == prsPlanSaturationState state0
        then
          pure
            ( state0 {prsPlanSaturationState = saturation1},
              normalization
            )
        else do
          state1 <-
            rekeyPlanReuseState saturation1 state0
          if prsPlanSaturationState state1 == saturation1
            then pure (state1, normalization)
            else normalizeFactorShapeForReuseLoop (remainingPasses - 1) rawShape state1

rekeyPlanReuseState ::
  Ord ctx =>
  Ord prop =>
  PlanSaturationState ->
  PlanReuseState ctx prop ->
  Either SubsumptionRegistrationError (PlanReuseState ctx prop)
rekeyPlanReuseState saturation0 state0 = do
  let entries0 =
        subsumptionIndexEntries (prsSubsumptionIndex state0)
      passLimit =
        max 1 (length entries0 + 1)
  (saturation1, entries1) <-
    normalizeExistingEntriesFixedPoint passLimit saturation0 entries0
  pure
    ( state0
        { prsPlanSaturationState = saturation1,
          prsSubsumptionIndex =
            List.foldl'
              (flip insertEntryIndex)
              emptySubsumptionIndex
              entries1
        }
    )

normalizeExistingEntriesFixedPoint ::
  Int ->
  PlanSaturationState ->
  [SubsumptionEntry ctx prop] ->
  Either SubsumptionRegistrationError (PlanSaturationState, [SubsumptionEntry ctx prop])
normalizeExistingEntriesFixedPoint remainingPasses saturation entries
  | remainingPasses <= 0 =
      Left (SubsumptionRegistrationNormalizationUnstable 0)
  | otherwise = do
      (saturationNext, entriesNextRev) <-
        List.foldl'
          normalizeOne
          (Right (saturation, []))
          entries
      let entriesNext =
            reverse entriesNextRev
      if saturationNext == saturation
        then Right (saturationNext, entriesNext)
        else normalizeExistingEntriesFixedPoint (remainingPasses - 1) saturationNext entries
  where
    normalizeOne ::
      Either SubsumptionRegistrationError (PlanSaturationState, [SubsumptionEntry ctx prop]) ->
      SubsumptionEntry ctx prop ->
      Either SubsumptionRegistrationError (PlanSaturationState, [SubsumptionEntry ctx prop])
    normalizeOne eitherAcc entry = do
      (saturationAcc, normalizedEntries) <- eitherAcc
      (saturation', normalization) <-
        first SubsumptionRegistrationPlanSaturationError $
          normalizeFactorShapeWithState planReuseSaturationBudget saturationAcc (seShape entry)
      pure
        ( saturation',
          entry
            { seShapeKey = fsnKey normalization,
              seShapeNormalization = normalization
            }
            : normalizedEntries
        )
