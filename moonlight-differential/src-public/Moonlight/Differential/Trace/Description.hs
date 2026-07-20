{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Trace.Description
  ( TraceDescription,
    TraceDescriptionReadError (..),
    TraceDescriptionAdvanceError (..),
    traceDescription,
    traceDescriptionLower,
    traceDescriptionUpper,
    traceDescriptionSince,
    traceDescriptionAdvanceSince,
    traceDescriptionAdvanceUpper,
    traceDescriptionReadAt,
    traceDescriptionReadAfter,
    traceDescriptionTimeCompactable,
    frontierAtOrBefore,
    upperFrontierAtOrBefore,
    mergeUpperFrontier,
  )
where

import Data.Foldable qualified as Foldable
import Data.Kind
  ( Type,
  )
import Moonlight.Core
  ( PartialOrder (..),
  )
import Moonlight.Delta.Frontier
  ( Frontier,
    UpperFrontier,
    frontierPoints,
    insertUpperFrontierPoint,
    singletonUpperFrontier,
    upperFrontierPoints,
  )

type TraceDescription :: Type -> Type
data TraceDescription time = TraceDescription
  { tdLower :: !(Frontier time),
    tdUpper :: !(UpperFrontier time),
    tdSince :: !(UpperFrontier time)
  }
  deriving stock (Eq, Ord, Show)

type TraceDescriptionReadError :: Type -> Type
data TraceDescriptionReadError time
  = TraceReadBeforeSince !time !(UpperFrontier time)
  | TraceReadBeyondUpper !time !(UpperFrontier time)
  deriving stock (Eq, Ord, Show)

type TraceDescriptionAdvanceError :: Type -> Type
data TraceDescriptionAdvanceError time = TraceDescriptionFrontierRegression
  { traceDescriptionCurrent :: !(UpperFrontier time),
    traceDescriptionRequested :: !(UpperFrontier time)
  }
  deriving stock (Eq, Ord, Show)

traceDescription ::
  Frontier time ->
  UpperFrontier time ->
  UpperFrontier time ->
  TraceDescription time
traceDescription lower upper since =
  TraceDescription
    { tdLower = lower,
      tdUpper = upper,
      tdSince = since
    }
{-# INLINE traceDescription #-}

traceDescriptionLower :: TraceDescription time -> Frontier time
traceDescriptionLower =
  tdLower
{-# INLINE traceDescriptionLower #-}

traceDescriptionUpper :: TraceDescription time -> UpperFrontier time
traceDescriptionUpper =
  tdUpper
{-# INLINE traceDescriptionUpper #-}

traceDescriptionSince :: TraceDescription time -> UpperFrontier time
traceDescriptionSince =
  tdSince
{-# INLINE traceDescriptionSince #-}

traceDescriptionAdvanceSince ::
  PartialOrder time =>
  UpperFrontier time ->
  TraceDescription time ->
  Either (TraceDescriptionAdvanceError time) (TraceDescription time)
traceDescriptionAdvanceSince since description
  | upperFrontierAtOrBefore (tdSince description) since =
      Right description {tdSince = since}
  | otherwise =
      Left
        TraceDescriptionFrontierRegression
          { traceDescriptionCurrent = tdSince description,
            traceDescriptionRequested = since
          }
{-# INLINE traceDescriptionAdvanceSince #-}

traceDescriptionAdvanceUpper ::
  PartialOrder time =>
  UpperFrontier time ->
  TraceDescription time ->
  Either (TraceDescriptionAdvanceError time) (TraceDescription time)
traceDescriptionAdvanceUpper upper description
  | upperFrontierAtOrBefore (tdUpper description) upper =
      Right description {tdUpper = upper}
  | otherwise =
      Left
        TraceDescriptionFrontierRegression
          { traceDescriptionCurrent = tdUpper description,
            traceDescriptionRequested = upper
          }
{-# INLINE traceDescriptionAdvanceUpper #-}

traceDescriptionReadAt ::
  PartialOrder time =>
  time ->
  TraceDescription time ->
  Either (TraceDescriptionReadError time) ()
traceDescriptionReadAt time description
  | singletonUpperFrontier time `upperFrontierAtOrBefore` tdSince description =
      Left (TraceReadBeforeSince time (tdSince description))
  | not (singletonUpperFrontier time `upperFrontierAtOrBefore` tdUpper description) =
      Left (TraceReadBeyondUpper time (tdUpper description))
  | otherwise =
      Right ()
{-# INLINE traceDescriptionReadAt #-}

traceDescriptionReadAfter ::
  PartialOrder time =>
  time ->
  TraceDescription time ->
  Either (TraceDescriptionReadError time) ()
traceDescriptionReadAfter time description
  | not (tdSince description `upperFrontierAtOrBefore` singletonUpperFrontier time) =
      Left (TraceReadBeforeSince time (tdSince description))
  | not (singletonUpperFrontier time `upperFrontierAtOrBefore` tdUpper description) =
      Left (TraceReadBeyondUpper time (tdUpper description))
  | otherwise =
      Right ()
{-# INLINE traceDescriptionReadAfter #-}

traceDescriptionTimeCompactable ::
  PartialOrder time =>
  time ->
  TraceDescription time ->
  Bool
traceDescriptionTimeCompactable time description =
  singletonUpperFrontier time `upperFrontierAtOrBefore` tdSince description
{-# INLINE traceDescriptionTimeCompactable #-}

frontierAtOrBefore ::
  PartialOrder time =>
  Frontier time ->
  Frontier time ->
  Bool
frontierAtOrBefore left right =
  case (frontierPoints left, frontierPoints right) of
    ([], _) ->
      True
    (_, []) ->
      False
    ([leftTime], [rightTime]) ->
      leq leftTime rightTime
    (leftTimes, rightTimes) ->
      Foldable.all
        (\leftTime -> Foldable.any (\rightTime -> leq leftTime rightTime) rightTimes)
        leftTimes
{-# INLINE frontierAtOrBefore #-}

upperFrontierAtOrBefore ::
  PartialOrder time =>
  UpperFrontier time ->
  UpperFrontier time ->
  Bool
upperFrontierAtOrBefore left right =
  case (upperFrontierPoints left, upperFrontierPoints right) of
    ([], _) ->
      True
    (_, []) ->
      False
    ([leftTime], [rightTime]) ->
      leq leftTime rightTime
    (leftTimes, rightTimes) ->
      Foldable.all
        (\leftTime -> Foldable.any (\rightTime -> leq leftTime rightTime) rightTimes)
        leftTimes
{-# INLINE upperFrontierAtOrBefore #-}

mergeUpperFrontier ::
  (Ord time, PartialOrder time) =>
  UpperFrontier time ->
  UpperFrontier time ->
  UpperFrontier time
mergeUpperFrontier left right =
  Foldable.foldl' (flip insertUpperFrontierPoint) left (upperFrontierPoints right)
{-# INLINE mergeUpperFrontier #-}
