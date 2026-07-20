module Moonlight.Differential.Operator.Join
  ( joinIndexed,
    indexedDeltaJoin,
    foldDeltaJoin,
    arrangedKeyZSet,
    indexedDeltaJoinArranged,
  )
where

import Moonlight.Core (AdditiveGroup)
import Moonlight.Algebra
  ( MultiplicativeMonoid (mul), Semiring,
  )
import Moonlight.Differential.Algebra.ZSet
  ( IndexedZSet,
    ZSet,
    indexedZSetFold,
    indexedZSetLookup,
    zsetEmpty,
    zsetFold,
    zsetInsert,
  )
import Moonlight.Differential.Arrangement
  ( Arrangement,
    cursorAt,
    foldSliceThrough,
  )
import Moonlight.Differential.Batch
  ( Batch,
    foldBatch,
  )
import Moonlight.Differential.Cursor
  ( foldCursorWithTime,
  )

-- | Bilinear differential join.  The law is linearity in both inputs:
-- @delta (left `join` right) = deltaLeft `join` right
-- <> left `join` deltaRight <> deltaLeft `join` deltaRight@.
joinIndexed ::
  (Ord key, Ord a, Ord b, Eq weight, AdditiveGroup weight, Semiring weight) =>
  IndexedZSet key a weight ->
  IndexedZSet key b weight ->
  ZSet (key, a, b) weight
joinIndexed leftIndex rightIndex =
  indexedZSetFold
    ( \acc key leftGroup ->
        maybe
          acc
          (joinKeyGroups acc key leftGroup)
          (indexedZSetLookup key rightIndex)
    )
    zsetEmpty
    leftIndex

joinKeyGroups ::
  (Ord key, Ord a, Ord b, Eq weight, AdditiveGroup weight, Semiring weight) =>
  ZSet (key, a, b) weight ->
  key ->
  ZSet a weight ->
  ZSet b weight ->
  ZSet (key, a, b) weight
joinKeyGroups initial key leftGroup rightGroup =
  zsetFold
    ( \accLeft leftValue leftWeight ->
        zsetFold
          ( \accRight rightValue rightWeight ->
              zsetInsert
                (key, leftValue, rightValue)
                (mul leftWeight rightWeight)
                accRight
          )
          accLeft
          rightGroup
    )
    initial
    leftGroup

indexedDeltaJoin ::
  (Ord key, Ord a, Ord b, Eq weight, AdditiveGroup weight, Semiring weight) =>
  IndexedZSet key a weight ->
  IndexedZSet key a weight ->
  IndexedZSet key b weight ->
  IndexedZSet key b weight ->
  ZSet (key, a, b) weight
indexedDeltaJoin integratedLeft deltaLeft integratedRight deltaRight =
  -- DBSP-style ordered delta decomposition: the changed left input joins the
  -- current right input, while the changed right input joins the delayed left
  -- input.  This is extensionally equal to the bilinear expansion but avoids a
  -- separate delta-delta join surface.
  joinIndexed deltaLeft (integratedRight <> deltaRight)
    <> joinIndexed integratedLeft deltaRight

-- | Join a delta batch against the prefix view of an arranged relation.
--
-- The batch side supplies the output time. The arranged side is interpreted as
-- the integral of the other input: for each delta cell at @t@, only arranged
-- cells with time @<= t@ participate.
foldDeltaJoin ::
  ( Ord time,
    Ord key,
    Semiring weight
  ) =>
  (key -> leftVal -> rightVal -> Maybe (outKey, outVal)) ->
  (acc -> time -> outKey -> outVal -> weight -> acc) ->
  acc ->
  Batch time key leftVal weight ->
  Arrangement time key rightVal weight ->
  acc
foldDeltaJoin project step initial delta arrangement =
  foldBatch collectDeltaRow initial delta
  where
    collectDeltaRow acc time key leftVal leftWeight =
      foldSliceThrough
        time
        key
        (collectArrangementRow time key leftVal leftWeight)
        acc
        arrangement

    collectArrangementRow deltaTime key leftVal leftWeight acc _arrangementTime rightVal rightWeight =
      case project key leftVal rightVal of
        Nothing ->
          acc
        Just (outKey, outVal) ->
          step acc deltaTime outKey outVal (mul leftWeight rightWeight)
{-# INLINE foldDeltaJoin #-}

arrangedKeyZSet ::
  (Ord key, Ord a, Eq weight, AdditiveGroup weight) =>
  key ->
  Arrangement time key a weight ->
  ZSet a weight
arrangedKeyZSet key arrangement =
  foldCursorWithTime
    (\acc _time value weight -> zsetInsert value weight acc)
    zsetEmpty
    (cursorAt key arrangement)

indexedDeltaJoinArranged ::
  (Ord key, Ord a, Ord b, Eq weight, AdditiveGroup weight, Semiring weight) =>
  Arrangement time key a weight ->
  IndexedZSet key b weight ->
  ZSet (key, a, b) weight
indexedDeltaJoinArranged arrangement deltaRight =
  indexedZSetFold
    ( \acc key rightGroup ->
        joinKeyGroups acc key (arrangedKeyZSet key arrangement) rightGroup
    )
    zsetEmpty
    deltaRight
