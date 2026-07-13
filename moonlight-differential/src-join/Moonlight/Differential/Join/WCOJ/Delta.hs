{-# LANGUAGE BangPatterns #-}

-- | Delta-restricted worst-case-optimal join: descend the full join but keep
-- only assignments supported by at least one dirty row; feasible sets are
-- persistent values threaded through the recursion, so backtracking is free
-- and no rollback machinery exists to corrupt. Source coherence (value index
-- agrees with the row sets) is the caller's obligation, stated here once.
module Moonlight.Differential.Join.WCOJ.Delta
  ( DeltaJoinSource (..),
    deltaJoinSourceSlots,
    DeltaJoinConstraint (..),
    deltaJoinConstraintSlots,
    DeltaJoinProblem,
    mkDeltaJoinProblem,
    deltaProblemSlots,
    deltaProblemSources,
    deltaProblemConstraints,
    foldDeltaWCOJ,
    deltaWCOJLeaves,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Kind
  ( Type,
  )
import Data.Maybe
  ( fromMaybe,
  )
import Data.Vector
  ( Vector,
  )
import Data.Vector qualified as Vector
import Moonlight.Differential.Index.RowId
  ( rowIdInt,
  )
import Moonlight.Differential.Index.RowIdSet
  ( RowIdSet,
  )
import Moonlight.Differential.Index.RowSet
  ( RowSet,
    RowSetRestriction (..),
    emptyRowSet,
    rowSetFoldl',
    rowSetIntersection,
    rowSetIntersectionWithRowIdSetChanged,
    rowSetIntersectsRowIdSet,
    rowSetNull,
    rowSetSize,
  )
import Moonlight.Differential.Join.WCOJ
  ( Env,
    Slot,
  )

type DeltaJoinSource :: Type
data DeltaJoinSource = DeltaJoinSource
  { deltaSourceRows :: !RowSet,
    deltaSourceDirtyRows :: !RowSet,
    deltaSourceValueIndex :: !(IntMap (IntMap RowIdSet)),
    deltaSourceValueAt :: !(Slot -> Int -> Maybe Int)
  }

deltaJoinSourceSlots :: DeltaJoinSource -> IntSet
deltaJoinSourceSlots =
  IntMap.keysSet . deltaSourceValueIndex
{-# INLINE deltaJoinSourceSlots #-}

type DeltaJoinConstraint :: Type
data DeltaJoinConstraint = DeltaJoinConstraint
  { deltaConstraintRows :: !RowSet,
    deltaConstraintValueIndex :: !(IntMap (IntMap RowIdSet))
  }

deltaJoinConstraintSlots :: DeltaJoinConstraint -> IntSet
deltaJoinConstraintSlots =
  IntMap.keysSet . deltaConstraintValueIndex
{-# INLINE deltaJoinConstraintSlots #-}

type DeltaJoinProblem :: Type
data DeltaJoinProblem = DeltaJoinProblem
  { deltaProblemSlots :: ![Slot],
    deltaProblemSources :: !(Vector DeltaJoinSource),
    deltaProblemConstraints :: !(Vector DeltaJoinConstraint),
    deltaProblemSourcesBySlot :: !(IntMap IntSet),
    deltaProblemConstraintsBySlot :: !(IntMap IntSet),
    deltaProblemStaticRank :: !(IntMap Int)
  }

mkDeltaJoinProblem ::
  [Slot] ->
  [DeltaJoinSource] ->
  [DeltaJoinConstraint] ->
  DeltaJoinProblem
mkDeltaJoinProblem slots sources constraints =
  DeltaJoinProblem
    { deltaProblemSlots = slots,
      deltaProblemSources = Vector.fromList sources,
      deltaProblemConstraints = Vector.fromList constraints,
      deltaProblemSourcesBySlot =
        membersBySlot (fmap deltaJoinSourceSlots sources),
      deltaProblemConstraintsBySlot =
        membersBySlot (fmap deltaJoinConstraintSlots constraints),
      deltaProblemStaticRank =
        IntMap.fromList (zip slots [0 ..])
    }

membersBySlot :: [IntSet] -> IntMap IntSet
membersBySlot slotSets =
  foldl' insertMember IntMap.empty (zip [0 ..] slotSets)
  where
    insertMember bySlot (memberId, memberSlots) =
      IntSet.foldl'
        ( \acc slot ->
            IntMap.insertWith IntSet.union slot (IntSet.singleton memberId) acc
        )
        bySlot
        memberSlots

type DeltaCursor :: Type
data DeltaCursor = DeltaCursor
  { dcEnv :: !(Env Int),
    dcFull :: !(IntMap RowSet),
    dcDirty :: !(IntMap RowSet),
    dcConstraintRows :: !(IntMap RowSet),
    dcDirtyLive :: !Int
  }

foldDeltaWCOJ ::
  DeltaJoinProblem ->
  (acc -> Env Int -> IntMap RowSet -> acc) ->
  acc ->
  acc
foldDeltaWCOJ problem leaf initial
  | Vector.any (rowSetNull . deltaSourceRows) (deltaProblemSources problem) =
      initial
  | dcDirtyLive initialCursor == 0 =
      initial
  | otherwise =
      go initialCursor (IntSet.fromList (deltaProblemSlots problem)) initial
  where
    initialCursor =
      DeltaCursor
        { dcEnv = IntMap.empty,
          dcFull =
            IntMap.fromList
              (Vector.toList (Vector.imap (\ix src -> (ix, deltaSourceRows src)) (deltaProblemSources problem))),
          dcDirty = initialDirty,
          dcConstraintRows =
            IntMap.fromList
              (Vector.toList (Vector.imap (\ix c -> (ix, deltaConstraintRows c)) (deltaProblemConstraints problem))),
          dcDirtyLive = IntMap.foldl' (\ !live rows -> if rowSetNull rows then live else live + 1) 0 initialDirty
        }

    initialDirty =
      IntMap.fromList
        ( Vector.toList
            ( Vector.imap
                (\ix src -> (ix, rowSetIntersection (deltaSourceRows src) (deltaSourceDirtyRows src)))
                (deltaProblemSources problem)
            )
        )

    go cursor unbound !acc =
      case chooseDeltaSlot problem cursor unbound of
        Nothing ->
          leaf acc (dcEnv cursor) (dcFull cursor)
        Just (slot, domain)
          | IntSet.null domain ->
              acc
          | otherwise ->
              let !unboundNext = IntSet.delete slot unbound
               in IntSet.foldl'
                    ( \ !accSoFar value ->
                        case bindDeltaValue problem cursor slot value of
                          Nothing ->
                            accSoFar
                          Just boundCursor ->
                            go boundCursor unboundNext accSoFar
                    )
                    acc
                    domain
{-# INLINABLE foldDeltaWCOJ #-}

deltaWCOJLeaves ::
  DeltaJoinProblem ->
  [(Env Int, IntMap RowSet)]
deltaWCOJLeaves problem =
  reverse
    (foldDeltaWCOJ problem (\acc env supports -> (env, supports) : acc) [])
{-# INLINABLE deltaWCOJLeaves #-}

bindDeltaValue ::
  DeltaJoinProblem ->
  DeltaCursor ->
  Slot ->
  Int ->
  Maybe DeltaCursor
bindDeltaValue problem cursor slot value = do
  afterConstraints <-
    foldM
      bindConstraint
      cursor {dcEnv = IntMap.insert slot value (dcEnv cursor)}
      (IntSet.toAscList (IntMap.findWithDefault IntSet.empty slot (deltaProblemConstraintsBySlot problem)))
  afterSources <-
    foldM
      bindSource
      afterConstraints
      (IntSet.toAscList (IntMap.findWithDefault IntSet.empty slot (deltaProblemSourcesBySlot problem)))
  if dcDirtyLive afterSources > 0
    then Just afterSources
    else Nothing
  where
    bindConstraint bound constraintId = do
      let constraint =
            Vector.unsafeIndex (deltaProblemConstraints problem) constraintId
          rows =
            IntMap.findWithDefault (deltaConstraintRows constraint) constraintId (dcConstraintRows bound)
      case restrictByValue (deltaConstraintValueIndex constraint) slot value rows of
        RowSetRestrictionEmpty ->
          Nothing
        RowSetRestrictionUnchanged ->
          Just bound
        RowSetRestrictionChanged restricted ->
          Just bound {dcConstraintRows = IntMap.insert constraintId restricted (dcConstraintRows bound)}

    bindSource bound sourceId = do
      let source =
            Vector.unsafeIndex (deltaProblemSources problem) sourceId
          fullRows =
            IntMap.findWithDefault (deltaSourceRows source) sourceId (dcFull bound)
      afterFull <-
        case restrictByValue (deltaSourceValueIndex source) slot value fullRows of
          RowSetRestrictionEmpty ->
            Nothing
          RowSetRestrictionUnchanged ->
            Just bound
          RowSetRestrictionChanged restricted ->
            Just bound {dcFull = IntMap.insert sourceId restricted (dcFull bound)}
      pure (restrictDirtySource source sourceId afterFull)

    restrictDirtySource source sourceId bound =
      let dirtyRows =
            IntMap.findWithDefault (deltaSourceDirtyRows source) sourceId (dcDirty bound)
       in if rowSetNull dirtyRows
            then bound
            else case restrictByValue (deltaSourceValueIndex source) slot value dirtyRows of
              RowSetRestrictionEmpty ->
                bound
                  { dcDirty = IntMap.insert sourceId emptyRowSet (dcDirty bound),
                    dcDirtyLive = dcDirtyLive bound - 1
                  }
              RowSetRestrictionUnchanged ->
                bound
              RowSetRestrictionChanged restricted ->
                bound {dcDirty = IntMap.insert sourceId restricted (dcDirty bound)}

restrictByValue ::
  IntMap (IntMap RowIdSet) ->
  Slot ->
  Int ->
  RowSet ->
  RowSetRestriction
restrictByValue valueIndex slot value rows =
  case IntMap.lookup slot valueIndex >>= IntMap.lookup value of
    Nothing ->
      RowSetRestrictionEmpty
    Just bucket ->
      rowSetIntersectionWithRowIdSetChanged bucket rows
{-# INLINE restrictByValue #-}

chooseDeltaSlot ::
  DeltaJoinProblem ->
  DeltaCursor ->
  IntSet ->
  Maybe (Slot, IntSet)
chooseDeltaSlot problem cursor unbound =
  go (IntSet.toAscList unbound) Nothing
  where
    go remaining best =
      case remaining of
        [] ->
          fmap (\(slot, domain, _) -> (slot, domain)) best
        slot : rest ->
          let !fullDomain = fullCandidateDomain problem cursor slot
              !incidence = dirtySourceIncidence problem cursor slot
              !domain = dirtyRestrictedDomain problem cursor slot incidence fullDomain
              !candidate = (slot, domain, slotScore problem cursor slot domain incidence)
           in if IntSet.null domain
                then Just (slot, domain)
                else go rest (Just (betterCandidate best candidate))

    betterCandidate ::
      Maybe (Slot, IntSet, (Int, Int, Int, Int, Int)) ->
      (Slot, IntSet, (Int, Int, Int, Int, Int)) ->
      (Slot, IntSet, (Int, Int, Int, Int, Int))
    betterCandidate maybeBest candidate@(_, _, candidateScore) =
      case maybeBest of
        Nothing ->
          candidate
        Just best@(_, _, bestScore)
          | candidateScore < bestScore ->
              candidate
          | otherwise ->
              best

slotScore ::
  DeltaJoinProblem ->
  DeltaCursor ->
  Slot ->
  IntSet ->
  Int ->
  (Int, Int, Int, Int, Int)
slotScore problem _cursor slot domain incidence =
  ( IntSet.size domain,
    negate incidence,
    negate (IntSet.size (IntMap.findWithDefault IntSet.empty slot (deltaProblemSourcesBySlot problem))),
    IntMap.findWithDefault maxBound slot (deltaProblemStaticRank problem),
    slot
  )
{-# INLINE slotScore #-}

fullCandidateDomain ::
  DeltaJoinProblem ->
  DeltaCursor ->
  Slot ->
  IntSet
fullCandidateDomain problem cursor slot =
  fromMaybe IntSet.empty (combine sourceDomain constraintDomain)
  where
    sourceDomain =
      IntSet.foldl'
        ( \acc sourceId ->
            let source = Vector.unsafeIndex (deltaProblemSources problem) sourceId
                rows = IntMap.findWithDefault (deltaSourceRows source) sourceId (dcFull cursor)
             in intersectMaybe acc (sourceSlotValues source slot rows)
        )
        Nothing
        (IntMap.findWithDefault IntSet.empty slot (deltaProblemSourcesBySlot problem))

    constraintDomain =
      IntSet.foldl'
        ( \acc constraintId ->
            let constraint = Vector.unsafeIndex (deltaProblemConstraints problem) constraintId
                rows = IntMap.findWithDefault (deltaConstraintRows constraint) constraintId (dcConstraintRows cursor)
             in intersectMaybe acc (bucketSlotValues (deltaConstraintValueIndex constraint) slot rows)
        )
        Nothing
        (IntMap.findWithDefault IntSet.empty slot (deltaProblemConstraintsBySlot problem))

    combine left right =
      case (left, right) of
        (Nothing, other) -> other
        (other, Nothing) -> other
        (Just leftDomain, Just rightDomain) -> Just (IntSet.intersection leftDomain rightDomain)

    intersectMaybe acc domain =
      case acc of
        Nothing -> Just domain
        Just existing -> Just (IntSet.intersection existing domain)

dirtySourceIncidence ::
  DeltaJoinProblem ->
  DeltaCursor ->
  Slot ->
  Int
dirtySourceIncidence problem cursor slot =
  IntSet.foldl'
    ( \ !acc sourceId ->
        if rowSetNull (IntMap.findWithDefault (deltaSourceDirtyRows (Vector.unsafeIndex (deltaProblemSources problem) sourceId)) sourceId (dcDirty cursor))
          then acc
          else acc + 1
    )
    0
    (IntMap.findWithDefault IntSet.empty slot (deltaProblemSourcesBySlot problem))
{-# INLINE dirtySourceIncidence #-}

dirtyRestrictedDomain ::
  DeltaJoinProblem ->
  DeltaCursor ->
  Slot ->
  Int ->
  IntSet ->
  IntSet
dirtyRestrictedDomain problem cursor slot incidence fullDomain
  | dcDirtyLive cursor > 0 && incidence == dcDirtyLive cursor && incidence > 0 =
      IntSet.intersection fullDomain dirtyUnionDomain
  | otherwise =
      fullDomain
  where
    dirtyUnionDomain =
      IntSet.foldl'
        ( \acc sourceId ->
            let source = Vector.unsafeIndex (deltaProblemSources problem) sourceId
                rows = IntMap.findWithDefault (deltaSourceDirtyRows source) sourceId (dcDirty cursor)
             in if rowSetNull rows
                  then acc
                  else IntSet.union acc (sourceSlotValues source slot rows)
        )
        IntSet.empty
        (IntMap.findWithDefault IntSet.empty slot (deltaProblemSourcesBySlot problem))

sourceSlotValues ::
  DeltaJoinSource ->
  Slot ->
  RowSet ->
  IntSet
sourceSlotValues source slot rows =
  case IntMap.lookup slot (deltaSourceValueIndex source) of
    Nothing ->
      IntSet.empty
    Just byRep
      | rowSetSize rows <= 64 || rowSetSize rows * 4 <= IntMap.size byRep ->
          rowSetFoldl'
            ( \acc rowId ->
                maybe acc (`IntSet.insert` acc) (deltaSourceValueAt source slot (rowIdInt rowId))
            )
            IntSet.empty
            rows
      | otherwise ->
          scanBuckets byRep rows
{-# INLINE sourceSlotValues #-}

bucketSlotValues ::
  IntMap (IntMap RowIdSet) ->
  Slot ->
  RowSet ->
  IntSet
bucketSlotValues valueIndex slot rows =
  maybe IntSet.empty (`scanBuckets` rows) (IntMap.lookup slot valueIndex)
{-# INLINE bucketSlotValues #-}

scanBuckets ::
  IntMap RowIdSet ->
  RowSet ->
  IntSet
scanBuckets byRep rows =
  IntMap.foldlWithKey'
    ( \acc value bucket ->
        if rowSetIntersectsRowIdSet bucket rows
          then IntSet.insert value acc
          else acc
    )
    IntSet.empty
    byRep
{-# INLINE scanBuckets #-}
