{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Join.WCOJ
  ( Slot,
    Env,
    Domain,
    domainEmpty,
    domainSingleton,
    domainFromList,
    domainFromListPreservingOrder,
    domainFromHashSet,
    domainToList,
    domainToHashSet,
    domainNull,
    domainSize,
    domainFilter,
    IntBinaryRelationIndex,
    IntBinaryConstraintIndex,
    IntIndexedJoinProblem,
    intBinaryRelationIndexFromList,
    intBinaryConstraintIndex,
    intIndexedJoinProblem,
    intBinaryRelationMember,
    intIndexedJoinProblemWeight,
    intIndexedJoinCandidateSet,
    intIndexedJoinCount,
    intIndexedJoinPropose,
    intIndexedJoinValidate,
    intIndexedJoinAlgebra,
    foldIntIndexedAdaptiveJoin,
    JoinAlgebra (..),
    foldGenericJoin,
    foldAdaptiveJoin,
    foldAdaptiveJoinWithFilter,
    existsJoin,
    adaptiveJoin,
    chooseSmallestSlot,
  )
where

import Data.HashSet
  ( HashSet,
  )
import Data.HashSet qualified as HashSet
import Data.Hashable
  ( Hashable,
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
  ( mapMaybe,
  )
import Data.Monoid
  ( Endo (..),
    appEndo,
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Vector
  ( Vector,
  )
import Data.Vector qualified as Vector

type Slot = Int

type Env value = IntMap value

type Domain :: Type -> Type
newtype Domain value = Domain
  { domainValues :: Vector value
  }
  deriving stock (Eq, Ord, Show)

domainEmpty :: Domain value
domainEmpty =
  Domain Vector.empty

domainSingleton :: value -> Domain value
domainSingleton =
  Domain . Vector.singleton

domainFromList ::
  Hashable value =>
  [value] ->
  Domain value
domainFromList =
  domainFromHashSet . HashSet.fromList

domainFromListPreservingOrder ::
  [value] ->
  Domain value
domainFromListPreservingOrder =
  Domain . Vector.fromList

domainFromHashSet ::
  HashSet value ->
  Domain value
domainFromHashSet =
  Domain . Vector.fromList . HashSet.toList

domainToList :: Domain value -> [value]
domainToList (Domain values) =
  Vector.toList values

domainToHashSet ::
  Hashable value =>
  Domain value ->
  HashSet value
domainToHashSet (Domain values) =
  HashSet.fromList (Vector.toList values)

domainNull :: Domain value -> Bool
domainNull (Domain values) =
  Vector.null values

domainSize :: Domain value -> Int
domainSize (Domain values) =
  Vector.length values

domainFilter ::
  (value -> Bool) ->
  Domain value ->
  Domain value
domainFilter keep (Domain values) =
  Domain (Vector.filter keep values)

type IntBinaryRelationIndex :: Type
data IntBinaryRelationIndex = IntBinaryRelationIndex
  { intBinaryRelationForward :: !(IntMap IntSet),
    intBinaryRelationBackward :: !(IntMap IntSet),
    intBinaryRelationPairs :: !(Set (Int, Int))
  }
  deriving stock (Eq, Ord, Show)

type IntBinaryConstraintIndex :: Type
data IntBinaryConstraintIndex = IntBinaryConstraintIndex
  { intBinaryConstraintLeftSlot :: {-# UNPACK #-} !Slot,
    intBinaryConstraintRightSlot :: {-# UNPACK #-} !Slot,
    intBinaryConstraintRelation :: !IntBinaryRelationIndex
  }
  deriving stock (Eq, Ord, Show)

type IntIndexedJoinProblem :: Type
data IntIndexedJoinProblem = IntIndexedJoinProblem
  { intIndexedJoinUniverse :: !IntSet,
    intIndexedJoinConstraints :: ![IntBinaryConstraintIndex]
  }
  deriving stock (Eq, Ord, Show)

intBinaryRelationIndexFromList :: [(Int, Int)] -> IntBinaryRelationIndex
intBinaryRelationIndexFromList pairs =
  IntBinaryRelationIndex
    { intBinaryRelationForward = foldl' collectForward IntMap.empty pairs,
      intBinaryRelationBackward = foldl' collectBackward IntMap.empty pairs,
      intBinaryRelationPairs = Set.fromList pairs
    }
  where
    collectForward index (left, right) =
      IntMap.insertWith IntSet.union left (IntSet.singleton right) index

    collectBackward index (left, right) =
      IntMap.insertWith IntSet.union right (IntSet.singleton left) index

intBinaryConstraintIndex :: Slot -> Slot -> IntBinaryRelationIndex -> IntBinaryConstraintIndex
intBinaryConstraintIndex leftSlot rightSlot relation =
  IntBinaryConstraintIndex
    { intBinaryConstraintLeftSlot = leftSlot,
      intBinaryConstraintRightSlot = rightSlot,
      intBinaryConstraintRelation = relation
    }

intIndexedJoinProblem :: IntSet -> [IntBinaryConstraintIndex] -> IntIndexedJoinProblem
intIndexedJoinProblem universe constraints =
  IntIndexedJoinProblem
    { intIndexedJoinUniverse = universe,
      intIndexedJoinConstraints = constraints
    }

intBinaryRelationMember :: Int -> Int -> IntBinaryRelationIndex -> Bool
intBinaryRelationMember left right =
  Set.member (left, right) . intBinaryRelationPairs

intIndexedJoinProblemWeight :: IntIndexedJoinProblem -> Int
intIndexedJoinProblemWeight problem =
  IntSet.size (intIndexedJoinUniverse problem)
    + foldl' (\weight constraint -> weight + intBinaryConstraintWeight constraint) 0 (intIndexedJoinConstraints problem)

intBinaryConstraintWeight :: IntBinaryConstraintIndex -> Int
intBinaryConstraintWeight constraint =
  intBinaryRelationWeight (intBinaryConstraintRelation constraint)

intBinaryRelationWeight :: IntBinaryRelationIndex -> Int
intBinaryRelationWeight index =
  IntMap.size (intBinaryRelationForward index)
    + IntMap.size (intBinaryRelationBackward index)
    + Set.size (intBinaryRelationPairs index)

intIndexedJoinCandidateSet :: IntIndexedJoinProblem -> Env Int -> Slot -> IntSet
intIndexedJoinCandidateSet problem assignmentEnv slot =
  foldl'
    IntSet.intersection
    (intIndexedJoinUniverse problem)
    (mapMaybe (intConstraintCandidateSet assignmentEnv slot) (intIndexedJoinConstraints problem))

intIndexedJoinCount :: IntIndexedJoinProblem -> Env Int -> Slot -> Int
intIndexedJoinCount problem assignmentEnv slot =
  IntSet.size (intIndexedJoinCandidateSet problem assignmentEnv slot)

intIndexedJoinPropose :: IntIndexedJoinProblem -> Env Int -> Slot -> Domain Int
intIndexedJoinPropose problem assignmentEnv slot =
  domainFromListPreservingOrder (IntSet.toAscList (intIndexedJoinCandidateSet problem assignmentEnv slot))

intIndexedJoinValidate :: IntIndexedJoinProblem -> Env Int -> Bool
intIndexedJoinValidate problem assignmentEnv =
  all (`IntSet.member` intIndexedJoinUniverse problem) (IntMap.elems assignmentEnv)
    && all (intConstraintSatisfied assignmentEnv) (intIndexedJoinConstraints problem)

intIndexedJoinAlgebra :: JoinAlgebra IntIndexedJoinProblem Int
intIndexedJoinAlgebra =
  JoinAlgebra
    { joinCount = intIndexedJoinCount,
      joinPropose = intIndexedJoinPropose,
      joinValidate = intIndexedJoinValidate
    }

foldIntIndexedAdaptiveJoin ::
  IntIndexedJoinProblem ->
  [Slot] ->
  Env Int ->
  (acc -> Env Int -> acc) ->
  acc ->
  acc
foldIntIndexedAdaptiveJoin problem slots env step initial =
  case chooseIntIndexedSmallestSlot problem slots env of
    Nothing ->
      if intIndexedJoinValidate problem env
        then step initial env
        else initial
    Just choice
      | IntSet.null (iiscCandidates choice) ->
          initial
      | otherwise ->
          IntSet.foldl'
            ( \accValue value ->
                foldIntIndexedAdaptiveJoin
                  problem
                  (iiscRemainingSlots choice)
                  (IntMap.insert (iiscSlot choice) value env)
                  step
                  accValue
            )
            initial
            (iiscCandidates choice)

type IntIndexedSlotChoice :: Type
data IntIndexedSlotChoice = IntIndexedSlotChoice
  { iiscSlot :: {-# UNPACK #-} !Slot,
    iiscRemainingSlots :: ![Slot],
    iiscCandidates :: !IntSet
  }

chooseIntIndexedSmallestSlot ::
  IntIndexedJoinProblem ->
  [Slot] ->
  Env Int ->
  Maybe IntIndexedSlotChoice
chooseIntIndexedSmallestSlot problem slots env =
  foldl' choose Nothing unboundSlots
  where
    unboundSlots =
      filter (`IntMap.notMember` env) slots

    choose maybeBest slot =
      let !candidate =
            IntIndexedSlotChoice
              { iiscSlot = slot,
                iiscRemainingSlots = filter (/= slot) slots,
                iiscCandidates = intIndexedJoinCandidateSet problem env slot
              }
       in case maybeBest of
            Nothing ->
              Just candidate
            Just best ->
              Just (betterIntIndexedSlotChoice best candidate)

betterIntIndexedSlotChoice ::
  IntIndexedSlotChoice ->
  IntIndexedSlotChoice ->
  IntIndexedSlotChoice
betterIntIndexedSlotChoice current candidate =
  case compare (IntSet.size (iiscCandidates candidate)) (IntSet.size (iiscCandidates current)) of
    LT ->
      candidate
    GT ->
      current
    EQ ->
      if iiscSlot candidate < iiscSlot current
        then candidate
        else current

intConstraintCandidateSet :: Env Int -> Slot -> IntBinaryConstraintIndex -> Maybe IntSet
intConstraintCandidateSet assignmentEnv slot constraint
  | slot == intBinaryConstraintLeftSlot constraint =
      intLeftCandidatesForRight
        (intBinaryConstraintRelation constraint)
        <$> IntMap.lookup (intBinaryConstraintRightSlot constraint) assignmentEnv
  | slot == intBinaryConstraintRightSlot constraint =
      intRightCandidatesForLeft
        (intBinaryConstraintRelation constraint)
        <$> IntMap.lookup (intBinaryConstraintLeftSlot constraint) assignmentEnv
  | otherwise =
      Nothing

intLeftCandidatesForRight :: IntBinaryRelationIndex -> Int -> IntSet
intLeftCandidatesForRight index rightValue =
  IntMap.findWithDefault IntSet.empty rightValue (intBinaryRelationBackward index)

intRightCandidatesForLeft :: IntBinaryRelationIndex -> Int -> IntSet
intRightCandidatesForLeft index leftValue =
  IntMap.findWithDefault IntSet.empty leftValue (intBinaryRelationForward index)

intConstraintSatisfied :: Env Int -> IntBinaryConstraintIndex -> Bool
intConstraintSatisfied assignmentEnv constraint =
  case ( IntMap.lookup (intBinaryConstraintLeftSlot constraint) assignmentEnv,
         IntMap.lookup (intBinaryConstraintRightSlot constraint) assignmentEnv
       ) of
    (Just left, Just right) ->
      intBinaryRelationMember left right (intBinaryConstraintRelation constraint)
    _ ->
      False

type JoinAlgebra :: Type -> Type -> Type
data JoinAlgebra ctx value = JoinAlgebra
  { -- | Count/propose/validate owner for WCOJ descent.
    --
    -- Laws:
    --
    -- * @domainSize (joinPropose ctx env slot) == joinCount ctx env slot@.
    -- * @joinPropose ctx env slot@ enumerates exactly the candidate values for
    --   @slot@ compatible with the already-bound local environment.
    -- * @joinValidate ctx env@ accepts exactly complete environments that satisfy
    --   the whole conjunctive query.
    --
    -- This is the local dogsdogsdogs-style extender surface.  Do not add a
    -- parallel proposer API unless a concrete caller needs a different carrier.
    joinCount :: !(ctx -> Env value -> Slot -> Int),
    joinPropose :: !(ctx -> Env value -> Slot -> Domain value),
    joinValidate :: !(ctx -> Env value -> Bool)
  }

foldGenericJoin ::
  JoinAlgebra ctx value ->
  ctx ->
  [Slot] ->
  Env value ->
  (acc -> Env value -> acc) ->
  acc ->
  acc
foldGenericJoin algebra ctx slots0 env0 step initial =
  go slots0 env0 initial
  where
    go [] env acc
      | joinValidate algebra ctx env =
          step acc env
      | otherwise =
          acc
    go (slot : remainingSlots) env acc =
      if IntMap.member slot env
        then go remainingSlots env acc
        else
          Vector.foldl'
            ( \accValue value ->
                go remainingSlots (IntMap.insert slot value env) accValue
            )
            acc
            (domainValues (joinPropose algebra ctx env slot))

existsJoin ::
  JoinAlgebra ctx value ->
  ctx ->
  [Slot] ->
  Env value ->
  Bool
existsJoin algebra ctx slots0 env0 =
  go slots0 env0
  where
    go [] env =
      joinValidate algebra ctx env
    go (slot : remainingSlots) env =
      if IntMap.member slot env
        then go remainingSlots env
        else
          Vector.any
            ( \value ->
                go remainingSlots (IntMap.insert slot value env)
            )
            (domainValues (joinPropose algebra ctx env slot))

adaptiveJoin ::
  JoinAlgebra ctx value ->
  ctx ->
  [Slot] ->
  Env value ->
  [Env value]
adaptiveJoin algebra ctx slots env =
  appEndo
    (foldAdaptiveJoin algebra ctx slots env (\envs joinedEnv -> envs <> Endo (joinedEnv :)) mempty)
    []

foldAdaptiveJoin ::
  JoinAlgebra ctx value ->
  ctx ->
  [Slot] ->
  Env value ->
  (acc -> Env value -> acc) ->
  acc ->
  acc
foldAdaptiveJoin =
  foldAdaptiveJoinWithFilter (\_slot _env domain -> domain)

foldAdaptiveJoinWithFilter ::
  ( Slot ->
    Env value ->
    Domain value ->
    Domain value
  ) ->
  JoinAlgebra ctx value ->
  ctx ->
  [Slot] ->
  Env value ->
  (acc -> Env value -> acc) ->
  acc ->
  acc
foldAdaptiveJoinWithFilter restrictDomain algebra ctx slots env step initial =
  case chooseSmallestSlotWithFilteredDomain restrictDomain algebra ctx slots env of
    Nothing ->
      if joinValidate algebra ctx env
        then step initial env
        else initial
    Just choice ->
      descend initial choice
  where
    descend acc (slot, remainingSlots, domain)
      | domainNull domain =
          acc
      | otherwise =
          Vector.foldl'
            ( \accValue value ->
                foldAdaptiveJoinWithFilter
                  restrictDomain
                  algebra
                  ctx
                  remainingSlots
                  (IntMap.insert slot value env)
                  step
                  accValue
            )
            acc
            (domainValues domain)

chooseSmallestSlot ::
  JoinAlgebra ctx value ->
  ctx ->
  [Slot] ->
  Env value ->
  Maybe (Slot, [Slot])
chooseSmallestSlot algebra ctx slots env =
  case chooseSmallestSlotWithFilteredDomain (\_slot _env domain -> domain) algebra ctx slots env of
    Nothing ->
      Nothing
    Just (slot, remainingSlots, _domain) ->
      Just (slot, remainingSlots)

type SlotChoice :: Type
data SlotChoice = SlotChoice
  { scSlot :: {-# UNPACK #-} !Slot,
    scBound :: {-# UNPACK #-} !Int
  }

chooseSmallestSlotWithFilteredDomain ::
  ( Slot ->
    Env value ->
    Domain value ->
    Domain value
  ) ->
  JoinAlgebra ctx value ->
  ctx ->
  [Slot] ->
  Env value ->
  Maybe (Slot, [Slot], Domain value)
chooseSmallestSlotWithFilteredDomain restrictDomain algebra ctx slots env =
  materializeChoice <$> foldl' choose Nothing unboundSlots
  where
    unboundSlots =
      filter (`IntMap.notMember` env) slots

    choose maybeBest slot =
      let !candidate =
            SlotChoice
              { scSlot = slot,
                scBound = joinCount algebra ctx env slot
              }
       in case maybeBest of
            Nothing ->
              Just candidate
            Just best ->
              Just (betterSlotChoice best candidate)

    materializeChoice choice =
      let slot =
            scSlot choice
       in (slot, filter (/= slot) slots, restrictDomain slot env (joinPropose algebra ctx env slot))

betterSlotChoice ::
  SlotChoice ->
  SlotChoice ->
  SlotChoice
betterSlotChoice current candidate =
  case compare (scBound candidate) (scBound current) of
    LT ->
      candidate
    GT ->
      current
    EQ ->
      if scSlot candidate < scSlot current
        then candidate
        else current
