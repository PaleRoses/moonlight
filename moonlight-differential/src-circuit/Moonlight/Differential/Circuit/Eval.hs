{-# LANGUAGE GADTs #-}

-- | Internal eager evaluator over the draft: the value semantics every
-- kernel's delta rule must agree with, and the engine under fixpoint bodies.
module Moonlight.Differential.Circuit.Eval
  ( evalIncluded,
    evalNodeAt,
    runFixpointEager,
    bodyConsequence,
    deindexZSet,
    eagerAggregate,
    fixpointDivergence,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List
  ( find,
  )
import Data.Map.Strict qualified as Map
import Data.Proxy
  ( Proxy,
  )
import Moonlight.Algebra
  ( MultiplicativeMonoid (one),
    Semiring,
  )
import Moonlight.Core
  ( AdditiveGroup,
  )
import Moonlight.Differential.Algebra.ZSet
  ( IndexedZSet,
    ZSet,
    indexedZSetEmpty,
    indexedZSetFold,
    zsetEmpty,
    zsetFold,
    zsetInsert,
    zsetNegate,
    zsetSize,
  )
import Moonlight.Differential.Circuit.Carrier
  ( ClosedScope (..),
    DraftNode (..),
    directScopeSpan,
    includedKernelIds,
    producerSlotOps,
    slotEmpty,
  )
import Moonlight.Differential.Circuit.Foreign
  ( ForeignKernel (..),
    ForeignKernel2 (..),
  )
import Moonlight.Differential.Circuit.Slot
  ( SlotValue,
    mkSlotValue,
    unsafeReadSlot,
  )
import Moonlight.Differential.Circuit.Types
  ( CircuitAdvanceError (..),
    IndexedNode,
    Node,
    indexedNodeId,
    nodeId,
  )
import Moonlight.Differential.Operator.Aggregate
  ( GroupChange (..),
    countByKey,
    distinctZSet,
    groupViewAdvance,
    mkGroupView,
  )
import Moonlight.Differential.Operator.Fixpoint
  ( SemiNaiveBudget,
    SemiNaiveDivergence (..),
    semiNaiveFixpointM,
  )
import Moonlight.Differential.Operator.Join
  ( joinIndexed,
  )
import Moonlight.Differential.Operator.Linear
  ( filterZSet,
    flatMapZSet,
    indexBy,
    mapZSet,
  )

deindexZSet ::
  (Ord b, Eq weight, AdditiveGroup weight) =>
  (key -> a -> b) ->
  IndexedZSet key a weight ->
  ZSet b weight
deindexZSet project =
  indexedZSetFold
    ( \acc key rows ->
        zsetFold
          (\projected value weight -> zsetInsert (project key value) weight projected)
          acc
          rows
    )
    zsetEmpty
{-# INLINE deindexZSet #-}

eagerAggregate ::
  forall key a reduced weight.
  (Ord key, Ord a, Ord reduced, Eq weight, AdditiveGroup weight, Semiring weight) =>
  (ZSet a weight -> reduced) ->
  IndexedZSet key a weight ->
  ZSet (key, reduced) weight
eagerAggregate reducer grouped =
  Map.foldlWithKey' collectChange zsetEmpty changes
  where
    changes =
      fst (groupViewAdvance reducer grouped (mkGroupView reducer indexedZSetEmpty))

    collectChange ::
      ZSet (key, reduced) weight ->
      key ->
      GroupChange reduced ->
      ZSet (key, reduced) weight
    collectChange acc key change =
      case change of
        GroupReduced reducedValue ->
          zsetInsert (key, reducedValue) one acc
        GroupVanished ->
          acc
{-# INLINE eagerAggregate #-}

fixpointDivergence ::
  Int ->
  SemiNaiveDivergence a weight ->
  CircuitAdvanceError fault
fixpointDivergence fixpointId divergence =
  CircuitFixpointDiverged
    { divergedNodeId = fixpointId,
      divergedRoundsSpent = sndRoundsSpent divergence,
      divergedResidualSize = zsetSize (sndResidualDelta divergence),
      divergedAccumulatedSize = zsetSize (sndAccumulated divergence)
    }
{-# INLINE fixpointDivergence #-}

readParentSlot ::
  (Ord weight, AdditiveGroup weight) =>
  IntMap (DraftNode s fault weight) ->
  IntMap SlotValue ->
  Int ->
  Either (CircuitAdvanceError fault) SlotValue
readParentSlot nodes env parentId =
  case IntMap.lookup parentId env of
    Just value ->
      Right value
    Nothing ->
      maybe
        (Left (CircuitEvaluationMissingParent parentId))
        (Right . slotEmpty . producerSlotOps)
        (IntMap.lookup parentId nodes)
{-# INLINE readParentSlot #-}

evalNodeAt ::
  forall s fault weight.
  (Ord weight, AdditiveGroup weight, Semiring weight) =>
  IntMap (DraftNode s fault weight) ->
  [ClosedScope] ->
  IntMap SlotValue ->
  Int ->
  Either (CircuitAdvanceError fault) SlotValue
evalNodeAt nodes scopes env selfId =
  case IntMap.lookup selfId nodes of
    Nothing ->
      Left (CircuitEvaluationMissingNode selfId)
    Just node ->
      case node of
        DraftInput (_ :: Proxy value) ->
          readParentSlot nodes env selfId
        DraftFeedback (_ :: Proxy a) ->
          readParentSlot nodes env selfId
        DraftMap (transform :: a -> b) parent ->
          fmap
            (mkSlotValue . mapZSet transform)
            (readParent @(ZSet a weight) (nodeId parent))
        DraftFilter (keep :: a -> Bool) parent ->
          fmap
            (mkSlotValue . filterZSet keep)
            (readParent @(ZSet a weight) (nodeId parent))
        DraftFlatMap (transform :: a -> [b]) parent ->
          fmap
            (mkSlotValue . flatMapZSet transform)
            (readParent @(ZSet a weight) (nodeId parent))
        DraftConcat (left :: Node s a) right ->
          fmap mkSlotValue
            ( liftA2
                (<>)
                (readParent @(ZSet a weight) (nodeId left))
                (readParent @(ZSet a weight) (nodeId right))
            )
        DraftNegate (parent :: Node s a) ->
          fmap
            (mkSlotValue . zsetNegate)
            (readParent @(ZSet a weight) (nodeId parent))
        DraftIndexBy (keyOf :: a -> key) parent ->
          fmap
            (mkSlotValue . indexBy keyOf)
            (readParent @(ZSet a weight) (nodeId parent))
        DraftDeindex (project :: key -> a -> b) parent ->
          fmap
            (mkSlotValue . deindexZSet project)
            (readParent @(IndexedZSet key a weight) (indexedNodeId parent))
        DraftJoin (left :: IndexedNode s key a) (right :: IndexedNode s key b) ->
          fmap mkSlotValue
            ( liftA2
                joinIndexed
                (readParent @(IndexedZSet key a weight) (indexedNodeId left))
                (readParent @(IndexedZSet key b weight) (indexedNodeId right))
            )
        DraftCountBy (parent :: IndexedNode s key a) ->
          fmap
            (mkSlotValue . countByKey)
            (readParent @(IndexedZSet key a weight) (indexedNodeId parent))
        DraftAggregate (reducer :: ZSet a weight -> reduced) (parent :: IndexedNode s key a) ->
          fmap
            (mkSlotValue . eagerAggregate reducer)
            (readParent @(IndexedZSet key a weight) (indexedNodeId parent))
        DraftDistinct (parent :: Node s a) ->
          fmap
            (mkSlotValue . distinctZSet)
            (readParent @(ZSet a weight) (nodeId parent))
        DraftFixpoint budget (seed :: Node s a) _ _ ->
          case scopeAt selfId of
            Nothing ->
              Right (mkSlotValue (zsetEmpty :: ZSet a weight))
            Just scope -> do
              seedValue <- readParent @(ZSet a weight) (nodeId seed)
              fmap
                mkSlotValue
                (runFixpointEager nodes scopes env scope budget seedValue)
        DraftForeign (kernel :: ForeignKernel fault weight a b) parent ->
          fmap
            (mkSlotValue . foreignDenote kernel)
            (readParent @(ZSet a weight) (nodeId parent))
        DraftForeign2 (kernel :: ForeignKernel2 fault weight a b c) left right ->
          fmap mkSlotValue
            ( liftA2
                (foreignDenote2 kernel)
                (readParent @(ZSet a weight) (nodeId left))
                (readParent @(ZSet b weight) (nodeId right))
            )
  where
    readParent :: forall payload. Int -> Either (CircuitAdvanceError fault) payload
    readParent parentId =
      fmap unsafeReadSlot (readParentSlot nodes env parentId)

    scopeAt fixpointId =
      find ((== fixpointId) . closedScopeFixpointId) scopes

evalIncluded ::
  (Ord weight, AdditiveGroup weight, Semiring weight) =>
  IntMap (DraftNode s fault weight) ->
  [ClosedScope] ->
  IntMap SlotValue ->
  Either (CircuitAdvanceError fault) (IntMap SlotValue)
evalIncluded nodes scopes seededEnv =
  foldM
    ( \env selfId -> do
        value <- evalNodeAt nodes scopes env selfId
        pure (IntMap.insert selfId value env)
    )
    seededEnv
    (includedKernelIds nodes scopes)

runFixpointEager ::
  forall s fault weight a.
  (Ord a, Ord weight, AdditiveGroup weight, Semiring weight) =>
  IntMap (DraftNode s fault weight) ->
  [ClosedScope] ->
  IntMap SlotValue ->
  ClosedScope ->
  SemiNaiveBudget ->
  ZSet a weight ->
  Either (CircuitAdvanceError fault) (ZSet a weight)
runFixpointEager nodes scopes env scope budget seedValue = do
  outcome <- semiNaiveFixpointM budget (bodyConsequence nodes scopes scope env) seedValue
  either (Left . fixpointDivergence (closedScopeFixpointId scope)) Right outcome

-- | The fixpoint body's immediate-consequence operator, lifted so incremental
-- maintenance can re-parameterize it over different outer-dependency
-- environments: fed a delta it is one semi-naive round; fed a whole set @X@ it
-- is the full immediate consequence @Tc[env](X)@ against the outer deps @env@.
bodyConsequence ::
  forall s fault weight a.
  (Ord a, Ord weight, AdditiveGroup weight, Semiring weight) =>
  IntMap (DraftNode s fault weight) ->
  [ClosedScope] ->
  ClosedScope ->
  IntMap SlotValue ->
  ZSet a weight ->
  Either (CircuitAdvanceError fault) (ZSet a weight)
bodyConsequence nodes scopes scope env frontier = do
  let seeded =
        IntMap.insert (closedScopeFeedbackId scope) (mkSlotValue frontier) env
  envAfter <-
    foldM
      ( \bodyEnv spanId -> do
          value <- evalNodeAt nodes scopes bodyEnv spanId
          pure (IntMap.insert spanId value bodyEnv)
      )
      seeded
      (IntSet.toAscList (directScopeSpan scopes scope))
  result <- readParentSlot nodes envAfter (closedScopeResultId scope)
  pure (unsafeReadSlot result)
{-# INLINABLE bodyConsequence #-}
