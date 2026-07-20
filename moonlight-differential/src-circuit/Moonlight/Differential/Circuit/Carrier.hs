{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Internal circuit carrier: the draft node table whose GADT constructors
-- capture every dictionary the seal will ever need, the delimited fixpoint
-- scope records, and the sealed kernel representation.
module Moonlight.Differential.Circuit.Carrier
  ( DraftNode (..),
    CircuitDraft (..),
    emptyCircuitDraft,
    ClosedScope (..),
    closedScopeForFixpoint,
    closedScopesFromNodes,
    Circuit (..),
    Kernel (..),
    SlotOps (..),
    CircuitBatch (..),
    CircuitOutputs (..),
    circuitShapeOf,
    circuitInputIds,
    draftNodeKind,
    draftNodeParents,
    producerSlotOps,
    zsetSlotOps,
    indexedSlotOps,
    excludedScopeIds,
    scopeOuterDeps,
    directScopeSpan,
    includedKernelIds,
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
import Data.Kind
  ( Type,
  )
import Data.Proxy
  ( Proxy,
  )
import Moonlight.Core
  ( AdditiveGroup,
    AdditiveMonoid (zero),
  )
import Moonlight.Differential.Algebra.ZSet
  ( IndexedZSet,
    ZSet,
    indexedZSetEmpty,
    zsetEmpty,
    zsetFold,
  )
import Moonlight.Differential.Circuit.Foreign
  ( ForeignKernel,
    ForeignKernel2,
  )
import Moonlight.Differential.Circuit.Slot
  ( SlotValue,
    mkSlotValue,
    unsafeReadSlot,
  )
import Moonlight.Differential.Circuit.Types
  ( CircuitAdvanceError,
    IndexedNode,
    Node,
    NodeKind (..),
    NodeShape (..),
    indexedNodeId,
    nodeId,
  )
import Moonlight.Differential.Operator.Fixpoint
  ( SemiNaiveBudget,
  )

type DraftNode :: Type -> Type -> Type -> Type
data DraftNode s fault weight where
  DraftInput ::
    Ord value =>
    Proxy value ->
    DraftNode s fault weight
  DraftMap ::
    Ord b =>
    (a -> b) ->
    Node s a ->
    DraftNode s fault weight
  DraftFilter ::
    Ord a =>
    (a -> Bool) ->
    Node s a ->
    DraftNode s fault weight
  DraftFlatMap ::
    Ord b =>
    (a -> [b]) ->
    Node s a ->
    DraftNode s fault weight
  DraftConcat ::
    Ord a =>
    Node s a ->
    Node s a ->
    DraftNode s fault weight
  DraftNegate ::
    Ord a =>
    Node s a ->
    DraftNode s fault weight
  DraftIndexBy ::
    (Ord key, Ord a) =>
    (a -> key) ->
    Node s a ->
    DraftNode s fault weight
  DraftDeindex ::
    Ord b =>
    (key -> a -> b) ->
    IndexedNode s key a ->
    DraftNode s fault weight
  DraftJoin ::
    (Ord key, Ord a, Ord b) =>
    IndexedNode s key a ->
    IndexedNode s key b ->
    DraftNode s fault weight
  DraftCountBy ::
    Ord key =>
    IndexedNode s key a ->
    DraftNode s fault weight
  DraftAggregate ::
    (Ord key, Ord a, Ord reduced) =>
    (ZSet a weight -> reduced) ->
    IndexedNode s key a ->
    DraftNode s fault weight
  DraftDistinct ::
    Ord a =>
    Node s a ->
    DraftNode s fault weight
  DraftFeedback ::
    Ord a =>
    Proxy a ->
    DraftNode s fault weight
  DraftFixpoint ::
    Ord a =>
    SemiNaiveBudget ->
    Node s a ->
    Node s a ->
    Node s a ->
    IntSet ->
    DraftNode s fault weight
  DraftForeign ::
    Ord b =>
    ForeignKernel fault weight a b ->
    Node s a ->
    DraftNode s fault weight
  DraftForeign2 ::
    Ord c =>
    ForeignKernel2 fault weight a b c ->
    Node s a ->
    Node s b ->
    DraftNode s fault weight

type ClosedScope :: Type
data ClosedScope = ClosedScope
  { closedScopeFixpointId :: !Int,
    closedScopeFeedbackId :: !Int,
    closedScopeResultId :: !Int,
    closedScopeSpan :: !IntSet
  }
  deriving stock (Eq, Show)

type CircuitDraft :: Type -> Type -> Type -> Type
data CircuitDraft s fault weight = CircuitDraft
  { draftNextId :: !Int,
    draftNodes :: !(IntMap (DraftNode s fault weight))
  }

emptyCircuitDraft :: CircuitDraft s fault weight
emptyCircuitDraft =
  CircuitDraft
    { draftNextId = 0,
      draftNodes = IntMap.empty
    }
{-# INLINE emptyCircuitDraft #-}

type Kernel :: Type -> Type -> Type
newtype Kernel fault weight = Kernel
  { runKernel ::
      IntMap SlotValue ->
      Either (CircuitAdvanceError fault) (SlotValue, Kernel fault weight)
  }

type Circuit :: Type -> Type -> Type -> Type
data Circuit s fault weight = Circuit
  { circuitNodes :: !(IntMap (DraftNode s fault weight)),
    circuitScopes :: ![ClosedScope],
    -- | Seal-computed execution order: ascending draft ids with each synthetic
    -- arrangement kernel spliced directly after its producing node, so shared
    -- integrals exist before any consuming join runs.
    circuitProgram :: ![(Int, Kernel fault weight)]
  }

type CircuitBatch :: Type -> Type -> Type
newtype CircuitBatch s weight = CircuitBatch (IntMap SlotValue)

type CircuitOutputs :: Type -> Type -> Type
newtype CircuitOutputs s weight = CircuitOutputs (IntMap SlotValue)

type SlotOps :: Type
data SlotOps = SlotOps
  { slotEmpty :: SlotValue,
    slotAppend :: SlotValue -> SlotValue -> SlotValue,
    -- | Does this delta, applied, preserve every positive support — i.e. can it
    -- retract nothing?  True iff no entry carries negative weight.  Sound but
    -- conservative: a non-negative delta can never drop a fact from positive
    -- support, so a fixpoint reading only such deltas needs no over-deletion.
    slotDeltaGrowsSupport :: SlotValue -> Bool
  }

zsetSlotOps ::
  forall value weight.
  (Ord value, Ord weight, AdditiveGroup weight) =>
  SlotOps
zsetSlotOps =
  SlotOps
    { slotEmpty = mkSlotValue (zsetEmpty :: ZSet value weight),
      slotAppend = \left right ->
        mkSlotValue (unsafeReadSlot left <> (unsafeReadSlot right :: ZSet value weight)),
      slotDeltaGrowsSupport = \delta ->
        zsetFold
          (\ok _value weight -> ok && weight >= zero)
          True
          (unsafeReadSlot delta :: ZSet value weight)
    }
{-# INLINE zsetSlotOps #-}

indexedSlotOps ::
  forall key value weight.
  (Ord key, Ord value, Ord weight, AdditiveGroup weight) =>
  SlotOps
indexedSlotOps =
  SlotOps
    { slotEmpty = mkSlotValue (indexedZSetEmpty :: IndexedZSet key value weight),
      slotAppend = \left right ->
        mkSlotValue (unsafeReadSlot left <> (unsafeReadSlot right :: IndexedZSet key value weight)),
      -- Indexed outer dependencies forgo the monotone fast path (conservatively
      -- unproven); such a fixpoint always takes the full over-delete route.
      slotDeltaGrowsSupport = const False
    }
{-# INLINE indexedSlotOps #-}

producerSlotOps ::
  forall s fault weight.
  (Ord weight, AdditiveGroup weight) =>
  DraftNode s fault weight ->
  SlotOps
producerSlotOps node =
  case node of
    DraftInput (_ :: Proxy value) ->
      zsetSlotOps @value @weight
    DraftMap (_ :: a -> b) _ ->
      zsetSlotOps @b @weight
    DraftFilter (_ :: a -> Bool) _ ->
      zsetSlotOps @a @weight
    DraftFlatMap (_ :: a -> [b]) _ ->
      zsetSlotOps @b @weight
    DraftConcat (_ :: Node s a) _ ->
      zsetSlotOps @a @weight
    DraftNegate (_ :: Node s a) ->
      zsetSlotOps @a @weight
    DraftIndexBy (_ :: a -> key) _ ->
      indexedSlotOps @key @a @weight
    DraftDeindex (_ :: key -> a -> b) _ ->
      zsetSlotOps @b @weight
    DraftJoin (_ :: IndexedNode s key a) (_ :: IndexedNode s key b) ->
      zsetSlotOps @(key, a, b) @weight
    DraftCountBy (_ :: IndexedNode s key a) ->
      zsetSlotOps @key @weight
    DraftAggregate (_ :: ZSet a weight -> reduced) (_ :: IndexedNode s key a) ->
      zsetSlotOps @(key, reduced) @weight
    DraftDistinct (_ :: Node s a) ->
      zsetSlotOps @a @weight
    DraftFeedback (_ :: Proxy a) ->
      zsetSlotOps @a @weight
    DraftFixpoint _ (_ :: Node s a) _ _ _ ->
      zsetSlotOps @a @weight
    DraftForeign (_ :: ForeignKernel fault weight a b) _ ->
      zsetSlotOps @b @weight
    DraftForeign2 (_ :: ForeignKernel2 fault weight a b c) _ _ ->
      zsetSlotOps @c @weight

draftNodeKind :: DraftNode s fault weight -> NodeKind
draftNodeKind node =
  case node of
    DraftInput {} -> InputNode
    DraftMap {} -> MapNode
    DraftFilter {} -> FilterNode
    DraftFlatMap {} -> FlatMapNode
    DraftConcat {} -> ConcatNode
    DraftNegate {} -> NegateNode
    DraftIndexBy {} -> IndexByNode
    DraftDeindex {} -> DeindexNode
    DraftJoin {} -> JoinNode
    DraftCountBy {} -> CountByNode
    DraftAggregate {} -> AggregateNode
    DraftDistinct {} -> DistinctNode
    DraftFeedback {} -> FeedbackNode
    DraftFixpoint {} -> FixpointNode
    DraftForeign {} -> ForeignNode
    DraftForeign2 {} -> ForeignNode

draftNodeParents :: DraftNode s fault weight -> [Int]
draftNodeParents node =
  case node of
    DraftInput _ -> []
    DraftMap _ parent -> [nodeId parent]
    DraftFilter _ parent -> [nodeId parent]
    DraftFlatMap _ parent -> [nodeId parent]
    DraftConcat left right -> [nodeId left, nodeId right]
    DraftNegate parent -> [nodeId parent]
    DraftIndexBy _ parent -> [nodeId parent]
    DraftDeindex _ parent -> [indexedNodeId parent]
    DraftJoin left right -> [indexedNodeId left, indexedNodeId right]
    DraftCountBy parent -> [indexedNodeId parent]
    DraftAggregate _ parent -> [indexedNodeId parent]
    DraftDistinct parent -> [nodeId parent]
    DraftFeedback _ -> []
    DraftFixpoint _ seed feedback result _ ->
      [nodeId seed, nodeId feedback, nodeId result]
    DraftForeign _ parent -> [nodeId parent]
    DraftForeign2 _ left right -> [nodeId left, nodeId right]

circuitShapeOf :: Circuit s fault weight -> IntMap NodeShape
circuitShapeOf circuit =
  fmap
    (\node -> NodeShape (draftNodeKind node) (draftNodeParents node))
    (circuitNodes circuit)
{-# INLINE circuitShapeOf #-}

circuitInputIds :: Circuit s fault weight -> IntSet
circuitInputIds circuit =
  IntMap.keysSet (IntMap.filter isInput (circuitNodes circuit))
  where
    isInput node =
      case node of
        DraftInput {} -> True
        _ -> False
{-# INLINE circuitInputIds #-}

excludedScopeIds :: [ClosedScope] -> IntSet
excludedScopeIds =
  IntSet.unions
    . fmap
      ( \scope ->
          IntSet.insert (closedScopeFeedbackId scope) (closedScopeSpan scope)
      )
{-# INLINE excludedScopeIds #-}

scopeOuterDeps ::
  IntMap (DraftNode s fault weight) ->
  ClosedScope ->
  IntSet
scopeOuterDeps nodes scope =
  IntSet.filter outside referenced
  where
    referenced =
      IntSet.fromList
        ( concatMap
            ( \spanId ->
                maybe [] draftNodeParents (IntMap.lookup spanId nodes)
            )
            (IntSet.toAscList (closedScopeSpan scope))
        )

    outside referencedId =
      referencedId /= closedScopeFeedbackId scope
        && not (IntSet.member referencedId (closedScopeSpan scope))
{-# INLINE scopeOuterDeps #-}

directScopeSpan :: [ClosedScope] -> ClosedScope -> IntSet
directScopeSpan allScopes scope =
  IntSet.difference (closedScopeSpan scope) nestedExclusions
  where
    nestedExclusions =
      excludedScopeIds
        ( filter
            ( \candidate ->
                IntSet.member (closedScopeFixpointId candidate) (closedScopeSpan scope)
            )
            allScopes
        )
{-# INLINE directScopeSpan #-}

includedKernelIds ::
  IntMap (DraftNode s fault weight) ->
  [ClosedScope] ->
  [Int]
includedKernelIds nodes scopes =
  filter
    (\candidateId -> not (IntSet.member candidateId excluded))
    (IntMap.keys nodes)
  where
    excluded =
      excludedScopeIds scopes
{-# INLINE includedKernelIds #-}

closedScopeForFixpoint :: Int -> Node s a -> Node s a -> IntSet -> ClosedScope
closedScopeForFixpoint fixpointId feedback result spanIds =
  ClosedScope
    { closedScopeFixpointId = fixpointId,
      closedScopeFeedbackId = nodeId feedback,
      closedScopeResultId = nodeId result,
      closedScopeSpan = spanIds
    }
{-# INLINE closedScopeForFixpoint #-}

closedScopesFromNodes :: IntMap (DraftNode s fault weight) -> [ClosedScope]
closedScopesFromNodes =
  IntMap.foldrWithKey collectClosedScope []

collectClosedScope :: Int -> DraftNode s fault weight -> [ClosedScope] -> [ClosedScope]
collectClosedScope fixpointId node scopes =
  case node of
    DraftFixpoint _ _ feedback result spanIds ->
      closedScopeForFixpoint fixpointId feedback result spanIds : scopes
    _ ->
      scopes
