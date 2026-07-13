{-# LANGUAGE GADTs #-}

-- | Internal seal-time kernel compiler: every node becomes a Mealy step over
-- slot deltas with its dictionaries and state discipline captured once.
module Moonlight.Differential.Circuit.Kernel
  ( compileKernels,
    sealKernels,
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
  ( AdditiveGroup (neg),
  )
import Moonlight.Differential.Algebra.ZSet
  ( IndexedZSet,
    ZSet,
    indexedZSetEmpty,
    zsetDifference,
    zsetEmpty,
    zsetInsert,
    zsetNegate,
    zsetNull,
  )
import Moonlight.Differential.Circuit.Carrier
  ( Circuit (..),
    ClosedScope (..),
    DraftNode (..),
    Kernel (..),
    SlotOps (..),
    directScopeSpan,
    draftNodeParents,
    includedKernelIds,
    producerSlotOps,
    scopeOuterDeps,
  )
import Moonlight.Differential.Circuit.Eval
  ( bodyConsequence,
    deindexZSet,
    fixpointDivergence,
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
    CircuitBuildError (..),
    IndexedNode,
    Node,
    indexedNodeId,
    nodeId,
  )
import Moonlight.Differential.Operator.Aggregate
  ( GroupChange (..),
    countByKey,
    distinctDelta,
    distinctZSet,
    groupViewAdvance,
    groupViewReduced,
    mkGroupView,
    positiveSupportMember,
  )
import Moonlight.Differential.Operator.Fixpoint
  ( SemiNaiveBudget (..),
    SemiNaiveDivergence (..),
    semiNaiveFixpointFromM,
  )
import Moonlight.Differential.Operator.Join
  ( indexedDeltaJoin,
  )
import Moonlight.Differential.Operator.Linear
  ( filterZSet,
    flatMapZSet,
    indexBy,
    mapZSet,
  )

compileKernels ::
  forall s fault weight.
  (Ord weight, AdditiveGroup weight, Semiring weight) =>
  IntMap (DraftNode s fault weight) ->
  [ClosedScope] ->
  Either CircuitBuildError [(Int, Kernel fault weight)]
compileKernels nodes scopes =
  compileProgramFor nodes scopes (includedKernelIds nodes scopes)

-- | Compile one kernel program over an explicit id set: the whole circuit for
-- 'compileKernels', or a fixpoint span for the inner differential program.
compileProgramFor ::
  forall s fault weight.
  (Ord weight, AdditiveGroup weight, Semiring weight) =>
  IntMap (DraftNode s fault weight) ->
  [ClosedScope] ->
  [Int] ->
  Either CircuitBuildError [(Int, Kernel fault weight)]
compileProgramFor nodes scopes programIds = do
  nodeKernels <- IntMap.traverseWithKey compileNode programNodes
  traverse (programEntry (IntMap.union nodeKernels arrangeKernels)) kernelOrder
  where
    programEntry kernels selfId =
      maybe
        (Left (CircuitBuildMissingArrangement selfId selfId))
        (\kernel -> Right (selfId, kernel))
        (IntMap.lookup selfId kernels)

    programIdSet :: IntSet.IntSet
    programIdSet =
      IntSet.fromList programIds

    programNodes :: IntMap (DraftNode s fault weight)
    programNodes =
      IntMap.restrictKeys nodes programIdSet

    -- One arrangement kernel per indexed input consumed by a compiled join:
    -- it owns the integral that every consuming join reads by reference, so
    -- fan-out joins share one integral instead of privately rebuilding it.
    joinSideIds :: IntSet.IntSet
    joinSideIds =
      IntMap.foldl' collectJoinSides IntSet.empty programNodes

    collectJoinSides ::
      IntSet.IntSet ->
      DraftNode s fault weight ->
      IntSet.IntSet
    collectJoinSides acc node =
      case node of
        DraftJoin left right ->
          IntSet.insert
            (indexedNodeId left)
            (IntSet.insert (indexedNodeId right) acc)
        _ ->
          acc

    arrangeBase :: Int
    arrangeBase =
      maybe 0 ((+ 1) . fst) (IntMap.lookupMax nodes)

    arrangedNodes :: [(Int, DraftNode s fault weight)]
    arrangedNodes =
      IntMap.toAscList (IntMap.restrictKeys nodes joinSideIds)

    arrangeIdOf :: IntMap Int
    arrangeIdOf =
      IntMap.fromDistinctAscList
        (zip (fmap fst arrangedNodes) [arrangeBase ..])

    arrangeKernels :: IntMap (Kernel fault weight)
    arrangeKernels =
      IntMap.fromDistinctAscList
        [ (arrangeId, arrangeKernelFor parentId parentNode)
          | ((parentId, parentNode), arrangeId) <- zip arrangedNodes [arrangeBase ..]
        ]

    -- Emits the PRE-batch integral, then folds the batch delta in, so a
    -- consuming join sees exactly the integrals the private-state kernel
    -- form saw at every batch.
    arrangeKernelFor ::
      Int ->
      DraftNode s fault weight ->
      Kernel fault weight
    arrangeKernelFor parentId parentNode =
      grow (slotEmpty ops)
      where
        ops =
          producerSlotOps parentNode

        grow integrated =
          Kernel
            ( \slots ->
                Right
                  ( integrated,
                    grow (slotAppend ops integrated (readAt (slotEmpty ops) slots parentId))
                  )
            )

    -- Arrangements of join sides fed from outside the program run first, so
    -- their integrals stand before any consuming join reads them.
    outerSideArrangeIds :: [Int]
    outerSideArrangeIds =
      [ arrangeId
        | (sideId, arrangeId) <- IntMap.toAscList arrangeIdOf,
          not (IntSet.member sideId programIdSet)
      ]

    kernelOrder :: [Int]
    kernelOrder =
      outerSideArrangeIds
        <> concatMap
          (\selfId -> selfId : maybe [] pure (IntMap.lookup selfId arrangeIdOf))
          programIds

    readAt :: SlotValue -> IntMap SlotValue -> Int -> SlotValue
    readAt emptyValue slots parentId =
      IntMap.findWithDefault emptyValue parentId slots

    parentEmpty :: Int -> Int -> Either CircuitBuildError SlotValue
    parentEmpty selfId parentId =
      maybe
        ( Left
            CircuitBuildMissingParent
              { missingParentConsumerId = selfId,
                missingParentId = parentId
              }
        )
        (Right . slotEmpty . producerSlotOps)
        (IntMap.lookup parentId nodes)

    statelessKernel1 ::
      Int ->
      (SlotValue -> SlotValue) ->
      Int ->
      Either CircuitBuildError (Kernel fault weight)
    statelessKernel1 selfId deltaRule parentId = do
      emptyValue <- parentEmpty selfId parentId
      let kernel =
            Kernel (\slots -> Right (deltaRule (readAt emptyValue slots parentId), kernel))
      pure kernel

    statelessKernel2 ::
      Int ->
      (SlotValue -> SlotValue -> SlotValue) ->
      Int ->
      Int ->
      Either CircuitBuildError (Kernel fault weight)
    statelessKernel2 selfId deltaRule leftId rightId = do
      leftEmpty <- parentEmpty selfId leftId
      rightEmpty <- parentEmpty selfId rightId
      let kernel =
            Kernel
              ( \slots ->
                  Right
                    ( deltaRule
                        (readAt leftEmpty slots leftId)
                        (readAt rightEmpty slots rightId),
                      kernel
                    )
              )
      pure kernel

    compileNode :: Int -> DraftNode s fault weight -> Either CircuitBuildError (Kernel fault weight)
    compileNode selfId node =
      case node of
        DraftInput (_ :: Proxy value) ->
          Right inputKernel
          where
            inputKernel =
              Kernel
                ( \slots ->
                    Right
                      ( IntMap.findWithDefault
                          (mkSlotValue (zsetEmpty :: ZSet value weight))
                          selfId
                          slots,
                        inputKernel
                      )
                )
        DraftFeedback (_ :: Proxy a) ->
          Right emptyKernel
          where
            emptyKernel =
              Kernel (\_ -> Right (mkSlotValue (zsetEmpty :: ZSet a weight), emptyKernel))
        DraftMap (transform :: a -> b) parent ->
          statelessKernel1
            selfId
            (mkSlotValue . mapZSet transform . (unsafeReadSlot :: SlotValue -> ZSet a weight))
            (nodeId parent)
        DraftFilter (keep :: a -> Bool) parent ->
          statelessKernel1
            selfId
            (mkSlotValue . filterZSet keep . (unsafeReadSlot :: SlotValue -> ZSet a weight))
            (nodeId parent)
        DraftFlatMap (transform :: a -> [b]) parent ->
          statelessKernel1
            selfId
            (mkSlotValue . flatMapZSet transform . (unsafeReadSlot :: SlotValue -> ZSet a weight))
            (nodeId parent)
        DraftConcat (left :: Node s a) right ->
          statelessKernel2
            selfId
            ( \dl dr ->
                mkSlotValue
                  ( (unsafeReadSlot dl :: ZSet a weight)
                      <> (unsafeReadSlot dr :: ZSet a weight)
                  )
            )
            (nodeId left)
            (nodeId right)
        DraftNegate (parent :: Node s a) ->
          statelessKernel1
            selfId
            (mkSlotValue . zsetNegate . (unsafeReadSlot :: SlotValue -> ZSet a weight))
            (nodeId parent)
        DraftIndexBy (keyOf :: a -> key) parent ->
          statelessKernel1
            selfId
            (mkSlotValue . indexBy keyOf . (unsafeReadSlot :: SlotValue -> ZSet a weight))
            (nodeId parent)
        DraftDeindex (project :: key -> a -> b) parent ->
          statelessKernel1
            selfId
            ( mkSlotValue
                . deindexZSet project
                . (unsafeReadSlot :: SlotValue -> IndexedZSet key a weight)
            )
            (indexedNodeId parent)
        DraftCountBy (parent :: IndexedNode s key a) ->
          statelessKernel1
            selfId
            (mkSlotValue . countByKey . (unsafeReadSlot :: SlotValue -> IndexedZSet key a weight))
            (indexedNodeId parent)
        DraftJoin (left :: IndexedNode s key a) (right :: IndexedNode s key b) ->
          case ( IntMap.lookup (indexedNodeId left) arrangeIdOf,
                 IntMap.lookup (indexedNodeId right) arrangeIdOf
               ) of
            (Just leftArrangeId, Just rightArrangeId) ->
              Right (joinKernel leftArrangeId rightArrangeId)
            (Nothing, _) ->
              Left (CircuitBuildMissingArrangement selfId (indexedNodeId left))
            (_, Nothing) ->
              Left (CircuitBuildMissingArrangement selfId (indexedNodeId right))
          where
            joinKernel ::
              Int ->
              Int ->
              Kernel fault weight
            joinKernel leftArrangeId rightArrangeId =
              kernel
              where
                kernel =
                  Kernel
                    ( \slots ->
                        let integratedLeft :: IndexedZSet key a weight
                            integratedLeft =
                              unsafeReadSlot
                                ( IntMap.findWithDefault
                                    (mkSlotValue (indexedZSetEmpty :: IndexedZSet key a weight))
                                    leftArrangeId
                                    slots
                                )
                            deltaLeft =
                              unsafeReadSlot
                                ( readAt
                                    (mkSlotValue (indexedZSetEmpty :: IndexedZSet key a weight))
                                    slots
                                    (indexedNodeId left)
                                )
                            integratedRight :: IndexedZSet key b weight
                            integratedRight =
                              unsafeReadSlot
                                ( IntMap.findWithDefault
                                    (mkSlotValue (indexedZSetEmpty :: IndexedZSet key b weight))
                                    rightArrangeId
                                    slots
                                )
                            deltaRight =
                              unsafeReadSlot
                                ( readAt
                                    (mkSlotValue (indexedZSetEmpty :: IndexedZSet key b weight))
                                    slots
                                    (indexedNodeId right)
                                )
                         in Right
                              ( mkSlotValue
                                  (indexedDeltaJoin integratedLeft deltaLeft integratedRight deltaRight),
                                kernel
                              )
                    )
        DraftAggregate (reducer :: ZSet a weight -> reduced) (parent :: IndexedNode s key a) ->
          fmap
            (\emptyValue -> aggregateKernel emptyValue (mkGroupView reducer indexedZSetEmpty))
            (parentEmpty selfId (indexedNodeId parent))
          where
            aggregateKernel emptyValue view =
              Kernel
                ( \slots ->
                    let delta =
                          unsafeReadSlot (readAt emptyValue slots (indexedNodeId parent))
                        (changes, advancedView) =
                          groupViewAdvance reducer delta view
                        outDelta =
                          Map.foldlWithKey' collectChange zsetEmpty changes
                        collectChange ::
                          ZSet (key, reduced) weight ->
                          key ->
                          GroupChange reduced ->
                          ZSet (key, reduced) weight
                        collectChange acc key change =
                          let retired =
                                maybe
                                  acc
                                  (\old -> zsetInsert (key, old) (neg one) acc)
                                  (Map.lookup key (groupViewReduced view))
                           in case change of
                                GroupReduced reducedValue ->
                                  zsetInsert (key, reducedValue) one retired
                                GroupVanished ->
                                  retired
                     in Right (mkSlotValue outDelta, aggregateKernel emptyValue advancedView)
                )
        DraftDistinct (parent :: Node s a) ->
          fmap (\emptyValue -> distinctKernel emptyValue zsetEmpty) (parentEmpty selfId (nodeId parent))
          where
            distinctKernel ::
              SlotValue ->
              ZSet a weight ->
              Kernel fault weight
            distinctKernel emptyValue integrated =
              Kernel
                ( \slots ->
                    let delta =
                          unsafeReadSlot (readAt emptyValue slots (nodeId parent))
                     in Right
                          ( mkSlotValue (distinctDelta integrated delta),
                            distinctKernel emptyValue (integrated <> delta)
                          )
                )
        DraftFixpoint budget (seed :: Node s a) _ result ->
          case find ((== selfId) . closedScopeFixpointId) scopes of
            Nothing ->
              Right trivialKernel
            Just scope -> do
              let depIds =
                    IntSet.delete
                      (closedScopeFeedbackId scope)
                      ( IntSet.insert (nodeId seed) $
                          IntSet.union
                            (scopeOuterDeps nodes scope)
                            ( if IntSet.member (nodeId result) (closedScopeSpan scope)
                                then IntSet.empty
                                else IntSet.singleton (nodeId result)
                            )
                      )
              depOps <-
                IntMap.traverseWithKey
                  ( \depId () ->
                      maybe
                        (Left (CircuitBuildMissingParent selfId depId))
                        (Right . producerSlotOps)
                        (IntMap.lookup depId nodes)
                  )
                  (IntMap.fromSet (const ()) depIds)
              -- The span compiled as its own kernel program: the feedback-side
              -- arrange kernel IS the maintained closure arrangement, so a
              -- monotone batch advances differentially instead of re-running
              -- the body over the whole accumulated fixpoint.
              innerProgram <-
                if differentiallyFaithfulSpan nodes scopes scope
                  then
                    fmap
                      Just
                      ( compileProgramFor
                          nodes
                          scopes
                          (IntSet.toAscList (directScopeSpan scopes scope))
                      )
                  else pure Nothing
              let
                  depDeltasAt slots =
                    IntMap.mapWithKey
                      (\depId ops -> readAt (slotEmpty ops) slots depId)
                      depOps

                  fixpointKernel integrals previousOut inner =
                    Kernel
                      ( \slots ->
                          let advancedIntegrals =
                                IntMap.mapWithKey
                                  ( \depId ops ->
                                      slotAppend
                                        ops
                                        (IntMap.findWithDefault (slotEmpty ops) depId integrals)
                                        (readAt (slotEmpty ops) slots depId)
                                  )
                                  depOps
                              seedIntegral :: ZSet a weight
                              seedIntegral =
                                unsafeReadSlot
                                  ( IntMap.findWithDefault
                                      (mkSlotValue (zsetEmpty :: ZSet a weight))
                                      (nodeId seed)
                                      advancedIntegrals
                                  )
                              batchGrowsSupport =
                                all
                                  ( \(depId, ops) ->
                                      slotDeltaGrowsSupport ops (readAt (slotEmpty ops) slots depId)
                                  )
                                  (IntMap.toList depOps)
                           in case inner of
                                Just program
                                  | batchGrowsSupport -> do
                                      (emitted, newOut, advanced) <-
                                        advanceFixpointMonotoneArranged
                                          scope
                                          budget
                                          (nodeId seed)
                                          (depDeltasAt slots)
                                          previousOut
                                          program
                                      Right
                                        ( mkSlotValue emitted,
                                          fixpointKernel advancedIntegrals newOut (Just advanced)
                                        )
                                _ -> do
                                  newOut <-
                                    advanceFixpointDRed
                                      nodes
                                      scopes
                                      scope
                                      budget
                                      batchGrowsSupport
                                      integrals
                                      advancedIntegrals
                                      seedIntegral
                                      previousOut
                                  let outDelta = zsetDifference newOut previousOut
                                  -- Resync the retained inner arrangements to
                                  -- the eager verdict: env deltas plus the
                                  -- Z-group output difference on the feedback
                                  -- slot; the program's emission is discarded.
                                  advancedInner <-
                                    traverse
                                      ( fmap snd
                                          . advanceInnerProgram
                                            ( IntMap.insert
                                                (closedScopeFeedbackId scope)
                                                (mkSlotValue outDelta)
                                                (depDeltasAt slots)
                                            )
                                      )
                                      inner
                                  Right
                                    ( mkSlotValue outDelta,
                                      fixpointKernel advancedIntegrals newOut advancedInner
                                    )
                      )
              pure (fixpointKernel (fmap slotEmpty depOps) zsetEmpty innerProgram)
          where
            trivialKernel =
              Kernel (\_ -> Right (mkSlotValue (zsetEmpty :: ZSet a weight), trivialKernel))
        DraftForeign (foreign1 :: ForeignKernel fault weight a b) parent ->
          fmap (foreignWrap foreign1) (parentEmpty selfId (nodeId parent))
          where
            foreignWrap held emptyValue =
              Kernel
                ( \slots ->
                    case foreignStep held (unsafeReadSlot (readAt emptyValue slots (nodeId parent))) of
                      Left fault ->
                        Left (CircuitForeignFault selfId fault)
                      Right (outDelta, next) ->
                        Right (mkSlotValue outDelta, foreignWrap next emptyValue)
                )
        DraftForeign2 (foreign2 :: ForeignKernel2 fault weight a b c) left right ->
          do
            leftEmpty <- parentEmpty selfId (nodeId left)
            rightEmpty <- parentEmpty selfId (nodeId right)
            pure (foreignWrap2 foreign2 leftEmpty rightEmpty)
          where
            foreignWrap2 held leftEmpty rightEmpty =
              Kernel
                ( \slots ->
                    case foreignStep2
                      held
                      (unsafeReadSlot (readAt leftEmpty slots (nodeId left)))
                      (unsafeReadSlot (readAt rightEmpty slots (nodeId right))) of
                      Left fault ->
                        Left (CircuitForeignFault selfId fault)
                      Right (outDelta, next) ->
                        Right (mkSlotValue outDelta, foreignWrap2 next leftEmpty rightEmpty)
                )

sealKernels ::
  (Ord weight, AdditiveGroup weight, Semiring weight) =>
  IntMap (DraftNode s fault weight) ->
  [ClosedScope] ->
  Either CircuitBuildError (Circuit s fault weight)
sealKernels nodes scopes = do
  program <- compileKernels nodes scopes
  pure
    Circuit
      { circuitNodes = nodes,
        circuitScopes = scopes,
        circuitProgram = program
      }
{-# INLINE sealKernels #-}

-- | A span node whose compiled kernel is stateless: its delta rule is Z-linear
-- (or a bilinear join over shared arrangements), so pushing a delta through it
-- equals the eager per-frontier consequence.
linearSpanDraftNode :: DraftNode s fault weight -> Bool
linearSpanDraftNode node =
  case node of
    DraftMap {} -> True
    DraftFilter {} -> True
    DraftFlatMap {} -> True
    DraftConcat {} -> True
    DraftNegate {} -> True
    DraftIndexBy {} -> True
    DraftDeindex {} -> True
    DraftJoin {} -> True
    DraftCountBy {} -> True
    _ -> False

-- | License for the monotone fast path: every span node carries a Z-linear
-- delta rule and no join reads the feedback through both sides — the same
-- feedback-linearity 'advanceFixpointDRed' already assumes of the body, made
-- checkable at seal time.  Anything else keeps the eager path verbatim.
differentiallyFaithfulSpan ::
  IntMap (DraftNode s fault weight) ->
  [ClosedScope] ->
  ClosedScope ->
  Bool
differentiallyFaithfulSpan nodes scopes scope =
  all linearAt spanList && all joinSidesSplit spanList
  where
    spanIds =
      directScopeSpan scopes scope

    spanList =
      IntSet.toList spanIds

    linearAt spanId =
      maybe False linearSpanDraftNode (IntMap.lookup spanId nodes)

    feedbackReach =
      growReach (IntSet.singleton (closedScopeFeedbackId scope))

    growReach reached =
      let grown =
            IntSet.filter
              (any (`IntSet.member` reached) . parentsAt)
              spanIds
          next = IntSet.union reached grown
       in if next == reached then reached else growReach next

    parentsAt spanId =
      maybe [] draftNodeParents (IntMap.lookup spanId nodes)

    joinSidesSplit spanId =
      case IntMap.lookup spanId nodes of
        Just (DraftJoin left right) ->
          not
            ( IntSet.member (indexedNodeId left) feedbackReach
                && IntSet.member (indexedNodeId right) feedbackReach
            )
        _ ->
          True

-- | One advance of an inner span program against seeded slot deltas — the
-- same fold 'advanceCircuit' runs over the outer program.
advanceInnerProgram ::
  IntMap SlotValue ->
  [(Int, Kernel fault weight)] ->
  Either
    (CircuitAdvanceError fault)
    (IntMap SlotValue, [(Int, Kernel fault weight)])
advanceInnerProgram seededSlots program = do
  (slots, reversedProgram) <- foldM step (seededSlots, []) program
  pure (slots, reverse reversedProgram)
  where
    step (slots, reversedProgram) (selfId, kernel) = do
      (outSlot, advancedKernel) <- runKernel kernel slots
      pure
        ( IntMap.insert selfId outSlot slots,
          (selfId, advancedKernel) : reversedProgram
        )
{-# INLINABLE advanceInnerProgram #-}

-- | Monotone fast path over the compiled span program.  One seeding advance
-- pushes the batch's own dependency deltas (feedback slot = the genuinely new
-- seed facts) through the retained arrangements, yielding the true
-- differential of the body — never a whole-closure consequence.  Budgeted
-- semi-naive rounds then push each fresh frontier alone, mirroring
-- 'semiNaiveFixpointFromM' — the same fresh-exclusion discipline, budget
-- accounting, and divergence witness — over state the kernel already keeps
-- support-canonical, and the emitted output delta is accumulated on the way
-- so the caller never differences the closure.
advanceFixpointMonotoneArranged ::
  forall fault weight a.
  (Ord a, Ord weight, AdditiveGroup weight, Semiring weight) =>
  ClosedScope ->
  SemiNaiveBudget ->
  Int ->
  IntMap SlotValue ->
  ZSet a weight ->
  [(Int, Kernel fault weight)] ->
  Either
    (CircuitAdvanceError fault)
    (ZSet a weight, ZSet a weight, [(Int, Kernel fault weight)])
advanceFixpointMonotoneArranged scope budget seedId depDeltas prevOut program0 = do
  (seedSlots, seededProgram) <-
    advanceInnerProgram
      (IntMap.insert feedbackId (mkSlotValue newSeedFacts) depDeltas)
      program0
  let frontier0 = sDiff (readOut seedSlots) base'
  loop
    rounds
    (base' <> frontier0)
    frontier0
    (newSeedFacts <> frontier0)
    seededProgram
  where
    feedbackId =
      closedScopeFeedbackId scope
    rounds =
      semiNaiveBudgetRounds budget

    emptyOut =
      mkSlotValue (zsetEmpty :: ZSet a weight)

    readOut slots =
      unsafeReadSlot
        (IntMap.findWithDefault emptyOut (closedScopeResultId scope) slots) ::
        ZSet a weight

    seedDelta :: ZSet a weight
    seedDelta =
      unsafeReadSlot (IntMap.findWithDefault emptyOut seedId depDeltas)

    newSeedFacts =
      sDiff seedDelta prevOut

    base' =
      prevOut <> newSeedFacts

    sDiff x y =
      filterZSet (\value -> not (positiveSupportMember value y)) (distinctZSet x)

    loop remaining accumulated frontier emitted program
      | zsetNull frontier =
          Right (emitted, accumulated, program)
      | remaining == 0 =
          Left
            ( fixpointDivergence
                (closedScopeFixpointId scope)
                SemiNaiveDivergence
                  { sndRoundsSpent = rounds,
                    sndResidualDelta = frontier,
                    sndAccumulated = accumulated
                  }
            )
      | otherwise = do
          (roundSlots, advanced) <-
            advanceInnerProgram
              (IntMap.singleton feedbackId (mkSlotValue frontier))
              program
          let fresh = sDiff (readOut roundSlots) accumulated
          loop (remaining - 1) (accumulated <> fresh) fresh (emitted <> fresh) advanced
{-# INLINABLE advanceFixpointMonotoneArranged #-}

-- | Incremental fixpoint maintenance by Delete–Rederive (DRed).  @prevOut@ is
-- the previous fixpoint under the environment @oldEnv@ it was built over;
-- @newEnv@ / @seed'@ carry this batch's outer-dependency change.  Over-delete
-- every fact that may have lost support (direct one-step hits under the new
-- environment, then forward-propagated through the old support graph), rederive
-- the over-deleted facts that retain a derivation from the survivors, then grow
-- the surviving base to the new fixpoint.  Agrees with a from-scratch eager
-- saturation under @newEnv@ — the CircuitFixpointAdvanceAgreesWithIncrementalize
-- fence.  Weights stay on the support lattice throughout; each phase draws from
-- the shared budget and surfaces a typed divergence on exhaustion.
--
-- @growsSupport@ certifies that this batch cannot reduce any input's positive
-- support (every incoming dependency delta has non-negative weight).  A batch
-- that only adds support can retract nothing, so no fact loses a derivation and
-- no self-supporting cycle can be stranded ungrounded — over-delete and
-- rederive are provably vacuous.  The monotone path skips both and grows
-- @prevOut@ directly to the new fixpoint through a single body-consequence pass,
-- where the full path pays three (one per phase).  The fixpoint body is linear
-- in its feedback, so the new frontier is exactly @fNew prevOut ∖ prevOut@ — the
-- new seed facts and the new one-step consequences of the old closure.
advanceFixpointDRed ::
  forall s fault weight a.
  (Ord a, Ord weight, AdditiveGroup weight, Semiring weight) =>
  IntMap (DraftNode s fault weight) ->
  [ClosedScope] ->
  ClosedScope ->
  SemiNaiveBudget ->
  Bool ->
  IntMap SlotValue ->
  IntMap SlotValue ->
  ZSet a weight ->
  ZSet a weight ->
  Either (CircuitAdvanceError fault) (ZSet a weight)
advanceFixpointDRed nodes scopes scope budget growsSupport oldEnv newEnv seed' prevOut
  | growsSupport =
      -- A monotone batch retracts nothing, so every prior fact survives: the
      -- surviving base is @prevOut@ itself and only the insert phase runs.
      insertFrom (sUnion prevOut seed')
  | otherwise = do
      directFp <- fNew prevOut
      let directDel = sDiff prevOut directFp
      overdel <- overdeleteLoop rounds directDel directDel
      let surv = sDiff prevOut overdel
      rederived <- rederiveLoop rounds overdel surv zsetEmpty
      let delFinal = sDiff overdel rederived
      insertFrom (sUnion (sDiff prevOut delFinal) seed')
  where
    insertFrom base' = do
      insertFp <- fNew base'
      outcome <- semiNaiveFixpointFromM budget tcNew base' (sDiff insertFp base')
      either (Left . fixpointDivergence fixId) Right outcome
    fixId =
      closedScopeFixpointId scope
    rounds =
      semiNaiveBudgetRounds budget

    tcOld, tcNew ::
      ZSet a weight -> Either (CircuitAdvanceError fault) (ZSet a weight)
    tcOld =
      bodyConsequence nodes scopes scope oldEnv
    tcNew =
      bodyConsequence nodes scopes scope newEnv

    supp =
      distinctZSet
    sInter p x =
      filterZSet (\v -> positiveSupportMember v p) (supp x)
    sDiff x y =
      filterZSet (\v -> not (positiveSupportMember v y)) (supp x)
    sUnion x y =
      supp (x <> y)

    fNew x = do
      c <- tcNew x
      pure (sUnion seed' c)

    overdeleteLoop remaining del front
      | zsetNull front =
          Right del
      | remaining == 0 =
          Left (fixpointDivergence fixId (divergence front del))
      | otherwise = do
          casc <- tcOld front
          let next = sDiff (sInter prevOut casc) del
          overdeleteLoop (remaining - 1) (sUnion del next) next

    rederiveLoop remaining overdel surv red = do
      fp <- fNew (sUnion surv red)
      let grown = sUnion red (sInter overdel fp)
          added = sDiff grown red
      if zsetNull added
        then Right red
        else
          if remaining == 0
            then Left (fixpointDivergence fixId (divergence added grown))
            else rederiveLoop (remaining - 1) overdel surv grown

    divergence residual accumulated =
      SemiNaiveDivergence
        { sndRoundsSpent = rounds,
          sndResidualDelta = residual,
          sndAccumulated = accumulated
        }
{-# INLINABLE advanceFixpointDRed #-}
