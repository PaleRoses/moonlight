{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Execution.Observe.Provenance.Support
  ( ProvSupport,
    ProvSupportMemo,
    ProvSupportEvalStats (..),
    ProvSupportMemoValidationError (..),
    emptyProvSupportMemo,
    provSupportMemoNodeCount,
    provSupportMemoRowEstimate,
    evalProvSupport,
    evalProvSupportWithMemo,
    pruneProvSupportMemo,
    remapProvSupportMemo,
    validateProvSupportMemo,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor
  ( first,
  )
import Data.Foldable (traverse_)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Moonlight.Flow.Execution.Observe.Provenance.Args
  ( ProvArgs,
    provArgsToIds,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Arena (nodeAt)
import Moonlight.Flow.Execution.Observe.Provenance.GC
  ( ProvIdRemap,
    provIdRemapIsIdentity,
    remapProvIdKey,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Id
  ( initialProvArenaScope,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Types.Internal
  ( ProvArena,
    ProvArenaScope,
    ProvEntry (..),
    ProvGen (..),
    ProvId (..),
    ProvNode (..),
    ProvVal (..),
    ProvenanceObstruction (..),
    paNodes,
    paScope,
  )
import Moonlight.Differential.Row.Tuple (RowTupleKey)
import Moonlight.Flow.Plan.Query.Core (atomIdKey)

-- | Support denotation for a provenance value: atom id -> content-addressed
-- atom rows.
--
-- Both sums and products contribute the set-union of child atom leaves. This
-- is intentionally not a derivation-counted structure.
type ProvSupport :: Type
type ProvSupport =
  IntMap (HashSet RowTupleKey)

type ProvSupportMemo :: Type
data ProvSupportMemo = ProvSupportMemo
  { psmArenaScope :: {-# UNPACK #-} !ProvArenaScope,
    psmByProvId :: !(IntMap ProvSupport)
  }
  deriving stock (Eq, Show)

type ProvSupportEvalStats :: Type
data ProvSupportEvalStats = ProvSupportEvalStats
  { pseNodesEvaluated :: {-# UNPACK #-} !Int,
    pseMemoHits :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

type ProvSupportMemoValidationError :: Type
data ProvSupportMemoValidationError
  = ProvSupportMemoWrongScope !ProvArenaScope !ProvArenaScope
  | ProvSupportMemoInvalidKey !ProvId !ProvenanceObstruction
  | ProvSupportMemoMismatch !ProvId !ProvSupport !ProvSupport
  deriving stock (Eq, Show)

type ProvSupportEvalState :: Type
data ProvSupportEvalState = ProvSupportEvalState
  { psesPersistent :: !ProvSupportMemo,
    psesLocal :: !(IntMap ProvSupport),
    psesStats :: !ProvSupportEvalStats
  }

emptyProvSupportMemo :: ProvSupportMemo
emptyProvSupportMemo =
  ProvSupportMemo
    { psmArenaScope = initialProvArenaScope,
      psmByProvId = IntMap.empty
    }
{-# INLINE emptyProvSupportMemo #-}

emptyProvSupportMemoForArena :: ProvArena -> ProvSupportMemo
emptyProvSupportMemoForArena arena =
  ProvSupportMemo
    { psmArenaScope = paScope arena,
      psmByProvId = IntMap.empty
    }
{-# INLINE emptyProvSupportMemoForArena #-}

-- | Evaluation is the only boundary that may silently discard a stale memo.
--
-- A mismatched memo is a cache miss, not a semantic failure. Moving-GC remap
-- remains stricter because it claims to transport old ids into a new scope.
supportMemoForArena :: ProvArena -> ProvSupportMemo -> ProvSupportMemo
supportMemoForArena arena memo
  | psmArenaScope memo == paScope arena =
      memo
  | otherwise =
      emptyProvSupportMemoForArena arena
{-# INLINE supportMemoForArena #-}

emptyProvSupportEvalStats :: ProvSupportEvalStats
emptyProvSupportEvalStats =
  ProvSupportEvalStats
    { pseNodesEvaluated = 0,
      pseMemoHits = 0
    }
{-# INLINE emptyProvSupportEvalStats #-}

initialProvSupportEvalState ::
  ProvArena ->
  ProvSupportMemo ->
  ProvSupportEvalState
initialProvSupportEvalState arena memo =
  ProvSupportEvalState
    { psesPersistent = supportMemoForArena arena memo,
      psesLocal = IntMap.empty,
      psesStats = emptyProvSupportEvalStats
    }
{-# INLINE initialProvSupportEvalState #-}

provSupportMemoNodeCount :: ProvSupportMemo -> Int
provSupportMemoNodeCount =
  IntMap.size . psmByProvId
{-# INLINE provSupportMemoNodeCount #-}

provSupportMemoRowEstimate :: ProvSupportMemo -> Int
provSupportMemoRowEstimate =
  IntMap.foldl' (\ !total support -> total + provSupportRowCount support) 0
    . psmByProvId
{-# INLINE provSupportMemoRowEstimate #-}

provSupportRowCount :: ProvSupport -> Int
provSupportRowCount =
  IntMap.foldl' (\ !total rows -> total + HashSet.size rows) 0
{-# INLINE provSupportRowCount #-}

-- | Fresh support evaluation, retained as the semantic reference path.
evalProvSupport ::
  ProvArena ->
  ProvVal ->
  Either ProvenanceObstruction ProvSupport
evalProvSupport arena value = do
  (support, _memo, _stats) <-
    evalProvSupportWithMemo arena value emptyProvSupportMemo
  Right support
{-# INLINE evalProvSupport #-}

-- | Evaluate support using a persistent per-'ProvId' memo.
--
-- Correctness invariant:
--
--   For every @pid -> support@ in the returned memo,
--   @support == evalProvSupport arena (PVRef pid)@.
--
-- The current root is still evaluated as a provenance value. The memo only
-- caches exact sub-DAG denotations; it never patches support by deletion.
evalProvSupportWithMemo ::
  ProvArena ->
  ProvVal ->
  ProvSupportMemo ->
  Either ProvenanceObstruction (ProvSupport, ProvSupportMemo, ProvSupportEvalStats)
evalProvSupportWithMemo arena value memo0 = do
  (support, state1) <-
    evalVal True value (initialProvSupportEvalState arena memo0)
  let memo1 =
        retainProvSupportMemoForRoot
          arena
          (rootProvId value)
          (psesPersistent state1)
  Right (support, memo1, psesStats state1)
  where
    evalVal ::
      Bool ->
      ProvVal ->
      ProvSupportEvalState ->
      Either ProvenanceObstruction (ProvSupport, ProvSupportEvalState)
    evalVal _isRoot PVZero state =
      Right (IntMap.empty, state)
    evalVal _isRoot PVOne state =
      Right (IntMap.empty, state)
    evalVal _isRoot (PVObstructed obstruction) _state =
      Left obstruction
    evalVal isRoot (PVRef pid) state =
      evalId isRoot pid state

    evalId ::
      Bool ->
      ProvId ->
      ProvSupportEvalState ->
      Either ProvenanceObstruction (ProvSupport, ProvSupportEvalState)
    evalId isRoot pid state0 = do
      node <- nodeAt arena pid
      let !key = unProvId pid
      case IntMap.lookup key (psmByProvId (psesPersistent state0)) of
        Just cached ->
          Right (cached, recordPersistentSupportMemoHit state0)
        Nothing ->
          case IntMap.lookup key (psesLocal state0) of
            Just cached ->
              Right (cached, state0)
            Nothing -> do
              (support, state1) <-
                case node of
                  PNAtom atomId row ->
                    Right
                      ( IntMap.singleton
                          (atomIdKey atomId)
                          (HashSet.singleton row),
                        state0
                      )
                  PNSum args ->
                    foldChildren args state0
                  PNProd args ->
                    foldChildren args state0
              let state2 =
                    recordSupportNodeEvaluation
                      ( state1
                          { psesLocal =
                              IntMap.insert key support (psesLocal state1)
                          }
                      )
                  state3 =
                    if retainProvSupportMemoEntry isRoot arena pid
                      then
                        state2
                          { psesPersistent =
                              insertProvSupportMemo key support (psesPersistent state2)
                          }
                      else state2
              Right (support, state3)

    foldChildren ::
      ProvArgs ->
      ProvSupportEvalState ->
      Either ProvenanceObstruction (ProvSupport, ProvSupportEvalState)
    foldChildren args state0 =
      foldM
        ( \(!acc, !state) child -> do
            (childSupport, state1) <- evalId False child state
            Right (unionProvSupport acc childSupport, state1)
        )
        (IntMap.empty, state0)
        (provArgsToIds args)

unionProvSupport :: ProvSupport -> ProvSupport -> ProvSupport
unionProvSupport =
  IntMap.unionWith HashSet.union
{-# INLINE unionProvSupport #-}

insertProvSupportMemo ::
  Int ->
  ProvSupport ->
  ProvSupportMemo ->
  ProvSupportMemo
insertProvSupportMemo key support memo =
  memo
    { psmByProvId =
        IntMap.insert key support (psmByProvId memo)
    }
{-# INLINE insertProvSupportMemo #-}

recordSupportNodeEvaluation ::
  ProvSupportEvalState ->
  ProvSupportEvalState
recordSupportNodeEvaluation state =
  state
    { psesStats =
        (psesStats state)
          { pseNodesEvaluated =
              pseNodesEvaluated (psesStats state) + 1
          }
    }
{-# INLINE recordSupportNodeEvaluation #-}

recordPersistentSupportMemoHit ::
  ProvSupportEvalState ->
  ProvSupportEvalState
recordPersistentSupportMemoHit state =
  state
    { psesStats =
        (psesStats state)
          { pseMemoHits =
              pseMemoHits (psesStats state) + 1
          }
    }
{-# INLINE recordPersistentSupportMemoHit #-}

rootProvId :: ProvVal -> Maybe ProvId
rootProvId value =
  case value of
    PVRef pid ->
      Just pid
    PVZero ->
      Nothing
    PVOne ->
      Nothing
    PVObstructed _obstruction ->
      Nothing
{-# INLINE rootProvId #-}

retainProvSupportMemoEntry ::
  Bool ->
  ProvArena ->
  ProvId ->
  Bool
retainProvSupportMemoEntry isRoot arena pid =
  isRoot || provSupportMemoEntryMature arena pid
{-# INLINE retainProvSupportMemoEntry #-}

provSupportMemoEntryMature ::
  ProvArena ->
  ProvId ->
  Bool
provSupportMemoEntryMature arena (ProvId key) =
  case IntMap.lookup key (paNodes arena) of
    Nothing ->
      False
    Just entry ->
      provEntryMemoMature entry
{-# INLINE provSupportMemoEntryMature #-}

provEntryMemoMature :: ProvEntry -> Bool
provEntryMemoMature entry =
  case peGen entry of
    GenNursery ->
      False
    GenCached ->
      True
    GenStable ->
      True
{-# INLINE provEntryMemoMature #-}

retainProvSupportMemoForRoot ::
  ProvArena ->
  Maybe ProvId ->
  ProvSupportMemo ->
  ProvSupportMemo
retainProvSupportMemoForRoot arena maybeRoot =
  retainProvSupportMemoKeys rootKeys arena
  where
    rootKeys =
      case maybeRoot of
        Nothing ->
          IntSet.empty
        Just pid ->
          IntSet.singleton (unProvId pid)
{-# INLINE retainProvSupportMemoForRoot #-}

retainProvSupportMemoKeys ::
  IntSet ->
  ProvArena ->
  ProvSupportMemo ->
  ProvSupportMemo
retainProvSupportMemoKeys rootKeys arena memo =
  let !memo0 =
        supportMemoForArena arena memo
   in memo0
        { psmArenaScope = paScope arena,
          psmByProvId =
            IntMap.filterWithKey keep (psmByProvId memo0)
        }
  where
    keep key _support =
      case IntMap.lookup key (paNodes arena) of
        Nothing ->
          False
        Just entry ->
          IntSet.member key rootKeys
            || provEntryMemoMature entry
{-# INLINE retainProvSupportMemoKeys #-}

pruneProvSupportMemo ::
  [ProvVal] ->
  ProvArena ->
  ProvSupportMemo ->
  ProvSupportMemo
pruneProvSupportMemo roots =
  retainProvSupportMemoKeys (provRootKeySet roots)
{-# INLINE pruneProvSupportMemo #-}

provRootKeySet :: [ProvVal] -> IntSet
provRootKeySet =
  IntSet.fromList . mapMaybe provRootKey
  where
    provRootKey (PVRef pid) = Just (unProvId pid)
    provRootKey PVZero = Nothing
    provRootKey PVOne = Nothing
    provRootKey (PVObstructed _obstruction) = Nothing
{-# INLINE provRootKeySet #-}

-- | Remap support memo keys after moving compaction.
--
-- Memo values contain atom-row content only, so only the outer 'ProvId' keys
-- require remapping. Keys absent from the remap table were not reachable from
-- the arena roots used by compaction and are dropped; the memo is not a GC root.
--
-- Unlike evaluation, remap is a transport claim. A non-empty memo from the
-- wrong old arena scope is rejected rather than silently erased.
remapProvSupportMemo ::
  ProvArena ->
  ProvArena ->
  ProvIdRemap ->
  ProvSupportMemo ->
  Either ProvenanceObstruction ProvSupportMemo
remapProvSupportMemo oldArena newArena remap memo = do
  memo0 <- memoScopedToArena oldArena memo
  if provIdRemapIsIdentity remap
    then
      if paScope oldArena == paScope newArena
        then Right memo0 {psmArenaScope = paScope newArena}
        else
          if IntMap.null (psmByProvId memo0)
            then Right (emptyProvSupportMemoForArena newArena)
            else Left (StaleProvSupportMemoScope (paScope oldArena) (paScope newArena))
    else
      Right
        ProvSupportMemo
          { psmArenaScope = paScope newArena,
            psmByProvId =
              IntMap.fromList
                [ (newKey, support)
                  | (oldKey, support) <- IntMap.toAscList (psmByProvId memo0),
                    Just newKey <- [remapProvIdKey remap oldKey]
                ]
          }
{-# INLINE remapProvSupportMemo #-}

memoScopedToArena ::
  ProvArena ->
  ProvSupportMemo ->
  Either ProvenanceObstruction ProvSupportMemo
memoScopedToArena arena memo
  | IntMap.null (psmByProvId memo) =
      Right memo {psmArenaScope = paScope arena}
  | psmArenaScope memo == paScope arena =
      Right memo
  | otherwise =
      Left (StaleProvSupportMemoScope (paScope arena) (psmArenaScope memo))
{-# INLINE memoScopedToArena #-}

validateProvSupportMemo ::
  ProvArena ->
  ProvSupportMemo ->
  Either ProvSupportMemoValidationError ()
validateProvSupportMemo arena memo
  | IntMap.null (psmByProvId memo) =
      Right ()
  | psmArenaScope memo /= paScope arena =
      Left (ProvSupportMemoWrongScope (paScope arena) (psmArenaScope memo))
  | otherwise =
      traverse_ (uncurry validateOne) (IntMap.toAscList (psmByProvId memo))
  where
    validateOne ::
      Int ->
      ProvSupport ->
      Either ProvSupportMemoValidationError ()
    validateOne key cached = do
      let !pid =
            ProvId key
      fresh <-
        first (ProvSupportMemoInvalidKey pid) $
          evalProvSupport arena (PVRef pid)
      if cached == fresh
        then Right ()
        else Left (ProvSupportMemoMismatch pid fresh cached)
{-# INLINE validateProvSupportMemo #-}
