-- | Reusable law predicates for the runtime plane: bounded settlement,
-- rows-cache pinned-drop refusal and over-budget observability, and
-- context-restriction endpoint refusal.
module Moonlight.Differential.Effect.Harness.Runtime
  ( settleQuiescentInputIsFixpoint,
    settleBudgetExhaustionHonest,
    rowsCachePinnedDropRefused,
    rowsCacheOverBudgetObservable,
    rowsCacheOverBudgetRequiresPins,
    contextRestrictionUnknownEndpointRefused,
  )
where

import Data.Functor.Identity
  ( Identity,
    runIdentity,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Differential.Context.Restriction
  ( ContextRestrictionEdge (..),
    ContextRestrictionRegistryError (..),
    mkContextRestrictionRegistry,
  )
import Moonlight.Differential.Context.RowsCache
  ( ContextRowsCache,
    crcBudgetBytes,
    crcCurrentBytes,
    crcEntries,
    crcOverBudgetBytes,
    crcPinned,
    crkContext,
    dropContextRowsFor,
  )
import Moonlight.Differential.Runtime.Error
  ( RuntimeSettleBudgetExhausted (..),
  )
import Moonlight.Differential.Runtime.Settle
  ( RuntimeSettleStep (..),
    runRuntimeSettleLoop,
  )

settleQuiescentInputIsFixpoint ::
  Eq state =>
  Int ->
  RuntimeSettleStep Identity state residual ->
  state ->
  Bool
settleQuiescentInputIsFixpoint iterationLimit settleStep state0 =
  not (rssQuiescent settleStep state0)
    || ( case runIdentity (runRuntimeSettleLoop iterationLimit settleStep state0) of
           Right settled -> settled == state0
           Left _ -> False
       )

settleBudgetExhaustionHonest ::
  (Eq state, Eq residual) =>
  Int ->
  RuntimeSettleStep Identity state residual ->
  state ->
  Bool
settleBudgetExhaustionHonest iterationLimit settleStep state0 =
  case runIdentity (runRuntimeSettleLoop iterationLimit settleStep state0) of
    Right settled ->
      rssQuiescent settleStep settled
    Left exhausted ->
      rsbeIterationLimit exhausted == iterationLimit
        && not (rssQuiescent settleStep spentState)
        && rsbeResidual exhausted == rssResidual settleStep spentState
  where
    spentState =
      stepUntilBudget 0 state0

    stepUntilBudget iteration state
      | rssQuiescent settleStep state =
          state
      | iteration >= iterationLimit =
          state
      | otherwise =
          stepUntilBudget
            (iteration + 1)
            (runIdentity (rssFlush settleStep =<< rssDrain settleStep state))

rowsCachePinnedDropRefused ::
  Ord ctx =>
  Set ctx ->
  ContextRowsCache ctx rows ->
  Bool
rowsCachePinnedDropRefused dirtyContexts cache =
  refusedKeys == pinnedMatching
    && Map.keysSet (crcEntries survivingCache)
      == Set.union (Set.difference cachedKeys matchingKeys) pinnedMatching
  where
    (refusedKeys, survivingCache) =
      dropContextRowsFor dirtyContexts cache

    cachedKeys =
      Map.keysSet (crcEntries cache)

    matchingKeys =
      Set.filter (\key -> Set.member (crkContext key) dirtyContexts) cachedKeys

    pinnedMatching =
      Set.intersection matchingKeys (crcPinned cache)

rowsCacheOverBudgetObservable ::
  ContextRowsCache ctx rows ->
  Bool
rowsCacheOverBudgetObservable cache =
  crcOverBudgetBytes cache
    == if crcCurrentBytes cache <= crcBudgetBytes cache
      then 0
      else crcCurrentBytes cache - crcBudgetBytes cache

rowsCacheOverBudgetRequiresPins ::
  ContextRowsCache ctx rows ->
  Bool
rowsCacheOverBudgetRequiresPins cache =
  crcOverBudgetBytes cache <= 0
    || not (Set.null (crcPinned cache))

contextRestrictionUnknownEndpointRefused ::
  Ord ctx =>
  Set ctx ->
  [ContextRestrictionEdge ctx] ->
  Bool
contextRestrictionUnknownEndpointRefused contexts edges =
  case mkContextRestrictionRegistry contexts edges of
    Right _ ->
      all edgeEndpointsKnown edges
    Left (ContextRestrictionEdgeEndpointUnknown offendingEdge) ->
      offendingEdge `elem` edges
        && not (edgeEndpointsKnown offendingEdge)
  where
    edgeEndpointsKnown edge =
      Set.member (creSourceContext edge) contexts
        && Set.member (creTargetContext edge) contexts
