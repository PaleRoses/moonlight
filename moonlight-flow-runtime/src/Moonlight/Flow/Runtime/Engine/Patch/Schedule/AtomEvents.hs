{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Runtime.Engine.Patch.Schedule.AtomEvents
  ( canonicalizeScopedAtomEvents,
    atomEventOps,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( listToMaybe,
  )
import Moonlight.Core
  ( AtomId,
    QueryId,
    atomIdKey,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
  )
import Moonlight.Flow.Carrier.Core.Delta.Emit
  ( CarrierEmitSpec (..),
  )
import Moonlight.Flow.Model.Delta
  ( AtomEvent (..),
    ScopedAtomEvents (..)
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
  )
import Moonlight.Flow.Plan.Query.Core
  ( QueryAtomId,
    SourceAtomId,
    mkQueryAtomId,
    queryAtomAsAtomId,
    sourceAtomKey,
  )
import Moonlight.Flow.Runtime.Carrier.Emit
  ( AtomCarrierPayload (..),
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Delta
  ( mergeRowDeltaDedupCopy,
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( RuntimeDataflowOp,
    applyAtomEventsDataflowOp,
  )
import Moonlight.Flow.Runtime.Factor.Program
  ( FactorProgram,
    factorProgramSpec,
  )
import Moonlight.Flow.Runtime.Factor.State
  ( factorQueryRepresentativeQueryId,
    lookupFactorProgram,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnv (..),
    RuntimeEnvelope (..),
    rsRouting,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( factorProgramSpecAtomSourceMap,
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( routeContextOfQuery,
  )

canonicalizeScopedAtomEvents ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  ScopedAtomEvents ->
  ScopedAtomEvents
canonicalizeScopedAtomEvents runtime scopedEvents =
  scopedEvents
    { saeAtomScopeByAtom = canonicalScopes,
      saeEvents = fmap snd canonicalEntries
    }
  where
    canonicalEntries =
      Map.elems
        . Map.fromListWith
          mergeRepresentativeEvent
        $ fmap representativeEventEntry (saeEvents scopedEvents)

    canonicalScopes =
      IntMap.fromListWith
        (<>)
        [ (atomIdKey (aeAtomId event), scopeValue)
        | (scopeValue, event) <- canonicalEntries
        ]

    mergeRepresentativeEvent ::
      (RelationalScope, AtomEvent) ->
      (RelationalScope, AtomEvent) ->
      (RelationalScope, AtomEvent)
    mergeRepresentativeEvent (newScope, newer) (oldScope, older) =
      ( oldScope <> newScope,
        older
          { aeRows =
            mergeRowDeltaDedupCopy
              (aeRows newer)
              (aeRows older)
          }
      )

    representativeEventEntry event =
      let !representativeEvent =
            canonicalRepresentativeEvent runtime event
          !eventScope =
            atomEventScope scopedEvents event
       in ((aeQueryId representativeEvent, aeAtomId representativeEvent), (eventScope, representativeEvent))

canonicalRepresentativeEvent ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  AtomEvent ->
  AtomEvent
canonicalRepresentativeEvent runtime event =
  case factorQueryRepresentativeQueryId runtime (aeQueryId event) of
    Nothing ->
      event
    Just representativeQueryId
      | representativeQueryId == aeQueryId event ->
          event
      | otherwise ->
          case representativeAtomIdForEvent runtime representativeQueryId event of
            Nothing ->
              event
            Just representativeAtomId ->
              event
                { aeQueryId = representativeQueryId,
                  aeAtomId = representativeAtomId
                }

representativeAtomIdForEvent ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  QueryId ->
  AtomEvent ->
  Maybe AtomId
representativeAtomIdForEvent runtime representativeQueryId event = do
  sourceAtomId <-
    eventSourceAtomId runtime event
  representativeProgram <-
    lookupFactorProgram representativeQueryId runtime
  representativeAtomId <-
    queryAtomForSourceAtom sourceAtomId representativeProgram
  pure (queryAtomAsAtomId representativeAtomId)

eventSourceAtomId ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  AtomEvent ->
  Maybe SourceAtomId
eventSourceAtomId runtime event = do
  program <- lookupFactorProgram (aeQueryId event) runtime
  IntMap.lookup
    (atomIdKey (aeAtomId event))
    (factorProgramSpecAtomSourceMap (factorProgramSpec program))

queryAtomForSourceAtom ::
  SourceAtomId ->
  FactorProgram ->
  Maybe QueryAtomId
queryAtomForSourceAtom sourceAtomId program =
  mkQueryAtomId
    <$> listToMaybe
      [ queryAtomKey
      | (queryAtomKey, candidateSourceAtomId) <-
          IntMap.toAscList (factorProgramSpecAtomSourceMap (factorProgramSpec program)),
        sourceAtomKey candidateSourceAtomId == sourceAtomKey sourceAtomId
      ]

atomEventOps ::
  (Ord ctx, Ord prop) =>
  ScopedAtomEvents ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    [RuntimeDataflowOp ctx prop boundary evidence]
atomEventOps scopedEvents runtime = do
  batches <-
    atomEventBatchesByCarrierChecked scopedEvents runtime
  Right
    [ applyAtomEventsDataflowOp addr scope events
    | (addr, scope, events) <- batches
    ]

atomEventBatchesByCarrierChecked ::
  (Ord ctx, Ord prop) =>
  ScopedAtomEvents ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    [(CarrierAddr ctx Carrier prop, RelationalScope, NonEmpty AtomEvent)]
atomEventBatchesByCarrierChecked scopedEvents runtime =
  fmap
    ( fmap
        ( \(addr, (scopeValue, events)) ->
            (addr, scopeValue, events)
        )
        . Map.toAscList
    )
    (foldM insertEvent Map.empty (saeEvents scopedEvents))
  where
    insertEvent byCarrier event = do
      contextValue <-
        case routeContextOfQuery (aeQueryId event) (rsRouting (rdrState runtime)) of
          Just value ->
            Right value
          Nothing ->
            Left (RuntimeMissingQueryRoute (aeQueryId event))
      let eventScope =
            atomEventScope scopedEvents event
          payload =
            AtomCarrierPayload
              { acpScope = eventScope,
                acpEvent = event
              }
          specAddr =
            cesAddrOf (reAtomCarrierEmitSpec (rdrEnv runtime)) payload
          addr =
            specAddr {caContext = contextValue}
      Right
        ( Map.insertWith
            ( \(newScope, newEvents) (oldScope, oldEvents) ->
                (oldScope <> newScope, oldEvents <> newEvents)
            )
            addr
            (eventScope, event :| [])
            byCarrier
        )

atomEventScope ::
  ScopedAtomEvents ->
  AtomEvent ->
  RelationalScope
atomEventScope scopedEvents event =
  IntMap.findWithDefault
    mempty
    (atomIdKey (aeAtomId event))
    (saeAtomScopeByAtom scopedEvents)
