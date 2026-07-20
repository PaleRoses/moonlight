module Moonlight.Flow.Runtime.Topology.Routing.Events
  ( quotientPatchEvents,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Moonlight.Core
  ( atomIdKey,
  )
import Moonlight.Flow.Model.Delta
  ( AtomEvent (..),
    QuotientPatch (..),
    ScopedAtomEvents (..),
    atomPatchRows
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( RuntimeRouting,
    runtimeRoutingAtomSubscribers,
  )

quotientPatchEvents ::
  RuntimeRouting ctx prop ->
  QuotientPatch ->
  ScopedAtomEvents
quotientPatchEvents routing patch =
  let eventEntries =
        IntMap.foldrWithKey emitAtomEventEntries [] (qpEvents patch)
   in ScopedAtomEvents
        { saeScope = qpScope patch,
          saeAtomScopeByAtom =
            IntMap.fromListWith
              (<>)
              [ (atomIdKey (aeAtomId event), scopeValue)
              | (_sourceAtomKey, event, Just scopeValue) <- eventEntries
              ],
          saeTouchScopeByAtom =
            IntMap.fromListWith
              (<>)
              [ (sourceAtomKey, scopeValue)
              | (sourceAtomKey, _event, Just scopeValue) <- eventEntries
              ],
          saeEvents = fmap (\(_sourceAtomKey, event, _scopeValue) -> event) eventEntries
        }
  where
    emitAtomEventEntries atomKey atomPatch events =
      foldr
        ( \(queryId, atomId) accumulatedEvents ->
            ( atomKey,
              AtomEvent
                { aeQueryId = queryId,
                  aeAtomId = atomId,
                  aeRows = atomPatchRows atomPatch
                },
              IntMap.lookup atomKey (qpAtomScopeByAtom patch)
            )
              : accumulatedEvents
        )
        events
        (IntMap.findWithDefault [] atomKey (runtimeRoutingAtomSubscribers routing))
