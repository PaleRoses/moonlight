{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Runtime.Carrier.Emit.Factor
  ( FactorCarrierEmitSpec,
    factorCarrierEmitSpec,
    factorMaintenanceDeltas,
    factorSnapshotDeltas,
    factorNodeCarrierVisible,
  )
where

import Data.Map.Strict qualified as Map
import Data.Maybe
  ( mapMaybe,
  )
import Data.Vector qualified as Vector
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
    CarrierAddressBook,
    queryFactorAddr,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDeltaP,
  )
import Moonlight.Flow.Carrier.Core.Delta.Emit
  ( CarrierEmitSpec (..),
    emitCarrierDelta,
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( OriginEvent (OriginFactor),
    RelationalOrigin (..),
    emptyDerivationRoute,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Execution.Factor.Core
  ( Factor,
  )
import Moonlight.Flow.Execution.Factor.Types
  ( FactorCache,
    FactorEntry (..),
    factorCacheEntries,
    factorCacheLookup,
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( MaintenanceMetrics (..),
    NodeAction (..),
    NodeMaintenance (..),
  )
import Moonlight.Differential.Row.Patch
  ( ShapedPatch (..),
    plainRowPatchNull,
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
  )
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Runtime.Carrier.Emit.FactorPayload
  ( FactorCarrierPayload (..),
    factorDeltaMembershipRows,
    factorPayloadCarrier,
    factorPayloadNode,
    factorPayloadRelationalScope,
    factorPayloadRows,
    factorPayloadSchema,
    factorSnapshotMembershipRows,
  )
import Moonlight.Differential.Index.IndexedRows
  ( indexedRowsLayout,
  )
import Moonlight.FiniteLattice
  ( SupportBasis
  )

type FactorCarrierEmitSpec ctx prop boundary evidence =
  CarrierEmitSpec
    ctx
    Carrier
    prop
    boundary
    evidence
    (QueryId, FactorCarrierPayload)
    FactorCarrierPayload

factorCarrierEmitSpec ::
  CarrierAddressBook ctx prop ->
  (QueryId -> FactorCarrierPayload -> SupportBasis ctx) ->
  (QueryId -> Carrier -> [SlotId] -> boundary) ->
  (QueryId -> FactorCarrierPayload -> evidence) ->
  FactorCarrierEmitSpec ctx prop boundary evidence
factorCarrierEmitSpec addressBook supportOf boundaryOf evidenceOf =
  CarrierEmitSpec
    { cesAddrOf =
        \(queryId, payload) ->
          queryFactorAddr addressBook queryId (factorPayloadNode payload),
      cesSupportOf =
        \(queryId, payload) ->
          supportOf queryId payload,
      cesBoundaryOf =
        \(queryId, payload) ->
          boundaryOf
            queryId
            (factorPayloadCarrier queryId payload)
            (factorPayloadSchema payload),
      cesEvidenceOf =
        \(queryId, payload) ->
          evidenceOf queryId payload,
      cesOriginOf =
        \(queryId, payload) ->
          RelationalOrigin
            { roEvent = OriginFactor queryId (factorPayloadNode payload),
              roRoute = emptyDerivationRoute
            },
      cesScopeOf =
        \(_queryId, payload) ->
          factorPayloadRelationalScope payload,
      cesRowsOf =
        \(_queryId, payload) ->
          factorPayloadRows payload,
      cesPayloadOf =
        snd
    }
{-# INLINE factorCarrierEmitSpec #-}

factorMaintenanceDeltas ::
  FactorCarrierEmitSpec ctx prop boundary evidence ->
  RelationalCarrierTime ctx ->
  QueryId ->
  RelationalScope ->
  MaintenanceMetrics ->
  FactorCache ->
  [RelationalCarrierDeltaP ctx Carrier prop boundary evidence FactorCarrierPayload]
factorMaintenanceDeltas spec eventTime queryId dirtyKeys metrics cache =
  builtDeltas <> patchedDeltas
  where
    builtDeltas =
      mapMaybe
        (snapshotFromNode spec eventTime queryId dirtyKeys cache)
        [ node
          | (node, nodeMetrics) <- Map.toAscList (mmNodes metrics),
            nmAction nodeMetrics == NodeBuilt,
            factorNodeCarrierVisible node
        ]

    patchedDeltas =
      mapMaybe
        (patchFromNode spec eventTime queryId dirtyKeys cache)
        [ node
          | (node, nodeMetrics) <- Map.toAscList (mmNodes metrics),
            nmAction nodeMetrics == NodePatched,
            factorNodeCarrierVisible node
        ]
{-# INLINE factorMaintenanceDeltas #-}

factorSnapshotDeltas ::
  FactorCarrierEmitSpec ctx prop boundary evidence ->
  RelationalCarrierTime ctx ->
  QueryId ->
  RelationalScope ->
  FactorCache ->
  [RelationalCarrierDeltaP ctx Carrier prop boundary evidence FactorCarrierPayload]
factorSnapshotDeltas spec eventTime queryId dirtyKeys cache =
  [ carrierDeltaFromSnapshot spec eventTime queryId dirtyKeys node (feFactor entry)
    | (node, entry) <- factorCacheEntries cache,
      factorNodeCarrierVisible node
  ]
{-# INLINE factorSnapshotDeltas #-}

snapshotFromNode ::
  FactorCarrierEmitSpec ctx prop boundary evidence ->
  RelationalCarrierTime ctx ->
  QueryId ->
  RelationalScope ->
  FactorCache ->
  FactorNode ->
  Maybe (RelationalCarrierDeltaP ctx Carrier prop boundary evidence FactorCarrierPayload)
snapshotFromNode spec eventTime queryId dirtyKeys cache node = do
  entry <- factorCacheLookup node cache
  pure (carrierDeltaFromSnapshot spec eventTime queryId dirtyKeys node (feFactor entry))
{-# INLINE snapshotFromNode #-}

patchFromNode ::
  FactorCarrierEmitSpec ctx prop boundary evidence ->
  RelationalCarrierTime ctx ->
  QueryId ->
  RelationalScope ->
  FactorCache ->
  FactorNode ->
  Maybe (RelationalCarrierDeltaP ctx Carrier prop boundary evidence FactorCarrierPayload)
patchFromNode spec eventTime queryId dirtyKeys cache node = do
  entry <- factorCacheLookup node cache
  let !deltaRows =
        factorDeltaMembershipRows (feDelta entry)
      !schema =
        spdShape (feDelta entry)
  if plainRowPatchNull deltaRows
    then Nothing
    else
      let !payload =
            FactorCarrierPayload
              { fcpRelationalScope = dirtyKeys,
                fcpNode = node,
                fcpSchema = schema,
                fcpRows = deltaRows
              }
       in Just (emitCarrierDelta spec eventTime (queryId, payload))
{-# INLINE patchFromNode #-}

carrierDeltaFromSnapshot ::
  FactorCarrierEmitSpec ctx prop boundary evidence ->
  RelationalCarrierTime ctx ->
  QueryId ->
  RelationalScope ->
  FactorNode ->
  Factor ->
  RelationalCarrierDeltaP ctx Carrier prop boundary evidence FactorCarrierPayload
carrierDeltaFromSnapshot spec eventTime queryId dirtyKeys node factorValue =
  emitCarrierDelta
    spec
    eventTime
    ( queryId,
      FactorCarrierPayload
        { fcpRelationalScope = dirtyKeys,
          fcpNode = node,
          fcpSchema = Vector.toList (indexedRowsLayout factorValue),
          fcpRows = factorSnapshotMembershipRows factorValue
        }
    )
{-# INLINE carrierDeltaFromSnapshot #-}

factorNodeCarrierVisible :: FactorNode -> Bool
factorNodeCarrierVisible =
  not . factorNodeIsBagBelief
{-# INLINE factorNodeCarrierVisible #-}
