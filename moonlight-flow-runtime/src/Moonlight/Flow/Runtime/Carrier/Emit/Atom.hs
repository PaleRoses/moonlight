{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Runtime.Carrier.Emit.Atom
  ( AtomCarrierPayload (..),
    AtomCarrierEmitSpec,
    atomCarrierEmitSpec,
    atomEventDelta,
    atomEventDeltaAt,
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Core
  ( AtomId,
    QueryId,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
    CarrierAddressBook,
    queryAtomAddr,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Delta.Emit
  ( CarrierEmitSpec (..),
    emitCarrierDelta,
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( OriginEvent (OriginAtom),
    RelationalOrigin (..),
    emptyDerivationRoute,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Model.Delta
  ( AtomEvent (..)
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
  )
import Moonlight.FiniteLattice
  ( SupportBasis
  )

type AtomCarrierPayload :: Type
data AtomCarrierPayload = AtomCarrierPayload
  { acpScope :: !RelationalScope,
    acpEvent :: !AtomEvent
  }
  deriving stock (Eq, Show)

type AtomCarrierEmitSpec ctx prop boundary evidence =
  CarrierEmitSpec
    ctx
    Carrier
    prop
    boundary
    evidence
    AtomCarrierPayload
    ()

atomCarrierEmitSpec ::
  CarrierAddressBook ctx prop ->
  (AtomCarrierPayload -> SupportBasis ctx) ->
  (QueryId -> AtomId -> RowDelta -> boundary) ->
  (AtomCarrierPayload -> evidence) ->
  AtomCarrierEmitSpec ctx prop boundary evidence
atomCarrierEmitSpec addressBook supportOf boundaryOf evidenceOf =
  CarrierEmitSpec
    { cesAddrOf =
        \payload ->
          let event =
                acpEvent payload
           in queryAtomAddr addressBook (aeQueryId event) (aeAtomId event),
      cesSupportOf =
        supportOf,
      cesBoundaryOf =
        \payload ->
          let event =
                acpEvent payload
           in boundaryOf (aeQueryId event) (aeAtomId event) (aeRows event),
      cesEvidenceOf =
        evidenceOf,
      cesOriginOf =
        \payload ->
          let event =
                acpEvent payload
           in RelationalOrigin
                { roEvent = OriginAtom (aeQueryId event) (aeAtomId event),
                  roRoute = emptyDerivationRoute
                },
      cesScopeOf =
        acpScope,
      cesRowsOf =
        aeRows . acpEvent,
      cesPayloadOf =
        const ()
    }
{-# INLINE atomCarrierEmitSpec #-}

atomEventDelta ::
  AtomCarrierEmitSpec ctx prop boundary evidence ->
  RelationalCarrierTime ctx ->
  RelationalScope ->
  AtomEvent ->
  RelationalCarrierDeltaP ctx Carrier prop boundary evidence ()
atomEventDelta spec eventTime scope event =
  emitCarrierDelta
    spec
    eventTime
    AtomCarrierPayload
      { acpScope = scope,
        acpEvent = event
      }
{-# INLINE atomEventDelta #-}

atomEventDeltaAt ::
  AtomCarrierEmitSpec ctx prop boundary evidence ->
  RelationalCarrierTime ctx ->
  CarrierAddr ctx Carrier prop ->
  RelationalScope ->
  AtomEvent ->
  RelationalCarrierDelta ctx Carrier prop boundary evidence
atomEventDeltaAt spec eventTime addr scope event =
  let payload =
        AtomCarrierPayload
          { acpScope = scope,
            acpEvent = event
          }
   in RelationalCarrierDelta
        { deAddr = addr,
          deTime = eventTime,
          deSupport = cesSupportOf spec payload,
          deBoundary = cesBoundaryOf spec payload,
          deEvidence = cesEvidenceOf spec payload,
          deOrigin = cesOriginOf spec payload,
          deScope = cesScopeOf spec payload,
          deRows = cesRowsOf spec payload,
          dePayload = cesPayloadOf spec payload
        }
{-# INLINE atomEventDeltaAt #-}
