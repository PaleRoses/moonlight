{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Core.Delta.Emit
  ( CarrierEmitSpec (..),
    emitCarrierDelta,
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( RelationalOrigin,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.FiniteLattice
  ( SupportBasis
  )

type CarrierEmitSpec :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data CarrierEmitSpec ctx carrier prop boundary evidence payload semanticPayload =
  CarrierEmitSpec
    { cesAddrOf :: payload -> CarrierAddr ctx carrier prop,
      cesSupportOf :: payload -> SupportBasis ctx,
      cesBoundaryOf :: payload -> boundary,
      cesEvidenceOf :: payload -> evidence,
      cesOriginOf :: payload -> RelationalOrigin ctx carrier prop,
      cesScopeOf :: payload -> RelationalScope,
      cesRowsOf :: payload -> RowDelta,
      cesPayloadOf :: payload -> semanticPayload
    }

emitCarrierDelta ::
  CarrierEmitSpec ctx carrier prop boundary evidence payload semanticPayload ->
  RelationalCarrierTime ctx ->
  payload ->
  RelationalCarrierDeltaP ctx carrier prop boundary evidence semanticPayload
emitCarrierDelta spec eventTime payload =
  RelationalCarrierDelta
    { deAddr = cesAddrOf spec payload,
      deTime = eventTime,
      deSupport = cesSupportOf spec payload,
      deBoundary = cesBoundaryOf spec payload,
      deEvidence = cesEvidenceOf spec payload,
      deOrigin = cesOriginOf spec payload,
      deScope = cesScopeOf spec payload,
      deRows = cesRowsOf spec payload,
      dePayload = cesPayloadOf spec payload
    }
{-# INLINE emitCarrierDelta #-}
