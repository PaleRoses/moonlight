{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Engine.Project
  ( CarrierProjectState (..),
    CarrierProjectError (..),
    carrierProjectOp,
    localDeltaToCarrierDeltas,
  )
where

import Data.Kind (Type)
import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
    CarrierAddressBook (..),
    queryAtomCarrier,
    queryBagCarrier,
    queryRootCarrier,
    querySeparatorCarrier,
  )
import Moonlight.Differential.Carrier.Address
  ( carrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDeltaP (..),
    RelationalCarrierDelta,
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchNull,
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( OriginEvent (OriginLocal), RelationalOrigin (..), emptyDerivationRoute,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Model.Event
  ( LocalRelationalAddr (..),
    LocalRelationalEvent (..),
    LocalRelationalSlice (..),
  )
import Moonlight.Delta.Operator
  ( OpResult (..),
    Operator (..),
    noOutput,
  )
import Moonlight.Delta.Time
  ( Timed (..),
  )
import Moonlight.FiniteLattice
  ( SupportBasis
  )

type CarrierProjectState :: Type -> Type -> Type -> Type -> Type
data CarrierProjectState ctx prop boundary evidence = CarrierProjectState
  { cpsAddressBook :: !(CarrierAddressBook ctx prop),
    cpsSupportOf :: LocalRelationalEvent -> LocalRelationalSlice -> SupportBasis ctx,
    cpsBoundaryOf :: QueryId -> Carrier -> boundary,
    cpsEvidenceOf :: LocalRelationalEvent -> LocalRelationalSlice -> evidence
  }

type CarrierProjectError :: Type
data CarrierProjectError
  = CarrierProjectEmpty
  deriving stock (Eq, Ord, Show)

carrierProjectOp ::
  Operator
    (RelationalCarrierTime ctx)
    (CarrierProjectState ctx prop boundary evidence)
    LocalRelationalEvent
    (RelationalCarrierDelta ctx Carrier prop boundary evidence)
    CarrierProjectError
carrierProjectOp =
  Operator
    { opStep =
        \stateValue timedDelta ->
          Right
            OpResult
              { orState = stateValue,
                orEmit =
                  fmap
                    (\carrierDelta -> Timed (deTime carrierDelta) carrierDelta)
                    (localDeltaToCarrierDeltas (timedAt timedDelta) stateValue (timedValue timedDelta))
              },
      opFlush =
        \stateValue ->
          Right (noOutput stateValue)
    }

localDeltaToCarrierDeltas ::
  RelationalCarrierTime ctx ->
  CarrierProjectState ctx prop boundary evidence ->
  LocalRelationalEvent ->
  [RelationalCarrierDelta ctx Carrier prop boundary evidence]
localDeltaToCarrierDeltas eventTime stateValue delta =
  let slice = lraSlice (lreAddr delta)
      rows = lreRows delta
   in if plainRowPatchNull rows
        then []
        else [carrierDeltaFor slice rows]
  where
    queryId =
      lraQueryId (lreAddr delta)

    carrierForSlice slice =
      case slice of
        LocalRootSlice ->
          queryRootCarrier queryId
        LocalAtomSlice atomId ->
          queryAtomCarrier queryId atomId
        LocalBagSlice bagId ->
          queryBagCarrier queryId bagId
        LocalSeparatorSlice child parent ->
          querySeparatorCarrier queryId child parent

    carrierDeltaFor slice rows =
      let carrier = carrierForSlice slice
          address =
            carrierAddr
              (cabContextOfQuery (cpsAddressBook stateValue) queryId)
              (cabPropOfQuery (cpsAddressBook stateValue) queryId)
              carrier
       in RelationalCarrierDelta
            { deAddr = address,
              deTime = eventTime,
              deSupport = cpsSupportOf stateValue delta slice,
              deBoundary = cpsBoundaryOf stateValue queryId carrier,
              deEvidence = cpsEvidenceOf stateValue delta slice,
              deRows = rows,
              dePayload = (),
              deOrigin = RelationalOrigin {roEvent = OriginLocal queryId, roRoute = emptyDerivationRoute},
              deScope = lreScope delta
            }
