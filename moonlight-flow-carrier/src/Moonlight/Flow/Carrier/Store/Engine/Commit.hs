module Moonlight.Flow.Carrier.Store.Engine.Commit
  ( CarrierStoreTouch (..),
    CarrierStoreError (..),
    CarrierStore,
    emptyCarrierStore,
    carrierStoreOperator,
    commitCarrierDelta,
    commitTimedCarrierDelta,
    commitCarrierTraceEntry,
    putCarrierTrace,
    putCarrierCurrentSnapshot,
    spliceCarrierAddressProjection,
  )
where

import Data.Bifunctor
  ( first,
  )
import Moonlight.Delta.Operator
  ( OpResult (..),
    Operator (..),
    noOutput,
  )
import Moonlight.Delta.Time
  ( Timed (..),
  )
import Moonlight.Differential.Trace.Indexed
  ( itNextId,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Carrier.Fact.Internal.LedgerIndex
  ( checkedSpliceCarrierFactAddress,
  )
import Moonlight.Flow.Carrier.Fact.Ledger
  ( applyCarrierFactTrace,
    deleteCarrierFactAddress,
    emptyCarrierFactLedger,
  )
import Moonlight.Flow.Carrier.Store.Core.Error
  ( CarrierStoreError (..),
  )
import Moonlight.Flow.Carrier.Store.Core.State
import Moonlight.Flow.Carrier.Store.Projection.Current
  ( applyCarrierCurrentProjection,
    emptyCarrierCurrentProjection,
    putCarrierCurrentSnapshotProjection,
    spliceCarrierCurrentProjection,
  )
import Moonlight.Flow.Carrier.Store.Journal.Trace
  ( emptyCarrierTrace,
    insertCarrierTraceEntry,
  )
import Moonlight.FiniteLattice
  ( ContextLattice
  )

emptyCarrierStore ::
  CarrierStore ctx carrier prop boundary evidence
emptyCarrierStore =
  CarrierStore
    { cstTrace = emptyCarrierTrace,
      cstViews =
        CarrierViews
          { cvCurrent = emptyCarrierCurrentProjection,
            cvFacts = emptyCarrierFactLedger
          }
    }
{-# INLINE emptyCarrierStore #-}

carrierStoreOperator ::
  (Ord ctx, Ord carrier, Ord prop) =>
  ContextLattice ctx ->
  Operator
    (RelationalCarrierTime ctx)
    (CarrierStore ctx carrier prop boundary evidence)
    (RelationalCarrierDelta ctx carrier prop boundary evidence)
    (CarrierStoreTouch ctx carrier prop)
    (CarrierStoreError ctx carrier prop boundary evidence)
carrierStoreOperator latticeValue =
  Operator
    { opStep =
        \stateValue timedDelta -> do
          nextState <- commitTimedCarrierDelta latticeValue timedDelta stateValue
          let delta = timedValue timedDelta
          Right
            OpResult
              { orState = nextState,
                orEmit =
                  [ Timed
                      (timedAt timedDelta)
                      CarrierStoreTouch
                        { cstAddr = deAddr delta,
                          cstContext = caContext (deAddr delta),
                          cstRelationalScope = deScope delta
                        }
                  ]
              },
      opFlush =
        \stateValue ->
          Right (noOutput stateValue)
    }
{-# INLINE carrierStoreOperator #-}

commitTimedCarrierDelta ::
  (Ord ctx, Ord carrier, Ord prop) =>
  ContextLattice ctx ->
  Timed
    (RelationalCarrierTime ctx)
    (RelationalCarrierDelta ctx carrier prop boundary evidence) ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (CarrierStore ctx carrier prop boundary evidence)
commitTimedCarrierDelta latticeValue timedDelta stateValue =
  let delta = timedValue timedDelta
   in if timedAt timedDelta == deTime delta
        then commitCarrierDelta latticeValue delta stateValue
        else Left (CarrierStoreDeltaTimeMismatch (timedAt timedDelta) (deTime delta))
{-# INLINE commitTimedCarrierDelta #-}

commitCarrierDelta ::
  (Ord ctx, Ord carrier, Ord prop) =>
  ContextLattice ctx ->
  RelationalCarrierDelta ctx carrier prop boundary evidence ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (CarrierStore ctx carrier prop boundary evidence)
commitCarrierDelta latticeValue delta stateValue =
  first CarrierStoreTraceMutationFailed (itNextId (cstTrace stateValue))
    >>= \traceId -> commitCarrierTraceEntry latticeValue traceId delta stateValue
{-# INLINE commitCarrierDelta #-}

commitCarrierTraceEntry ::
  (Ord ctx, Ord carrier, Ord prop) =>
  ContextLattice ctx ->
  TraceId ->
  RelationalCarrierDelta ctx carrier prop boundary evidence ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (CarrierStore ctx carrier prop boundary evidence)
commitCarrierTraceEntry latticeValue traceId delta stateValue = do
  let traceEntry =
        CarrierTraceEntry
          { cteId = traceId,
            cteDelta = delta
          }
  traceNext <-
    first CarrierStoreTraceMutationFailed $
      insertCarrierTraceEntry traceEntry (cstTrace stateValue)
  let
      stateWithTrace =
        stateValue {cstTrace = traceNext}
  applyCarrierCurrentStep traceEntry stateWithTrace
    >>= applyCarrierFactStep latticeValue traceEntry
{-# INLINE commitCarrierTraceEntry #-}

applyCarrierCurrentStep ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierTraceEntry ctx carrier prop boundary evidence ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (CarrierStore ctx carrier prop boundary evidence)
applyCarrierCurrentStep traceEntry stateValue = do
  nextCurrent <-
    applyCarrierCurrentProjection
      (cteId traceEntry)
      (cteDelta traceEntry)
      (cvCurrent (cstViews stateValue))
  pure
    stateValue
      { cstViews =
          (cstViews stateValue)
            { cvCurrent = nextCurrent
            }
      }
{-# INLINE applyCarrierCurrentStep #-}

applyCarrierFactStep ::
  (Ord ctx, Ord carrier, Ord prop) =>
  ContextLattice ctx ->
  CarrierTraceEntry ctx carrier prop boundary evidence ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (CarrierStore ctx carrier prop boundary evidence)
applyCarrierFactStep latticeValue traceEntry stateValue = do
  nextFacts <-
    applyCarrierFactTrace
      latticeValue
      traceEntry
      (ccpSnapshots (cvCurrent (cstViews stateValue)))
      (cvFacts (cstViews stateValue))
  pure
    stateValue
      { cstViews =
          (cstViews stateValue)
            { cvFacts = nextFacts
            }
      }
{-# INLINE applyCarrierFactStep #-}

putCarrierTrace ::
  CarrierTrace ctx carrier prop boundary evidence ->
  CarrierStore ctx carrier prop boundary evidence ->
  CarrierStore ctx carrier prop boundary evidence
putCarrierTrace traceValue stateValue =
  stateValue
    { cstTrace = traceValue
    }
{-# INLINE putCarrierTrace #-}

putCarrierCurrentSnapshot ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierSnapshot ctx carrier prop boundary evidence ->
  CarrierStore ctx carrier prop boundary evidence ->
  CarrierStore ctx carrier prop boundary evidence
putCarrierCurrentSnapshot addr snapshot stateValue =
  let nextCurrent =
        putCarrierCurrentSnapshotProjection addr snapshot (cvCurrent (cstViews stateValue))
   in stateValue
        { cstViews =
            (cstViews stateValue)
              { cvCurrent = nextCurrent
              }
        }
{-# INLINE putCarrierCurrentSnapshot #-}

spliceCarrierAddressProjection ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierStore ctx carrier prop boundary evidence ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (CarrierStore ctx carrier prop boundary evidence)
spliceCarrierAddressProjection addr localProjection stateValue =
  let nextCurrent =
        spliceCarrierCurrentProjection
          addr
          (cvCurrent (cstViews localProjection))
          (cvCurrent (cstViews stateValue))
      baseFacts =
        deleteCarrierFactAddress addr (cvFacts (cstViews stateValue))
   in case checkedSpliceCarrierFactAddress addr (cvFacts (cstViews localProjection)) baseFacts of
        Nothing ->
          Left (CarrierStoreFactProjectionSpliceInvalid addr)
        Just nextFacts ->
          Right
            stateValue
              { cstViews =
                  (cstViews stateValue)
                    { cvCurrent = nextCurrent,
                      cvFacts = nextFacts
                    }
              }
{-# INLINE spliceCarrierAddressProjection #-}
