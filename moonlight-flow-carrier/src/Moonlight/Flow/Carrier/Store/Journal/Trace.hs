{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Store.Journal.Trace
  ( TraceId,
    traceIdKey,
    initialTraceId,
    CarrierTraceEntry (..),
    CarrierTrace,
    CarrierTraceIndexes (..),
    CarrierTraceIndexError (..),
    CarrierTraceReverseIndexMetrics (..),
    carrierTraceIndexOps,
    emptyCarrierTrace,
    insertCarrierTraceEntry,
    deleteCarrierTraceEntryAt,
    carrierTraceSlice,
    carrierTraceSliceSince,
    carrierTraceKeysSince,
    carrierTraceForContext,
    carrierTraceReverseIndexMetrics,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( LiveEpoch,
    QuotientEpoch,
  )
import Moonlight.Differential.Index.IntSet
  ( deleteIntSetIndex,
    deleteMapIntSetIndex,
    insertIntSetIndex,
    insertMapIndex,
    intSetAxisMembers,
  )
import Moonlight.Differential.Time
  ( FrontierStamp,
  )
import Moonlight.Differential.Trace.Indexed
  ( IndexedTraceError,
    TraceIndexOps (..),
    emptyIndexedTrace,
    indexedTraceEntriesForKeys,
    indexedTraceEntriesForKeysChecked,
    insertIndexedTraceEntry,
    deleteIndexedTraceEntryAt,
    itIndexes,
  )
import Moonlight.Differential.Trace.ReadIndex
  ( TimeFrontier (..),
    emptyTimeIndex,
    insertTimeIndex,
    deleteTimeIndex,
    sliceTimeIndexAfter,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    caProp,
    caCarrier,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( relationalTimeFrontierStamp,
    relationalTimeLiveEpoch,
    relationalTimeQuotientEpoch,
  )
import Moonlight.Flow.Carrier.Store.Core.Read
  ( CarrierReadFrontier (..),
  )
import Moonlight.Flow.Carrier.Store.Core.State
import Moonlight.Flow.Model.Scope
  ( WitnessReverseIndex (..),
    scopeDeps,
    scopeResults,
    scopeRoots,
    scopeTopo,
  )

emptyCarrierTrace :: CarrierTrace ctx carrier prop boundary evidence
emptyCarrierTrace =
  emptyIndexedTrace initialTraceId emptyCarrierTraceIndexes

emptyCarrierTraceIndexes :: CarrierTraceIndexes ctx carrier prop
emptyCarrierTraceIndexes =
  CarrierTraceIndexes
    { ctiByAddr = Map.empty,
      ctiByAddrFrontier = emptyTimeIndex,
      ctiByContext = Map.empty,
      ctiByCarrier = Map.empty,
      ctiByProp = Map.empty,
      ctiByDep = WitnessReverseIndex IntMap.empty,
      ctiByTopo = WitnessReverseIndex IntMap.empty,
      ctiByRoot = WitnessReverseIndex IntMap.empty,
      ctiByResult = WitnessReverseIndex IntMap.empty,
      ctiByOrigin = Map.empty
    }

carrierTraceIndexOps ::
  (Ord ctx, Ord carrier, Ord prop) =>
  TraceIndexOps
    (CarrierTraceEntry ctx carrier prop boundary evidence)
    (CarrierTraceIndexes ctx carrier prop)
    (CarrierTraceIndexError ctx carrier prop)
carrierTraceIndexOps =
  TraceIndexOps
    { tioEntryId = cteId,
      tioEmptyIndexes = emptyCarrierTraceIndexes,
      tioInsertIndexes = insertCarrierTraceIndexes,
      tioDeleteIndexes = deleteCarrierTraceIndexes,
      tioValidateIndexes = validateCarrierTraceIndexes
    }

insertCarrierTraceEntry ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierTraceEntry ctx carrier prop boundary evidence ->
  CarrierTrace ctx carrier prop boundary evidence ->
  Either IndexedTraceError (CarrierTrace ctx carrier prop boundary evidence)
insertCarrierTraceEntry =
  insertIndexedTraceEntry carrierTraceIndexOps

deleteCarrierTraceEntryAt ::
  (Ord ctx, Ord carrier, Ord prop) =>
  TraceId ->
  CarrierTrace ctx carrier prop boundary evidence ->
  Either IndexedTraceError (CarrierTrace ctx carrier prop boundary evidence)
deleteCarrierTraceEntryAt traceId =
  deleteIndexedTraceEntryAt carrierTraceIndexOps (traceIdKey traceId)

insertCarrierTraceIndexes ::
  (Ord ctx, Ord carrier, Ord prop) =>
  TraceId ->
  CarrierTraceEntry ctx carrier prop boundary evidence ->
  CarrierTraceIndexes ctx carrier prop ->
  CarrierTraceIndexes ctx carrier prop
insertCarrierTraceIndexes traceId traceEntry indexes =
  let traceKey =
        traceIdKey traceId
      traceSingleton =
        IntSet.singleton traceKey
      delta =
        cteDelta traceEntry
      addr =
        deAddr delta
   in indexes
        { ctiByAddr =
            insertMapIndex addr traceSingleton (ctiByAddr indexes),
          ctiByAddrFrontier =
            insertTimeIndex
              addr
              (carrierTraceEpochKey delta)
              (carrierTraceStamp delta)
              traceSingleton
              (ctiByAddrFrontier indexes),
          ctiByContext =
            insertMapIndex (caContext addr) traceSingleton (ctiByContext indexes),
          ctiByCarrier =
            insertMapIndex (caCarrier addr) traceSingleton (ctiByCarrier indexes),
          ctiByProp =
            insertMapIndex (caProp addr) traceSingleton (ctiByProp indexes),
          ctiByDep =
            insertWitnessReverseIndex (scopeDeps (deScope delta)) traceSingleton (ctiByDep indexes),
          ctiByTopo =
            insertWitnessReverseIndex (scopeTopo (deScope delta)) traceSingleton (ctiByTopo indexes),
          ctiByRoot =
            insertWitnessReverseIndex (scopeRoots (deScope delta)) traceSingleton (ctiByRoot indexes),
          ctiByResult =
            insertWitnessReverseIndex (scopeResults (deScope delta)) traceSingleton (ctiByResult indexes),
          ctiByOrigin =
            insertMapIndex (deOrigin delta) traceSingleton (ctiByOrigin indexes)
        }

deleteCarrierTraceIndexes ::
  (Ord ctx, Ord carrier, Ord prop) =>
  TraceId ->
  CarrierTraceEntry ctx carrier prop boundary evidence ->
  CarrierTraceIndexes ctx carrier prop ->
  CarrierTraceIndexes ctx carrier prop
deleteCarrierTraceIndexes traceId traceEntry indexes =
  let traceKey =
        traceIdKey traceId
      delta =
        cteDelta traceEntry
      addr =
        deAddr delta
   in indexes
        { ctiByAddr =
            deleteMapIntSetIndex addr traceKey (ctiByAddr indexes),
          ctiByAddrFrontier =
            deleteTimeIndex
              addr
              (carrierTraceEpochKey delta)
              (carrierTraceStamp delta)
              traceKey
              (ctiByAddrFrontier indexes),
          ctiByContext =
            deleteMapIntSetIndex (caContext addr) traceKey (ctiByContext indexes),
          ctiByCarrier =
            deleteMapIntSetIndex (caCarrier addr) traceKey (ctiByCarrier indexes),
          ctiByProp =
            deleteMapIntSetIndex (caProp addr) traceKey (ctiByProp indexes),
          ctiByDep =
            deleteWitnessReverseIndex (scopeDeps (deScope delta)) traceKey (ctiByDep indexes),
          ctiByTopo =
            deleteWitnessReverseIndex (scopeTopo (deScope delta)) traceKey (ctiByTopo indexes),
          ctiByRoot =
            deleteWitnessReverseIndex (scopeRoots (deScope delta)) traceKey (ctiByRoot indexes),
          ctiByResult =
            deleteWitnessReverseIndex (scopeResults (deScope delta)) traceKey (ctiByResult indexes),
          ctiByOrigin =
            deleteMapIntSetIndex (deOrigin delta) traceKey (ctiByOrigin indexes)
        }

validateCarrierTraceIndexes ::
  (Ord ctx, Ord carrier, Ord prop) =>
  IntMap (CarrierTraceEntry ctx carrier prop boundary evidence) ->
  CarrierTraceIndexes ctx carrier prop ->
  [CarrierTraceIndexError ctx carrier prop]
validateCarrierTraceIndexes entries actual =
  let expected =
        IntMap.foldl'
          (\indexes entry -> insertCarrierTraceIndexes (cteId entry) entry indexes)
          emptyCarrierTraceIndexes
          entries
   in if expected == actual
        then []
        else [CarrierTraceIndexesMismatch expected actual]

carrierTraceSlice ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierStore ctx carrier prop boundary evidence ->
  [CarrierTraceEntry ctx carrier prop boundary evidence]
carrierTraceSlice addr indexState =
  indexedTraceEntriesForKeys
    (Map.findWithDefault IntSet.empty addr (ctiByAddr (itIndexes (cstTrace indexState))))
    (cstTrace indexState)

carrierTraceSliceSince ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierReadFrontier ->
  CarrierAddr ctx carrier prop ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either IndexedTraceError [CarrierTraceEntry ctx carrier prop boundary evidence]
carrierTraceSliceSince frontier addr indexState =
  fmap IntMap.elems $
    indexedTraceEntriesForKeysChecked
      (carrierTraceKeysSince frontier addr (cstTrace indexState))
      (cstTrace indexState)

carrierTraceKeysSince ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierReadFrontier ->
  CarrierAddr ctx carrier prop ->
  CarrierTrace ctx carrier prop boundary evidence ->
  IntSet
carrierTraceKeysSince frontier addr traceValue =
  sliceTimeIndexAfter
    addr
    (carrierTraceReadFrontier frontier)
    (ctiByAddrFrontier (itIndexes traceValue))

carrierTraceReadFrontier ::
  CarrierReadFrontier ->
  TimeFrontier (QuotientEpoch, LiveEpoch) FrontierStamp
carrierTraceReadFrontier frontier =
  TimeFrontier
    { tfEpoch =
        (crfQuotientEpoch frontier, crfLiveEpoch frontier),
      tfStamp =
        crfFrontierStamp frontier
    }

carrierTraceForContext ::
  Ord ctx =>
  ctx ->
  CarrierStore ctx carrier prop boundary evidence ->
  [CarrierTraceEntry ctx carrier prop boundary evidence]
carrierTraceForContext contextValue indexState =
  indexedTraceEntriesForKeys
    (Map.findWithDefault IntSet.empty contextValue (ctiByContext (itIndexes (cstTrace indexState))))
    (cstTrace indexState)

carrierTraceEpochKey ::
  RelationalCarrierDeltaP ctx carrier prop boundary evidence payload ->
  (QuotientEpoch, LiveEpoch)
carrierTraceEpochKey delta =
  let timeValue =
        deTime delta
   in (relationalTimeQuotientEpoch timeValue, relationalTimeLiveEpoch timeValue)

carrierTraceStamp ::
  RelationalCarrierDeltaP ctx carrier prop boundary evidence payload ->
  FrontierStamp
carrierTraceStamp =
  relationalTimeFrontierStamp . deTime

carrierTraceReverseIndexMetrics ::
  CarrierTrace ctx carrier prop boundary evidence ->
  CarrierTraceReverseIndexMetrics
carrierTraceReverseIndexMetrics traceValue =
  let indexes =
        itIndexes traceValue
   in CarrierTraceReverseIndexMetrics
        { ctrimDepMembers = witnessReverseIndexMembers (ctiByDep indexes),
          ctrimTopoMembers = witnessReverseIndexMembers (ctiByTopo indexes),
          ctrimRootMembers = witnessReverseIndexMembers (ctiByRoot indexes),
          ctrimResultMembers = witnessReverseIndexMembers (ctiByResult indexes)
        }

insertWitnessReverseIndex ::
  IntSet ->
  IntSet ->
  WitnessReverseIndex witness ->
  WitnessReverseIndex witness
insertWitnessReverseIndex keys members (WitnessReverseIndex reverseIndex) =
  WitnessReverseIndex (insertIntSetIndex keys members reverseIndex)

deleteWitnessReverseIndex ::
  IntSet ->
  Int ->
  WitnessReverseIndex witness ->
  WitnessReverseIndex witness
deleteWitnessReverseIndex keys member (WitnessReverseIndex reverseIndex) =
  WitnessReverseIndex (deleteIntSetIndex keys member reverseIndex)

witnessReverseIndexMembers :: WitnessReverseIndex witness -> Int
witnessReverseIndexMembers (WitnessReverseIndex reverseIndex) =
  intSetAxisMembers reverseIndex
