{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Store.Engine.Read
  ( CarrierReadFrontier (..),
    CarrierReadCapability,
    CarrierArrangedDelta (..),
    CarrierHeldReads (..),
    emptyCarrierHeldReads,
    insertCarrierHeldRead,
    carrierHeldReadsFromList,
    carrierReadFrontierFromTime,
    carrierReadFrontierCompatible,
    carrierReadCapabilityFromTime,
    carrierReadCapabilityFromFrontier,
    carrierReadCapabilityFrontier,
    carrierReadCapabilityBoundaryDigest,
    downgradeCarrierReadCapability,
    carrierTraceEntryAfterReadFrontier,
    readCarrierSince,
  )
where

import Data.Foldable qualified as Foldable
import Data.Bifunctor
  ( first,
  )
import Data.IntSet qualified as IntSet
import Data.Kind
  ( Type,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( OriginEvent (OriginCompacted),
    RelationalOrigin (..),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( relationalTimeFrontierStamp,
    relationalTimeLiveEpoch,
    relationalTimeQuotientEpoch,
  )
import Moonlight.Flow.Carrier.Store.Core.Error
  ( CarrierStoreError (..),
  )
import Moonlight.Flow.Carrier.Store.Core.Read
import Moonlight.Flow.Carrier.Store.Core.Runtime
  ( CarrierStoreRuntime (..),
  )
import Moonlight.Flow.Carrier.Store.Core.State
  ( CarrierStore,
    CarrierTraceEntry (..),
    traceIdKey,
  )
import Moonlight.Flow.Carrier.Store.Journal.Trace
  ( carrierTraceSliceSince,
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange,
    addMultiplicityChange
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchChangeMap,
    plainRowPatchFromChangeMap
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )

type CarrierReadRows :: Type -> Type -> Type -> Type
data CarrierReadRows ctx carrier prop = CarrierReadRows
  { crrTraceIds :: !IntSet.IntSet,
    crrRows :: !(Map.Map RowTupleKey MultiplicityChange)
  }

readCarrierSince ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierStoreRuntime ctx boundary ->
  CarrierReadCapability ctx carrier prop ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either (CarrierStoreError ctx carrier prop boundary evidence) CarrierArrangedDelta
readCarrierSince runtime capability store = do
  traceEntries <-
    first CarrierStoreTraceMutationFailed $
      carrierTraceSliceSince
        (crcFrontier capability)
        (crcAddr capability)
        store
  readRows <-
    Foldable.foldlM
      (collectReadableEntry runtime capability)
      emptyCarrierReadRows
      traceEntries
  pure
    CarrierArrangedDelta
      { cadRows = plainRowPatchFromChangeMap (crrRows readRows),
        cadTraceIds = crrTraceIds readRows
      }
{-# INLINE readCarrierSince #-}

emptyCarrierReadRows :: CarrierReadRows ctx carrier prop
emptyCarrierReadRows =
  CarrierReadRows
    { crrTraceIds = IntSet.empty,
      crrRows = Map.empty
    }
{-# INLINE emptyCarrierReadRows #-}

collectReadableEntry ::
  CarrierStoreRuntime ctx boundary ->
  CarrierReadCapability ctx carrier prop ->
  CarrierReadRows ctx carrier prop ->
  CarrierTraceEntry ctx carrier prop boundary evidence ->
  Either (CarrierStoreError ctx carrier prop boundary evidence) (CarrierReadRows ctx carrier prop)
collectReadableEntry runtime capability acc entry
  | not (carrierTraceEntryAfterReadFrontier (crcFrontier capability) entry) =
      Right acc
  | traceEntryCompacted entry =
      Left
        ( CarrierStoreReadCompacted
            (crcAddr capability)
            (crcFrontier capability)
            (cteId entry)
        )
  | actualBoundaryDigest /= crcBoundaryDigest capability =
      Left
        ( CarrierStoreReadBoundaryChanged
            (crcAddr capability)
            (crcBoundaryDigest capability)
            actualBoundaryDigest
            (cteId entry)
        )
  | otherwise =
      Right
        CarrierReadRows
          { crrTraceIds =
              IntSet.insert (traceIdKey (cteId entry)) (crrTraceIds acc),
            crrRows =
              appendReadableEntryRows entry (crrRows acc)
          }
  where
    actualBoundaryDigest =
      csrBoundaryDigest runtime (deBoundary (cteDelta entry))
{-# INLINE collectReadableEntry #-}

appendReadableEntryRows ::
  CarrierTraceEntry ctx carrier prop boundary evidence ->
  Map.Map RowTupleKey MultiplicityChange ->
  Map.Map RowTupleKey MultiplicityChange
appendReadableEntryRows entry rows =
  Map.foldlWithKey'
    ( \acc rowValue multiplicity ->
        Map.insertWith addMultiplicityChange rowValue multiplicity acc
    )
    rows
    (plainRowPatchChangeMap (deRows (cteDelta entry)))
{-# INLINE appendReadableEntryRows #-}

carrierTraceEntryAfterReadFrontier ::
  CarrierReadFrontier ->
  CarrierTraceEntry ctx carrier prop boundary evidence ->
  Bool
carrierTraceEntryAfterReadFrontier frontier entry =
  let timeValue =
        deTime (cteDelta entry)
   in relationalTimeQuotientEpoch timeValue == crfQuotientEpoch frontier
        && relationalTimeLiveEpoch timeValue == crfLiveEpoch frontier
        && relationalTimeFrontierStamp timeValue > crfFrontierStamp frontier
{-# INLINE carrierTraceEntryAfterReadFrontier #-}

traceEntryCompacted ::
  CarrierTraceEntry ctx carrier prop boundary evidence ->
  Bool
traceEntryCompacted =
  (== OriginCompacted) . roEvent . deOrigin . cteDelta
{-# INLINE traceEntryCompacted #-}
