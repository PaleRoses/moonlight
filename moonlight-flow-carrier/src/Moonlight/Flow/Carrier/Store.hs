{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Store
  ( CarrierStore,
    CarrierStoreRuntime (..),
    CarrierStoreTouch (..),
    CarrierStoreError (..),
    TraceId,
    CarrierStoreSummaryEntry (..),
    CarrierStoreDiagnostics (..),
    CarrierSnapshot,
    carrierSnapshotRows,
    carrierSnapshotLatestTrace,
    lookupCarrierSnapshot,
    emptyCarrierStore,
    carrierStoreOperator,
    commitCarrierDelta,
    commitTimedCarrierDelta,
    carrierCurrentAddressesByContext,
    carrierCurrentAddressesByCarrier,
    carrierCurrentAddressesByProp,
    carrierCurrentAddresses,
    CarrierReadFrontier (..),
    CarrierReadCapability,
    CarrierArrangedDelta (..),
    CarrierHeldReads,
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
    readCarrierSince,
    compactCarrierStoreBefore,
    validateCarrierStore,
    carrierStoreDiagnostics,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Moonlight.Differential.Trace.Indexed
  ( itEntries,
  )
import Data.Kind
  ( Type,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    CarrierProp,
  )
import Moonlight.Flow.Carrier.Core.Summary
  ( CarrierStoreSummaryEntry (..),
  )
import Moonlight.Flow.Carrier.Store.Engine.Commit
  ( carrierStoreOperator,
    commitCarrierDelta,
    commitTimedCarrierDelta,
    emptyCarrierStore,
  )
import Moonlight.Flow.Carrier.Store.Engine.Compact
  ( compactCarrierStoreBefore,
  )
import Moonlight.Flow.Carrier.Store.Core.Error
  ( CarrierStoreError (..),
  )
import Moonlight.Flow.Carrier.Store.Core.Read
  ( CarrierArrangedDelta (..),
    CarrierHeldReads,
    CarrierReadCapability,
    CarrierReadFrontier (..),
    carrierHeldReadsFromList,
    carrierReadCapabilityBoundaryDigest,
    carrierReadCapabilityFromFrontier,
    carrierReadCapabilityFromTime,
    carrierReadCapabilityFrontier,
    carrierReadFrontierCompatible,
    carrierReadFrontierFromTime,
    downgradeCarrierReadCapability,
    emptyCarrierHeldReads,
    insertCarrierHeldRead,
  )
import Moonlight.Flow.Carrier.Store.Core.Runtime
  ( CarrierStoreRuntime (..),
  )
import Moonlight.Flow.Carrier.Store.Core.State
  ( CarrierSnapshot (..),
    CarrierStore (..),
    CarrierStoreTouch (..),
    TraceId,
    ctrimDepMembers,
    ctrimResultMembers,
    ctrimRootMembers,
    ctrimTopoMembers,
    ciCurrentByCarrier,
    ciCurrentByContext,
    ciCurrentByProp,
    ccpIndexes,
    ccpSnapshots,
    csCurrentRows,
    csLatestTrace,
    cstTrace,
    cstViews,
    cflSeeds,
    cvCurrent,
    cvFacts,
  )
import Moonlight.Flow.Carrier.Store.Projection.Current
  ( carrierCurrentRowsPlain,
  )
import Moonlight.Flow.Carrier.Store.Engine.Read
  ( readCarrierSince,
  )
import Moonlight.Flow.Carrier.Store.Engine.Replay
  ( validateCarrierStore,
  )
import Moonlight.Flow.Carrier.Store.Journal.Trace
  ( carrierTraceReverseIndexMetrics,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )

type CarrierStoreDiagnostics :: Type
data CarrierStoreDiagnostics = CarrierStoreDiagnostics
  { csdTraceEntries :: {-# UNPACK #-} !Int,
    csdCurrentAddresses :: {-# UNPACK #-} !Int,
    csdFactSeeds :: {-# UNPACK #-} !Int,
    csdTraceDepMembers :: {-# UNPACK #-} !Int,
    csdTraceTopoMembers :: {-# UNPACK #-} !Int,
    csdTraceRootMembers :: {-# UNPACK #-} !Int,
    csdTraceResultMembers :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

lookupCarrierSnapshot ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierStore ctx carrier prop boundary evidence ->
  Maybe (CarrierSnapshot ctx carrier prop boundary evidence)
lookupCarrierSnapshot addr store =
  Map.lookup addr (ccpSnapshots (cvCurrent (cstViews store)))
{-# INLINE lookupCarrierSnapshot #-}

carrierSnapshotRows ::
  CarrierSnapshot ctx carrier prop boundary evidence ->
  RowDelta
carrierSnapshotRows =
  carrierCurrentRowsPlain . csCurrentRows
{-# INLINE carrierSnapshotRows #-}

carrierSnapshotLatestTrace ::
  CarrierSnapshot ctx carrier prop boundary evidence ->
  TraceId
carrierSnapshotLatestTrace =
  csLatestTrace
{-# INLINE carrierSnapshotLatestTrace #-}

carrierCurrentAddressesByContext ::
  Ord ctx =>
  ctx ->
  CarrierStore ctx carrier prop boundary evidence ->
  Set (CarrierAddr ctx carrier prop)
carrierCurrentAddressesByContext contextValue store =
  Map.findWithDefault Set.empty contextValue (ciCurrentByContext (ccpIndexes (cvCurrent (cstViews store))))
{-# INLINE carrierCurrentAddressesByContext #-}

carrierCurrentAddressesByCarrier ::
  Ord carrier =>
  carrier ->
  CarrierStore ctx carrier prop boundary evidence ->
  Set (CarrierAddr ctx carrier prop)
carrierCurrentAddressesByCarrier carrierValue store =
  Map.findWithDefault Set.empty carrierValue (ciCurrentByCarrier (ccpIndexes (cvCurrent (cstViews store))))
{-# INLINE carrierCurrentAddressesByCarrier #-}

carrierCurrentAddressesByProp ::
  Ord (CarrierProp prop) =>
  CarrierProp prop ->
  CarrierStore ctx carrier prop boundary evidence ->
  Set (CarrierAddr ctx carrier prop)
carrierCurrentAddressesByProp propValue store =
  Map.findWithDefault Set.empty propValue (ciCurrentByProp (ccpIndexes (cvCurrent (cstViews store))))
{-# INLINE carrierCurrentAddressesByProp #-}

carrierCurrentAddresses ::
  CarrierStore ctx carrier prop boundary evidence ->
  Set (CarrierAddr ctx carrier prop)
carrierCurrentAddresses =
  Map.keysSet . ccpSnapshots . cvCurrent . cstViews
{-# INLINE carrierCurrentAddresses #-}

carrierStoreDiagnostics ::
  CarrierStore ctx carrier prop boundary evidence ->
  CarrierStoreDiagnostics
carrierStoreDiagnostics store =
  let reverseMetrics =
        carrierTraceReverseIndexMetrics (cstTrace store)
   in CarrierStoreDiagnostics
        { csdTraceEntries =
            IntMap.size (itEntries (cstTrace store)),
          csdCurrentAddresses =
            Map.size (ccpSnapshots (cvCurrent (cstViews store))),
          csdFactSeeds =
            IntMap.size (cflSeeds (cvFacts (cstViews store))),
          csdTraceDepMembers =
            ctrimDepMembers reverseMetrics,
          csdTraceTopoMembers =
            ctrimTopoMembers reverseMetrics,
          csdTraceRootMembers =
            ctrimRootMembers reverseMetrics,
          csdTraceResultMembers =
            ctrimResultMembers reverseMetrics
        }
{-# INLINE carrierStoreDiagnostics #-}
