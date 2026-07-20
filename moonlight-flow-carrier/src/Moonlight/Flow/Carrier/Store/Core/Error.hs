{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Store.Core.Error
  ( CarrierReplayField (..),
    CarrierSummaryError (..),
    CarrierStoreError (..),
  )
where

import Data.IntSet
  ( IntSet,
  )
import Data.List.NonEmpty
  ( NonEmpty,
  )
import Data.Kind
  ( Type,
  )
import Data.Set
  ( Set,
  )
import Moonlight.Differential.Trace.Compact
  ( PartitionedPrefixCompactionError,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    RelationalRuntimeEpoch,
  )
import Moonlight.Flow.Carrier.Store.Core.Read
  ( CarrierReadFrontier,
  )
import Moonlight.Flow.Carrier.Store.Core.State
  ( CarrierTraceIndexError,
  )
import Moonlight.Differential.Trace.Id
  ( TraceId,
  )
import Moonlight.Differential.Trace.Indexed
  ( IndexedTraceError,
  )
import Moonlight.Delta.Signed
  ( Multiplicity,
    MultiplicityChange
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Storage.Relation
  ( RelationPatchError,
  )
import Moonlight.FiniteLattice
  ( ContextLatticeLookupError
  )

type CarrierReplayField :: Type
data CarrierReplayField
  = CarrierReplayTrace
  | CarrierReplayCurrent
  | CarrierReplayIndexes
  | CarrierReplayFacts
  deriving stock (Eq, Ord, Show, Read)

data CarrierSummaryError ctx
  = CarrierSummaryMissingVisibleCutoff !ctx
  | CarrierSummaryNonSingletonVisibleCutoff !ctx
  | CarrierSummaryLatticeLookupFailed !(ContextLatticeLookupError ctx)
  deriving stock (Eq, Show)

type CarrierStoreError :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierStoreError ctx carrier prop boundary evidence
  = CarrierStoreDeltaTimeMismatch
      !(RelationalCarrierTime ctx)
      !(RelationalCarrierTime ctx)
  | CarrierStoreTraceMutationFailed
      !IndexedTraceError
  | CarrierStoreRowMultiplicityUnderflow
      !(CarrierAddr ctx carrier prop)
      !RowTupleKey
      !Multiplicity
      !MultiplicityChange
  | CarrierStoreFactContributionUnderflow
      !(CarrierAddr ctx carrier prop)
      !RowTupleKey
      !Multiplicity
      !MultiplicityChange
  | CarrierStoreFactProjectionSpliceInvalid
      !(CarrierAddr ctx carrier prop)
  | CarrierStoreLatticeLookupFailed
      !(ContextLatticeLookupError ctx)
  | CarrierStoreReadFrontierOutsideRuntime
      !(CarrierAddr ctx carrier prop)
      !CarrierReadFrontier
      !CarrierReadFrontier
  | CarrierStoreReadCompacted
      !(CarrierAddr ctx carrier prop)
      !CarrierReadFrontier
      !TraceId
  | CarrierStoreReadBoundaryChanged
      !(CarrierAddr ctx carrier prop)
      !StableDigest128
      !StableDigest128
      !TraceId
  | CarrierStoreRelationProjectionBuildFailed
      !(CarrierAddr ctx carrier prop)
      !StableDigest128
      !RelationPatchError
  | CarrierStoreCompactionPinnedTraceIds !IntSet
  | CarrierStoreCompactionPendingWorkBeforeFrontier !(Set (RelationalCarrierTime ctx))
  | CarrierStoreCompactionWouldInvalidateHeldRead
      !(CarrierAddr ctx carrier prop)
      !CarrierReadFrontier
      !TraceId
  | CarrierStoreCompactionMissingVisibleCutoff !ctx
  | CarrierStorePrefixCompactionFailed
      !( PartitionedPrefixCompactionError
           ctx
           RelationalRuntimeEpoch
           RelationalPhase
           (CarrierSummaryError ctx)
       )
  | CarrierStoreReplayTraceKeyMismatch !Int !TraceId
  | CarrierStoreReplayFieldMismatch !CarrierReplayField
  | CarrierStoreTraceIndexesInvalid
      !(NonEmpty (CarrierTraceIndexError ctx carrier prop))
  deriving stock (Eq, Show)
