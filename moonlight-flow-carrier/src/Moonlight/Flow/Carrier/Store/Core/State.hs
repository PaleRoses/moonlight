{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Store.Core.State
  ( TraceId,
    traceIdKey,
    initialTraceId,
    CarrierCurrentFactEvidence (..),
    CarrierFactSeed (..),
    CarrierFactSeedRowKey,
    CarrierFactCell,
    CarrierFactCurrentCell,
    CarrierFactLedger (..),
    CarrierTraceEntry (..),
    carrierStoreSummaryEntryFromTraceEntry,
    CarrierStoreTouch (..),
    CarrierTraceReadIndex,
    CarrierTraceIndexes (..),
    CarrierTraceIndexError (..),
    CarrierTrace,
    CarrierTraceReverseIndexMetrics (..),
    CarrierCurrentRows (..),
    CarrierSnapshot (..),
    CarrierCurrentIndex (..),
    CarrierCurrentProjection (..),
    CarrierViews (..),
    CarrierStore (..),
  )
where

import Data.IntSet
  ( IntSet,
  )
import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Set
  ( Set,
  )
import Moonlight.Core
  ( LiveEpoch,
    QuotientEpoch,
  )
import Moonlight.Differential.Time
  ( FrontierStamp,
  )
import Moonlight.Differential.Trace.Id
  ( TraceId,
    initialTraceId,
    traceIdKey,
  )
import Moonlight.Differential.Trace.Indexed
  ( IndexedTrace,
  )
import Moonlight.Differential.Trace.ReadIndex
  ( TimeIndex,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    CarrierProp,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( RelationalOrigin,
  )
import Moonlight.Flow.Carrier.Core.Summary
  ( CarrierStoreSummaryEntry (..),
  )
import Moonlight.Flow.Carrier.Fact.Internal.LedgerIndex
  ( CarrierCurrentFactEvidence (..),
    CarrierFactCell,
    CarrierFactCurrentCell,
    CarrierFactLedger (..),
    CarrierFactSeed (..),
    CarrierFactSeedRowKey,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Flow.Model.Scope
  ( DepsWitness,
    RelationalScope,
    ResultsWitness,
    RootsWitness,
    TopoWitness,
    WitnessReverseIndex,
  )

type CarrierTraceEntry :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierTraceEntry ctx carrier prop boundary evidence = CarrierTraceEntry
  { cteId :: !TraceId,
    cteDelta :: !(RelationalCarrierDelta ctx carrier prop boundary evidence)
  }
  deriving stock (Eq, Show)

carrierStoreSummaryEntryFromTraceEntry ::
  CarrierTraceEntry ctx carrier prop boundary evidence ->
  CarrierStoreSummaryEntry ctx carrier prop boundary evidence
carrierStoreSummaryEntryFromTraceEntry entry =
  let delta =
        cteDelta entry
   in CarrierStoreSummaryEntry
        { csseAddr = deAddr delta,
          csseTime = deTime delta,
          csseBoundary = deBoundary delta,
          csseEvidence = deEvidence delta,
          csseOrigin = deOrigin delta
        }

type CarrierStoreTouch :: Type -> Type -> Type -> Type
data CarrierStoreTouch ctx carrier prop = CarrierStoreTouch
  { cstAddr :: !(CarrierAddr ctx carrier prop),
    cstContext :: !ctx,
    cstRelationalScope :: !RelationalScope
  }
  deriving stock (Eq, Show)

type CarrierTraceReadIndex :: Type -> Type -> Type -> Type
type CarrierTraceReadIndex ctx carrier prop =
  TimeIndex
    (CarrierAddr ctx carrier prop)
    (QuotientEpoch, LiveEpoch)
    FrontierStamp

type CarrierTraceIndexes :: Type -> Type -> Type -> Type
data CarrierTraceIndexes ctx carrier prop = CarrierTraceIndexes
  { ctiByAddr :: !(Map (CarrierAddr ctx carrier prop) IntSet),
    ctiByAddrFrontier :: !(CarrierTraceReadIndex ctx carrier prop),
    ctiByContext :: !(Map ctx IntSet),
    ctiByCarrier :: !(Map carrier IntSet),
    ctiByProp :: !(Map (CarrierProp prop) IntSet),
    ctiByDep :: !(WitnessReverseIndex DepsWitness),
    ctiByTopo :: !(WitnessReverseIndex TopoWitness),
    ctiByRoot :: !(WitnessReverseIndex RootsWitness),
    ctiByResult :: !(WitnessReverseIndex ResultsWitness),
    ctiByOrigin :: !(Map (RelationalOrigin ctx carrier prop) IntSet)
  }
  deriving stock (Eq, Show)

type CarrierTraceIndexError :: Type -> Type -> Type -> Type
data CarrierTraceIndexError ctx carrier prop
  = CarrierTraceIndexesMismatch
      !(CarrierTraceIndexes ctx carrier prop)
      !(CarrierTraceIndexes ctx carrier prop)
  deriving stock (Eq, Show)

type CarrierTrace :: Type -> Type -> Type -> Type -> Type -> Type
type CarrierTrace ctx carrier prop boundary evidence =
  IndexedTrace
    (CarrierTraceEntry ctx carrier prop boundary evidence)
    (CarrierTraceIndexes ctx carrier prop)

type CarrierTraceReverseIndexMetrics :: Type
data CarrierTraceReverseIndexMetrics = CarrierTraceReverseIndexMetrics
  { ctrimDepMembers :: {-# UNPACK #-} !Int,
    ctrimTopoMembers :: {-# UNPACK #-} !Int,
    ctrimRootMembers :: {-# UNPACK #-} !Int,
    ctrimResultMembers :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show, Read)

type CarrierCurrentRows :: Type
data CarrierCurrentRows = CarrierCurrentRows
  { ccrRows :: !RowDelta
  }
  deriving stock (Eq, Show)

type CarrierSnapshot :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierSnapshot ctx carrier prop boundary evidence = CarrierSnapshot
  { csCurrentRows :: !CarrierCurrentRows,
    csLatestTrace :: !TraceId
  }
  deriving stock (Eq, Show)

type CarrierCurrentIndex :: Type -> Type -> Type -> Type
data CarrierCurrentIndex ctx carrier prop = CarrierCurrentIndex
  { ciCurrentByContext :: !(Map ctx (Set (CarrierAddr ctx carrier prop))),
    ciCurrentByCarrier :: !(Map carrier (Set (CarrierAddr ctx carrier prop))),
    ciCurrentByProp :: !(Map (CarrierProp prop) (Set (CarrierAddr ctx carrier prop)))
  }
  deriving stock (Eq, Show)

type CarrierCurrentProjection :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierCurrentProjection ctx carrier prop boundary evidence = CarrierCurrentProjection
  { ccpSnapshots :: !(Map (CarrierAddr ctx carrier prop) (CarrierSnapshot ctx carrier prop boundary evidence)),
    ccpIndexes :: !(CarrierCurrentIndex ctx carrier prop)
  }
  deriving stock (Eq, Show)

type CarrierViews :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierViews ctx carrier prop boundary evidence = CarrierViews
  { cvCurrent :: !(CarrierCurrentProjection ctx carrier prop boundary evidence),
    cvFacts :: !(CarrierFactLedger ctx carrier prop boundary evidence)
  }
  deriving stock (Eq, Show)

type CarrierStore :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierStore ctx carrier prop boundary evidence = CarrierStore
  { cstTrace :: !(CarrierTrace ctx carrier prop boundary evidence),
    cstViews :: !(CarrierViews ctx carrier prop boundary evidence)
  }
  deriving stock (Eq, Show)
